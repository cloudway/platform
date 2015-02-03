/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container.plugin;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.OptionalInt;
import java.util.Set;
import java.util.stream.Stream;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Sets;

import jnr.constants.platform.Signal;
import com.cloudway.platform.common.Config;
import com.cloudway.platform.common.util.Etc;
import com.cloudway.platform.common.util.IO;
import com.cloudway.platform.common.util.IOBiConsumer;
import com.cloudway.platform.common.util.IOFunction;
import com.cloudway.platform.common.util.MoreFiles;

import static java.nio.file.StandardCopyOption.*;
import static java.util.stream.Collectors.*;
import static com.cloudway.platform.common.util.MoreCollectors.*;
import static com.cloudway.platform.common.util.MoreFiles.*;
import static com.cloudway.platform.common.util.Predicates.*;
import static com.cloudway.platform.common.util.Conditionals.*;

public class Cgroup
{
    public static final String CG_ROOT;
    public static final String DEFAULT_CG_ROOT = "/cloudway";

    public static final Set<String> CG_SUBSYSTEMS;
    public static final String DEFAULT_CG_SUBSYSTEMS = "cpu,cpuacct,memory,net_cls,freezer";

    public static final Map<String, Path>   CG_PATHS;
    public static final Map<String, Object> CG_PARAMETERS;

    public static final boolean enabled;

    private static final Path CGRULES = Paths.get("/etc/cgrules.conf");
    private static final Path CGCONFIG = Paths.get("/etc/cgconfig.conf");

    public static final int NET_CLS_MAJOR  = 1;
    public static final int UID_WRAPAROUND = 65536;

    static {
        Config config = Config.getDefault();
        CG_ROOT = config.get("CGROUP_ROOT", DEFAULT_CG_ROOT);
        CG_SUBSYSTEMS = ImmutableSet.copyOf(config.get("CGROUP_SUBSYSTEMS", DEFAULT_CG_SUBSYSTEMS).split(","));

        Map<String,Path>   paths;
        Map<String,Object> params;

        try {
            paths  = init_cgpaths();
            params = init_cgparameters(paths);
        } catch (Exception ex) {
            // TODO: log warning
            paths  = ImmutableMap.of();
            params = ImmutableMap.of();
        }

        CG_PATHS = paths;
        CG_PARAMETERS = params;
        enabled = !CG_PATHS.isEmpty();
    }

    // fs_spec fs_file fs_vtype fs_mntopts fs_freq fs_passno
    private enum FS {
        SPEC(0), FILE(1), VTYPE(2), MOUNTS(3), FREQ(4), PASSNO(5);

        private final int index;

        FS(int index) {
            this.index = index;
        }

        public String get(String[] fs) {
            return fs[index];
        }

        public Set<String> split(String[] fs) {
            return ImmutableSet.copyOf(fs[index].split(","));
        }
    }

    private static Map<String,Path> init_cgpaths()
        throws IOException
    {
        try (Stream<String> lines = Files.lines(Paths.get("/proc/mounts"), StandardCharsets.ISO_8859_1)) {
            return lines
                .map(line -> line.split("\\s+"))
                .filter(having(FS.VTYPE::get, is("cgroup")))
                .collect(ImmutableMap::<String,Path>builder,
                         (b, fs) ->
                             Sets.intersection(CG_SUBSYSTEMS, FS.MOUNTS.split(fs))
                                 .stream()
                                 .forEach(subsys -> b.put(subsys, root_cgpath(FS.FILE.get(fs)))),
                         (l, r) -> l.putAll(r.build()))
                .build();
        }
    }

    private static Path root_cgpath(String fs) {
        try {
            Path path = Paths.get(fs, CG_ROOT);
            if (!Files.exists(path))
                mkdir(path, 0755);
            return path;
        } catch (IOException ex) {
            throw new UncheckedIOException(ex);
        }
    }

    private static Map<String,Object> init_cgparameters(Map<String,Path> paths)
        throws IOException
    {
        ImmutableMap.Builder<String,Object> builder = ImmutableMap.builder();
        IO.forEach(paths, (subsys, path) -> {
            try (Stream<Path> files = MoreFiles.find(path, 1, subsys + ".*")) {
                files.sorted(Comparator.comparingInt(Cgroup::count_dots))
                     .forEach(file -> {
                         try {
                             String pval = chomp(file);
                             builder.put(file.getFileName().toString(), parse_cgparam(pval));
                         } catch (IOException ex) {
                             // ignore unreadable file
                         }
                     });
            }
        });
        return builder.build();
    }

    private static int count_dots(Path path) {
        String filename = path.getFileName().toString();
        int c = 0, len = filename.length();
        for (int i = 0; i < len; i++) {
            if (filename.charAt(i) == '.') {
                c++;
            }
        }
        return c;
    }

    private static Object parse_cgparam(String pval) {
        if (pval.indexOf('\n') == -1 && pval.indexOf(' ') == -1) {
            return pval;
        } else {
            return Arrays.stream(pval.split("\n")).collect(toImmutableSplittingMap(' '));
        }
    }

    @SuppressWarnings("unchecked")
    private static String format_cgparam(Object val) {
        if (val instanceof Map) {
            return ((Map<String,Object>)val).entrySet().stream()
                .map(e -> e.getKey() + " " + e.getValue())
                .collect(joining("\n"));
        } else {
            return String.valueOf(val);
        }
    }

    private final String user;
    private final int uid;
    private final Map<String, Path> cgpaths;

    public Cgroup(String user, int uid) {
        this.user = user;
        this.uid  = uid;

        cgpaths = CG_PATHS.entrySet().stream().collect(
            toImmutableMap(Map.Entry::getKey, e -> e.getValue().resolve(user)));
    }

    private Optional<Path> cgpath(String subsys) {
        return Optional.ofNullable(cgpaths.get(subsys)).filter(Files::exists);
    }

    /**
     * Create a cgroup namespace for the application container.
     */
    public void create(Map<String,Object> defaults)
        throws IOException
    {
        Map<String, Map<String,Object>> newcfg = new LinkedHashMap<>();
        Map<String, Object> to_store = new LinkedHashMap<>();

        // add the cgroup permissions:
        // { perm { task  { uid = 1000; gid = 1000; }
        //          admin { uid = root, gid = root; } } }
        newcfg.put("perm", pmap("task",  pmap("uid",  uid,   "gid",  uid),
                                "admin", pmap("uid", "root", "gid", "root")));

        // add the net_cls.classid
        int net_cls = (NET_CLS_MAJOR << 16) + (uid % UID_WRAPAROUND);
        newcfg.put("net_cls", pmap("net_cls.classid", net_cls));
        to_store.put("net_cls.classid", net_cls);

        CG_SUBSYSTEMS.forEach(s -> newcfg.computeIfAbsent(s, x -> new LinkedHashMap<>()));

        CG_PARAMETERS.keySet().forEach(k -> {
            Object v = defaults.get(k);
            if (v != null) {
                String subsys = k.substring(0, k.indexOf('.'));
                if (newcfg.containsKey(subsys)) {
                    newcfg.get(subsys).put(k, v);
                    to_store.put(k, v);
                }
            }
        });

        update_cgconfig(newcfg);
        update_cgrules(true);
        store(to_store);
        cgclassify();
    }

    private static Map<String,Object> pmap(Object... args) {
        ImmutableMap.Builder<String,Object> builder = ImmutableMap.builder();
        for (int i = 0; i < args.length; i += 2) {
            builder.put((String)args[i], args[i+1]);
        }
        return builder.build();
    }

    /**
     * Delete a cgroup namespace for the application container.
     */
    public void delete() throws IOException {
        update_cgconfig(null);
        update_cgrules(false);
        cgdelete();
    }

    /**
     * Fetch a parameter for a specific user.
     */
    public Optional<Object> fetch(String key) throws IOException {
        String subsys = key.substring(0, key.indexOf('.'));
        return IO.produce(() ->
            cgpath(subsys)
                .map(path -> path.resolve(key))
                .map(IOFunction.wrap(MoreFiles::chomp))
                .map(Cgroup::parse_cgparam));
    }

    public void store(Map<String,Object> vals) throws IOException {
        // recreate cgroup because it may lost during system boot
        cgcreate();

        IO.forEach(CG_PARAMETERS, (key, val) -> {
            val = vals.get(key);
            if (val != null) {
                String subsys = key.substring(0, key.indexOf('.'));
                Path path = cgpath(subsys).orElseThrow(
                    () -> new IOException("User does not exist in cgroups: " + user));
                writeText(path.resolve(key), format_cgparam(val));
            }
        });
    }

    public void freeze() throws IOException {
        store(pmap("freezer.state", "FROZEN"));
    }

    public void thaw() throws IOException {
        store(pmap("freezer.state", "THAWED"));
    }

    /**
     * Distribute this user's processes into their cgroup.
     */
    public void cgclassify() throws IOException {
        try (Stream<Task> ts = threads()) {
            String tasks =
                ts.filter(p -> p.uid == uid)
                  .map(p -> String.valueOf(p.tid))
                  .collect(joining("\n"));

            if (tasks.length() != 0) {
                IO.forEach(cgpaths, (subsys, path) -> writeText(path.resolve("tasks"), tasks));
            }
        }
    }

    /**
     * List tasks in a cgroup.
     */
    public int[] tasks() throws IOException {
        return IO.produce(() ->
            cgpaths.values().stream()
                   .flatMap(IOFunction.wrap(path -> Files.lines(path.resolve("tasks"))))
                   .mapToInt(Integer::parseInt)
                   .distinct()
                   .toArray());
    }

    static class Task {
        int pid, tid;
        String name;
        int uid, gid;
    }

    static Stream<Task> processes() throws IOException {
        return Files.list(Paths.get("/proc"))
                    .filter(Cgroup::is_pid_file)
                    .map(Cgroup::read_process_info)
                    .filter(Objects::nonNull);
    }

    static Stream<Task> threads() throws IOException {
        return processes().flatMap(IOFunction.wrap((Task p) ->
            Files.list(Paths.get("/proc", String.valueOf(p.pid), "task"))
                 .filter(Cgroup::is_pid_file)
                 .map(file -> get_thread_info(p, file))));
    }

    private static Task read_process_info(Path file) {
        try {
            Task t = new Task();
            t.pid = Integer.parseInt(file.getFileName().toString());
            readLines(file, StandardCharsets.ISO_8859_1, line -> {
                int i = line.indexOf(':');
                if (i != -1) {
                    String token = line.substring(0, i).trim();
                    String values = line.substring(i + 1).trim();
                    with(token)
                      .when("Name", () -> t.name = values)
                      .when("Uid",  () -> parse_uid(values).ifPresent(uid -> t.uid = uid))
                      .when("Gid",  () -> parse_uid(values).ifPresent(gid -> t.gid = gid));
                }
            });
            return t;
        } catch (IOException ex) {
            // the process file may no longer exist after list
            return null;
        }
    }

    private static Task get_thread_info(Task proc, Path file) {
        Task t = new Task();
        t.pid  = proc.pid;
        t.tid  = Integer.parseInt(file.getFileName().toString());
        t.name = proc.name;
        t.uid  = proc.uid;
        t.gid  = proc.gid;
        return t;
    }

    private static boolean is_pid_file(Path file) {
        String s = file.getFileName().toString();
        int len = s.length();
        for (int i = 0; i < len; i++) {
            if (!Character.isDigit(s.charAt(i))) {
                return false;
            }
        }
        return true;
    }

    private static OptionalInt parse_uid(String s) {
        String[] a = s.split("\\s+");
        if (a.length > 0) {
            return OptionalInt.of(Integer.parseInt(a[0]));
        } else {
            return OptionalInt.empty();
        }
    }

    private void cgcreate() throws IOException {
        IO.forEach(cgpaths, (subsys, path) -> {
            if (!Files.exists(path))
                mkdir(path, 0755);
            chown(path.resolve("tasks"), user, user);
        });
    }

    private void cgdelete() throws IOException {
        IO.forEach(cgpaths, (subsys, path) -> Files.deleteIfExists(path));
    }

    /**
     * Update the cgrules.conf file. This removes the requested user
     * and re-adds it at the end if a new path is provided.
     */
    private void update_cgrules(boolean recreate) throws IOException {
        overwrite_with_safe_swap(CGRULES, (in, out) -> {
            String line;
            while ((line = in.readLine()) != null) {
                if (!line.startsWith(user)) {
                    out.writeBytes(line);
                    out.write('\n');
                }
            }

            if (recreate) {
                out.writeBytes(user + '\t' +
                               String.join(",", CG_SUBSYSTEMS) + '\t' +
                               CG_ROOT + '/' + user + '\n');
            }
        });

        // reload cgrules.conf
        try (Stream<Task> ps = processes()) {
            ps.filter(having(p -> p.name, is("cgrulesengd")))
              .forEach(p -> Etc.kill(p.pid, Signal.SIGUSR2));
        }
    }

    /**
     * Update the cgconfig.conf file. This removes the requested
     * path and re-adds it at the end if a new configuration
     * is provided.
     */
    private void update_cgconfig(Map<String,?> newcfg)
        throws IOException
    {
        String prefix = "group " + CG_ROOT + "/" + user + " ";

        overwrite_with_safe_swap(CGCONFIG, (in, out) -> {
            String line;
            while ((line = in.readLine()) != null) {
                if (!line.startsWith(prefix)) {
                    out.writeBytes(line);
                    out.write('\n');
                }
            }

            if (newcfg != null) {
                out.writeBytes(prefix);
                out.writeBytes(gen_cgconfig(newcfg));
                out.write('\n');
            }
        });
    }

    /**
     * Generate configuration stanzas for cgconfig.conf from a map.
     */
    @SuppressWarnings("unchecked")
    private static String gen_cgconfig(Object data) {
        StringBuilder rbuf = new StringBuilder();
        if (data instanceof Map) {
            rbuf.append('{');
            ((Map<String,?>)data).forEach((k, v) -> {
                rbuf.append(' ').append(k).append(' ');
                rbuf.append(gen_cgconfig(v));
            });
            rbuf.append('}');
        } else {
            rbuf.append("= ").append(data).append("; ");
        }
        return rbuf.toString();
    }

    private static void overwrite_with_safe_swap(Path file,
            IOBiConsumer<RandomAccessFile, RandomAccessFile> action)
        throws IOException
    {
        Path tempfile = file.resolveSibling(file.getFileName() + "~");

        RandomAccessFile in;
        try {
            in = new RandomAccessFile(file.toFile(), "r");
        } catch (FileNotFoundException ex) {
            in = new RandomAccessFile("/dev/null", "r");
        }

        try (RandomAccessFile out = new RandomAccessFile(tempfile.toFile(), "rw")) {
            action.consume(in, out);
        } finally {
            in.close();
        }

        Files.move(tempfile, file, REPLACE_EXISTING, ATOMIC_MOVE);
    }
}
