/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container.adapters;

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
import java.util.OptionalInt;
import java.util.Set;
import java.util.stream.Stream;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Sets;

import jnr.constants.platform.Signal;
import static java.nio.file.StandardCopyOption.*;
import static java.util.stream.Collectors.*;

import com.cloudway.platform.common.fp.data.Maybe;
import com.cloudway.platform.common.fp.io.IO;
import com.cloudway.platform.common.fp.io.IOBiConsumer;
import com.cloudway.platform.common.fp.io.IOFunction;
import com.cloudway.platform.common.util.MoreFiles;
import com.cloudway.platform.common.os.Config;
import com.cloudway.platform.common.os.Etc;

import static com.cloudway.platform.common.util.MoreFiles.*;
import static com.cloudway.platform.common.util.MoreCollectors.*;
import static com.cloudway.platform.common.fp.control.Conditionals.*;
import static com.cloudway.platform.common.fp.control.Predicates.*;

// @formatter:off
class LinuxCgroup implements LibCgroup
{
    static final class Default {
        private Default() {}
        static final LibCgroup INSTANCE = new LinuxCgroup(Paths.get("/"));
    }

    private static final String PROC = "/proc";
    private static final String PROC_MOUNTS = "/proc/mounts";
    private static final String CGRULES  = "/etc/cgrules.conf";
    private static final String CGCONFIG = "/etc/cgconfig.conf";

    private final Path                  sysroot;
    private final String                cg_root;
    private final Set<String>           cg_subsystems;
    private final Map<String,Path>      cg_paths;
    private final Map<String,Object>    cg_parameters;
    private final boolean               cg_enabled;

    LinuxCgroup(Path sysroot) {
        this.sysroot = sysroot;

        Config cfg = Config.getDefault();
        this.cg_root = cfg.get("CGROUP_ROOT", Cgroup.DEFAULT_CG_ROOT);
        this.cg_subsystems = ImmutableSet.copyOf(
            cfg.get("CGROUP_SUBSYSTEMS", Cgroup.DEFAULT_CG_SUBSYSTEMS).split(","));

        Map<String,Path> paths;
        Map<String,Object> params;
        try {
            paths = init_cgpaths();
            params = init_cgparameters(paths);
        } catch (Exception ex) {
            // TODO: log warning
            paths = ImmutableMap.of();
            params = ImmutableMap.of();
        }

        this.cg_paths = paths;
        this.cg_parameters = params;
        this.cg_enabled = !cg_paths.isEmpty();
    }

    private Path sysfile(String name) {
        assert name.startsWith("/");
        return sysroot.resolve(name.substring(1));
    }

    // fs_spec fs_file fs_vtype fs_mntopts fs_freq fs_passno
    private enum FS {
        SPEC(0), FILE(1), VTYPE(2), MNTOPTS(3), FREQ(4), PASSNO(5);

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

    private Map<String,Path> init_cgpaths() throws IOException {
        try (Stream<String> lines = Files.lines(sysfile(PROC_MOUNTS), StandardCharsets.ISO_8859_1)) {
            return lines
                .map(line -> line.split("\\s+"))
                .filter(having(FS.VTYPE::get, is("cgroup")))
                .collect(ImmutableMap::<String,Path>builder,
                         (b, fs) ->
                             Sets.intersection(cg_subsystems, FS.MNTOPTS.split(fs)).stream()
                                 .forEach(subsys -> b.put(subsys, root_cgpath(FS.FILE.get(fs)))),
                         (l, r) -> l.putAll(r.build()))
                .build();
        }
    }

    private Path root_cgpath(String fs) {
        try {
            return mkdir(Paths.get(fs, cg_root), 0755);
        } catch (IOException ex) {
            throw new UncheckedIOException(ex);
        }
    }

    private static Map<String,Object> init_cgparameters(Map<String,Path> paths)
        throws IOException
    {
        ImmutableMap.Builder<String,Object> builder = ImmutableMap.builder();
        IO.forEach(paths, (subsys, path) -> {
            try (Stream<Path> files = find(path, 1, subsys + ".*")) {
                files.sorted(Comparator.comparingInt(LinuxCgroup::count_dots))
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

    private Maybe<Path> cgpath(String user, String subsys) {
        return Maybe.ofNullable(cg_paths.get(subsys))
                    .map(path -> path.resolve(user))
                    .filter(Files::exists);
    }

    @Override
    public boolean enabled() {
        return cg_enabled;
    }

    @Override
    public Map<String,Path> paths() {
        return cg_paths;
    }

    @Override
    public Map<String,Object> parameters() {
        return cg_parameters;
    }

    @Override
    public void create(String user, int uid, Map<String,Object> defaults) throws IOException {
        Map<String, Map<String,Object>> newcfg = new LinkedHashMap<>();
        Map<String, Object> to_store = new LinkedHashMap<>();

        // add the cgroup permissions:
        // { perm { task  { uid = 1000; gid = 1000; }
        //          admin { uid = root, gid = root; } } }
        newcfg.put("perm", pmap("task",  pmap("uid",  uid,   "gid",  uid),
                                "admin", pmap("uid", "root", "gid", "root")));

        // add the net_cls.classid
        int net_cls = (Cgroup.NET_CLS_MAJOR << 16) + (uid % Cgroup.UID_WRAPAROUND);
        newcfg.put("net_cls", pmap("net_cls.classid", net_cls));
        to_store.put("net_cls.classid", net_cls);

        cg_subsystems.forEach(s -> newcfg.computeIfAbsent(s, x -> new LinkedHashMap<>()));

        cg_parameters.keySet().forEach(k -> {
            Object v = defaults.get(k);
            if (v != null) {
                String subsys = k.substring(0, k.indexOf('.'));
                if (newcfg.containsKey(subsys)) {
                    newcfg.get(subsys).put(k, v);
                    to_store.put(k, v);
                }
            }
        });

        update_cgconfig(user, newcfg);
        update_cgrules(user, true);
        store(user, to_store);
        cgclassify(user, uid);
    }

    private static Map<String,Object> pmap(Object... args) {
        ImmutableMap.Builder<String,Object> builder = ImmutableMap.builder();
        for (int i = 0; i < args.length; i += 2) {
            builder.put((String)args[i], args[i+1]);
        }
        return builder.build();
    }

    @Override
    public void delete(String user) throws IOException {
        update_cgconfig(user, null);
        update_cgrules(user, false);
        cgdelete(user);
    }

    @Override
    public Maybe<Object> fetch(String user, String key) throws IOException {
        String subsys = key.substring(0, key.indexOf('.'));
        return IO.run(() -> cgpath(user, subsys)
            .map(path -> path.resolve(key))
            .map(IOFunction.wrap(MoreFiles::chomp))
            .map(LinuxCgroup::parse_cgparam));
    }

    @Override
    public void store(String user, Map<String,Object> vals) throws IOException {
        // recreate cgroup because it may lost during system boot
        cgcreate(user);

        IO.forEach(cg_parameters, (key, val) -> {
            val = vals.get(key);
            if (val != null) {
                String subsys = key.substring(0, key.indexOf('.'));
                Path path = cgpath(user, subsys).orElseThrow(
                    () -> new IOException("User does not exist in cgroups: " + user));
                writeText(path.resolve(key), format_cgparam(val));
            }
        });
    }

    @Override
    public void freeze(String user) throws IOException {
        store(user, pmap("freezer.state", "FROZEN"));
    }

    @Override
    public void thaw(String user) throws IOException {
        store(user, pmap("freezer.state", "THAWED"));
    }

    @Override
    public int[] tasks(String user) throws IOException {
        return IO.run(() -> cg_paths.values().stream()
            .flatMap(IOFunction.wrap(path -> Files.lines(join(path, user, "tasks"))))
            .mapToInt(Integer::parseInt)
            .distinct()
            .toArray());
    }

    static class Task {
        int pid, tid;
        String name;
        int uid, gid;
    }

    private Stream<Task> processes() throws IOException {
        return Files.list(sysfile(PROC))
                    .filter(LinuxCgroup::is_pid_file)
                    .map(LinuxCgroup::read_process_info)
                    .filter(Objects::nonNull);
    }

    private Stream<Task> threads() throws IOException {
        return processes().flatMap(IOFunction.wrap((Task p) ->
            Files.list(join(sysfile(PROC), String.valueOf(p.pid), "task"))
                 .filter(LinuxCgroup::is_pid_file)
                 .map(file -> get_thread_info(p, file))));
    }

    private static Task read_process_info(Path file) {
        try {
            Task t = new Task();
            t.pid = Integer.parseInt(file.getFileName().toString());
            readLines(file.resolve("status"), StandardCharsets.ISO_8859_1, line -> {
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

    protected void cgcreate(String user) throws IOException {
        IO.forEach(cg_paths, (subsys, path) ->
            create_cgroup(path.resolve(user), user, subsys));
    }

    protected void cgdelete(String user) throws IOException {
        IO.forEach(cg_paths, (subsys, path) ->
            delete_cgroup(path.resolve(user), user, subsys));
    }

    protected void create_cgroup(Path path, String user, String subsys) throws IOException {
        if (!Files.exists(path))
            mkdir(path, 0755);
        chown(path.resolve("tasks"), user, user);
    }

    protected void delete_cgroup(Path path, String user, String subsys) throws IOException {
        Files.deleteIfExists(path);
    }

    protected void cgclassify(String user, int uid) throws IOException {
        try (Stream<Task> ts = threads()) {
            String tasks =
                ts.filter(p -> p.uid == uid)
                  .map(p -> String.valueOf(p.tid))
                  .collect(joining("\n"));

            if (!tasks.isEmpty()) {
                IO.forEach(cg_paths, (subsys, path) -> writeText(join(path, user, "tasks"), tasks));
            }
        }
    }

    /**
     * Update the cgrules.conf file. This removes the requested user
     * and re-adds it at the end if a new path is provided.
     */
    private void update_cgrules(String user, boolean recreate) throws IOException {
        overwrite_with_safe_swap(sysfile(CGRULES), (in, out) -> {
            String line;
            while ((line = in.readLine()) != null) {
                if (!line.startsWith(user)) {
                    out.writeBytes(line);
                    out.write('\n');
                }
            }

            if (recreate) {
                out.writeBytes(user + '\t' +
                               String.join(",", cg_subsystems) + '\t' +
                               cg_root + '/' + user + '\n');
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
    private void update_cgconfig(String user, Map<String,?> newcfg)
        throws IOException
    {
        String prefix = "group " + cg_root + "/" + user + " ";

        overwrite_with_safe_swap(sysfile(CGCONFIG), (in, out) -> {
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
