/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container.adapters;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;
import java.util.stream.IntStream;

import static java.util.stream.Collectors.*;
import static com.cloudway.fp.control.StringPredicates.*;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableTable;

import com.cloudway.fp.data.Maybe;
import com.cloudway.fp.data.IntSeq;
import com.cloudway.fp.data.Seq;
import com.cloudway.fp.io.IO;
import com.cloudway.fp.io.IOConsumer;
import com.cloudway.fp.io.IOBiConsumer;
import com.cloudway.platform.common.os.Config;
import com.cloudway.platform.common.os.Etc;
import com.cloudway.platform.common.os.Exec;
import com.cloudway.platform.container.Container;
import com.cloudway.platform.container.ResourceLimits;
import jnr.constants.platform.Signal;

public class LinuxContainerAdapter extends UnixContainerAdapter
{
    public LinuxContainerAdapter(Container container) {
        super(container);
    }

    @Override
    public void create() throws IOException {
        try {
            createUser();
            createHomeDir();
            cgcreate();
            startTrafficControl();
            initQuota();
            initPamLimits();
        } catch (IOException ex) {
            // cleanup when container creation failed
            nothrow(
                this::deleteUser,
                this::cgdelete,
                this::stopTrafficControl,
                this::removePamLimits
            );
            throw ex;
        }
    }

    @Override
    public void destroy() {
        nothrow(
            this::killProcs,
            this::cgfreeze,
            this::killProcs,
            this::deleteUser,
            this::cgunfreeze,
            this::cgdelete,
            this::stopTrafficControl,
            this::removePamLimits
        );
    }

    @Override
    public void start() throws IOException {
        cgrestore();
    }

    // SELinux

    private static final String DEF_RUN_USER    = "unconfined_u";
    private static final String DEF_RUN_ROLE    = "system_r";
    private static final String DEF_RUN_TYPE    = "cloudway_t";

    private static final int DEF_MCS_SET_SIZE   = 1024;
    private static final int DEF_MCS_GROUP_SIZE = 2;
    private static final int DEF_MCS_UID_OFFSET = 0;
    private static final int DEF_MLS_NUM        = 0;

    private static final boolean selinux_enabled = SELinux.enabled();
    private String mcs_label;

    private String get_mcs_label() {
        if (selinux_enabled && mcs_label == null && container.getUID() != 0) {
            mcs_label = get_mcs_label_for(container.getUID());
        }
        return mcs_label;
    }

    private String get_target_context() {
        return String.join(":", DEF_RUN_USER, DEF_RUN_ROLE, DEF_RUN_TYPE, get_mcs_label());
    }

    /**
     * Determine the MCS label for the given UID.
     */
    private static String get_mcs_label_for(int uid) {
        Config config = Config.getDefault();

        int set_size   = config.getInt("SELINUX_MCS_SET_SIZE",   DEF_MCS_SET_SIZE);
        int group_size = config.getInt("SELINUX_MCS_GROUP_SIZE", DEF_MCS_GROUP_SIZE);
        int uid_offset = config.getInt("SELINUX_MCS_UID_OFFSET", DEF_MCS_UID_OFFSET);
        int mls_num    = config.getInt("SELINUX_MLS_NUM",        DEF_MLS_NUM);

        if (uid < uid_offset + group_size - 1) {
            throw new IllegalArgumentException("The UID must be greater than " +
                                               (uid_offset + group_size - 1));
        }

        if (uid < uid_offset + set_size * (set_size - 1) / 2) {
            if (group_size == 2) { // optimize
                // offset uid
                int ouid = uid - uid_offset;

                // Quadratic formula
                int a = 1;
                int b = 2 * set_size - 1; // This is actually negative b
                int c = 2 * ouid - 2;

                // Root of the equation
                int root = (int)((b - Math.sqrt(b * b - 4 * a * c)) / (2 * a));
                // remainder
                int remainder = (ouid - (2 * set_size - root - 1) * root / 2) + root;

                return "s" + mls_num + ":c" + root + ",c" + remainder;
            } else {
                // combination for large groups
                String mcs_label = combination(set_size, 0, group_size)
                    .drop(uid - (uid_offset + group_size - 1))
                    .head().show(",c", "c", "");
                return "s" + mls_num + ":" + mcs_label;
            }
        } else {
            throw new IllegalArgumentException("The UID is too large for MCS set parameters");
        }
    }

    private static Seq<IntSeq> combination(int n, int k, int m) {
        return m == 0
            ? Seq.of(IntSeq.nil())
            : IntSeq.rangeClosed(k, n - m).flatMapToObj(i ->
              combination(n, i + 1, m - 1).map(c ->
              IntSeq.cons(i, c)));
    }

    @Override
    public void setFileReadOnly(Path file) throws IOException {
        super.setFileReadOnly(file);
        set_mcs_label(file, get_mcs_label());
    }

    @Override
    public void setFileReadWrite(Path file) throws IOException {
        super.setFileReadWrite(file);
        set_mcs_label(file, get_mcs_label());
    }

    private static void set_mcs_label(Path path, String label)
        throws IOException
    {
        if (selinux_enabled) {
            SELinux.chcon(path, label, null, null, null);
        }
    }

    // Cgroups

    static final class CgProfiles {
        private CgProfiles() {}

        private static final ImmutableTable<String, String, Object> profiles =
            LibCgroup.getDefault().load_profiles(ResourceLimits.getDefault());

        static Map<String,Object> get(String profile) {
            return profiles.rowMap().get(profile);
        }

        static boolean enabled() {
            return !profiles.isEmpty();
        }
    }

    private boolean cgcall(IOConsumer<Cgroup> action)
        throws IOException
    {
        if (CgProfiles.enabled()) {
            action.consume(new Cgroup(container.getId(), container.getUID()));
            return true;
        } else {
            return false;
        }
    }

    private boolean cgcall(IOBiConsumer<Cgroup, Map<String,Object>> action)
        throws IOException
    {
        if (CgProfiles.enabled()) {
            Map<String, Object> cfg = CgProfiles.get(container.getCapacity());
            if (cfg == null) {
                throw new IllegalArgumentException("Unknown cgroup profile: " + container.getCapacity());
            }

            Cgroup cg = new Cgroup(container.getId(), container.getUID());
            action.consume(cg, cfg);
            return true;
        } else {
            return false;
        }
    }

    private void cgcreate() throws IOException {
        cgcall(Cgroup::create);
        cgcall(this::fetch_limits);
    }

    private void cgdelete() throws IOException {
        cgcall(Cgroup::delete);
    }

    private void cgfreeze() throws IOException {
        cgcall(cg -> {
            int[] pids;
            cg.freeze();
            for (int i = 20; --i >= 0 && (pids = cg.tasks()).length != 0; ) {
                try {
                    IntStream.of(pids).forEach(pid -> Etc.kill(pid, Signal.SIGKILL));
                    cg.thaw();
                    Thread.sleep(100);
                    cg.freeze();
                } catch (Exception ex) {
                    // log and ignore
                }
            }
        });
    }

    private void cgunfreeze() throws IOException {
        cgcall(Cgroup::thaw);
    }

    private void cgrestore() throws IOException {
        cgcall(Cgroup::store);
        cgcall(this::fetch_limits);
    }

    private void fetch_limits(Cgroup cg) {
        try {
            cg.fetch("memory.limit_in_bytes").ifPresent(val -> {
                long mb = Long.parseLong((String)val) / (1024 * 1024);
                container.setenv("CLOUDWAY_MEMORY_LIMIT", String.valueOf(mb));
            });
        } catch (Exception ex) {
            // ignored
        }
    }

    private void startTrafficControl() throws IOException {
        TrafficControl.startUser(container.getUID());
    }

    private void stopTrafficControl() throws IOException {
        TrafficControl.stopUser(container.getUID());
    }

    // Quota

    public static final int DEFAULT_QUOTA_BLOCKS = 1048576; // 1GB
    public static final int DEFAULT_QUOTA_FILES  = 80000;

    private void initQuota() throws IOException {
        ResourceLimits limits = ResourceLimits.getDefault();
        String category = container.getCapacity();
        set_quota(limits.getIntProperty(category, "quota.blocks", DEFAULT_QUOTA_BLOCKS),
                  limits.getIntProperty(category, "quota.files", DEFAULT_QUOTA_FILES));
    }

    private void set_quota(int maxblocks, int maxfiles)
        throws IOException
    {
        int curblocks = 0, curfiles = 0;

        Maybe<String[]> quota = get_quota();
        if (quota.isPresent()) {
            String[] cur_quota = quota.get();
            curblocks = Integer.parseInt(cur_quota[1]);
            maxblocks = Integer.parseInt(cur_quota[3]);
            curfiles  = Integer.parseInt(cur_quota[5]);
            maxfiles  = Integer.parseInt(cur_quota[7]);
        }

        if (curblocks > maxblocks || curfiles > maxfiles) {
            // Log warning: current usage exceeds requested quota
        }

        Exec.args("setquota",
                  "-u", container.getId(),
                  0, maxblocks, 0, maxfiles,
                  "-a",
                  get_mountpoint(container.getHomeDir()))
            .silentIO()
            .checkError()
            .run();

    }

    private Maybe<String[]> get_quota() throws IOException {
        String out = Exec.args("quota", "-pw", container.getId()).silentIO().subst();
        return Seq.of(out.split("\n"))
            .filter(matches("^.*/dev/.*"))
            .map(line -> line.split("\\s+"))
            .peek();
    }

    private static Path get_mountpoint(Path path) {
        return Etc.do_posix(posix -> {
            Path oldpath = path.toAbsolutePath();
            long olddev = posix.lstat(oldpath.toString()).dev();

            while (true) {
                Path newpath = oldpath.getParent();
                if (newpath == null || newpath.equals(oldpath) ||
                    olddev != posix.lstat(newpath.toString()).dev()) {
                    break;
                }
                oldpath = newpath;
            }
            return oldpath;
        });
    }

    // PAM resource limits

    private static final String PAM_LIMITS_DIR = "/etc/security/limits.d";

    private static final ImmutableList<String> PAM_LIMITS_VARS = ImmutableList.of(
        "core", "data", "fsize", "memlock", "nofile", "rss", "cpu", "nproc", "as",
        "maxlogins", "priority", "locks", "sigpending", "msgqueue", "nice", "rtprio"
    );
    private static final ImmutableList<String> PAM_SOFT_VARS = ImmutableList.of("nproc");

    private static Path pam_limits_file(String id) {
        int limits_order = ResourceLimits.getDefault().getGlobalProperty("limits.order", 85);
        return Paths.get(PAM_LIMITS_DIR, limits_order + "-" + id + ".conf");
    }

    private void initPamLimits() throws IOException {
        ResourceLimits cfg = ResourceLimits.getDefault();
        String id = container.getId();
        String profile = container.getCapacity();

        try (BufferedWriter out = Files.newBufferedWriter(pam_limits_file(id))) {
            out.write("# PAM process limits for guest " + id + "\n");
            IO.forEach(PAM_LIMITS_VARS, k ->
                cfg.getProperty(profile, "limits." + k).ifPresent(IOConsumer.wrap(v -> {
                    String limtype =
                        (PAM_SOFT_VARS.contains(k) && !"0".equals(v)) ? "soft" : "hard";
                    out.write(String.join("\t", id, limtype, k, v));
                    out.newLine();
                }))
            );
        }
    }

    private void removePamLimits() throws IOException {
        Files.deleteIfExists(pam_limits_file(container.getId()));
    }

    // Switch context

    // TODO: we can use a C program to perform the following process
    @Override
    public Exec join(Exec exec) throws IOException {
        if (Etc.getuid() != container.getUID()) {
            String cmd = exec.command().stream()
                .map(arg -> arg.isEmpty() ? "''" :
                            arg.indexOf(' ') != -1 ? "\\\"" + arg + "\\\"" :
                            arg)
                .collect(joining(" "));

            if (selinux_enabled) {
                String current_context = SELinux.getcon();
                String target_context = get_target_context();

                // Only switch contexts if necessary
                if (!current_context.equals(target_context)) {
                    cmd = "exec /usr/bin/runcon '" + target_context + "' /bin/sh -c \"" + cmd + "\"";
                }
            }

            exec.command("/sbin/runuser", "-s", "/bin/sh", container.getId(), "-c", cmd);
        }

        return exec;
    }
}
