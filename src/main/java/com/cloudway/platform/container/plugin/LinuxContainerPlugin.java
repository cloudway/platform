/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container.plugin;

import java.io.IOException;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import com.cloudway.platform.common.Config;
import com.cloudway.platform.common.util.Exec;
import com.cloudway.platform.common.util.ExtendedProperties;
import com.cloudway.platform.common.util.IO;
import com.cloudway.platform.container.ApplicationContainer;

import jnr.posix.POSIX;
import jnr.posix.POSIXFactory;

public class LinuxContainerPlugin extends UnixContainerPlugin
{
    private static final POSIX posix = POSIXFactory.getPOSIX();

    public LinuxContainerPlugin(ApplicationContainer container) {
        super(container);
    }

    @Override
    public void create() throws IOException {
        try {
            createUser();
            cgcreate();
            createHomeDir();
            initQuota();
        } catch (IOException ex) {
            // cleanup when creating container failed
            nothrow(this::deleteUser);
            nothrow(this::cgdelete);
            throw ex;
        }
    }

    @Override
    public void destroy() throws IOException {
        nothrow(this::killProcs);
        nothrow(this::cgfreeze);
        nothrow(this::killProcs);
        nothrow(this::deleteUser);
        nothrow(this::cgunfreeze);
        nothrow(this::cgdelete);
        nothrow(this::removeQuota);
    }

    @Override
    public void start() throws IOException {
        cgrestore();
    }

    // SELinux

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
                    .skip(uid - (uid_offset + group_size - 1))
                    .findFirst()            // an optional IntStream
                    .get()                  // an IntStream
                    .mapToObj(i -> "c" + i) // joining the IntStream
                    .collect(Collectors.joining(","));
                return "s" + mls_num + ":" + mcs_label;
            }
        } else {
            throw new IllegalArgumentException("The UID is too large for MCS set parameters");
        }
    }

    private static Stream<IntStream> combination(int n, int k, int m) {
        if (m == 0) {
            return Stream.of(IntStream.empty());
        } else {
            return IntStream.rangeClosed(k, n-m)
                .mapToObj(Integer::valueOf) // this is required because an IntStream cannot do flatMap
                .flatMap(i -> combination(n, i+1, m-1).map(c -> IntStream.concat(IntStream.of(i), c)));
        }
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

    private  void set_mcs_label(Path path, String label)
        throws IOException
    {
        if (selinux_enabled) {
            SELinux.chcon(path, label, null, null, null);
        }
    }

    // Cgroups

    private static final String[] CG_PROFILE_KEYS = {
        "default", "boosted", "throttled", "frozen", "thawed"
    };

    private static final String CG_DEFAULT = "default";

    private static final Map<String, Map<String,Object>> cgprofiles;

    static {
        if (Cgroup.enabled) {
            cgprofiles = load_cgprofiles();
        } else {
            cgprofiles = Collections.emptyMap();
        }
    }

    private static Map<String, Map<String,Object>> load_cgprofiles() {
        Map<String, Map<String,Object>> profiles = new HashMap<>();
        Config res = new Config(Config.CONF_DIR.resolve("limits.conf"), null);

        // load configuration for each cgroup profiles
        Arrays.stream(CG_PROFILE_KEYS)
            .forEach(profile -> {
                Map<String,Object> t = new LinkedHashMap<>();
                profiles.put(profile, t);
                ExtendedProperties c = res.group("cgroup." + profile);
                if (c != null) {
                    c.stringPropertyNames().forEach(k -> t.put(k, c.get(k)));
                }
            });

        // apply default configuration values for default cgroup profile
        Map<String,Object> deflt = profiles.get(CG_DEFAULT);
        profiles.forEach((profile, cfg) -> {
            if (!CG_DEFAULT.equals(profile)) {
                cfg.keySet().forEach(k -> {
                    if (!deflt.containsKey(k)) {
                        Object v = Cgroup.CG_PARAMETERS.get(k);
                        if (v != null) {
                            deflt.put(k, v);
                        }
                    }
                });
            }
        });

        return Collections.unmodifiableMap(profiles);
    }

    private boolean cgroup(IO.Consumer<Cgroup> action)
        throws IOException
    {
        if (Cgroup.enabled) {
            action.accept(new Cgroup(container.getUuid(), container.getUID()));
            return true;
        } else {
            return false;
        }
    }

    private void cgcreate() throws IOException {
        cgroup(cg -> cg.create(cgprofiles.get(CG_DEFAULT)));
    }

    private void cgdelete() throws IOException {
        cgroup(Cgroup::delete);
    }

    private void cgfreeze() throws IOException {
        apply_cgroup_profile("frozen", null);
    }

    private void cgunfreeze() throws IOException {
        apply_cgroup_profile("thawed", null);
    }

    private void cgrestore() throws IOException {
        apply_cgroup_profile(CG_DEFAULT, null);
    }

    /**
     * Apply a cgroup template. If called with an action, the default
     * will be restored after the action is completed.
     */
    private void apply_cgroup_profile(String profile, Runnable action)
        throws IOException
    {
        cgroup(cg -> {
            Map<String,Object> t = cgprofiles.get(profile);

            if (t == null) {
                throw new IllegalArgumentException("Unknown profile: " + profile);
            }

            cg.store(t);

            if (action != null) {
                try {
                    action.run();
                } finally {
                    cg.store(cgprofiles.get(CG_DEFAULT));
                }
            }
        });
    }

    // Quota

    public static final int DEFAULT_QUOTA_BLOCKS = 1048576; // 1GB
    public static final int DEFAULT_QUOTA_FILES  = 80000;

    private void initQuota() throws IOException {
        Config limits = new Config(Config.CONF_DIR.resolve("limits.conf"), null);
        set_quota(limits.getInt("quota.blocks", DEFAULT_QUOTA_BLOCKS),
                  limits.getInt("quota.files", DEFAULT_QUOTA_FILES));
    }

    private void removeQuota() throws IOException {
        set_quota(0, 0);
    }

    private void set_quota(int maxblocks, int maxfiles)
        throws IOException
    {
        int curblocks = 0, curfiles = 0;

        Optional<String[]> quota = get_quota();
        if (quota.isPresent()) {
            String[] cur_quota = quota.get();
            curblocks = Integer.parseInt(cur_quota[1]);
            maxblocks = Integer.parseInt(cur_quota[3]);
            curfiles  = Integer.parseInt(cur_quota[5]);
            maxfiles  = Integer.parseInt(cur_quota[7]);
        }

        if (curblocks > maxblocks || curfiles > maxfiles) {
            // Log warning: current usage exceeds requested quota
            return;
        }

        Exec.args("setquota",
                  "-u", container.getUuid(),
                  0, maxblocks, 0, maxfiles,
                  "-a",
                  get_mountpoint(container.getHomeDir()))
            .silentIO()
            .checkError()
            .run();

    }

    private Optional<String[]> get_quota() throws IOException {
        String out = Exec.args("quota", "-pw", container.getUuid()).silentIO().subst();
        return Arrays.stream(out.split("\n"))
            .filter(line -> line.matches("^.*/dev/.*"))
            .findFirst()
            .map(line -> line.split("\\s+"));
    }

    private static Path get_mountpoint(Path path) {
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
    }
}
