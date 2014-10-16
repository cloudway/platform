/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container.plugin;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import com.cloudway.platform.common.Config;
import com.cloudway.platform.common.util.Etc;
import com.cloudway.platform.common.util.Exec;
import com.cloudway.platform.container.ApplicationContainer;
import com.cloudway.platform.container.ResourceLimits;

public class TrafficControl
{
    // The network interface we're planning on limiting bandwidth.
    private static final String tc_if;

    // User bandwidth limits in Mbit/s
    private static final int tc_max_bandwidth;
    private static final int tc_user_share;
    private static final int tc_user_limit;
    private static final int tc_user_quantum;

    static {
        Config config = Config.getDefault();
        ResourceLimits limits = ResourceLimits.getInstance();

        tc_if = config.get("EXTERNAL_ETH_DEV", "eth0");
        tc_max_bandwidth = limits.getInt("tc.max_bandwidth", 800);
        tc_user_share = limits.getInt("tc.user_share", 2);
        tc_user_limit = limits.getInt("tc.user_limit", tc_max_bandwidth);
        tc_user_quantum = limits.getInt("tc.user_quantum", 100000);
    }

    public static void start()
        throws IOException
    {
        List<Integer> uids = ApplicationContainer.ids().stream()
            .map(Etc::getpwnam)
            .filter(Objects::nonNull)
            .mapToInt(pwent -> pwent.pw_uid)
            .boxed()
            .collect(Collectors.toList());

        StringBuilder cmd = new StringBuilder();

        boolean all_stopped = uids.stream().allMatch(uid -> !exists(uid));
        if (all_stopped) {
            cmd.append("qdisc add dev ").append(tc_if)
               .append(" root handle 1: htb\n");
            cmd.append("class add dev ").append(tc_if)
               .append(" parent 1: classid 1:1 htb rate ")
               .append(tc_max_bandwidth).append("mbit\n");
            cmd.append("filter add dev ").append(tc_if)
               .append(" parent 1: protocol ip prio 10 handle 1: cgroup\n");
        }

        uids.stream().forEach(uid -> {
            if (!exists(uid)) {
                start_user_cmd(cmd, uid);
            }
        });

        tc_batch(cmd.toString());
    }

    public static void stop() throws IOException {
        Exec.line("tc qdisc del dev " + tc_if + " root").silentIO().run();
    }

    public static boolean exists(int uid) {
        try {
            String clsid = uid_to_clsid(uid);
            String cmd = "tc -s class show dev " + tc_if + " classid 1:" + clsid;
            String out = Exec.line(cmd).silentIO().subst();
            return !out.isEmpty();
        } catch (IOException ex) {
            return false; // FIXME
        }
    }

    public static void startUser(int uid)
        throws IOException
    {
        if (!exists(uid)) {
            StringBuilder cmd = new StringBuilder();
            start_user_cmd(cmd, uid);
            tc_batch(cmd.toString());
        }
    }

    private static void start_user_cmd(StringBuilder cmd, int uid) {
        String clsid = uid_to_clsid(uid);

        // Overall class for the guest
        cmd.append("class add dev ").append(tc_if)
           .append(" parent 1:1 classid 1:").append(clsid)
           .append(" htb rate ").append(tc_user_share).append("mbit")
           .append(" ceil ").append(tc_user_limit).append("mbit")
           .append(" quantum ").append(tc_user_quantum)
           .append("\n");

        // Specific constraints within the guest's limit
        cmd.append("qdisc add dev ").append(tc_if)
           .append(" parent 1:").append(clsid)
           .append(" handle ").append(clsid).append(":")
           .append(" htb default 0")
           .append("\n");
    }

    public static void stopUser(int uid)
        throws IOException
    {
        String clsid = uid_to_clsid(uid);
        String cmd = "tc -force class del dev " + tc_if + " parent 1:1 classid 1:" + clsid;
        Exec.line(cmd).silentIO().checkError().run();
    }

    private static void tc_batch(String cmd) throws IOException {
        Exec.args("tc", "-force", "-batch")
            .redirectInputStream(new ByteArrayInputStream(cmd.getBytes()))
            .redirectOutput((File)null)
            .checkError()
            .run();
    }

    private static final String uid_to_clsid(int uid) {
        return Integer.toHexString(uid % Cgroup.UID_WRAPAROUND);
    }
}
