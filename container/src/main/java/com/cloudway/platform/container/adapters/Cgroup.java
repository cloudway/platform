/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container.adapters;

import java.io.IOException;
import java.util.Map;
import com.cloudway.platform.common.fp.data.Maybe;

public class Cgroup
{
    public static final String DEFAULT_CG_ROOT = "cloudway";
    public static final String DEFAULT_CG_SUBSYSTEMS = "cpu,cpuacct,memory,net_cls,freezer";

    public static final String CG_KEY_PREFIX = "cgroup.";

    public static final int NET_CLS_MAJOR  = 1;
    public static final int UID_WRAPAROUND = 65536;

    private final LibCgroup cg;
    private final String    user;
    private final int       uid;

    public Cgroup(String user, int uid) {
        this(LibCgroup.getDefault(), user, uid);
    }

    Cgroup(LibCgroup cg, String user, int uid) {
        this.cg   = cg;
        this.user = user;
        this.uid  = uid;
    }

    public void create(Map<String,Object> defaults) throws IOException {
        cg.create(user, uid, defaults);
    }

    public void delete() throws IOException {
        cg.delete(user);
    }

    public Maybe<Object> fetch(String key) throws IOException {
        return cg.fetch(user, key);
    }

    public void store(Map<String,Object> vals) throws IOException {
        cg.store(user, vals);
    }

    public void freeze() throws IOException {
        cg.freeze(user);
    }

    public void thaw() throws IOException {
        cg.thaw(user);
    }

    public int[] tasks() throws IOException {
        return cg.tasks(user);
    }
}
