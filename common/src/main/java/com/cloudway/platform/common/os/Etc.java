/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.os;

import java.util.Objects;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.function.Consumer;
import java.util.function.Function;

import jnr.constants.platform.Signal;
import jnr.posix.POSIX;
import jnr.posix.POSIXFactory;
import jnr.posix.Passwd;

public final class Etc
{
    private Etc() {}

    public static final POSIX posix = POSIXFactory.getPOSIX();

    public static class PASSWD {
        public final String   pw_name;    // user name
        public final String   pw_passwd;  // encrypted password
        public final int      pw_uid;     // user uid
        public final int      pw_gid;     // user gid
        public final String   pw_gecos;   // Honeywell login info
        public final String   pw_dir;     // home directory
        public final String   pw_shell;   // default shell

        public static PASSWD from(jnr.posix.Passwd pwent) {
            if (pwent == null) {
                return null;
            } else {
                return new PASSWD(pwent);
            }
        }

        private PASSWD(jnr.posix.Passwd pwent) {
            this.pw_name   = pwent.getLoginName();
            this.pw_passwd = pwent.getPassword();
            this.pw_uid    = (int)pwent.getUID();
            this.pw_gid    = (int)pwent.getGID();
            this.pw_gecos  = pwent.getGECOS();
            this.pw_dir    = pwent.getHome();
            this.pw_shell  = pwent.getShell();
        }
    }

    public static class GROUP {
        public final String   gr_name;      // group name
        public final String   gr_passwd;    // group password
        public final int      gr_gid;       // group id
        public final String[] gr_mem;     // group members

        public static GROUP from(jnr.posix.Group grent) {
            if (grent == null) {
                return null;
            } else {
                return new GROUP(grent);
            }
        }

        private GROUP(jnr.posix.Group grent) {
            this.gr_name   = grent.getName();
            this.gr_passwd = grent.getPassword();
            this.gr_gid    = (int)grent.getGID();
            this.gr_mem    = grent.getMembers();
        }
    }

    // The following method must be synchronized because returned Passwd pointer is shared

    private static final Lock pw_lock = new ReentrantLock();

    public static void getpwent(Consumer<PASSWD> action) {
        pw_lock.lock();
        try {
            for (Passwd pwent = posix.getpwent(); pwent != null; pwent = posix.getpwent()) {
                action.accept(PASSWD.from(pwent));
            }
        } finally {
            try {
                posix.endpwent();
            } finally {
                pw_lock.unlock();
            }
        }
    }

    public static PASSWD getpwuid(int which) {
        pw_lock.lock();
        try {
            return PASSWD.from(posix.getpwuid(which));
        } finally {
            pw_lock.unlock();
        }
    }

    public static PASSWD getpwnam(String which) {
        Objects.requireNonNull(which);
        pw_lock.lock();
        try {
            return PASSWD.from(posix.getpwnam(which));
        } finally {
            pw_lock.unlock();
        }
    }

    private static final Lock gr_lock = new ReentrantLock();

    public static GROUP getgrgid(int which) {
        gr_lock.lock();
        try {
            return GROUP.from(posix.getgrgid(which));
        } finally {
            gr_lock.unlock();
        }
    }

    public static int getuid() {
        return posix.getuid();
    }

    public static int getgid() {
        return posix.getgid();
    }

    public static int kill(int pid, Signal signal) {
        return posix.kill(pid, signal.intValue());
    }

    private static final Lock posix_lock = new ReentrantLock();

    public static <R> R do_posix(Function<POSIX, R> action) {
        posix_lock.lock();
        try {
            return action.apply(posix);
        } finally {
            posix_lock.unlock();
        }
    }
}
