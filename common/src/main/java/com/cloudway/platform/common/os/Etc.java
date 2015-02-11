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
import java.util.function.Supplier;

import com.cloudway.platform.common.fp.function.ExceptionFunction;
import jnr.constants.platform.Signal;
import jnr.posix.FileStat;

/**
 * This class provides a subset of POSIX API in a portable manner.
 */
public final class Etc {
    private Etc() {}

    private static final Supplier<Posix> DEFAULT_PROVIDER = () -> PosixImpl.INSTANCE;
    private static Supplier<Posix> _provider = DEFAULT_PROVIDER;

    public static void setPosixProvider(Supplier<Posix> provider) {
        _provider = provider != null ? provider : DEFAULT_PROVIDER;
    }

    public static Posix provider() {
        return _provider.get();
    }

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

        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (!(obj instanceof PASSWD))
                return false;
            PASSWD other = (PASSWD)obj;
            return Objects.equals(pw_name, other.pw_name)
                && Objects.equals(pw_passwd, other.pw_passwd)
                && pw_uid == other.pw_uid && pw_gid == other.pw_gid
                && Objects.equals(pw_gecos, other.pw_gecos)
                && Objects.equals(pw_dir, other.pw_dir)
                && Objects.equals(pw_shell, other.pw_shell);
        }

        public int hashCode() {
            return Objects.hash(pw_name, pw_passwd, pw_uid, pw_gid, pw_gecos, pw_dir, pw_shell);
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

        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (!(obj instanceof GROUP))
                return false;
            GROUP other = (GROUP)obj;
            return Objects.equals(gr_name, other.gr_name)
                && Objects.equals(gr_passwd, other.gr_passwd)
                && gr_gid == other.gr_gid
                && Objects.deepEquals(gr_mem, other.gr_mem);
        }

        public int hashCode() {
            return 31 * Objects.hash(gr_name, gr_passwd, gr_gid)
                 + (gr_mem == null ? 0 : Objects.hash((Object[])gr_mem));
        }
    }

    /**
     * Retrieve user information from user database.
     *
     * @param action the action to perform on every password entry
     */
    public static void getpwent(Consumer<PASSWD> action) {
        provider().getpwent(action);
    }

    /**
     * Search user database for a user ID.
     *
     * @param which the user ID to search
     * @return a password entry if found, otherwise {@code null} if not found
     */
    public static PASSWD getpwuid(int which) {
        return provider().getpwuid(which);
    }

    /**
     * Search user database for a user name.
     *
     * @param which the user name to search
     * @return a password entry if found, otherwise {@code null} if not found
     */
    public static PASSWD getpwnam(String which) {
        return provider().getpwnam(which);
    }

    /**
     * Search group database for a group ID.
     *
     * @param which the group ID to search
     * @return a group entry if found, otherwise {@code null} if not found
     */
    public static GROUP getgrgid(int which) {
        return provider().getgrgid(which);
    }

    /**
     * Get the real user ID.
     *
     * @return the real user ID
     */
    public static int getuid() {
        return provider().getuid();
    }

    /**
     * Get the real group ID.
     *
     * @return the real group ID
     */
    public static int getgid() {
        return provider().getgid();
    }

    /**
     * Send a signal to a process or a group of process.
     *
     * @param pid the process ID
     * @param signal the signal number
     * @return 0 if success, otherwise an error occurred
     */
    public static int kill(int pid, Signal signal) {
        return provider().kill(pid, signal);
    }

    /**
     * Get file status.
     *
     * @param path the file name to obtain information
     * @return the file status information
     */
    public static FileStat lstat(String path) {
        return provider().lstat(path);
    }

    private static final Lock posix_lock = new ReentrantLock();

    /**
     * Locks current thread and perform a destructive posix operation.
     *
     * @param action the action to perform
     * @return the result from action
     */
    public static <R, X extends Throwable> R do_posix(ExceptionFunction<Posix, ? extends R, X> action) throws X {
        posix_lock.lock();
        try {
            return action.evaluate(provider());
        } finally {
            posix_lock.unlock();
        }
    }
}
