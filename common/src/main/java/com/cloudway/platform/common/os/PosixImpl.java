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

import jnr.constants.platform.Signal;
import jnr.posix.FileStat;
import jnr.posix.POSIXFactory;
import jnr.posix.Passwd;

import com.cloudway.platform.common.os.Etc.PASSWD;
import com.cloudway.platform.common.os.Etc.GROUP;

enum PosixImpl implements Posix
{
    INSTANCE;

    // Create POSIX instance when it's actually used
    static final class Holder {
        private Holder() {}
        static final jnr.posix.POSIX posix = POSIXFactory.getPOSIX();
        static final Lock pw_lock = new ReentrantLock();
        static final Lock gr_lock = new ReentrantLock();
    }

    // The following method must be synchronized because returned Passwd pointer is shared

    @Override
    public void getpwent(Consumer<PASSWD> action) {
        jnr.posix.POSIX posix = Holder.posix;
        Holder.pw_lock.lock();
        try {
            for (Passwd pwent = posix.getpwent(); pwent != null; pwent = posix.getpwent()) {
                action.accept(PASSWD.from(pwent));
            }
        } finally {
            try {
                posix.endpwent();
            } finally {
                Holder.pw_lock.unlock();
            }
        }
    }

    @Override
    public PASSWD getpwuid(int which) {
        Holder.pw_lock.lock();
        try {
            return PASSWD.from(Holder.posix.getpwuid(which));
        } finally {
            Holder.pw_lock.unlock();
        }
    }

    @Override
    public PASSWD getpwnam(String which) {
        Objects.requireNonNull(which);
        Holder.pw_lock.lock();
        try {
            return PASSWD.from(Holder.posix.getpwnam(which));
        } finally {
            Holder.pw_lock.unlock();
        }
    }

    @Override
    public GROUP getgrgid(int which) {
        Holder.gr_lock.lock();
        try {
            return GROUP.from(Holder.posix.getgrgid(which));
        } finally {
            Holder.gr_lock.unlock();
        }
    }

    @Override
    public int getuid() {
        return Holder.posix.getuid();
    }

    @Override
    public int getgid() {
        return Holder.posix.getgid();
    }

    @Override
    public int kill(int pid, Signal signal) {
        return Holder.posix.kill(pid, signal.intValue());
    }

    @Override
    public FileStat lstat(String path) {
        return Holder.posix.lstat(path);
    }
}
