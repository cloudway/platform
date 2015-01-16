/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container.plugin;

import jnr.constants.platform.Errno;
import jnr.posix.POSIX;
import jnr.posix.POSIXFactory;

public final class ExecHelper
{
    private static final POSIX posix = POSIXFactory.getPOSIX();

    public static void main(String[] args) throws Exception {
        int uid = Integer.parseInt(System.getProperty("posix.uid"));

        if (posix.setgid(uid) < 0 || posix.setuid(uid) < 0) {
            perror("failed to set uid to " + uid);
            System.exit(-1);
        }

        execvp(args[0], args);
        perror("exec failed: " + args[0]);
        System.exit(-1);
    }

    private static void execvp(String file, String[] argv) {
        if (file.indexOf('/') != -1) {
            posix.execv(file, argv);
        } else {
            // We must search PATH
            int sticky_errno = 0;
            for (String path : getPathElements()) {
                String expanded_file = path;
                if (!path.endsWith("/"))
                    expanded_file += "/";
                expanded_file += file;

                posix.execv(expanded_file, argv);
                switch (Errno.valueOf(posix.errno())) {
                case EACCES:
                    sticky_errno = posix.errno();
                    // FALLTHRU
                case ENOENT:
                case ENOTDIR:
                case ELOOP:
                case ESTALE:
                case ENODEV:
                case ETIMEDOUT:
                    // try other directories in PATH
                    break;
                default:
                    return;
                }
            }
            if (sticky_errno != 0) {
                posix.errno(sticky_errno);
            }
        }
    }

    private static String[] getPathElements() {
        String path = System.getenv("PATH");
        if (path == null)
            path = "/usr/bin:bin";
        return path.split(":");
    }

    private static void perror(String s) {
        System.err.println(s + ": " + Errno.valueOf(posix.errno()).description());
    }
}
