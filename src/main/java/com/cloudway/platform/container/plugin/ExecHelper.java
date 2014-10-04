/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container.plugin;

import java.io.IOException;

import jnr.constants.platform.Errno;
import jnr.posix.POSIX;
import jnr.posix.POSIXFactory;

public class ExecHelper
{
    public static void main(String[] args) throws Exception {
        POSIX posix = POSIXFactory.getPOSIX();

        int uid = Integer.parseInt(System.getProperty("posix.uid"));

        if (posix.setuid(uid) < 0) {
            System.err.println("failed to set uid to " + uid + ": " +
                                   Errno.valueOf(posix.errno()).description());
        }

        try {
            ProcessBuilder pb = new ProcessBuilder(args).inheritIO();
            System.exit(pb.start().waitFor());
        } catch (IOException ex) {
            System.err.println("exec failed: " + ex.getMessage());
            System.exit(-1);
        }

        /*
        posix.execv(args[0], args);

        System.err.println("exec failed: " + Errno.valueOf(posix.errno()).description());
        System.exit(-1);
        */
    }
}
