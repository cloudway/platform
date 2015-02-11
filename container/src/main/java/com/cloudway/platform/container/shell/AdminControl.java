/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container.shell;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import com.cloudway.platform.common.os.Etc;
import com.cloudway.platform.common.util.MoreFiles;
import com.cloudway.platform.common.fp.io.IO;
import com.cloudway.platform.container.ApplicationContainer;
import com.cloudway.platform.container.ApplicationState;

public final class AdminControl
{
    private static final String USAGE = "Usage: cwadminctl {startall|stopall|restartall}";

    public static void main(String args[]) {
        if (Etc.getuid() != 0) {
            System.exit(-1);
        }

        if (args.length != 1) {
            System.err.println(USAGE);
            System.exit(1);
        }

        try {
            IO.with(args[0])
              .when("startall",   AdminControl::startall)
              .when("stopall",    AdminControl::stopall)
              .when("restartall", AdminControl::restartall)
              .otherwise(() -> {
                  System.err.println(USAGE);
                  System.exit(1);
              });
        } catch (Exception ex) {
            System.err.println("Command failure");
            ex.printStackTrace();
            System.exit(2);
        }
    }

    private static void startall() throws IOException {
        IO.forEach(ApplicationContainer.all().parallel(), ac -> {
            try {
                if (ac.getState() == ApplicationState.STARTED || isShuttingDown(ac)) {
                    ac.start();
                }
            } catch (Exception ex) {
                System.err.println("Failed to start application " + ac.getId());
                ex.printStackTrace();
            } finally {
                setShutdown(ac, false);
            }
        });
    }

    private static void stopall() throws IOException {
        IO.forEach(ApplicationContainer.all().parallel(), ac -> {
            if (ac.getState() == ApplicationState.STARTED) {
                try {
                    ac.stop();
                } catch (Exception ex) {
                    System.err.println("Failed to stop application " + ac.getId());
                    ex.printStackTrace();
                } finally {
                    setShutdown(ac, true);
                }
            }
        });
    }

    private static void restartall() throws IOException {
        IO.forEach(ApplicationContainer.all().parallel(), ac -> {
            try {
                if (ac.getState() == ApplicationState.STARTED || isShuttingDown(ac)) {
                    ac.restart();
                }
            } catch (Exception ex) {
                System.err.println("Failed to restart application " + ac.getId());
                ex.printStackTrace();
            } finally {
                setShutdown(ac, false);
            }
        });
    }

    private static boolean isShuttingDown(ApplicationContainer ac) {
        return Files.exists(getShutdownFile(ac));
    }

    private static void setShutdown(ApplicationContainer ac, boolean shutdown)
        throws IOException
    {
        if (shutdown) {
            MoreFiles.touch(getShutdownFile(ac), 0600);
        } else {
            Files.deleteIfExists(getShutdownFile(ac));
        }
    }

    private static Path getShutdownFile(ApplicationContainer ac) {
        return MoreFiles.join(ac.getHomeDir(), "app", ".shutdown");
    }
}
