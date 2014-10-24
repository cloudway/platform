/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package cloudway.worker;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import com.cloudway.platform.common.util.Etc;
import com.cloudway.platform.common.util.FileUtils;
import com.cloudway.platform.common.util.IO;
import com.cloudway.platform.container.ApplicationContainer;
import com.cloudway.platform.container.ApplicationState;

public class AdminControl
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
            switch (args[0]) {
            case "startall":
                startall();
                break;

            case "stopall":
                stopall();
                break;

            case "restartall":
                restartall();
                break;

            default:
                System.err.println(USAGE);
                System.exit(1);
            }
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
            FileUtils.touch(getShutdownFile(ac), 0600);
        } else {
            Files.deleteIfExists(getShutdownFile(ac));
        }
    }

    private static Path getShutdownFile(ApplicationContainer ac) {
        return FileUtils.join(ac.getHomeDir(), "app", ".shutdown");
    }
}
