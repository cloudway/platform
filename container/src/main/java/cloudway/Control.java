/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package cloudway;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Comparator;
import java.util.stream.Stream;

import com.cloudway.platform.common.Config;
import com.cloudway.platform.common.util.Etc;
import com.cloudway.platform.common.util.FileUtils;
import com.cloudway.platform.container.ApplicationContainer;

/**
 * Entry point for application control commands.
 */
public abstract class Control
{
    public static void main(String args[]) {
        Control control;
        if (Etc.getuid() == 0) {
            control = new PrivilegedControl();
        } else {
            control = new UserControl();
        }

        Method action = getActionMethod(control.getClass(), args);
        Throwable failure = null;

        try {
            Object[] arguments = new Object[1];
            arguments[0] = Arrays.copyOfRange(args, 1, args.length);
            action.invoke(control, arguments);
        } catch (InvocationTargetException ex) {
            failure = ex.getCause();
        } catch (Exception ex) {
            failure = ex;
        }

        if (failure != null) {
            if (failure.getMessage() != null) {
                System.err.println("command failure: " + failure);
            } else {
                System.err.println("command failure");
                failure.printStackTrace();
            }
            System.exit(2);
        }
    }

    private static Method getActionMethod(Class<? extends Control> cls, String[] args) {
        Method action = null;

        if (args.length != 0) {
            try {
                String command = args[0].replaceAll("-", "_");
                action = cls.getMethod(command, String[].class);
            } catch (Exception ex) {
                // fallthrough to report error
            }
        }

        if (action == null) {
            System.err.println("invalid command. Use \"cwctl help\" for more information");
            System.exit(1);
            return null;
        }

        return action;
    }

    @Command("Show this help message")
    @SuppressWarnings("unused")
    public void help(String[] args) {
        System.err.println("Usage: cwctl COMMAND [ARGS...]");
        System.err.println();
        System.err.println("COMMANDS:");
        System.err.println();
        Stream.of(this.getClass().getMethods())
            .sorted(Comparator.comparing(Method::getName))
            .forEach(m -> {
                Command description = m.getAnnotation(Command.class);
                if (description != null) {
                    System.err.printf("  %-12s%s%n", m.getName(), description.value());
                }
            });
        System.err.println();
    }

    protected void install(ApplicationContainer container, String source, String repo)
        throws IOException
    {
        Path source_path;
        if (source.indexOf('/') != -1) {
            source_path = Paths.get(source).toAbsolutePath().normalize();
        } else {
            source_path = FileUtils.join(Config.VAR_DIR, ".addons", source);
        }

        if (!Files.exists(source_path)) {
            System.err.print(source + ": No such file or directory");
            System.exit(2);
            return;
        }

        container.install(source_path, repo);
    }
}
