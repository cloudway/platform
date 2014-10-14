/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package cloudway;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.stream.Stream;

import jnr.posix.POSIX;
import jnr.posix.POSIXFactory;

/**
 * Entry point for application actions.
 */
public abstract class Control
{
    protected static POSIX posix = POSIXFactory.getPOSIX();

    public static void main(String args[]) {
        Control control;
        if (posix.getuid() == 0) {
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
        System.err.println("usage: cwctl command [args...]");
        System.err.println();
        System.err.println("COMMANDS:");
        System.err.println();
        Stream.of(this.getClass().getMethods()).forEach(m -> {
            Command description = m.getAnnotation(Command.class);
            if (description != null) {
                System.err.printf("  %-10s%s%n", m.getName(), description.value());
            }
        });
        System.err.println();
    }
}
