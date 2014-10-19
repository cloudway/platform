/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container.velocity;

/**
 * A tool object that handles alternator.
 */
public final class Alt
{
    /**
     * <p>Returns the first non-null argument.
     *
     * <pre>
     * Usage:
     *    $alt.of($MY_OPTION, "default-value")
     * </pre>
     *
     * <p>will generate the variable value if it's present in context,
     * otherwise the default value is generated.
     */
    public Object of(Object... args) {
        if (args == null || args.length == 0)
            return null;

        for (Object arg : args) {
            if (arg != null)
                return arg;
        }

        return "";
    }

    /**
     * <p>Insert formatted string if the given argument is present.
     *
     * <pre>
     * Usage:
     *    $alt.insert("my_option = %s", $MY_OPTION)
     * </pre>
     *
     * <p>will generate the formatted string if the variable is present,
     * otherwise nothing is generated.
     */
    public String insert(String format, Object arg) {
        if (format == null)
            return null;

        if (arg == null)
            return "";

        return String.format(format, arg);
    }

    public String toString() {
        return "$alt";
    }
}
