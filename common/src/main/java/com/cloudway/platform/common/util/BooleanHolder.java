/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util;

/**
 * Java lambda "closes" environment variables, a {@code BooleanHolder} can captures
 * these variables.
 */
public class BooleanHolder implements java.io.Serializable {
    private static final long serialVersionUID = 3461124418194635930L;

    private boolean value;

    /**
     * Creates a new BooleanHolder with the given initial value.
     *
     * @param initialValue the initial value
     */
    public BooleanHolder(boolean initialValue) {
        value = initialValue;
    }

    /**
     * Creates a new BooleanHolder with initial value {@code false}.
     */
    public BooleanHolder() {}

    /**
     * Gets the hold value.
     *
     * @return the current hold value
     */
    public boolean get() {
        return value;
    }

    /**
     * Sets to the given value.
     *
     * @param newValue the new value
     */
    public void set(boolean newValue) {
        value = newValue;
    }

    /**
     * Sets to the given value and returns the previous value.
     *
     * @param newValue the new value
     * @return the previous value
     */
    public boolean getAndSet(boolean newValue) {
        boolean oldValue = value;
        value = newValue;
        return oldValue;
    }

    /**
     * Returns the String representation of the current value.
     * @return the String representation of the current value
     */
    public String toString() {
        return String.valueOf(value);
    }
}
