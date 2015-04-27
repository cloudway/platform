/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.data;

import java.util.function.BooleanSupplier;

/**
 * Java lambda "closes" environment variables, a {@code BooleanRef} can captures
 * these variables.
 */
public class BooleanRef implements BooleanSupplier, java.io.Serializable {
    private static final long serialVersionUID = 3461124418194635930L;

    private boolean value;

    /**
     * Creates a new BooleanRef with the given initial value.
     *
     * @param initialValue the initial value
     */
    public BooleanRef(boolean initialValue) {
        value = initialValue;
    }

    /**
     * Creates a new BooleanRef with initial value {@code false}.
     */
    public BooleanRef() {}

    /**
     * Gets the indirect referenced value.
     *
     * @return the current indirect referenced value
     */
    public boolean get() {
        return value;
    }

    /**
     * Gets the hold value as a result.
     *
     * @return the current hold value
     */
    @Override
    public boolean getAsBoolean() {
        return value;
    }

    /**
     * Sets to the given value.
     *
     * @param newValue the new value
     */
    public boolean set(boolean newValue) {
        return value = newValue;
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
     * Negates the referenced boolean value.
     *
     * @return the negation value
     */
    public boolean negate() {
        return value = !value;
    }

    /**
     * Returns the String representation of the current value.
     * @return the String representation of the current value
     */
    public String toString() {
        return String.valueOf(value);
    }
}
