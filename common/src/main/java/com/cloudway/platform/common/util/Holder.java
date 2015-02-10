/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util;

import java.util.function.BiFunction;
import java.util.function.UnaryOperator;

/**
 * Java lambda "closes" environment variables, a {@code Holder} can captures
 * these variables.
 */
public class Holder<V> implements java.io.Serializable {
    private static final long serialVersionUID = -4356253247069623807L;

    private V value;

    /**
     * Creates a new holder with the given initial value
     *
     * @param initialValue the initialValue
     */
    public Holder(V initialValue) {
        this.value = initialValue;
    }

    /**
     * Creates a new Holder with null initial value.
     */
    public Holder() {}

    /**
     * Gets the hold value.
     *
     * @return the hold value.
     */
    public V get() {
        return value;
    }

    /**
     * Holds the given value.
     *
     * @param newValue the new value
     */
    public void set(V newValue) {
        this.value = newValue;
    }

    /**
     * Sets to the given value and returns the old value.
     *
     * @param newValue the new value
     * @return the previous value
     */
    public V getAndSet(V newValue) {
        V oldValue = value;
        value = newValue;
        return oldValue;
    }

    /**
     * Updates the current value with the results of applying the given function,
     * returning the previous value.
     *
     * @param updater a function to evaluate new value
     * @return the previous value
     */
    public V getAndUpdate(UnaryOperator<V> updater) {
        V oldValue = value;
        value = updater.apply(value);
        return oldValue;
    }

    /**
     * Updates the current value with the results of applying the given function,
     * returning the updated value.
     *
     * @param updater a function to evaluate new value
     * @return the updated value
     */
    public V updateAndGet(UnaryOperator<V> updater) {
        return value = updater.apply(value);
    }

    /**
     * Updates the current value with the results of applying the given
     * function to the current and given value, returning the previous
     * value.
     *
     * @param x the update value
     * @param accumulator a function of two arguments
     * @return the previous value
     */
    public <U> V getAndAccumulate(U x, BiFunction<V, ? super U, V> accumulator) {
        V oldValue = value;
        value = accumulator.apply(value, x);
        return oldValue;
    }

    /**
     * Updates the current value with the results of applying the given
     * function to the current and given value, returning the updated
     * value.
     *
     * @param x the update value
     * @param accumulator a function of two arguments
     * @return the updated value
     */
    public <U> V accumulateAndGet(U x, BiFunction<V, ? super U, V> accumulator) {
        return value = accumulator.apply(value, x);
    }

    /**
     * Returns the String representation of the current value.
     * @return the String representation of the current value
     */
    public String toString() {
        return String.valueOf(value);
    }
}
