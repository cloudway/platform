/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.data;

import java.util.function.IntBinaryOperator;
import java.util.function.IntSupplier;
import java.util.function.IntUnaryOperator;

/**
 * Java lambda "closes" environment variables, an {@code IntRef} can captures
 * these variables.
 */
public class IntRef implements IntSupplier, java.io.Serializable {
    private static final long serialVersionUID = 5409316215531576258L;

    private int value;

    /**
     * Creates a new IntRef with the given initial value.
     *
     * @param initialValue the initial value
     */
    public IntRef(int initialValue) {
        value = initialValue;
    }

    /**
     * Creates a new IntRef with initial value {@code 0}.
     */
    public IntRef() {}

    /**
     * Gets the indirect referenced value.
     *
     * @return the current indirect referenced value
     */
    public int get() {
        return value;
    }

    /**
     * Gets the hold value as a result.
     *
     * @return the current hold value
     */
    @Override
    public int getAsInt() {
        return value;
    }

    /**
     * Sets to the given value.
     *
     * @param newValue the new value
     */
    public int set(int newValue) {
        return value = newValue;
    }

    /**
     * Sets to the given value and returns the old value.
     *
     * @param newValue the new value
     * @return the previous value
     */
    public int getAndSet(int newValue) {
        int oldValue = value;
        value = newValue;
        return oldValue;
    }

    /**
     * Increments by one the current value.
     *
     * @return the updated value
     */
    public int increment() {
        return ++value;
    }

    /**
     * Increments by one the current value.
     *
     * @return the previous value
     */
    public int getAndIncrement() {
        return value++;
    }

    /**
     * Decrements by one the current value.
     *
     * @return the updated value
     */
    public int decrement() {
        return --value;
    }

    /**
     * Decrements by one the current value.
     *
     * @return the previous value
     */
    public int getAndDecrement() {
        return value--;
    }

    /**
     * Adds the given value to the current value
     *
     * @param delta the value to add
     * @return the updated value
     */
    public int add(int delta) {
        return value += delta;
    }

    /**
     * Adds the given value to the current value.
     *
     * @param delta the value to add
     * @return the previous value
     */
    public int getAndAdd(int delta) {
        int oldValue = value;
        value += delta;
        return oldValue;
    }

    /**
     * Updates the current value with the results of applying the given function,
     * returning the updated value.
     *
     * @param updater a function to evaluate new value
     * @return the updated value
     */
    public int update(IntUnaryOperator updater) {
        return value = updater.applyAsInt(value);
    }

    /**
     * Updates the current value with the results of applying the given function,
     * returning the previous value.
     *
     * @param updater a function to evaluate new value
     * @return the previous value
     */
    public int getAndUpdate(IntUnaryOperator updater) {
        int oldValue = value;
        value = updater.applyAsInt(value);
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
    public int accumulate(int x, IntBinaryOperator accumulator) {
        return value = accumulator.applyAsInt(value, x);
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
    public int getAndAccumulate(int x, IntBinaryOperator accumulator) {
        int oldValue = value;
        value = accumulator.applyAsInt(value, x);
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
