/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.data;

import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.function.UnaryOperator;

/**
 * Java lambda "closes" environment variables, a {@code Ref} can captures
 * these variables.
 */
public class Ref<V> implements Supplier<V>, java.io.Serializable {
    private static final long serialVersionUID = -4356253247069623807L;

    private V value;

    /**
     * Creates a new Ref with the given initial value
     *
     * @param initialValue the initialValue
     */
    public Ref(V initialValue) {
        this.value = initialValue;
    }

    /**
     * Creates a new Ref with null initial value.
     */
    public Ref() {}

    /**
     * Gets the indirect referenced value.
     *
     * @return the indirect referenced value.
     */
    @Override
    public V get() {
        return value;
    }

    /**
     * Holds the given value.
     *
     * @param newValue the new value
     */
    public V set(V newValue) {
        return this.value = newValue;
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
     * returning the updated value.
     *
     * @param updater a function to evaluate new value
     * @return the updated value
     */
    public V update(UnaryOperator<V> updater) {
        return value = updater.apply(value);
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
     * Updates the current value with the results of applying the given
     * function to the current and given value, returning the updated
     * value.
     *
     * @param x the update value
     * @param accumulator a function of two arguments
     * @return the updated value
     */
    public <U> V accumulate(U x, BiFunction<V, ? super U, V> accumulator) {
        return value = accumulator.apply(value, x);
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
     * Returns the String representation of the current value.
     * @return the String representation of the current value
     */
    public String toString() {
        return String.valueOf(value);
    }

    /**
     * Convenient method to create a self referenced computation.
     *
     * @param <T> the return type of the computation
     * @param computation a computation that accept a reference to the
     * intermediate computation value and return the final computation
     * value
     */
    public static <T> T cycle(Function<Ref<T>, T> computation) {
        Ref<T> ref = new Ref<>();
        return ref.set(computation.apply(ref));
    }
}
