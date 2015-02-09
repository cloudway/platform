/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.io;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.function.Predicate;

import com.cloudway.platform.common.util.function.ExceptionPredicate;

/**
 * Represents a predicate (boolean-valued function) that performs I/O operation.
 *
 * @param <T> the type of the input to the predicate
 *
 * @see java.util.function.Predicate
 */
@FunctionalInterface
public interface IOPredicate<T> extends Predicate<T>, ExceptionPredicate<T, IOException>
{
    /**
     * Evaluate this predicate on the given argument.
     *
     * @param t the input argument
     * @return {@code true} if the input argument matches the predicate,
     * otherwise {@code false}
     * @throws UncheckedIOException if I/O error occurs
     */
    @Override
    default boolean test(T t) {
        try {
            return evaluate(t);
        } catch (IOException ex) {
            throw new UncheckedIOException(ex);
        }
    }

    /**
     * Wraps an I/O predicate into a regular predicate. If the I/O predicate throws
     * {@link IOException}, then the wrapped predicate wraps the IOException to
     * an {@link UncheckedIOException} and thrown.
     *
     * @param other the I/O predicate
     * @return the regular predicate
     */
    @SuppressWarnings("unchecked")
    static <T> Predicate<T> wrap(IOPredicate<T> other) {
        return other;
    }
}
