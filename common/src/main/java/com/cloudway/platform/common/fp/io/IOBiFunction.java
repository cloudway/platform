/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.io;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.function.BiFunction;

import com.cloudway.platform.common.fp.function.ExceptionBiFunction;

/**
 * Represents an I/O operation that accepts two input arguments.
 *
 * @param <T> the type of the first argument to the operation
 * @param <U> the type of the second argument to the operation
 * @param <R> the type of the result of the operation
 */
@FunctionalInterface
public interface IOBiFunction<T, U, R> extends BiFunction<T, U, R>, ExceptionBiFunction<T, U, R, IOException> {
    /**
     * Applies this operation to the given argument.
     *
     * @param t the first operation argument
     * @param u the second operation argument
     * @return the operation result
     * @throws UncheckedIOException if I/O error occurs
     */
    @Override
    default R apply(T t, U u) {
        try {
            return evaluate(t, u);
        } catch (IOException ex) {
            throw new UncheckedIOException(ex);
        }
    }

    /**
     * wraps an I/O function into a regular function. If the I/O function throws
     * {@link IOException}, then the wrapped function wraps the IOException to
     * an {@link UncheckedIOException} and thrown.
     *
     * @param other the I/O function
     * @return the regular function
     */
    static <T, U, R> BiFunction<T, U, R> wrap(IOBiFunction<T, U, R> other) {
        return other;
    }
}
