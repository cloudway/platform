/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.io;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.function.Function;

import com.cloudway.platform.common.fp.function.ExceptionFunction;

/**
 * Represents a function that perform I/O operation.
 *
 * @param <T> the type of the input to the function
 * @param <R> the type of the result of the function
 * @see java.util.function.Function
 */
@FunctionalInterface
public interface IOFunction<T, R> extends Function<T, R>, ExceptionFunction<T, R, IOException>
{
    /**
     * Applies this function to the given argument.
     *
     * @param t the function argument
     * @return the function result
     * @throws UncheckedIOException if I/O error occurs
     */
    @Override
    default R apply(T t) {
        try {
            return evaluate(t);
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
    static <T, R> Function<T, R> wrap(IOFunction<T, R> other) {
        return other;
    }
}
