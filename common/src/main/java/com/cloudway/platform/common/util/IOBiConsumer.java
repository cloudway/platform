/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.function.BiConsumer;

/**
 * Represents an I/O operation that accepts two input arguments and returns no
 * result.
 *
 * @param <T> the type of the first argument to the operation
 * @param <U> the type of the second argument to the operation
 *
 * @see java.util.function.BiConsumer
 */
@FunctionalInterface
public interface IOBiConsumer<T, U> extends BiConsumer<T, U>, ExceptionBiConsumer<T, U, IOException>
{
    /**
     * Performs this I/O operation on the given arguments.
     *
     * @param t the first input argument
     * @param u the second input argument
     * @throws UncheckedIOException if I/O error occurs
     */
    @Override
    default void accept(T t, U u) {
        try {
            consume(t, u);
        } catch (IOException ex) {
            throw new UncheckedIOException(ex);
        }
    }

    /**
     * Wraps an I/O consumer into a regular consumer. If the I/O consumer throws
     * {@link IOException}, then the wrapped consumer wraps the IOException to
     * an {@link UncheckedIOException} and thrown.
     *
     * @param other the I/O consumer
     * @return the regular consumer
     */
    @SuppressWarnings("unchecked")
    static <T, U> BiConsumer<T, U> wrap(IOBiConsumer<T, U> other) {
        return other;
    }
}
