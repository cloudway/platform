/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.io;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.function.Consumer;

import com.cloudway.fp.function.ExceptionConsumer;

/**
 * Represents an I/O operation that accepts a single input argument and returns
 * no result.
 *
 * @param <T> the type of the input to the operation.
 *
 * @see java.util.function.Consumer
 */
@FunctionalInterface
public interface IOConsumer<T> extends Consumer<T>, ExceptionConsumer<T, IOException>
{
    /**
     * Performs this operation on the given argument.
     *
     * @param t the input argument
     * @throws UncheckedIOException if I/O error occurs
     */
    @Override
    default void accept(T t) {
        try {
            consume(t);
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
    static <T> Consumer<T> wrap(IOConsumer<T> other) {
        return other;
    }
}
