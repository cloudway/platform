/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.Objects;
import java.util.function.Consumer;

/**
 * Represents an I/O operation that accepts a single input argument and returns
 * no result.
 *
 * @param <T> the type of the input to the operation.
 *
 * @see java.util.function.Consumer
 */
@FunctionalInterface
public interface IOConsumer<T>
{
    /**
     * Performs this operation on the given argument.
     *
     * @param t the input argument
     * @throws IOException if I/O error occurs
     */
    void accept(T t) throws IOException;

    /**
     * Returns a composed {@code IOConsumer} that performs, in sequence, this
     * operation followed by the {@code after} operation. If performing either
     * operation throws an exception, it is relayed to the caller of the
     * composed operation.  If performing this operation throws an exception,
     * the {@code after} operation will not be performed.
     *
     * @param after the operation to perform after this operation
     * @return a composed {@code IOConsumer} that performs in sequence this
     * operation followed by the {@code after} operation
     * @throws NullPointerException if {@code after} is null
     */
    default IOConsumer<T> andThen(IOConsumer<? super T> after) {
        Objects.requireNonNull(after);
        return (T t) -> { accept(t); after.accept(t); };
    }

    /**
     * Wraps an I/O consumer into a regular consumer. If the I/O consumer throws
     * {@link IOException}, then the wrapped consumer wraps the IOException to
     * an {@link UncheckedIOException} and thrown.
     *
     * @param other the I/O consumer
     * @return the regular consumer
     */
    static <T> Consumer<T> wrap(IOConsumer<? super T> other) {
        return (T t) -> {
            try {
                other.accept(t);
            } catch (IOException ex) {
                throw new UncheckedIOException(ex);
            }
        };
    }
}
