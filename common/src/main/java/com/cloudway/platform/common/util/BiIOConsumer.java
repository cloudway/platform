/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.Objects;
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
public interface BiIOConsumer<T, U>
{
    /**
     * Performs this I/O operation on the given arguments.
     *
     * @param t the first input argument
     * @param u the second input argument
     * @throws IOException if I/O error occurs
     */
    void accept(T t, U u) throws IOException;

    /**
     * Returns a composed {@code IOBiConsumer} that performs, in sequence, this
     * operation followed by the {@code after} operation. If performing either
     * operation throws an exception, it is relayed to the caller of the
     * composed operation.  If performing this operation throws an exception,
     * the {@code after} operation will not be performed.
     *
     * @param after the operation to perform after this operation
     * @return a composed {@code IOBiConsumer} that performs in sequence this
     * operation followed by the {@code after} operation
     * @throws NullPointerException if {@code after} is null
     */
    default BiIOConsumer<T, U> andThen(BiIOConsumer<? super T, ? super U> after) {
        Objects.requireNonNull(after);

        return (l, r) -> {
            accept(l, r);
            after.accept(l, r);
        };
    }

    /**
     * Wraps an I/O consumer into a regular consumer. If the I/O consumer throws
     * {@link IOException}, then the wrapped consumer wraps the IOException to
     * an {@link UncheckedIOException} and thrown.
     *
     * @param other the I/O consumer
     * @return the regular consumer
     */
    static <T, U> BiConsumer<T,U> wrap(BiIOConsumer<? super T, ? super U> other) {
        return (T t, U u) -> {
            try {
                other.accept(t, u);
            } catch (IOException ex) {
                throw new UncheckedIOException(ex);
            }
        };
    }
}
