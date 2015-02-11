/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.io;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.function.Supplier;

import com.cloudway.platform.common.fp.function.ExceptionSupplier;

/**
 * Represents a supplier that perform I/O operation.
 *
 * @param <T> the type of results supplied by this supplier
 * @see java.util.function.Supplier
 */
@FunctionalInterface
public interface IOSupplier<T> extends Supplier<T>, ExceptionSupplier<T, IOException>
{
    /**
     * Produce the result and wrap {@link IOException} to a {@link UncheckedIOException}.
     *
     * @return a result
     * @throws UncheckedIOException if I/O error occurs
     */
    @Override
    default T get() {
        try {
            return produce();
        } catch (IOException ex) {
            throw new UncheckedIOException(ex);
        }
    }

    /**
     * wraps an I/O supplier into a regular supplier. If the I/O supplier throws
     * {@link IOException}, then the wrapped supplier wraps the IOException to
     * an {@link UncheckedIOException} and thrown.
     *
     * @param other the I/O supplier
     * @return the regular supplier
     */
    static <T> Supplier<T> wrap(IOSupplier<T> other) {
        return other;
    }
}
