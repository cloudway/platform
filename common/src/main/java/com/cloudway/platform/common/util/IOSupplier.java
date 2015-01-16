/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.function.Supplier;

/**
 * Represents a supplier that perform I/O operation.
 *
 * @param <T> the type of results supplied by this supplier
 * @see java.util.function.Supplier
 */
@FunctionalInterface
public interface IOSupplier<T>
{
    /**
     * Gets the result.
     *
     * @return a result
     * @throws IOException if I/O error occurs
     */
    T get() throws IOException;

    /**
     * wraps an I/O supplier into a regular supplier. If the I/O supplier throws
     * {@link IOException}, then the wrapped supplier wraps the IOException to
     * an {@link UncheckedIOException} and thrown.
     *
     * @param other the I/O supplier
     * @return the regular supplier
     */
    static <T> Supplier<T> wrap(IOSupplier<? extends T> other) {
        return () -> {
            try {
                return other.get();
            } catch (IOException ex) {
                throw new UncheckedIOException(ex);
            }
        };
    }
}
