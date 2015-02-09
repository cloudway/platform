/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util.function;

/**
 * Represents a supplier that may produce exception when supplying the result.
 *
 * @param <T> the type of result supplied by the supplier
 * @param <X> the exception raised by supplier
 */
@FunctionalInterface
public interface ExceptionSupplier<T, X extends Throwable>
{
    /**
     * Produce the result.
     *
     * @return a result
     * @throws X if error occurs
     */
    T produce() throws X;
}
