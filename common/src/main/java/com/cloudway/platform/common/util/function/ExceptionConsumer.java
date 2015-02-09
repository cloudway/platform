/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util.function;

/**
 * Represents an operation that accepts a single input argument and returns
 * no result. The operation may raise exception at runtime.
 *
 * @param <T> the type of the input to the operation
 * @param <X> the exception raised by operation
 */
@FunctionalInterface
public interface ExceptionConsumer<T, X extends Throwable>
{
    /**
     * Performs this operation on the given argument.
     *
     * @param t the input argument
     * @throws X if I/O error occurs
     */
    void consume(T t) throws X;
}
