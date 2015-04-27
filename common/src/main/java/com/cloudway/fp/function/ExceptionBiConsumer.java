/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.function;

/**
 * Represents an operation that accepts two input arguments and returns no
 * result. The operation may throw exceptions at runtime
 *
 * @param <T> the type of the first argument to the operation
 * @param <U> the type of the second argument to the operation
 * @param <X> the exception type that may throw by the operation
 */
@FunctionalInterface
public interface ExceptionBiConsumer<T, U, X extends Throwable>
{
    /**
     * Performs this operation on the given arguments
     *
     * @param t the first input argument
     * @param u the second input argument
     * @throws X if error occurs
     */
    void consume(T t, U u) throws X;
}
