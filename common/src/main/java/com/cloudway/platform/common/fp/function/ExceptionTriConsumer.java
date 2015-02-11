/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.function;

/**
 * Represents an operation that accepts three arguments and result no
 * result. The operation may raise exception at runtime.
 *
 * @param <T> the type of the first argument to the operation
 * @param <U> the type of the second argument to the operation
 * @param <V> the type of the third argument to the operation
 * @param <X> the type of the exception raised by the operation
 */
@FunctionalInterface
public interface ExceptionTriConsumer<T, U, V, X extends Throwable>
{
    /**
     * Performs this operation to the given arguments.
     *
     * @param t the first input argument
     * @param u the second input argument
     * @param v the third input argument
     * throws X if error occurs
     */
    void consume(T t, U u, V v) throws X;
}
