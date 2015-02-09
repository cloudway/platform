/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util.function;

/**
 * Represents a function that accepts three arguments and produces a result.
 * The function may raise exception at runtime.
 *
 * @param <T> the type of the first argument to the function
 * @param <U> the type of the second argument to the function
 * @param <V> the type of the third argument to the function
 * @param <R> the type of the result of the function
 * @param <X> the type of the exception raised by the function
 */
@FunctionalInterface
public interface ExceptionTriFunction<T, U, V, R, X extends Throwable>
{
    /**
     * Evaluate this function to the given arguments.
     *
     * @param t the first function argument
     * @param u the second function argument
     * @param v the third function argument
     * @return the function result
     * @throws X if error occurs
     */
    R evaluate(T t, U u, V v) throws X;
}
