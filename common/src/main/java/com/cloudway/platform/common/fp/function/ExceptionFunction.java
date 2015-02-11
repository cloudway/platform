/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.function;

/**
 * Represents a function that may produce exception when applied.
 *
 * @param <T> the type of the input to the function
 * @param <R> the type of the result of the function
 * @param <X> the type of the exception raised by the function
 */
@FunctionalInterface
public interface ExceptionFunction<T, R, X extends Throwable>
{
    /**
     * Calls this function to the given argument.
     *
     * @param t the function argument
     * @return the function result
     * @throws X if error occurs
     */
    R evaluate(T t) throws X;
}
