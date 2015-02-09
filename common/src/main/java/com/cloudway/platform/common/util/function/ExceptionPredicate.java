/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util.function;

/**
 * Represents a predicate (boolean-valued function) that may produce exception
 * when evaluating the result.
 *
 * @param <T> the type of the input to the predicate
 * @param <X> the exception raised by predicate
 */
@FunctionalInterface
public interface ExceptionPredicate<T, X extends Throwable>
{
    /**
     * Evaluate this predicate on this given argument.
     *
     * @param t the input argument
     * @return {@code true} if the input argument matches the predicate,
     * otherwise {@code false}
     * @throws X if error occurs
     */
    boolean evaluate(T t) throws X;
}
