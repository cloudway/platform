/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.control;

import com.cloudway.platform.common.fp.function.ExceptionFunction;
import com.cloudway.platform.common.fp.function.ExceptionSupplier;

/**
 * Synonym interface that represents a conditional case.
 *
 * @param <T> the type of argument to the conditional case
 * @param <R> the result type of conditional execution
 * @param <X> the exception type that raised by conditional action
 */
@FunctionalInterface
public interface ConditionCase<T, R, X extends Throwable>
    extends ExceptionFunction<T, ExceptionSupplier<R, X>, X>
{
    /**
     * Lift an inner conditional case to return the final result.
     *
     * <pre>{@code
     * Given a function: T -> () -> R
     * Return a function: T -> R
     * }</pre>
     *
     * @param value the input value to this conditional case
     * @param defaultResult the default result value if conditional case
     * doesn't matches input value
     * @return the final result
     * @throws X if error occurs
     */
    default R lift(T value, R defaultResult) throws X {
        ExceptionSupplier<R, X> sup = evaluate(value);
        return sup != null ? sup.produce() : defaultResult;
    }

    /**
     * Lift an inner conditional case to return the final result.
     *
     * @param value the input value to this conditional case
     * @return the final result if this conditional case matches input value,
     * otherwise {@code null} is returned
     * @throws X if error occurs
     */
    default R lift(T value) throws X {
        return lift(value, null);
    }
}
