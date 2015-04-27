/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.function;

/**
 * Represents an operation that may raise exceptions.
 *
 * @param <X> the exception type that raised by action.
 */
@FunctionalInterface
public interface ExceptionAction<X extends Throwable>
{
    /**
     * Perform the operation.
     *
     * @throws X if an error occurs
     */
    void perform() throws X;
}
