/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util;

/**
 * Synonym interface that represents a conditional case.
 */
@FunctionalInterface
public interface ConditionCase<T, R, X extends Throwable>
    extends ExceptionFunction<T, ExceptionSupplier<? extends R, X>, X>
{
}
