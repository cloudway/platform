/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp;

/**
 * The type-lifting interface for type constructors with two arguments.
 *
 * @param <T> the type class of the data structure
 * @param <A> the first element type of the data structure
 * @param <B> the second element type of the data structure
 */
public interface π<T, A, B> {
    /**
     * Returns the type class of this data structure at runtime.
     */
    T getTypeClass();
}
