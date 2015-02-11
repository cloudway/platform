/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.data;

/**
 * A pair of two elements with same type.
 *
 * @param <T> the element type
 */
public class Pair<T> extends Tuple<T, T>
{
    private static final long serialVersionUID = -3283951855776432147L;

    public Pair(T left, T right) {
        super(left, right);
    }

    /**
     * Construct a new Pair with two arguments.
     *
     * @param left the first argument
     * @param right the second argument
     */
    public static <T> Pair<T> make(T left, T right) {
        return new Pair<>(left, right);
    }
}
