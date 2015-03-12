/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.data;

/**
 * A pair of two elements with same type.
 *
 * @param <T> the elements type
 */
public class Pair<T> extends Tuple<T, T> {
    private static final long serialVersionUID = -3283951855776432147L;

    public Pair(T first, T second) {
        super(first, second);
    }
}
