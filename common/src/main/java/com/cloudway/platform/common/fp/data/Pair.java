/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.data;

import java.util.function.Function;

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

    public <R> Pair<R> map2(Function<? super T, ? extends R> f) {
        return map2(f, f);
    }

    public <R> Pair<R> map2(Function<? super T, ? extends R> f,
                            Function<? super T, ? extends R> g) {
        return new Pair<>(f.apply(first()), g.apply(second()));
    }
}
