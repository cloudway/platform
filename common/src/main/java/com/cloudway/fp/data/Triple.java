/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.data;

import com.cloudway.fp.function.TriFunction;

/**
 * A tuple with three elements.
 *
 * @param <A> the type of the first element
 * @param <B> the type of the second element
 * @param <C> the type of the third element
 */
public class Triple<A, B, C> extends Tuple<A, Tuple<B, C>> {
    private static final long serialVersionUID = 2154886533689247459L;

    /**
     * Construct a new tuple with three elements.
     *
     * @param a the first element
     * @param b the second element
     * @param c the third element
     */
    public Triple(A a, B b, C c) {
        super(a, Tuple.of(b, c));
    }

    /**
     * Returns the first element.
     *
     * @return the first element
     */
    public A _1() {
        return first();
    }

    /**
     * Returns the second element.
     *
     * @return the second element
     */
    public B _2() {
        return second().first();
    }

    /**
     * Returns the third element.
     *
     * @return the third element
     */
    public C _3() {
        return second().second();
    }

    /**
     * Apply this triple as arguments to a function.
     */
    public <R> R as(TriFunction<? super A, ? super B, ? super C, ? extends R> f) {
        return f.apply(_1(), _2(), _3());
    }

    public String toString() {
        return "(" + _1() + "," + _2() + "," + _3() + ")";
    }
}
