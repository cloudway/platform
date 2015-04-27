/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.control.arrow;

import com.cloudway.fp.π;

/**
 * A class for categories. {@link #id} and {@link #compose} must form a monoid.
 *
 * @param <CAT> category typeclass
 */
public interface Category<CAT> {
    /**
     * The identity morphism.
     *
     * <pre>{@code id :: cat a a}</pre>
     */
    <A> π<CAT, A, A> id();

    /**
     * Morphism composition.
     *
     * <pre>{@code (.) :: cat b c -> cat a b -> cat a c}</pre>
     */
    <A, B, C> π<CAT, A, C> compose(π<CAT, B, C> f, π<CAT, A, B> g);

    /**
     * Chained right to left composition.
     *
     * <pre>{@code cat c d . cat b c . cat a b = (cat c d . cat b c) . cat a b}</pre>
     */
    default <A, B, C, D> π<CAT, A, D> compose(π<CAT, C, D> f, π<CAT, B, C> g, π<CAT, A, B> h) {
        return compose(compose(f, g), h);
    }

    /**
     * Left to right composition.
     *
     * <pre>{@code (>>>) :: cat a b -> cat b c -> cat a c}</pre>
     */
    default <A, B, C> π<CAT, A, C> then(π<CAT, A, B> f, π<CAT, B, C> g) {
        return compose(g, f);
    }

    /**
     * Chained left to right composition.
     *
     * <pre>{@code cat a b >>> cat b c >>> cat c d = cat a b >>> (cat b c >>> cat c d)}</pre>
     */
    default <A, B, C, D> π<CAT, A, D> then(π<CAT, A, B> f, π<CAT, B, C> g, π<CAT, C, D> h) {
        return compose(h, g, f);
    }
}
