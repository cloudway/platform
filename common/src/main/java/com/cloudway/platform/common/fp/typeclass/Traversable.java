/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.typeclass;

import java.util.function.Function;

import com.cloudway.platform.common.fp.data.Fn;

/**
 * Class of data structures that can be traversed from left to right, performing
 * an action on each element.
 *
 * <p>A definition of {@link Traversable#traverse traverse} must satisfy the
 * following laws:</p>
 *
 * <p><strong>naturality</strong>
 * <pre>{@code t . traverse f = traverse (t . f)}</pre>
 *
 * <p><strong>identity</strong>
 * <pre>{@code traverse Identity = Identity}</pre>
 *
 * <p><strong>composition</strong></p>
 * <pre>{@code traverse (Compose . map g . f) = Compose . map (traverse g) . traverse f}</pre>
 *
 * <p>A definition of {@link Traversable#sequence sequence} must satisfy the
 * following laws:
 *
 * <p><strong>naturality</strong>
 * <pre>{@code t . sequenceA = sequenceA . map t}</pre>
 *
 * <p><strong>identity</strong>
 * <pre>{@code sequenceA . map Identity = Identity}</pre>
 *
 * <p><strong>composition</strong>
 * <pre>{@code sequence . map Compose = Compose . map sequenceA . sequenceA}</pre>
 */
public interface Traversable<T> extends Functor<T> {
    /**
     * Map each element of a structure to an action, evaluate these actions from
     * left to right, and collect the results.
     *
     * <pre>{@code traverse :: Applicative f => t a -> (a -> f b) -> f (t b)}</pre>
     */
    default <F, A, B> $<F, ? extends $<T, B>>
    traverse(Applicative<F> m, $<T, A> a, Function<? super A, ? extends $<F, B>> f) {
        return sequence(m, map(a, f));
    }

    /**
     * Evaluate each action in the structure from left to right, and collect the
     * results.
     *
     * <pre>{@code Applicative f => t (f a) -> f (t a)}</pre>
     */
    default <F, A> $<F, ? extends $<T, A>>
    sequence(Applicative<F> m, $<T, $<F, A>> a) {
        return traverse(m, a, Fn.id());
    }
}
