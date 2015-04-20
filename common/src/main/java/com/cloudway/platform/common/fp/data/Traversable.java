/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.data;

import java.util.function.Function;

import com.cloudway.platform.common.fp.$;
import com.cloudway.platform.common.fp.control.Applicative;

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
 */
public interface Traversable<T, A> {
    /**
     * Map each element of a structure to an action, evaluate these actions from
     * left to right, and collect the results.
     *
     * <pre>{@code traverse :: Applicative f => t a -> (a -> f b) -> f (t b)}</pre>
     */
    <F, B> $<F, ? extends Traversable<T, B>>
    traverse(Applicative<F> m, Function<? super A, ? extends $<F, B>> f);
}
