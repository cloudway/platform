/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.control.arrow;

import java.util.function.Function;

import com.cloudway.fp.data.Fn;
import com.cloudway.fp.data.Tuple;
import com.cloudway.fp.π;

/**
 * The basic arrow class.
 *
 * <p>Instances should satisfy the following laws:
 *
 * <pre>{@code
 * arr id = id
 * arr (f >>> g) = arr f >>> arr g
 * first (arr f) = arr (first f)
 * first (f >>> g) = first f >>> first g
 * first f >>> arr fst = arr fst >>> f
 * first f >>> arr (id *** g) = arr (id *** g) >>> first f
 * first (first f) >>> arr assoc = arr assoc >>> first f
 *
 * where
 *   assoc ((a,b),c) = (a,(b,c))
 * }</pre>
 */
public interface Arrow<A> extends Category<A> {
    /**
     * Lift a function to an arrow.
     *
     * <pre>{@code arr :: (b -> c) -> a b c}</pre>
     */
    <B, C> π<A, B, C> arr(Function<? super B, ? extends C> f);

    /**
     * Send the first component of the input through the argument arrow,
     * and copy the rest unchanged to the output.
     *
     * <pre>{@code first :: a b c -> a (b, d) (c, d)}</pre>
     */
    <B, C, D> π<A, Tuple<B,D>, Tuple<C,D>> first(π<A, B, C> f);

    /**
     * A mirror image of {@link #first}. The default definition may be overridden
     * with a more efficient version if desired.
     *
     * <pre>{@code second :: a b c -> a (d, b) (d, c)}</pre>
     */
    default <B, C, D> π<A, Tuple<D,B>, Tuple<D,C>> second(π<A, B, C> f) {
        return then(arr(Tuple::swap), first(f), arr(Tuple::swap));
    }

    /**
     * Split the input between the two argument arrows and combine their output.
     * Note that this is in general not a functor.
     *
     * <pre>{@code (***) :: a b c -> a b' c' -> a (b, b') (c, c')}</pre>
     */
    default <B, C, B1, C1> π<A, Tuple<B,B1>, Tuple<C,C1>> split(π<A, B, C> f, π<A, B1, C1> g) {
        return then(first(f), second(g));
    }

    /**
     * Send the input to both argument arrows and combine their output.
     *
     * <pre>{@code (&&&) :: a b c -> a b c' -> a b (c c')}</pre>
     */
    default <B, C, C1> π<A, B, Tuple<C,C1>> fanout(π<A, B, C> f, π<A, B, C1> g) {
        return then(arr(b -> Tuple.of(b, b)), split(f, g));
    }

    /**
     * The identity arrow, which plays the role of {@code return} in arrow notation.
     */
    default <B> π<A, B, B> returnA() {
        return arr(Fn.id());
    }
}
