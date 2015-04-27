/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.control.arrow;

import java.util.function.Function;

import com.cloudway.fp.data.Either;
import com.cloudway.fp.π;

/**
 * Choice, for arrows that support it. This class underlies the {@code if}
 * and {@code code} constructs in arrow notation.
 *
 * <p>Instances should satisfy the following laws:
 *
 * <pre>{@code
 * left (arr f) = arr (left f)
 * left (f >>> g) = left f >>> left g
 * f >>> arr left = arr left >>> left f
 * left f >>> arr (id +++ g) = arr (id +++ g) >>> left f
 * left (left f) >>> arr assocsum = arr assocsum >>> left f
 *
 * where
 *   assocsum (Left (Left x)) = Left x
 *   assocsum (Left (Right y)) = Right (Left y)
 *   assocsum (Right z) = Right (Right z)
 * }</pre>
 */
public interface ArrowChoice<A> extends Arrow<A> {
    /**
     * Feed marked inputs through the argument arrow, passing the rest
     * through unchanged to the output.
     *
     * <pre>{@code left :: a b c -> a (Either b d) (Either c d)}</pre>
     */
    <B, C, D> π<A, Either<B,D>, Either<C,D>> left(π<A, B, C> f);

    /**
     * A mirror image of {@link #left}.
     *
     * <pre>{@code right :: a b c -> a (Either d b) (Either d c)}</pre>
     */
    default <B, C, D> π<A, Either<D,B>, Either<D,C>> right(π<A, B, C> f) {
        return then(arr(Either::swap), left(f), arr(Either::swap));
    }

    /**
     * Split the input between the two argument arrows, retagging and
     * merging their outputs. Note that this is in general not a functor.
     *
     * <pre>{@code (+++) :: a b c -> a b' c' -> a (Either b b') (Either c c')}</pre>
     */
    default <B, C, B1, C1> π<A, Either<B,B1>, Either<C,C1>> merge(π<A, B, C> f, π<A, B1, C1> g) {
        return then(left(f), right(g));
    }

    /**
     * Split the input between the two argument arrows and merge their outputs.
     *
     * <pre>{@code (|||) :: a b d -> a c d -> a (Either b c) d}</pre>
     */
    default <B, C, D> π<A, Either<B,C>, D> fanin(π<A, B, D> f, π<A, C, D> g) {
        Function<Either<D, D>, D> untag = e -> e.isLeft() ? e.left() : e.right();
        return then(merge(f, g), arr(untag));
    }
}
