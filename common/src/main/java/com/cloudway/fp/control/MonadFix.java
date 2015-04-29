/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.control;

import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;

import com.cloudway.fp.$;
import com.cloudway.fp.function.TriFunction;

/**
 * Monads having fixed points with a "knot-tying" semantics. Instances of
 * {@code MonadFix} should satisfy the following laws:
 *
 * <p><strong>purity</strong></p>
 * <pre>{@code mfix (return . h) = return (fix h)}</pre>
 * <p><strong>left shrinking (or tightening)</strong></p>
 * <pre>{@code mfix (\x -> a >>= \y -> f x y) = a >> \y -> mfix (\x -> f x y)}</pre>
 * <p><strong>sliding</strong></p>
 * <pre>{@code mfix (liftM h . f) = liftM h (mfix (f . h))}</pre>
 * <p><strong>nesting</strong></p>
 * <pre>{@code mfix (\x -> mfix (\y -> f x y)) = mfix (\x -> f x x}</pre>
 */
public interface MonadFix<M> extends Monad<M> {
    /**
     * The fixed point of a monadic computation. Executes the given action only
     * once, with the eventual output fed back as the input. Hence the function
     * should not be strict, for then {@code mfix} would diverge.
     */
    <A> $<M, A> mfix(Function<Supplier<A>, ? extends $<M, A>> f);

    /**
     * Returns the fixed point of a monadic binary operator computation.
     */
    default <A, B> Function<A, $<M, B>>
    mfix(BiFunction<Function<? super A, ? extends $<M, B>>, ? super A, ? extends $<M, B>> f) {
        Function<Supplier<Function<A, $<M,B>>>, $<M, Function<A, $<M,B>>>>
            c = rec -> pure(x -> f.apply(y -> rec.get().apply(y), x));
        return x -> bind(mfix(c), h -> h.apply(x));
    }

    /**
     * Returns the fixed point of a monadic ternary operator computation.
     */
    default <A, B, C> BiFunction<A, B, $<M, C>>
    mfix(TriFunction<BiFunction<? super A, ? super B, ? extends $<M, C>>, ? super A, ? super B, ? extends $<M, C>> f) {
        Function<Supplier<BiFunction<A, B, $<M, C>>>, $<M, BiFunction<A, B, $<M, C>>>>
            c = rec -> pure((x, y) -> f.apply((a, b) -> rec.get().apply(a, b), x, y));
        return (x, y) -> bind(mfix(c), h -> h.apply(x, y));
    }
}
