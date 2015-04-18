/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.typeclass;

import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;

import com.cloudway.platform.common.fp.data.Foldable;
import com.cloudway.platform.common.fp.data.Seq;
import com.cloudway.platform.common.fp.data.Unit;
import com.cloudway.platform.common.fp.function.TriFunction;

/**
 * The {@code Monad} typeclass defines the basic operations over a monad,
 * a concept from a branch of mathematics known as category theory. From
 * the perspective of a Haskell programmer, however, it is best to think
 * of a monad as an abstract datatype of actions.
 *
 * <p>Instances of {@code Monad} should satisfy the following laws:
 *
 * <pre>{@code
 * pure a >>= k = k a
 * m >>= pure = m
 * m >>= (\x -> k x >>= h) = (m >>= k) >>= h
 * }</pre>
 *
 * <p>The above laws imply:
 *
 * <pre>{@code
 * map f xs = xs >>= pure . f
 * (>>) = (*>)
 * }</pre>
 */
public interface Monad<M> extends Applicative<M> {
    /**
     * Sequentially compose two actions, passing any value produced by the
     * first as an argument to the second.
     *
     * <pre>{@code (>>=) :: m a -> (a -> m b) -> m b}</pre>
     */
    <A, B> $<M,B> bind($<M,A> a, Function<? super A, ? extends $<M,B>> k);

    /**
     * Sequentially compose two actions, discarding any value produced by the
     * first.
     *
     * <pre>{@code (>>) :: m a -> m b -> m b}</pre>
     */
    @Override
    default <A, B> $<M,B> seqR($<M,A> a, $<M,B> b) {
        return bind(a, __ -> b);
    }

    /**
     * Sequentially compose two actions, discarding any value produced by the
     * first.
     *
     * <pre>{@code (>>) :: m a -> m b -> m b}</pre>
     */
    default <A, B> $<M,B> seqR($<M,A> a, Supplier<? extends $<M,B>> b) {
        return bind(a, __ -> b.get());
    }

    /**
     * Returns a container consisting of the results of applying the given
     * function to the elements of given container.
     */
    @Override
    default <A, B> $<M,B> map($<M,A> a, Function<? super A, ? extends B> f) {
        return bind(a, x -> pure(f.apply(x)));
    }

    /**
     * Sequential application.
     */
    @Override
    default <A, B> $<M,B> ap($<M, Function<? super A, ? extends B>> fs, $<M,A> a) {
        return bind(fs, f -> map(a, f));
    }

    /**
     * Fail with a message.  This operation is not part of the mathematical
     * definition of a monad, but is invoked on pattern-match failure in a
     * do expression.
     */
    default <A> $<M,A> fail(String msg) {
        throw new RuntimeException(msg);
    }

    /**
     * Promote a function to a monad.
     *
     * <pre>{@code liftM :: Monad m => (a -> b) -> m a -> m b}</pre>
     */
    default <A, B> Function<$<M,A>, $<M,B>> liftM(Function<? super A, ? extends B> f) {
        return m -> map(m, f);
    }

    /**
     * Promote a function to a monad, scanning the monadic arguments from left
     * to right.
     *
     * <pre>{@code liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c}</pre>
     */
    default <A, B, C> BiFunction<$<M,A>, $<M,B>, $<M,C>>
    liftM2(BiFunction<? super A, ? super B, ? extends C> f) {
        return (m1, m2) -> bind(m1, x1 -> map(m2, x2 -> f.apply(x1, x2)));
    }

    /**
     * Promote a function to a monad, scanning the monadic arguments from left
     * to right.
     *
     * <pre>{@code liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d}</pre>
     */
    default <A, B, C, D> TriFunction<$<M,A>, $<M,B>, $<M,C>, $<M,D>>
    liftM3(TriFunction<? super A, ? super B, ? super C, ? extends D> f) {
        return (m1, m2, m3) -> bind(m1, x1 -> bind(m2, x2 -> map(m3, x3 -> f.apply(x1, x2, x3))));
    }

    /**
     * Evaluate each action in the list from left to right, and collect the
     * result.
     */
    default <A> $<M, Seq<A>> flatM(Seq<? extends $<M, A>> ms) {
        BiFunction<A, Seq<A>, Seq<A>> cf = Seq::cons;
        return ms.foldRight_(pure(Seq.nil()), liftM2(cf));
    }

    /**
     * Evaluate each action in the list from left to right, and ignore the
     * result.
     */
    default <A> $<M, Unit> sequence(Foldable<? extends $<M, A>> ms) {
        return ms.foldRight(pure(Unit.U), this::seqR);
    }

    /**
     * The {@code mapM} is analogous to {@link Seq#map(Function) map} except
     * that its result is encapsulated in a {@code Monad}.
     */
    default <A, B> $<M, Seq<B>>
    mapM(Seq<A> xs, Function<? super A, ? extends $<M, B>> f) {
        return flatM(xs.map(f));
    }

    /**
     * {@code mapM_} is equivalent to {@code sequence(xs.map(f))}.
     */
    default <A, B> $<M, Unit>
    mapM_(Foldable<A> xs, Function<? super A, ? extends $<M, B>> f) {
        return xs.foldRight(pure(Unit.U), (x, r) -> seqR(f.apply(x), r));
    }

    /**
     * Generalizes {@link Seq#zip(Seq,BiFunction)} to arbitrary monads.
     * Bind the given function to the given computations with a final join.
     */
    default <A, B, C> $<M, Seq<C>>
    zipM(Seq<A> xs, Seq<B> ys, BiFunction<? super A, ? super B, ? extends $<M, C>> f) {
        return flatM(Seq.zip(xs, ys, f));
    }

    /**
     * The extension of {@link #zipM} which ignores the final result.
     */
    default <A, B, C> $<M, Unit>
    zipM_(Seq<A> xs, Seq<B> ys, BiFunction<? super A, ? super B, ? extends $<M, C>> f) {
        return sequence(Seq.zip(xs, ys, f));
    }

    /**
     * This generalizes the list-based filter function.
     */
    default <A> $<M, Seq<A>>
    filterM(Seq<A> xs, Function<? super A, ? extends $<M, Boolean>> p) {
        return xs.isEmpty()
               ? pure(Seq.nil())
               : bind(p.apply(xs.head()), flg ->
                 bind(filterM(xs.tail(), p), ys ->
                 pure(flg ? Seq.cons(xs.head(), ys) : ys)));
    }

    /**
     * The {@ocde foldM} is analogous to {@link Seq#foldLeft(Object,BiFunction)
     * foldLeft}, except that its result is encapsulated in a {@code Monad}. Note
     * hat {@code foldM} works from left-to-right over the list arguments, If
     * right-to-left evaluation is required, the input list should be reversed.
     */
    default <A, B> $<M, B>
    foldM(B r0, Foldable<A> xs, BiFunction<B, ? super A, ? extends $<M, B>> f) {
        return xs.foldLeft(pure(r0), (m, x) -> bind(m, r -> f.apply(r, x)));
    }

    /**
     * Perform the action n times, gathering the results.
     */
    default <A> $<M, Seq<A>> replicateM(int n, $<M, A> a) {
        return flatM(Seq.replicate(n, a));
    }

    /**
     * Perform the action n times, discarding the result.
     */
    default <A> $<M, Unit> replicateM_(int n, $<M, A> a) {
        return sequence(Seq.replicate(n, a));
    }

    /**
     * Kleisli composition of monads.
     */
    default <A, B, C> Function<A, $<M, C>>
    compose(Function<? super A, ? extends $<M, B>> f, Function<? super B, ? extends $<M, C>> g) {
        return x -> bind(f.apply(x), g);
    }
}
