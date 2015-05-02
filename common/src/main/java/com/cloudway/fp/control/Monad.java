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
import com.cloudway.fp.data.Fn;
import com.cloudway.fp.data.Foldable;
import com.cloudway.fp.data.Seq;
import com.cloudway.fp.data.Traversable;
import com.cloudway.fp.data.Tuple;
import com.cloudway.fp.data.Unit;
import com.cloudway.fp.function.TriFunction;

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
    <A, B> $<M,B> bind($<M,A> m, Function<? super A, ? extends $<M,B>> k);

    /**
     * Convenient method for binding on tuple computation.
     */
    default <A, B, C> $<M,C> bind($<M, Tuple<A,B>> m, BiFunction<? super A, ? super B, ? extends $<M,C>> k) {
        return bind(m, t -> t.as(k));
    }

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
     * Delay evaluate the computation.
     */
    default <A> $<M, A> delay(Supplier<$<M, A>> m) {
        return seqR(unit(), m);
    }

    /**
     * Lift a lazy value.
     */
    default <A> $<M, A> lazy(Supplier<A> a) {
        return seqR(unit(), Fn.lazy(() -> pure(a.get())));
    }

    /**
     * Lift an action with side effect.
     */
    default $<M, Unit> action(Runnable a) {
        return lazy(() -> {
            a.run();
            return Unit.U;
        });
    }

    /**
     * Returns a container consisting of the results of applying the given
     * function to the elements of given container.
     */
    @Override
    default <A, B> $<M,B> map($<M,A> m, Function<? super A, ? extends B> f) {
        return bind(m, x -> pure(f.apply(x)));
    }

    /**
     * Sequential application.
     */
    @Override
    default <A, B> $<M,B> ap($<M, Function<? super A, ? extends B>> mf, $<M,A> v) {
        return bind(mf, f -> map(v, f));
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
     * The join function is the conventional monad join operator. It is used
     * to remove one level of monadic structure, projecting its bound argument
     * into the outer level.
     */
    default <A> $<M, A> join($<M, ? extends $<M, A>> x) {
        return bind(x, y -> y);
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
        return (m1, m2) -> liftM2(f, m1, m2);
    }

    /**
     * Promote a function to a monad and apply it immediately.
     */
    default <A, B, C> $<M,C>
    liftM2(BiFunction<? super A, ? super B, ? extends C> f, $<M,A> m1, $<M,B> m2) {
        return bind(m1, x1 -> map(m2, x2 -> f.apply(x1, x2)));
    }

    /**
     * Promote a function to a monad, scanning the monadic arguments from left
     * to right.
     *
     * <pre>{@code liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d}</pre>
     */
    default <A, B, C, D> TriFunction<$<M,A>, $<M,B>, $<M,C>, $<M,D>>
    liftM3(TriFunction<? super A, ? super B, ? super C, ? extends D> f) {
        return (m1, m2, m3) -> liftM3(f, m1, m2, m3);
    }

    /**
     * Promote a function to a monad and apply it immediately.
     */
    default <A, B, C, D> $<M,D>
    liftM3(TriFunction<? super A, ? super B, ? super C, ? extends D> f, $<M,A> m1, $<M,B> m2, $<M,C> m3) {
        return bind(m1, x1 -> bind(m2, x2 -> map(m3, x3 -> f.apply(x1, x2, x3))));
    }

    /**
     * Evaluate each action in the data structure from left to right, and
     * collect the result.
     *
     * <pre>{@code flatM :: (Traversable t, Monad m) => t (m a) -> m (t a)}</pre>
     */
    default <T, A> $<M, ? extends Traversable<T, A>>
    flatM(Traversable<T, ? extends $<M, A>> ms) {
        return ms.traverse(this, Fn.<$<M,A>>id());
    }

    /**
     * Evaluate each action in the list from left to right, and collect the
     * result.
     */
    default <A> $<M, Seq<A>> flatM(Seq<? extends $<M, A>> ms) {
        return ms.traverse(this, Fn.<$<M,A>>id());
    }

    /**
     * Evaluate each action in the list from left to right, and ignore the
     * result.
     *
     * <pre>{@code sequence :: (Foldable t, Monad m) => t (m a) -> m ()}</pre>
     */
    default <A> $<M, Unit> sequence(Foldable<? extends $<M, A>> ms) {
        return ms.foldRight(pure(Unit.U), this::seqR);
    }

    /**
     * The {@code mapM} is analogous to {@link Seq#map(Function) map} except
     * that its result is encapsulated in a {@code Monad}.
     *
     * <pre>{@code mapM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)}</pre>
     */
    default <T, A, B> $<M, ? extends Traversable<T, B>>
    mapM(Traversable<T, A> xs, Function<? super A, ? extends $<M, B>> f) {
        return xs.traverse(this, f);
    }

    /**
     * The {@code mapM} is analogous to {@link Seq#map(Function) map} except
     * that its result is encapsulated in a {@code Monad}.
     */
    default <A, B> $<M, Seq<B>> mapM(Seq<A> xs, Function<? super A, ? extends $<M, B>> f) {
        return xs.traverse(this, f);
    }

    /**
     * {@code mapM_} is equivalent to {@code sequence(xs.map(f))}.
     *
     * <pre>{@code mapM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()}</pre>
     */
    default <A, B> $<M, Unit>
    mapM_(Foldable<A> xs, Function<? super A, ? extends $<M, B>> f) {
        return xs.foldRight(pure(Unit.U), (x, r) -> seqR(f.apply(x), r));
    }

    /**
     * Generalizes {@link Seq#zip(Seq,BiFunction)} to arbitrary monads.
     * Bind the given function to the given computations with a final join.
     *
     * <pre>{@code zipM :: Monad m => [a] -> [b] -> (a -> b -> m c) -> m [c]}</pre>
     */
    default <A, B, C> $<M, Seq<C>>
    zipM(Seq<A> xs, Seq<B> ys, BiFunction<? super A, ? super B, ? extends $<M, C>> f) {
        return flatM(Seq.zip(xs, ys, f));
    }

    /**
     * The extension of {@link #zipM} which ignores the final result.
     *
     * <pre>{@code zipM_ :: Monad m => [a] -> [b] -> (a -> b -> m c) -> m ()}</pre>
     */
    default <A, B, C> $<M, Unit>
    zipM_(Seq<A> xs, Seq<B> ys, BiFunction<? super A, ? super B, ? extends $<M, C>> f) {
        return sequence(Seq.zip(xs, ys, f));
    }

    /**
     * This generalizes the list-based filter function.
     *
     * <pre>{@code filterM :: Monad m => [a] -> (a -> m Bool) -> m [a]}</pre>
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
     *
     * <pre>{@code foldM :: (Foldable t, Monad m) => b -> t a -> (b -> a -> m b) -> m b}</pre>
     */
    default <A, B> $<M, B>
    foldM(B r0, Foldable<A> xs, BiFunction<B, ? super A, ? extends $<M, B>> f) {
        return xs.foldLeft(pure(r0), (m, x) -> bind(m, r -> f.apply(r, x)));
    }

    /**
     * Perform the action n times, gathering the results.
     *
     * <pre>{@code replicateM :: Monad m => Int -> m a -> m [a]}</pre>
     */
    default <A> $<M, Seq<A>> replicateM(int n, $<M, A> a) {
        return flatM(Seq.replicate(n, a));
    }

    /**
     * Perform the action n times, discarding the result.
     *
     * <pre>{@code replicateM_ :: Monad m => Int -> m a -> m ()}</pre>
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
