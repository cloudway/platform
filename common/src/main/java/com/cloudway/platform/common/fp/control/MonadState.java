/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.control;

import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;

import com.cloudway.platform.common.fp.data.Fn;
import com.cloudway.platform.common.fp.data.Foldable;
import com.cloudway.platform.common.fp.data.Seq;
import com.cloudway.platform.common.fp.data.Tuple;
import com.cloudway.platform.common.fp.data.Unit;
import com.cloudway.platform.common.fp.function.TriFunction;

import static com.cloudway.platform.common.fp.control.Trampoline.immediate;
import static com.cloudway.platform.common.fp.control.Trampoline.suspend;

/**
 * The state monad, passing an updatable state through a computation.
 *
 * @param <A> the type of result of computation
 * @param <S> the type of state passing to the computation
 */
public final class MonadState<A, S> {
    // the state transfer function
    private final Function<S, Trampoline<Tuple<A, S>>> sf;

    // construct a monad state
    private MonadState(Function<S, Trampoline<Tuple<A, S>>> f) {
        this.sf = f;
    }

    // helper method to construct a monad state from a transfer function
    static <A, S> MonadState<A, S> $(Function<S, Trampoline<Tuple<A, S>>> f) {
        return new MonadState<>(f);
    }

    // single step the state transfer
    Trampoline<Tuple<A, S>> go(S s) {
        return sf.apply(s);
    }

    /**
     * Create a state monad from the given state transfer function.
     *
     * @param f the state transfer function
     * @return the state monad
     */
    public static <A, S> MonadState<A, S> state(Function<? super S, Tuple<A, S>> f) {
        return $(s -> immediate(f.apply(s)));
    }

    /**
     * Constructs a pure computation that results in the given value.
     *
     * @param a the final result
     * @return the state monad that hold the final result
     */
    public static <A, S> MonadState<A, S> pure(A a) {
        return $(s -> immediate(Tuple.of(a, s)));
    }

    private static final MonadState<Unit,?> _unit = pure(Unit.U);

    /**
     * Returns a do nothing computation.
     */
    @SuppressWarnings("unchecked")
    public static <S> MonadState<Unit, S> unit() {
        return (MonadState<Unit,S>)_unit;
    }

    /**
     * Constructs a pure computation that results in the given lazy evaluation
     * thunk.
     *
     * @param a a thunk that eventually produce computation result
     * @return the state monad that hold the computation
     */
    public static <A, S> MonadState<A, S> lazy(Supplier<A> a) {
        Supplier<A> t = Fn.lazy(a);
        return $(s -> immediate(Tuple.of(t.get(), s)));
    }

    /**
     * Constructs a computation that performs the given action with no result.
     */
    public static <S> MonadState<Unit, S> action(Runnable a) {
        return $(s -> { a.run(); return immediate(Tuple.of(Unit.U, s)); });
    }

    /**
     * Evaluate a state computation with the given initial state and return
     * a tuple of final value and final state.
     *
     * @param s the initial state
     * @return the tuple of final value and final state
     */
    public Tuple<A, S> run(S s) {
        return go(s).run();
    }

    /**
     * Evaluate a state computation with the given initial state and return
     * the final value, discarding the final state.
     */
    public A eval(S s) {
        return run(s).first();
    }

    /**
     * Evaluate a state computation with the given initial state and return
     * the final state, discarding the final value.
     */
    public S exec(S s) {
        return run(s).second();
    }

    /**
     * Transfer a state computation by feeding the value to the given function
     * and wrapping the result to new state.
     */
    public <B> MonadState<B, S> map(Function<? super A, ? extends B> f) {
        return $(s -> suspend(() -> go(s).map(t -> t.mapFirst(f))));
    }

    /**
     * Transfer a state computation by feeding the value to the given function.
     */
    public <B> MonadState<B, S> bind(Function<? super A, MonadState<B, S>> f) {
        return $(s -> suspend(() -> go(s).bind(t -> suspend(() -> f.apply(t.first()).go(t.second())))));
    }

    /**
     * Transfer a state computation by discarding the intermediate value.
     */
    public <B> MonadState<B, S> then(Supplier<MonadState<B, S>> next) {
        return $(s -> suspend(() -> go(s).bind(t -> suspend(() -> next.get().go(t.second())))));
    }

    /**
     * Transfer a state computation by discarding the intermediate value.
     */
    public <B> MonadState<B, S> then(MonadState<B, S> next) {
        return $(s -> suspend(() -> go(s).bind(t -> suspend(() -> next.go(t.second())))));
    }

    /**
     * Map both the return value and final state of computation using the given
     * function.
     */
    public <B> MonadState<B, S> mapState(Function<Tuple<A, S>, Tuple<B, S>> f) {
        return $(s -> suspend(() -> go(s).map(f)));
    }

    /**
     * Map both the return value and final state of computation using the given
     * function.
     */
    public <B> MonadState<B, S> mapState(BiFunction<? super A, ? super S, Tuple<B, S>> f) {
        return $(s -> suspend(() -> go(s).map(t -> t.as(f))));
    }

    /**
     * Executes action on a state modified by applying function.
     */
    public MonadState<A, S> withState(Function<S, S> f) {
        return $(s -> suspend(() -> go(f.apply(s))));
    }

    /**
     * Fetch the current value of the state within the monad.
     */
    public static <S> MonadState<S,S> get() {
        return $(s -> immediate(Tuple.of(s, s)));
    }

    /**
     * Fetch the current value of the state and bind the value to the given function.
     */
    public static <B,S> MonadState<B,S> get(Function<? super S, MonadState<B, S>> f) {
        return $(s -> suspend(() -> f.apply(s).go(s))); // get().bind(f)
    }

    /**
     * Sets the state within the monad.
     */
    public static <S> MonadState<Unit, S> put(S s) {
        return $(__ -> immediate(Tuple.of(Unit.U, s)));
    }

    /**
     * Updates the state to the result of applying a function to the current state.
     */
    public static <S> MonadState<Unit, S> modify(Function<S, S> f) {
        return $(s -> immediate(Tuple.of(Unit.U, f.apply(s))));
    }

    /**
     * Get a specific component of the state, using a projection function supplied.
     */
    public static <A, S> MonadState<A, S> gets(Function<S, A> f) {
        return $(s -> immediate(Tuple.of(f.apply(s), s)));
    }

    /**
     * Evaluate each action in the sequence from left to right, and collect
     * the result.
     */
    public static <A, S> MonadState<Seq<A>, S> flatM(Seq<MonadState<A, S>> ms) {
        return ms.foldRight_(pure(Seq.nil()), liftM2(Seq::cons));
    }

    /**
     * Evaluate each action in the sequence from left to right, and ignore
     * the result.
     */
    public static <A, S> MonadState<Unit, S> sequence(Foldable<MonadState<A, S>> ms) {
        return ms.foldRight(pure(Unit.U), MonadState::then);
    }

    /**
     * The {@code mapM} is analogous to {@link Seq#map(Function) map} except that
     * its result is encapsulated in an {@code MonadState}.
     */
    public static <A, B, S> MonadState<Seq<B>, S>
    mapM(Seq<A> xs, Function<? super A, MonadState<B,S>> f) {
        return flatM(xs.map(f));
    }

    /**
     * {@code mapM_} is equivalent to {@code sequence(xs.map(f))}.
     */
    public static <A, B, S> MonadState<Unit, S>
    mapM_(Foldable<A> xs, Function<? super A, MonadState<B,S>> f) {
        return xs.foldRight(unit(), (x, r) -> f.apply(x).then(r));
    }

    /**
     * Generalizes {@link Seq#zip(Seq,BiFunction)} to arbitrary monads.
     * Bind the given function to the given computations with a final join.
     */
    public static <A, B, C, S> MonadState<Seq<C>, S>
    zipM(Seq<A> xs, Seq<B> ys, BiFunction<? super A, ? super B, MonadState<C, S>> f) {
        return flatM(Seq.zip(xs, ys, f));
    }

    /**
     * The extension of {@link #zipM(Seq,Seq,BiFunction) zipM} which ignores the
     * final result.
     */
    public static <A, B, C, S> MonadState<Unit, S>
    zipM_(Seq<A> xs, Seq<B> ys, BiFunction<? super A, ? super B, MonadState<C, S>> f) {
        return sequence(Seq.zip(xs, ys, f));
    }

    /**
     * This generalizes the list-based filter function.
     */
    public static <A, S> MonadState<Seq<A>, S>
    filterM(Seq<A> xs, Function<? super A, MonadState<Boolean,S>> p) {
        return xs.isEmpty()
            ? pure(Seq.nil())
            : p.apply(xs.head()).bind(flg ->
              filterM(xs.tail(), p).bind(ys ->
              pure(flg ? Seq.cons(xs.head(), ys) : ys)));
    }

    /**
     * The {@code foldM} is analogous to {@link Seq#foldLeft(Object,BiFunction) foldLeft},
     * except that its result is encapsulated in a {@code MonadState}. Note that
     * {@code foldM} works from left-to-right over the lists arguments. If right-to-left
     * evaluation is required, the input list should be reversed.
     */
    public static <A, B, S> MonadState<B, S>
    foldM(B r0, Foldable<A> xs, BiFunction<B, ? super A, MonadState<B, S>> f) {
        return xs.foldLeft(pure(r0), (m, x) -> m.bind(r -> f.apply(r, x)));
    }

    /**
     * Perform the action n times, gathering the results.
     */
    public static <A, S> MonadState<Seq<A>, S> replicateM(int n, MonadState<A, S> a) {
        return flatM(Seq.replicate(n, a));
    }

    /**
     * Perform the action n times, discards the result.
     */
    public static <A, S> MonadState<Unit, S> replicateM_(int n, MonadState<A, S> a) {
        return sequence(Seq.replicate(n, a));
    }

    /**
     * Kleisli composition of monads.
     */
    public static <A, B, C, S> Function<A, MonadState<C, S>>
    kleisli(Function<A, MonadState<B, S>> f, Function<B, MonadState<C, S>> g) {
        return x -> f.apply(x).bind(g);
    }

    /**
     * Promote a function to a monad state function.
     */
    public static <A, B, S> Function<MonadState<A, S>, MonadState<B, S>>
    liftM(Function<? super A, ? extends B> f) {
        return m -> m.map(f);
    }

    /**
     * Promote a function to a monad state function.
     */
    public static <A, B, C, S> BiFunction<MonadState<A,S>, MonadState<B,S>, MonadState<C,S>>
    liftM2(BiFunction<? super A, ? super B, ? extends C> f) {
        return (m1, m2) -> m1.bind(x1 -> m2.map(x2 -> f.apply(x1, x2)));
    }

    /**
     * Promote a function to a monad state function.
     */
    public static <A, B, C, D, S> TriFunction<MonadState<A,S>, MonadState<B,S>, MonadState<C,S>, MonadState<D,S>>
    liftM3(TriFunction<? super A, ? super B, ? super C, ? extends D> f) {
        return (m1, m2, m3) -> m1.bind(x1 -> m2.bind(x2 -> m3.map(x3 -> f.apply(x1, x2, x3))));
    }
}
