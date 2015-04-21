/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.control;

import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;

import com.cloudway.platform.common.fp.$;
import com.cloudway.platform.common.fp.data.Foldable;
import com.cloudway.platform.common.fp.data.Seq;
import com.cloudway.platform.common.fp.data.Traversable;
import com.cloudway.platform.common.fp.data.Tuple;
import com.cloudway.platform.common.fp.data.Unit;

/**
 * The state monad, passing an updatable state through a computation.
 *
 * @param <A> the type of result of computation
 * @param <S> the type of state passing to the computation
 */
public final class MonadState<S, A> extends StateT.Monadic<MonadState.µ<S>, S, Trampoline.µ, A> {
    private MonadState(Function<S, $<Trampoline.µ, Tuple<A,S>>> f) {
        super(f);
    }

    /**
     * Create a state monad from the given state transfer function.
     *
     * @param f the state transfer function
     * @return the state monad
     */
    public static <S, A> MonadState<S, A> state(Function<? super S, Tuple<A, S>> f) {
        return narrow(MonadState.<S>tclass().state(f));
    }

    /**
     * Constructs a pure computation that results in the given value.
     *
     * @param a the final result
     * @return the state monad that hold the final result
     */
    public static <S, A> MonadState<S, A> pure(A a) {
        return narrow(MonadState.<S>tclass().pure(a));
    }

    /**
     * Returns a do nothing computation.
     */
    public static <S> MonadState<S, Unit> unit() {
        return pure(Unit.U);
    }

    /**
     * Constructs a pure computation that results in the given lazy evaluation
     * thunk.
     *
     * @param a a thunk that eventually produce computation result
     * @return the state monad that hold the computation
     */
    public static <S, A> MonadState<S, A> lazy(Supplier<A> a) {
        return narrow(MonadState.<S>tclass().lazy(a));
    }

    /**
     * Constructs a computation that performs the given action with no result.
     */
    public static <S> MonadState<S, Unit> action(Runnable a) {
        return narrow(MonadState.<S>tclass().action(a));
    }

    /**
     * Evaluate a state computation with the given initial state and return
     * a tuple of final value and final state.
     *
     * @param s the initial state
     * @return the tuple of final value and final state
     */
    public Tuple<A, S> run(S s) {
        return Trampoline.narrow(MonadState.<S>tclass().runState(this, s)).run();
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
    public <B> MonadState<S, B> map(Function<? super A, ? extends B> f) {
        return narrow(MonadState.<S>tclass().map(this, f));
    }

    /**
     * Transfer a state computation by feeding the value to the given function.
     */
    public <B> MonadState<S, B> bind(Function<? super A, ? extends $<µ<S>, B>> f) {
        return narrow(MonadState.<S>tclass().bind(this, f));
    }

    /**
     * Transfer a state computation by discarding the intermediate value.
     */
    public <B> MonadState<S, B> then(Supplier<? extends $<µ<S>, B>> next) {
        return narrow(MonadState.<S>tclass().seqR(this, next));
    }

    /**
     * Transfer a state computation by discarding the intermediate value.
     */
    public <B> MonadState<S, B> then($<µ<S>, B> next) {
        return narrow(MonadState.<S>tclass().seqR(this, next));
    }

    /**
     * Map both the return value and final state of computation using the given
     * function.
     */
    public <B> MonadState<S, B> mapState(Function<Tuple<A, S>, Tuple<B, S>> f) {
        return narrow(MonadState.<S>tclass().mapState(this, t -> Trampoline.narrow(t).map(f)));
    }

    /**
     * Map both the return value and final state of computation using the given
     * function.
     */
    public <B> MonadState<S, B> mapState(BiFunction<? super A, ? super S, Tuple<B, S>> f) {
        return mapState(t -> t.as(f));
    }

    /**
     * Executes action on a state modified by applying function.
     */
    public MonadState<S, A> withState(Function<S, S> f) {
        return narrow(MonadState.<S>tclass().withState(this, f));
    }

    /**
     * Fetch the current value of the state within the monad.
     */
    public static <S> MonadState<S, S> get() {
        return narrow(MonadState.<S>tclass().get());
    }

    /**
     * Fetch the current value of the state and bind the value to the given function.
     */
    public static <S, B> MonadState<S, B> get(Function<? super S, ? extends $<µ<S>, B>> f) {
        return narrow(MonadState.<S>tclass().get(f));
    }

    /**
     * Sets the state within the monad.
     */
    public static <S> MonadState<S, Unit> put(S s) {
        return narrow(MonadState.<S>tclass().put(s));
    }

    /**
     * Updates the state to the result of applying a function to the current state.
     */
    public static <S> MonadState<S, Unit> modify(Function<S, S> f) {
        return narrow(MonadState.<S>tclass().modify(f));
    }

    /**
     * Get a specific component of the state, using a projection function supplied.
     */
    public static <S, A> MonadState<S, A> gets(Function<S, A> f) {
        return narrow(MonadState.<S>tclass().gets(f));
    }

    // Monad

    public static final class µ<S> extends StateT<µ<S>, S, Trampoline.µ> {
        private µ() {
            super(Trampoline.tclass);
        }
        
        @Override
        protected <A> MonadState<S, A> $(Function<S, $<Trampoline.µ, Tuple<A, S>>> f) {
            return new MonadState<>(f);
        }
    }

    private static final µ<?> _TCLASS = new µ<>();

    @SuppressWarnings("unchecked")
    public static <S> µ<S> tclass() {
        return (µ<S>)_TCLASS;
    }

    @Override
    public µ<S> getTypeClass() {
        return tclass();
    }

    public static <S, A> MonadState<S, A> narrow($<µ<S>, A> value) {
        return (MonadState<S,A>)value;
    }

    public static <S, A> Tuple<A, S> runState($<µ<S>, A> m, S s) {
        return narrow(m).run(s);
    }

    public static <S, A> A evalState($<µ<S>, A> m, S s) {
        return narrow(m).eval(s);
    }

    public static <S, A> S execState($<µ<S>, A> m, S s) {
        return narrow(m).exec(s);
    }

    // Convenient static monad methods

    public static <T, S, A> MonadState<S, ? extends Traversable<T, A>>
    flatM(Traversable<T, ? extends $<µ<S>, A>> ms) {
        return narrow(MonadState.<S>tclass().flatM(ms));
    }

    @SuppressWarnings("unchecked")
    public static <S, A> MonadState<S, Seq<A>>
    flatM(Seq<? extends $<µ<S>, A>> ms) {
        return (MonadState<S, Seq<A>>)MonadState.<S>tclass().flatM(ms);
    }

    public static <T, S, A, B> MonadState<S, ? extends Traversable<T, B>>
    mapM(Traversable<T, A> xs, Function<? super A, ? extends $<µ<S>, B>> f) {
        return narrow(MonadState.<S>tclass().mapM(xs, f));
    }

    @SuppressWarnings("unchecked")
    public static <S, A, B> MonadState<S, Seq<B>>
    mapM(Seq<A> xs, Function<? super A, ? extends $<µ<S>, B>> f) {
        return (MonadState<S, Seq<B>>)MonadState.<S>tclass().mapM(xs, f);
    }

    public static <S, A> MonadState<S, Unit>
    sequence(Foldable<? extends $<µ<S>, A>> ms) {
        return narrow(MonadState.<S>tclass().sequence(ms));
    }

    public static <S, A, B> MonadState<S, Unit>
    mapM_(Foldable<A> xs, Function<? super A, ? extends $<µ<S>, B>> f) {
        return narrow(MonadState.<S>tclass().mapM_(xs, f));
    }

    public static <S, A> MonadState<S, Seq<A>>
    filterM(Seq<A> xs, Function<? super A, ? extends $<µ<S>, Boolean>> p) {
        return narrow(MonadState.<S>tclass().filterM(xs, p));
    }

    public static <S, A, B> MonadState<S, B>
    foldM(B r0, Foldable<A> xs, BiFunction<B, ? super A, ? extends $<µ<S>, B>> f) {
        return narrow(MonadState.<S>tclass().foldM(r0, xs, f));
    }

    public static <S, A> MonadState<S, Seq<A>> replicateM(int n, $<µ<S>, A> a) {
        return narrow(MonadState.<S>tclass().replicateM(n, a));
    }

    public static <S, A> MonadState<S, Unit> replicateM_(int n, $<µ<S>, A> a) {
        return narrow(MonadState.<S>tclass().replicateM_(n, a));
    }
}
