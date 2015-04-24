/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.control.trans;

import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;

import com.cloudway.platform.common.fp.control.TrampolineIO;
import com.cloudway.platform.common.fp.data.Foldable;
import com.cloudway.platform.common.fp.data.Seq;
import com.cloudway.platform.common.fp.data.Traversable;
import com.cloudway.platform.common.fp.data.Tuple;
import com.cloudway.platform.common.fp.data.Unit;
import com.cloudway.platform.common.fp.io.IO;
import com.cloudway.platform.common.fp.io.VoidIO;
import com.cloudway.platform.common.fp.$;

/**
 * The stateful IO monad.
 *
 * @param <S> the type of state passing to the computation
 * @param <A> the type of result of computation
 */
public final class StateIO<S, A> extends StateTC.Monadic<StateIO.µ<S>, S, TrampolineIO.µ, A> {
    private StateIO(Function<S, $<TrampolineIO.µ, Tuple<A,S>>> f) {
        super(f);
    }

    /**
     * Create a stateful IO monad from the given state transfer function.
     *
     * @param f the state transfer function
     * @return the stateful IO monad
     */
    public static <S, A> StateIO<S, A> state(Function<? super S, Tuple<A, S>> f) {
        return narrow(StateIO.<S>tclass().state(f));
    }

    /**
     * Construct a pure computation that results in the given value.
     *
     * @param a the final result
     * @return the stateful IO monad that hold the final result
     */
    public static <S, A> StateIO<S, A> pure(A a) {
        return narrow(StateIO.<S>tclass().pure(a));
    }

    /**
     * Returns a do nothing I/O action.
     */
    public static <S> StateIO<S, Unit> unit() {
        return pure(Unit.U);
    }

    /**
     * Constructs a pure computation that results in the given lazy evaluation
     * thunk.
     *
     * @param a a thunk that eventually produce computation result
     * @return the state monad that hold the computation
     */
    public static <S, A> StateIO<S, A> lazy(IO<A> a) {
        return lift(a);
    }

    /**
     * Constructs a computation that performs the given action with no result.
     */
    public static <S> StateIO<S, Unit> action(VoidIO a) {
        return lift(a);
    }

    /**
     * Promote an IO action to a stateful IO action.
     *
     * @param m the IO action
     * @return the prompted stateful IO action
     */
    public static <S, A> StateIO<S, A> lift(IO<A> m) {
        return narrow(StateIO.<S>tclass().lift(TrampolineIO.lift(m)));
    }

    /**
     * Promote an IO action to a stateful IO action.
     *
     * @param m the IO action which has no return value
     * @return the prompted stateful IO action
     */
    public static <S> StateIO<S, Unit> lift_(VoidIO m) {
        return lift(m);
    }

    /**
     * Evaluate a state computation with the given initial state and return
     * a tuple of final value and final state.
     *
     * @param s the initial state
     * @return the tuple of final value and final state wrapped in an IO monad
     */
    public IO<Tuple<A, S>> run(S s) {
        return TrampolineIO.narrow(StateIO.<S>tclass().runState(this, s)).run();
    }

    /**
     * Evaluate a state computation with the given initial state and return
     * the final value, discarding the final state.
     *
     * @param s the initial state
     * @return the final computation value wrapped in an IO monad
     */
    public IO<A> eval(S s) {
        return run(s).map(Tuple::first);
    }

    /**
     * Evaluate a state computation with the given initial state and return
     * the final value, discarding the final state.
     *
     * @param s the initial state
     * @return the final state wrapped in an IO monad
     */
    public IO<S> exec(S s) {
        return run(s).map(Tuple::second);
    }

    /**
     * Transfer a state computation by feeding the value to the given function
     * and wrapping the result to a new state.
     */
    public <B> StateIO<S, B> map(Function<? super A, ? extends B> f) {
        return narrow(StateIO.<S>tclass().map(this, f));
    }

    /**
     * Transfer a state computation by feeding the value to the given function.
     */
    public <B> StateIO<S, B> bind(Function<? super A, ? extends $<µ<S>, B>> f) {
        return narrow(StateIO.<S>tclass().bind(this, f));
    }

    /**
     * Transfer a state computation by discarding the intermediate value.
     */
    public <B> StateIO<S, B> then(Supplier<? extends $<µ<S>, B>> next) {
        return narrow(StateIO.<S>tclass().seqR(this, next));
    }

    /**
     * Transfer a state computation by discarding the intermediate value.
     */
    public <B> StateIO<S, B> then($<µ<S>, B> next) {
        return narrow(StateIO.<S>tclass().seqR(this, next));
    }

    /**
     * Map both the return value and final state of computation using the
     * given function.
     */
    public <B> StateIO<S, B> mapState(Function<IO<Tuple<A, S>>, IO<Tuple<B, S>>> f) {
        return narrow(StateIO.<S>tclass().mapState(this, a ->
            TrampolineIO.lift(f.apply(TrampolineIO.run(a)))));
    }

    /**
     * Execute action on a state modified by applying function.
     */
    public StateIO<S, A> withState(Function<S, S> f) {
        return narrow(StateIO.<S>tclass().withState(this, f));
    }

    /**
     * Fetch the current value of the state within the monad.
     */
    public static <S> StateIO<S, S> get() {
        return narrow(StateIO.<S>tclass().get());
    }

    /**
     * Fetch the current value of the state and bind the value to the given function.
     */
    public static <S, B> StateIO<S, B> get(Function<? super S, ? extends $<µ<S>, B>> f) {
        return narrow(StateIO.<S>tclass().get(f));
    }

    /**
     * Sets the state within the monad.
     */
    public static <S> StateIO<S, Unit> put(S s) {
        return narrow(StateIO.<S>tclass().put(s));
    }

    /**
     * Updates the state to the result of applying a function to the current state.
     */
    public static <S> StateIO<S, Unit> modify(Function<S, S> f) {
        return narrow(StateIO.<S>tclass().modify(f));
    }

    /**
     * Get specific component of the state, using a projection function applied.
     */
    public static <S, A> StateIO<S, A> gets(Function<? super S, ? extends A> f) {
        return narrow(StateIO.<S>tclass().gets(f));
    }

    // Type Class

    public static final class µ<S> extends StateTC<µ<S>, S, TrampolineIO.µ> {
        private µ() {
            super(TrampolineIO.tclass);
        }
        
        @Override
        protected <A> StateIO<S, A> $(Function<S, $<TrampolineIO.µ, Tuple<A, S>>> f) {
            return new StateIO<>(f);
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

    public static <S, A> StateIO<S, A> narrow($<µ<S>, A> value) {
        return (StateIO<S,A>)value;
    }

    public static <S, A> IO<Tuple<A, S>> run($<µ<S>, A> m, S s) {
        return narrow(m).run(s);
    }

    public static <S, A> IO<A> eval($<µ<S>, A> m, S s) {
        return narrow(m).eval(s);
    }

    public static <S, A> IO<S> exec($<µ<S>, A> m, S s) {
        return narrow(m).exec(s);
    }

    // Convenient static monad methods

    public static <T, S, A> StateIO<S, ? extends Traversable<T, A>>
    flatM(Traversable<T, ? extends $<µ<S>, A>> ms) {
        return narrow(StateIO.<S>tclass().flatM(ms));
    }

    @SuppressWarnings("unchecked")
    public static <S, A> StateIO<S, Seq<A>> flatM(Seq<? extends $<µ<S>, A>> ms) {
        return (StateIO<S, Seq<A>>)StateIO.<S>tclass().flatM(ms);
    }

    public static <T, S, A, B> StateIO<S, ? extends Traversable<T, B>>
    mapM(Traversable<T, A> xs, Function<? super A, ? extends $<µ<S>, B>> f) {
        return narrow(StateIO.<S>tclass().mapM(xs, f));
    }

    @SuppressWarnings("unchecked")
    public static <S, A, B> StateIO<S, Seq<B>>
    mapM(Seq<A> xs, Function<? super A, ? extends $<µ<S>, B>> f) {
        return (StateIO<S, Seq<B>>)StateIO.<S>tclass().mapM(xs, f);
    }

    public static <S, A> StateIO<S, Unit> sequence(Foldable<? extends $<µ<S>, A>> ms) {
        return narrow(StateIO.<S>tclass().sequence(ms));
    }

    public static <S, A, B> StateIO<S, Unit>
    mapM_(Foldable<A> xs, Function<? super A, ? extends $<µ<S>, B>> f) {
        return narrow(StateIO.<S>tclass().mapM_(xs, f));
    }

    public static <S, A> StateIO<S, Seq<A>>
    filterM(Seq<A> xs, Function<? super A, ? extends $<µ<S>, Boolean>> p) {
        return narrow(StateIO.<S>tclass().filterM(xs, p));
    }

    public static <S, A, B> StateIO<S, B>
    foldM(B r0, Foldable<A> xs, BiFunction<B, ? super A, ? extends $<µ<S>, B>> f) {
        return narrow(StateIO.<S>tclass().foldM(r0, xs, f));
    }

    public static <S, A> StateIO<S, Seq<A>> replicateM(int n, $<µ<S>, A> a) {
        return narrow(StateIO.<S>tclass().replicateM(n, a));
    }

    public static <S, A> StateIO<S, Unit> replicateM_(int n, $<µ<S>, A> a) {
        return narrow(StateIO.<S>tclass().replicateM_(n, a));
    }
}
