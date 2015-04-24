/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.control.trans;

import java.util.function.Function;
import java.util.function.Supplier;

import com.cloudway.platform.common.fp.$;
import com.cloudway.platform.common.fp.control.Monad;
import com.cloudway.platform.common.fp.data.Fn;
import com.cloudway.platform.common.fp.data.Tuple;
import com.cloudway.platform.common.fp.data.Unit;

/**
 * The {@link StateT} typeclass definition.
 *
 * @param <T> the state monad typeclass
 * @param <S> the state type
 * @param <M> the inner monad typeclass
 */
public abstract class StateTC<T, S, M extends Monad<M>>
    implements MonadTrans<T, M>, MonadState<T, S>
{
    /**
     * The monadic data that encapsulate a state transfer function. This
     * class may be overridden by concrete state transformer monad.
     */
    public static abstract class Monadic<T, S, M, A> implements $<T, A> {
        /**
         * The state transfer function.
         */
        final Function<S, $<M, Tuple<A, S>>> sf;

        /**
         * Construct a state transformer monad.
         */
        protected Monadic(Function<S, $<M, Tuple<A, S>>> f) {
            this.sf = f;
        }
    }

    private final M nm;

    /**
     * Construct a state transformer monad.
     *
     * @param nm the inner monad
     */
    protected StateTC(M nm) {
        this.nm = nm;
    }

    /**
     * Implemented by concrete class to instantiate a new State monad.
     */
    protected abstract <A> $<T, A> $(Function<S, $<M, Tuple<A,S>>> f);

    /**
     * Create a state transformer monad from the given state transfer function.
     *
     * @param f the state transfer function
     * @return the state transformer monad
     */
    @Override
    public <A> $<T, A> state(Function<? super S, Tuple<A,S>> f) {
        return $(s -> nm.pure(f.apply(s)));
    }

    /**
     * Construct a pure computation that results in the given value.
     *
     * @param a the final result
     * @return the state transformer monad that hold the final result
     */
    @Override
    public <A> $<T, A> pure(A a) {
        return $(s -> nm.pure(Tuple.of(a, s)));
    }

    /**
     * Construct a pure computation that results in the given lazy evaluation
     * thunk.
     *
     * @param a a thunk that eventually produce computation result
     * @return the state transformer monad that hold the computation
     */
    @Override
    public <A> $<T, A> lazy(Supplier<A> a) {
        Supplier<A> t = Fn.lazy(a);
        return $(s -> nm.pure(Tuple.of(t.get(), s)));
    }

    /**
     * Fail with a message.
     */
    @Override
    public <A> $<T, A> fail(String msg) {
        return $(s -> nm.fail(msg));
    }

    /**
     * Promote an inner monad to a state transformer monad.
     *
     * @param m the inner monad
     * @return a promoted state transformer monad
     */
    @Override
    public <A> $<T, A> lift($<M, A> m) {
        return $(s -> nm.map(m, a -> Tuple.of(a, s)));
    }

    /**
     * Returns the inner monad typeclass.
     */
    @Override
    public M inner() {
        return nm;
    }

    /**
     * Evaluate a state computation with the given initial state and return
     * a tuple of final value and final state.
     *
     * @param m the state monad
     * @param s the initial state
     * @return the monadic tuple of final value and final state
     */
    @SuppressWarnings("unchecked")
    public <A> $<M, Tuple<A,S>> runState($<T, A> m, S s) {
        return ((Monadic<T,S,M,A>)m).sf.apply(s);
    }

    /**
     * Evaluate a state computation with the given initial state and return
     * the final result, discarding the final state.
     *
     * @param m the state monad
     * @param s the initial state
     * @return the monadic final result
     */
    public <A> $<M, A> evalState($<T, A> m, S s) {
        return nm.map(runState(m, s), Tuple::first);
    }

    /**
     * Evaluate a state computation with the given initial state and return
     * the final state, discarding the final value.
     *
     * @param m the state monad
     * @param s the initial state
     * @return monadic final state
     */
    public <A> $<M, S> execState($<T, A> m, S s) {
        return nm.map(runState(m, s), Tuple::second);
    }

    /**
     * Map both the return value and final state of computation using the given
     * function.
     */
    public <A, B> $<T, B> mapState($<T, A> m,
            Function<? super $<M, Tuple<A,S>>, ? extends $<M, Tuple<B,S>>> f) {
        return $(s -> f.apply(runState(m, s)));
    }

    /**
     * Execute action on a state modified by applying function.
     */
    public <A> $<T, A> withState($<T, A> m, Function<S, S> f) {
        return $(s -> runState(m, f.apply(s)));
    }

    /**
     * Transfer a state computation by feeding the value to the given function
     * and wrapping the result to new state.
     */
    @Override
    public <A, B> $<T, B> map($<T, A> m, Function<? super A, ? extends B> f) {
        return $(s -> nm.map(runState(m, s), t -> t.mapFirst(f)));
    }

    /**
     * Transfer a state computation by feeding the value to the given function
     * and wrapping the result to new state.
     */
    @Override
    public <A, B> $<T, B> bind($<T, A> m, Function<? super A, ? extends $<T, B>> k) {
        return $(s -> nm.bind(runState(m, s), t -> t.as((a, s1) -> runState(k.apply(a), s1))));
    }

    /**
     * Transfer a state computation by discarding the intermediate value.
     */
    @Override
    public <A, B> $<T, B> seqR($<T, A> m, Supplier<? extends $<T, B>> n) {
        return $(s -> nm.bind(runState(m, s), t -> runState(n.get(), t.second())));
    }

    /**
     * Transfer a state computation by discarding the intermediate value.
     */
    @Override
    public <A, B> $<T, B> seqR($<T, A> m, $<T, B> n) {
        return $(s -> nm.bind(runState(m, s), t -> runState(n, t.second())));
    }

    /**
     * Fetch the current value of the state within the monad.
     */
    @Override
    public $<T, S> get() {
        return $(s -> nm.pure(Tuple.of(s, s)));
    }

    /**
     * Sets the state within the monad.
     */
    @Override
    public $<T, Unit> put(S s) {
        return $(__ -> nm.pure(Tuple.of(Unit.U, s)));
    }

    /**
     * Update the state to the result of applying a function to the current state.
     */
    @Override
    public $<T, Unit> modify(Function<S, S> f) {
        return $(s -> nm.pure(Tuple.of(Unit.U, f.apply(s))));
    }

    /**
     * Fetch the current value of the state and bind the value to the given function.
     */
    @Override
    public <A> $<T, A> get(Function<? super S, ? extends $<T, A>> f) {
        return $(s -> runState(f.apply(s), s));
    }

    /**
     * Get a specific component of the state, using a projection function supplied.
     */
    @Override
    public <A> $<T, A> gets(Function<? super S, ? extends A> f) {
        return $(s -> nm.pure(Tuple.of(f.apply(s), s)));
    }

    // Lifting other operations

    <W, A> $<T, Tuple<A, W>> liftListen(WriterTC<M, W, ?> wt, $<T, A> m) {
        return $(s -> wt.bind(wt.listen(runState(m, s)), (Tuple<Tuple<A, S>, W> t) ->
            t.as((a, w) -> wt.pure(a.mapFirst(r -> Tuple.of(r, w))))));
    }

    <W, A> $<T, A> liftPass(WriterTC<M, W, ?> wt, $<T, Tuple<A, Function<W, W>>> m) {
        return $(s -> wt.pass(wt.bind(runState(m, s), (Tuple<Tuple<A, Function<W, W>>, S> t) ->
            t.as((a, s1) -> wt.pure(a.mapFirst(r -> Tuple.of(r, s1)))))));
    }

    <E, A> $<T, A> liftCatch(ExceptTC<M, E, ?> et, Function<? super E, ? extends $<T, A>> h, $<T, A> m) {
        return $(s -> et.catchE(e -> runState(h.apply(e), s), runState(m, s)));
    }
}
