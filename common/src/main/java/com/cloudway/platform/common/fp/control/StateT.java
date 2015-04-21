/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.control;

import java.util.function.Function;
import java.util.function.Supplier;

import com.cloudway.platform.common.fp.$;
import com.cloudway.platform.common.fp.data.Fn;
import com.cloudway.platform.common.fp.data.Tuple;
import com.cloudway.platform.common.fp.data.Unit;

/**
 * An abstract state transformer monad.
 *
 * @param <T> the state monad typeclass
 * @param <S> the state type
 * @param <M> the inner monad typeclass
 */
public abstract class StateT<T extends StateT<T,S,M>, S, M extends Monad<M>>
    implements Monad<T>, MonadTrans<T, M>
{
    /**
     * The monadic datatype that encapsulate a state transfer function. This
     * class may be overridden by concrete state transformer monad.
     *
     * @param <T> the state monad typeclass
     * @param <S> the state type
     * @param <M> the inner monad typeclass
     * @param <A> the computation value type
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
    protected StateT(M nm) {
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
    public $<T, S> get() {
        return $(s -> nm.pure(Tuple.of(s, s)));
    }

    /**
     * Fetch the current value of the state and bind the value to the given function.
     *
     * <p>Equivalent to: {@code get().bind(f)}</p>
     */
    public <B> $<T, B> get(Function<? super S, ? extends $<T, B>> f) {
        return $(s -> runState(f.apply(s), s));
    }

    /**
     * Sets the state within the monad.
     */
    public $<T, Unit> put(S s) {
        return $(__ -> nm.pure(Tuple.of(Unit.U, s)));
    }

    /**
     * Update the state to the result of applying a function to the current state.
     */
    public $<T, Unit> modify(Function<S, S> f) {
        return $(s -> nm.pure(Tuple.of(Unit.U, f.apply(s))));
    }

    /**
     * Get a specific component of the state, using a projection function supplied.
     */
    public <A> $<T, A> gets(Function<S, A> f) {
        return $(s -> nm.pure(Tuple.of(f.apply(s), s)));
    }

    // Monad Stacking

    /**
     * Stack monad on another monad.
     *
     * @param nm the inner monad
     * @return a stacked monad
     */
    public static <S, M extends Monad<M>> On<S, M> on(M nm) {
        return new On<>(nm);
    }

    /**
     * The stacked monad typeclass.
     */
    public static final class On<S, M extends Monad<M>> extends StateT<On<S,M>, S, M> {
        private On(M nm) {
            super(nm);
        }

        @Override
        protected <A> $<On<S,M>, A> $(Function<S, $<M, Tuple<A, S>>> f) {
            return new Monadic<On<S,M>, S, M, A>(f) {
                @Override public On<S,M> getTypeClass() {
                    return On.this;
                }
            };
        }
    }
}
