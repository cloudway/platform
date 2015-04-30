/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.control.monad.trans;

import java.util.function.Function;
import java.util.function.Supplier;

import com.cloudway.fp.$;
import com.cloudway.fp.control.Monad;
import com.cloudway.fp.control.ForwardingMonad;
import com.cloudway.fp.data.Fn;
import com.cloudway.fp.data.Tuple;
import com.cloudway.fp.data.Unit;

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
        return $(s -> nm.bind(runState(m, s), (a, s1) -> runState(k.apply(a), s1)));
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

    @Override
    @SuppressWarnings("unchecked")
    public <R> MonadReader<T, R> liftReader() {
        MonadReader<M, R> inner;
        if (nm instanceof MonadReader) {
            inner = (MonadReader<M,R>)nm;
        } else if (nm instanceof MonadTrans) {
            inner = (MonadReader<M,R>)((MonadTrans<T,M>)nm).liftReader();
        } else {
            throw new UnsupportedOperationException("liftReader");
        }
        return new LiftReader<>(inner);
    }

    private class LiftReader<R> implements MonadReader<T, R>, ForwardingMonad<T> {
        private final MonadReader<M, R> inner;

        LiftReader(MonadReader<M, R> inner) {
            this.inner = inner;
        }

        @Override
        public Monad<T> delegate() {
            return StateTC.this;
        }

        @Override
        public <A> $<T, A> reader(Function<? super R, ? extends A> f) {
            return lift(inner.reader(f));
        }

        @Override
        public $<T, R> ask() {
            return lift(inner.ask());
        }

        @Override
        public <A> $<T, A> local(Function<R, R> f, $<T, A> m) {
            return mapState(m, v -> inner.local(f, v));
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <W> MonadWriter<T, W> liftWriter() {
        MonadWriter<M, W> inner;
        if (nm instanceof MonadWriter) {
            inner = (MonadWriter<M,W>)nm;
        } else if (nm instanceof MonadTrans) {
            inner = (MonadWriter<M,W>)((MonadTrans<T,M>)nm).liftWriter();
        } else {
            throw new UnsupportedOperationException("liftWriter");
        }
        return new LiftWriter<>(inner);
    }

    private class LiftWriter<W> implements MonadWriter<T, W>, ForwardingMonad<T> {
        private final MonadWriter<M, W> inner;

        LiftWriter(MonadWriter<M, W> inner) {
            this.inner = inner;
        }

        @Override
        public Monad<T> delegate() {
            return StateTC.this;
        }

        @Override
        public <A> $<T, A> writer(Tuple<A, W> aw) {
            return lift(inner.writer(aw));
        }

        @Override
        public $<T, Unit> tell(W w) {
            return lift(inner.tell(w));
        }

        @Override
        public <A> $<T, Tuple<A, W>> listen($<T, A> m) {
            return $(s -> inner.map(inner.listen(runState(m, s)), (a, w) ->
                a.mapFirst(r -> Tuple.of(r, w))));
        }

        @Override
        public <A> $<T, A> pass($<T, Tuple<A, Function<W, W>>> m) {
            return $(s -> inner.pass(inner.map(runState(m, s), (a, s1) ->
                a.mapFirst(r -> Tuple.of(r, s1)))));
        }
    }

    @SuppressWarnings("unchecked")
    static <T, S, M extends Monad<M>> MonadState<T, S> liftState(MonadTrans<T, M> outer) {
        MonadState<M, S> inner;
        M nm = outer.inner();
        if (nm instanceof MonadState) {
            inner = (MonadState<M,S>)nm;
        } else if (nm instanceof MonadTrans) {
            inner = (MonadState<M,S>)((MonadTrans<T,M>)nm).liftState();
        } else {
            throw new UnsupportedOperationException("liftState");
        }
        return new LiftState<>(outer, inner);
    }

    private static class LiftState<T, S, M extends Monad<M>> implements MonadState<T, S>, ForwardingMonad<T> {
        private final MonadTrans<T, M> outer;
        private final MonadState<M, S> inner;

        LiftState(MonadTrans<T, M> outer, MonadState<M, S> inner) {
            this.outer = outer;
            this.inner = inner;
        }

        @Override
        public Monad<T> delegate() {
            return outer;
        }

        @Override
        public <A> $<T, A> state(Function<? super S, Tuple<A, S>> f) {
            return outer.lift(inner.state(f));
        }

        @Override
        public $<T, S> get() {
            return outer.lift(inner.get());
        }

        @Override
        public $<T, Unit> put(S s) {
            return outer.lift(inner.put(s));
        }

        @Override
        public $<T, Unit> modify(Function<S, S> f) {
            return outer.lift(inner.modify(f));
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <E> MonadExcept<T, E> liftExcept() {
        MonadExcept<M, E> inner;
        if (nm instanceof MonadExcept) {
            inner = (MonadExcept<M,E>)nm;
        } else if (nm instanceof MonadTrans) {
            inner = (MonadExcept<M,E>)((MonadTrans<T,M>)nm).liftExcept();
        } else {
            throw new UnsupportedOperationException("liftExcept");
        }
        return new LiftExcept<>(inner);
    }

    private class LiftExcept<E> implements MonadExcept<T, E>, ForwardingMonad<T> {
        private final MonadExcept<M, E> inner;

        LiftExcept(MonadExcept<M, E> inner) {
            this.inner = inner;
        }

        @Override
        public Monad<T> delegate() {
            return StateTC.this;
        }

        @Override
        public <A> $<T, A> throwE(E e) {
            return lift(inner.throwE(e));
        }

        @Override
        public <A> $<T, A> catchE(Function<? super E, ? extends $<T, A>> h, $<T, A> m) {
            return $(s -> inner.catchE(e -> runState(h.apply(e), s), runState(m, s)));
        }
    }
}
