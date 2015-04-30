/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.control.monad.trans;

import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;

import com.cloudway.fp.$;
import com.cloudway.fp.control.ForwardingMonad;
import com.cloudway.fp.control.Monad;
import com.cloudway.fp.data.Fn;
import com.cloudway.fp.data.Monoid;
import com.cloudway.fp.data.Triple;
import com.cloudway.fp.data.Tuple;
import com.cloudway.fp.data.Unit;

/**
 * The {@link RWST} monad typeclass definition.
 *
 * @param <T> the monad typeclass
 * @param <R> the reader input type
 * @param <W> the writer output type
 * @param <S> the state type
 * @param <M> the inner monad typeclass
 */
public abstract class RWSTC<T, R, W, S, M extends Monad<M>>
    implements MonadTrans<T, M>, MonadReader<T, R>, MonadWriter<T, W>, MonadState<T, S>
{
    /**
     * The monadic data that encapsulate the computation transformation.
     */
    public static abstract class Monadic<T, R, W, S, M, A> implements $<T, A> {
        private final BiFunction<? super R, ? super S, ? extends $<M, Triple<A, S, W>>> rwsf;

        /**
         * Construct a transformation monadic.
         */
        protected Monadic(BiFunction<? super R, ? super S, ? extends $<M, Triple<A, S, W>>> f) {
            this.rwsf = f;
        }
    }

    private final Monoid<W> wm;
    private final M nm;

    /**
     * Construct a {@code RWST} transformation monad
     *
     * @param wm the {@link Monoid} used to collect outputs
     * @param nm the inner monad
     */
    protected RWSTC(Monoid<W> wm, M nm) {
        this.wm = wm;
        this.nm = nm;
    }

    /**
     * Implemented by concrete class to instantiate a new monad.
     */
    protected abstract <A> $<T, A>
    $(BiFunction<? super R, ? super S, ? extends $<M, Triple<A, S, W>>> f);

    // helper method to construct a raw (result, state, output) triplet
    private <A> $<M, Triple<A, S, W>> raw(A a, S s, W w) {
        return nm.pure(Triple.of(a, s, w));
    }

    /**
     * Construct an RWS computation from a function.
     */
    public <A> $<T, A> rws(BiFunction<? super R, ? super S, Triple<A, S, W>> f) {
        return $((r, s) -> nm.pure(f.apply(r, s)));
    }

    /**
     * Construct a pure computation that results in the given value.
     */
    @Override
    public <A> $<T, A> pure(A a) {
        return $((r, s) -> raw(a, s, wm.empty()));
    }

    /**
     * Construct a pure computation that results in the given lazy evaluation
     * thunk.
     */
    @Override
    public <A> $<T, A> lazy(Supplier<A> a) {
        Supplier<A> t = Fn.lazy(a);
        return $((r, s) -> raw(t.get(), s, wm.empty()));
    }

    /**
     * Fail with a message.
     */
    @Override
    public <A> $<T, A> fail(String msg) {
        return $((r, s) -> nm.fail(msg));
    }

    /**
     * Promote an inner monad to a RWS computation monad.
     */
    @Override
    public <A> $<T, A> lift($<M, A> m) {
        return $((r, s) -> nm.bind(m, a -> raw(a, s, wm.empty())));
    }

    /**
     * Returns the inner monad typeclass.
     */
    @Override
    public M inner() {
        return nm;
    }

    /**
     * Unwrap an RWS computation as a (result, state, output) triplet.
     */
    @SuppressWarnings("unchecked")
    public <A> $<M, Triple<A, S, W>> runRWS($<T, A> m, R r, S s) {
        return ((Monadic<T,R,W,S,M,A>)m).rwsf.apply(r, s);
    }

    /**
     * Evaluate a computation with the given initial state and environment,
     * returning the final value and output, discarding the final state.
     */
    public <A> $<M, Tuple<A, W>> evalRWS($<T, A> m, R r, S s) {
        return nm.map(runRWS(m, r, s), t -> Tuple.of(t._1(), t._3()));
    }

    /**
     * Evaluate a computation with the given initial state and environment,
     * returning the final state and output, discarding the final value.
     */
    public <A> $<M, Tuple<S, W>> execRWS($<T, A> m, R r, S s) {
        return nm.map(runRWS(m, r, s), t -> Tuple.of(t._2(), t._3()));
    }

    /**
     * Map the inner computation using the given function.
     */
    public <A, B> $<T, B> mapRWS($<T, A> m,
            Function<? super $<M, Triple<A,S,W>>, ? extends $<M, Triple<B,S,W>>> f) {
        return $((r, s) -> f.apply(runRWS(m, r, s)));
    }

    /**
     * Executes the given action with an initial environment and state modified
     * by applying the given function.
     */
    public <A> $<T, A> withRWS($<T, A> m, BiFunction<? super R, ? super S, Tuple<R, S>> f) {
        return $((r, s) -> f.apply(r, s).as((r1, s1) -> runRWS(m, r1, s1)));
    }

    /**
     * Transfer a RWS computation by feeding the value to the given function
     * and wraps the result to new computation.
     */
    @Override
    public <A, B> $<T, B> map($<T, A> m, Function<? super A, ? extends B> f) {
        return $((r, s) -> nm.map(runRWS(m, r, s), t -> Triple.of(f.apply(t._1()), t._2(), t._3())));
    }

    /**
     * Transfer a RWS computation by feeding the value to the given function
     * and wraps the result to new computation.
     */
    @Override
    public <A, B> $<T, B> bind($<T, A> m, Function<? super A, ? extends $<T, B>> k) {
        return $((r, s) -> nm.bind(runRWS(m, r, s), t1 -> t1.as((a, s1, w1) ->
                           nm.bind(runRWS(k.apply(a), r, s1), t2 -> t2.as((b, s2, w2) ->
                           raw(b, s2, wm.append(w1, w2)))))));
    }

    // Reader operations

    /**
     * Retrieve a function of the current environment.
     */
    @Override
    public <A> $<T, A> reader(Function<? super R, ? extends A> f) {
        return $((r, s) -> raw(f.apply(r), s, wm.empty()));
    }

    /**
     * Fetch the value of the environment.
     */
    @Override
    public $<T, R> ask() {
        return $((r, s) -> raw(r, s, wm.empty()));
    }

    /**
     * Execute a computation in a modified environment.
     */
    @Override
    public <A> $<T, A> local(Function<R, R> f, $<T, A> m) {
        return $((r, s) -> runRWS(m, f.apply(r), s));
    }

    // Writer operations

    /**
     * Embeds a simple writer action.
     */
    @Override
    public <A> $<T, A> writer(Tuple<A, W> p) {
        return $((r, s) -> raw(p.first(), s, p.second()));
    }

    /**
     * An action that produces the given output.
     */
    @Override
    public $<T, Unit> tell(W w) {
        return $((r, s) -> raw(Unit.U, s, w));
    }

    /**
     * An action that executes the given action and adds its output to the
     * value of the computation.
     */
    @Override
    public <A> $<T, Tuple<A, W>> listen($<T, A> m) {
        return $((r, s) -> nm.bind(runRWS(m, r, s), t ->
            t.as((a, s1, w) -> raw(Tuple.of(a, w), s1, w))));
    }

    /**
     * An action that executes the given action and adds the result of applying
     * given function to the output to the value of the computation.
     */
    @Override
    public <A, B> $<T, Tuple<A, B>> listens(Function<? super W, ? extends B> f, $<T, A> m) {
        return $((r, s) -> nm.bind(runRWS(m, r, s), t ->
            t.as((a, s1, w) -> raw(Tuple.of(a, f.apply(w)), s1, w))));
    }

    /**
     * An action that executes the given action, which returns a value and a
     * function, and returns the value, applying the function to the output.
     */
    @Override
    public <A> $<T, A> pass($<T, Tuple<A, Function<W, W>>> m) {
        return $((r, s) -> nm.bind(runRWS(m, r, s), t ->
            t.as((af, s1, w) -> af.as((A a, Function<W, W> f) -> raw(a, s1, f.apply(w))))));
    }

    /**
     * An action that executes the given action and applies the given function
     * to its output, leaving the return value unchanged.
     */
    @Override
    public <A> $<T, A> censor(Function<W, W> f, $<T, A> m) {
        return $((r, s) -> nm.bind(runRWS(m, r, s), t ->
            t.as((a, s1, w) -> raw(a, s1, f.apply(w)))));
    }

    // State operations

    /**
     * Embed a simple state action into the monad.
     */
    @Override
    public <A> $<T, A> state(Function<? super S, Tuple<A, S>> f) {
        return $((r, s) -> f.apply(s).as((a, s1) -> raw(a, s1, wm.empty())));
    }

    /**
     * Fetch the current value of the state within the monad.
     */
    @Override
    public $<T, S> get() {
        return $((r, s) -> raw(s, s, wm.empty()));
    }

    /**
     * Sets the state within the monad.
     */
    @Override
    public $<T, Unit> put(S s) {
        return $((_r, _s) -> raw(Unit.U, s, wm.empty()));
    }

    /**
     * An action that updates the state to the result of applying given
     * function to the current state.
     */
    @Override
    public $<T, Unit> modify(Function<S, S> f) {
        return $((r, s) -> raw(Unit.U, f.apply(s), wm.empty()));
    }

    /**
     * Fetch the current value of the state and bind the value to the given function.
     */
    @Override
    public <A> $<T, A> get(Function<? super S, ? extends $<T, A>> f) {
        return $((r, s) -> runRWS(f.apply(s), r, s));
    }
    /**
     * Get a specific component of the state, using a projection function supplied.
     */
    @Override
    public <A> $<T, A> gets(Function<? super S, ? extends A> f) {
        return $((r, s) -> raw(f.apply(s), s, wm.empty()));
    }

    // Lifting other operations

    @Override
    @SuppressWarnings("unchecked")
    public <R1> MonadReader<T, R1> liftReader() {
        MonadReader<M, R1> inner;
        if (nm instanceof MonadReader) {
            inner = (MonadReader<M,R1>)nm;
        } else if (nm instanceof MonadTrans) {
            inner = (MonadReader<M,R1>)((MonadTrans<T,M>)nm).liftReader();
        } else {
            throw new UnsupportedOperationException("liftReader");
        }
        return new LiftReader<>(inner);
    }

    private class LiftReader<R1> implements MonadReader<T, R1>, ForwardingMonad<T> {
        private final MonadReader<M, R1> inner;

        LiftReader(MonadReader<M, R1> inner) {
            this.inner = inner;
        }

        @Override
        public Monad<T> delegate() {
            return RWSTC.this;
        }

        @Override
        public <A> $<T, A> reader(Function<? super R1, ? extends A> f) {
            return lift(inner.reader(f));
        }

        @Override
        public $<T, R1> ask() {
            return lift(inner.ask());
        }

        @Override
        public <A> $<T, A> local(Function<R1, R1> f, $<T, A> m) {
            return mapRWS(m, v -> inner.local(f, v));
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <W1> MonadWriter<T, W1> liftWriter() {
        MonadWriter<M, W1> inner;
        if (nm instanceof MonadWriter) {
            inner = (MonadWriter<M,W1>)nm;
        } else if (nm instanceof MonadTrans) {
            inner = (MonadWriter<M,W1>)((MonadTrans<T,M>)nm).liftWriter();
        } else {
            throw new UnsupportedOperationException("liftWriter");
        }
        return new LiftWriter<>(inner);
    }

    private class LiftWriter<W1> implements MonadWriter<T, W1>, ForwardingMonad<T> {
        private final MonadWriter<M, W1> inner;

        LiftWriter(MonadWriter<M, W1> inner) {
            this.inner = inner;
        }

        @Override
        public Monad<T> delegate() {
            return RWSTC.this;
        }

        @Override
        public <A> $<T, A> writer(Tuple<A, W1> aw) {
            return lift(inner.writer(aw));
        }

        @Override
        public $<T, Unit> tell(W1 w) {
            return lift(inner.tell(w));
        }

        @Override
        public <A> $<T, Tuple<A, W1>> listen($<T, A> m) {
            MonadWriter<M, W1> wt = this.inner;
            return $((r, s) -> wt.bind(wt.listen(runRWS(m, r, s)), (Tuple<Triple<A, S, W>, W1> t) ->
                t.as((asw, w) -> asw.as((a, s1, w1) -> wt.pure(Triple.of(Tuple.of(a, w), s1, w1))))));
        }

        @Override
        public <A> $<T, A> pass($<T, Tuple<A, Function<W1, W1>>> m) {
            MonadWriter<M, W1> wt = this.inner;
            return $((r, s) -> wt.pass(wt.bind(runRWS(m, r, s), (Triple<Tuple<A, Function<W1, W1>>, S, W> t) ->
                t.as((af, w1, s1) -> af.as((a, f) -> wt.pure(Tuple.of(Triple.of(a, w1, s1), f)))))));
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
            return RWSTC.this;
        }

        @Override
        public <A> $<T, A> throwE(E e) {
            return lift(inner.throwE(e));
        }

        @Override
        public <A> $<T, A> catchE(Function<? super E, ? extends $<T, A>> h, $<T, A> m) {
            return $((r, s) -> inner.catchE(e -> runRWS(h.apply(e), r, s), runRWS(m, r, s)));
        }
    }
}
