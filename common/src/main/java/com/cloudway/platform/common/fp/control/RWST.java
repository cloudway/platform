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
import com.cloudway.platform.common.fp.data.Fn;
import com.cloudway.platform.common.fp.data.Monoid;
import com.cloudway.platform.common.fp.data.Triple;
import com.cloudway.platform.common.fp.data.Tuple;
import com.cloudway.platform.common.fp.data.Unit;

/**
 * A monad transformer that combines {@link ReaderT}, {@link WriterT} and
 * {@link StateT}.
 *
 * @param <T> the monad typeclass
 * @param <R> the reader input type
 * @param <W> the writer output type
 * @param <S> the state type
 * @param <M> the inner monad typeclass
 */
public abstract class RWST<T extends RWST<T,R,W,S,M>, R, W, S, M extends Monad<M>>
    implements Monad<T>, MonadTrans<T, M>
{
    /**
     * The monadic datatype that encapsulate the computation transformation.
     *
     * @param <T> the monad typeclass
     * @param <R> the reader input type
     * @param <W> the writer output type
     * @param <S> the state type
     * @param <M> the inner monad typeclass
     * @param <A> the computation type
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
    protected RWST(Monoid<W> wm, M nm) {
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
     * Construct a computation in the reader monad (equivalent to {@link #asks}).
     */
    public <A> $<T, A> reader(Function<? super R, ? extends A> f) {
        return asks(f);
    }

    /**
     * Fetch the value of the environment.
     */
    public $<T, R> ask() {
        return $((r, s) -> raw(r, s, wm.empty()));
    }

    /**
     * Execute a computation in a modified environment.
     */
    public <A> $<T, A> local($<T, A> m, Function<R, R> f) {
        return $((r, s) -> runRWS(m, f.apply(r), s));
    }

    /**
     * Retrieve a function of the current environment.
     */
    public <A> $<T, A> asks(Function<? super R, ? extends A> f) {
        return $((r, s) -> raw(f.apply(r), s, wm.empty()));
    }

    // Writer operations

    /**
     * Construct a writer computation from a (result, output) pair.
     */
    public <A> $<T, A> writer(Tuple<A, W> p) {
        return $((r, s) -> raw(p.first(), s, p.second()));
    }

    /**
     * An action that produces the given output.
     */
    public $<T, Unit> tell(W w) {
        return $((r, s) -> raw(Unit.U, s, w));
    }

    /**
     * An action that executes the given action and adds its output to the
     * value of the computation.
     */
    public <A> $<T, Tuple<A, W>> listen($<T, A> m) {
        return $((r, s) -> nm.bind(runRWS(m, r, s), t ->
            t.as((a, s1, w) -> raw(Tuple.of(a, w), s1, w))));
    }

    /**
     * An action that executes the given action and adds the result of applying
     * given function to the output to the value of the computation.
     */
    public <A, B> $<T, Tuple<A, B>> listens($<T, A> m, Function<? super W, ? extends B> f) {
        return $((r, s) -> nm.bind(runRWS(m, r, s), t ->
            t.as((a, s1, w) -> raw(Tuple.of(a, f.apply(w)), s1, w))));
    }

    /**
     * An action that executes the given action, which returns a value and a
     * function, and returns the value, applying the function to the output.
     */
    public <A> $<T, A> pass($<T, Tuple<A, Function<W, W>>> m) {
        return $((r, s) -> nm.bind(runRWS(m, r, s), t ->
            t.as((af, s1, w) -> af.as((A a, Function<W, W> f) -> raw(a, s1, f.apply(w))))));
    }

    /**
     * An action that executes the given action and applies the given function
     * to its output, leaving the return value unchanged.
     */
    public <A> $<T, A> censor($<T, A> m, Function<W, W> f) {
        return $((r, s) -> nm.bind(runRWS(m, r, s), t ->
            t.as((a, s1, w) -> raw(a, s1, f.apply(w)))));
    }

    // State operations

    /**
     * Construct a state monad computation from a state transformer function.
     */
    public <A> $<T, A> state(Function<? super S, Tuple<A, S>> f) {
        return $((r, s) -> f.apply(s).as((a, s1) -> raw(a, s1, wm.empty())));
    }

    /**
     * Fetch the current value of the state within the monad.
     */
    public $<T, S> get() {
        return $((r, s) -> raw(s, s, wm.empty()));
    }

    /**
     * Sets the state within the monad.
     */
    public $<T, Unit> put(S s) {
        return $((_r, _s) -> raw(Unit.U, s, wm.empty()));
    }

    /**
     * An action that updates the state to the result of applying given
     * function to the current state.
     */
    public $<T, Unit> modify(Function<S, S> f) {
        return $((r, s) -> raw(Unit.U, f.apply(s), wm.empty()));
    }

    /**
     * Get a specific component of the state, using a projection function supplied.
     */
    public <A> $<T, A> gets(Function<? super S, ? extends A> f) {
        return $((r, s) -> raw(f.apply(s), s, wm.empty()));
    }

    // Monad Stacking

    /**
     * Stack RWS monad on another monad.
     *
     * @param wm the monoid used to collect output
     * @param nm the inner monad
     * @return a stacked monad
     */
    public static <R, W, S, M extends Monad<M>> On<R, W, S, M> on(Monoid<W> wm, M nm) {
        return new On<>(wm, nm);
    }

    /**
     * The stacked RWS monad typeclass.
     */
    public static final class On<R, W, S, M extends Monad<M>>
        extends RWST<On<R,W,S,M>, R, W, S, M>
    {
        private On(Monoid<W> wm, M nm) {
            super(wm, nm);
        }

        @Override
        protected <A> $<On<R,W,S,M>, A>
        $(BiFunction<? super R, ? super S, ? extends $<M, Triple<A,S,W>>> f) {
            return new Monadic<On<R,W,S,M>, R, W, S, M, A>(f) {
                @Override public On<R,W,S,M> getTypeClass() {
                    return On.this;
                }
            };
        }
    }
}
