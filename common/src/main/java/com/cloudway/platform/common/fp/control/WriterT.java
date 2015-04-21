/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.control;

import java.util.function.BiFunction;
import java.util.function.Function;

import com.cloudway.platform.common.fp.$;
import com.cloudway.platform.common.fp.data.Monoid;
import com.cloudway.platform.common.fp.data.Tuple;
import com.cloudway.platform.common.fp.data.Unit;

/**
 * The Writer monad adds collection of outputs (such as a count or string
 * output) to a given monad.
 *
 * <p>This monad transformer provides only limited access to the output
 * during the computation. For more general access, use {@link StateT}
 * instead.
 *
 * @param <T> the writer monad typeclass
 * @param <W> the writer output type
 * @param <M> the inner monad typeclass
 */
public abstract class WriterT<T extends WriterT<T,W,M>, W, M extends Monad<M>>
    implements Monad<T>, MonadTrans<T, M>
{
    /**
     * The monadic datatype that encapsulate the writer outputs.
     *
     * @param <T> the writer monad typeclass
     * @param <W> the writer output type
     * @param <M> the inner monad typeclass
     * @param <A> the computation type
     */
    public static abstract class Monadic<T, W, M, A> implements $<T, A> {
        private final $<M, Tuple<A, W>> value;

        /**
         * Construct a writer transformation monadic.
         */
        protected Monadic($<M, Tuple<A, W>> value) {
            this.value = value;
        }
    }

    private final Monoid<W> wm;
    private final M nm;

    /**
     * Construct a writer transformation monad
     *
     * @param wm the {@link Monoid} used to collect outputs
     * @param nm the inner monad
     */
    protected WriterT(Monoid<W> wm, M nm) {
        this.wm = wm;
        this.nm = nm;
    }

    /**
     * Implemented by concrete class to instantiate a new writer monad.
     */
    protected abstract <A> $<T, A> $($<M, Tuple<A, W>> value);

    // helper method to construct a raw (result, output) pair.
    private <A> $<M, Tuple<A, W>> raw(A a, W w) {
        return nm.pure(Tuple.of(a, w));
    }

    /**
     * Construct a writer computation from a (result, output) pair.
     */
    public <A> $<T, A> writer(Tuple<A, W> t) {
        return $(nm.pure(t));
    }

    /**
     * Construct a pure computation that results in the given value.
     */
    @Override
    public <A> $<T, A> pure(A a) {
        return $(raw(a, wm.empty()));
    }

    /**
     * Fail with a message.
     */
    @Override
    public <A> $<T, A> fail(String msg) {
        return $(nm.fail(msg));
    }

    /**
     * Promote an inner monad to a writer monad.
     */
    @Override
    public <A> $<T, A> lift($<M, A> m) {
        return $(nm.bind(m, a -> raw(a, wm.empty())));
    }

    /**
     * Returns the inner monad typeclass.
     */
    @Override
    public M inner() {
        return nm;
    }

    /**
     * Unwrap a writer computation as a (result, output) pair.
     */
    @SuppressWarnings("unchecked")
    public <A> $<M, Tuple<A,W>> runWriter($<T, A> m) {
        return ((Monadic<T,W,M,A>)m).value;
    }

    /**
     * Extract the result from a writer computation.
     */
    public <A> $<M, A> evalWriter($<T, A> m) {
        return nm.map(runWriter(m), Tuple::first);
    }

    /**
     * Extract the output from a writer computation.
     */
    public <A> $<M, W> execWriter($<T, A> m) {
        return nm.map(runWriter(m), Tuple::second);
    }

    /**
     * Map both the return value and output of a computation using the given
     * function.
     */
    public <A, B> $<T, B>
    mapWriter($<T, A> m, Function<? super $<M, Tuple<A, W>>, ? extends $<M, Tuple<B, W>>> f) {
        return $(f.apply(runWriter(m)));
    }

    /**
     * Transfer a writer computation by feeding the value to the given function
     * and wrapping the result to a new writer computation.
     */
    @Override
    public <A, B> $<T, B> map($<T, A> m, Function<? super A, ? extends B> f) {
        return $(nm.map(runWriter(m), t -> t.mapFirst(f)));
    }

    /**
     * Transfer a writer computation by feeding the value to the given function
     * and wrapping the result to a new writer computation.
     */
    @Override
    public <A, B> $<T, B> bind($<T, A> m, Function<? super A, ? extends $<T, B>> k) {
        return $(nm.bind(runWriter(m), t1 -> t1.as((a, w) ->
                 nm.bind(runWriter(k.apply(a)), t2 -> t2.as((b, w1) ->
                 raw(b, wm.append(w, w1)))))));
    }

    /**
     * Sequential application.
     */
    @Override
    public <A, B> $<T, B> ap($<T, Function<? super A, ? extends B>> f, $<T, A> v) {
        BiFunction<Tuple<Function<? super A, ? extends B>, W>, Tuple<A, W>, Tuple<B, W>> k =
            (x, y) -> Tuple.of(x.first().apply(y.first()), wm.append(x.second(), y.second()));
        return $(nm.ap2(k, runWriter(f), runWriter(v)));
    }

    /**
     * An action that produces the output.
     */
    public $<T, Unit> tell(W w) {
        return $(raw(Unit.U, w));
    }

    /**
     * An action that executes the given action and adds its output to the
     * value of the computation.
     */
    public <A> $<T, Tuple<A, W>> listen($<T, A> m) {
        return $(nm.bind(runWriter(m), t -> t.as((a, w) -> raw(Tuple.of(a, w), w))));
    }

    /**
     * An action that executes the given action and adds the result of applying
     * given function to the output to the value of the computation.
     */
    public <A, B> $<T, Tuple<A, B>> listens($<T, A> m, Function<? super W, ? extends B> f) {
        return $(nm.bind(runWriter(m), t -> t.as((a, w) -> raw(Tuple.of(a, f.apply(w)), w))));
    }

    /**
     * An action that executes the given action, which returns a value and a
     * function, and returns the value, applying the function to the output.
     */
    public <A> $<T, A> pass($<T, Tuple<A, Function<W, W>>> m) {
        return $(nm.bind(runWriter(m), t -> t.as((af, w) -> af.as((a, f) -> raw(a, f.apply(w))))));
    }

    /**
     * An action that executes the given action and applies the given function
     * to its output, leaving the return value unchanged.
     */
    public <A> $<T, A> censor($<T, A> m, Function<W, W> f) {
        return $(nm.bind(runWriter(m), t -> t.as((a, w) -> raw(a, f.apply(w)))));
    }

    // Monad Stacking

    /**
     * Stack writer monad on another monad.
     *
     * @param wm the monoid used to collect output
     * @param nm the inner monad
     * @return a stacked writer monad
     */
    public static <W, M extends Monad<M>> On<W, M> on(Monoid<W> wm, M nm) {
        return new On<>(wm, nm);
    }

    /**
     * The stacked writer monad typeclass.
     */
    public static final class On<W, M extends Monad<M>> extends WriterT<On<W,M>, W, M> {
        private On(Monoid<W> wm, M nm) {
            super(wm, nm);
        }

        @Override
        protected <A> $<On<W,M>, A> $($<M, Tuple<A, W>> value) {
            return new Monadic<On<W,M>, W, M, A>(value) {
                @Override public On<W,M> getTypeClass() {
                    return On.this;
                }
            };
        }
    }
}
