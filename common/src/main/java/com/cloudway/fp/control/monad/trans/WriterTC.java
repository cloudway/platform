/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.control.monad.trans;

import java.util.function.BiFunction;
import java.util.function.Function;

import com.cloudway.fp.$;
import com.cloudway.fp.control.ForwardingMonad;
import com.cloudway.fp.control.Monad;
import com.cloudway.fp.data.Monoid;
import com.cloudway.fp.data.Tuple;
import com.cloudway.fp.data.Unit;

/**
 * The {@link WriterT} monad typeclass definition.
 *
 * @param <T> the writer monad typeclass
 * @param <W> the writer output type
 * @param <M> the inner monad typeclass
 */
public abstract class WriterTC<T, W, M extends Monad<M>>
    implements MonadTrans<T, M>, MonadWriter<T, W>
{
    /**
     * The monadic data that encapsulate the writer outputs.
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
    protected WriterTC(Monoid<W> wm, M nm) {
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
        return $(nm.bind(runWriter(m), (a, w) ->
                 nm.bind(runWriter(k.apply(a)), (b, w1) ->
                 raw(b, wm.append(w, w1)))));
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
     * Embeds a simple writer action.
     */
    @Override
    public <A> $<T, A> writer(Tuple<A, W> t) {
        return $(nm.pure(t));
    }

    /**
     * An action that produces the output.
     */
    @Override
    public $<T, Unit> tell(W w) {
        return $(raw(Unit.U, w));
    }

    /**
     * An action that executes the given action and adds its output to the
     * value of the computation.
     */
    @Override
    public <A> $<T, Tuple<A, W>> listen($<T, A> m) {
        return $(nm.bind(runWriter(m), (a, w) -> raw(Tuple.of(a, w), w)));
    }

    /**
     * An action that executes the given action and adds the result of applying
     * given function to the output to the value of the computation.
     */
    @Override
    public <A, B> $<T, Tuple<A, B>> listens(Function<? super W, ? extends B> f, $<T, A> m) {
        return $(nm.bind(runWriter(m), (a, w) -> raw(Tuple.of(a, f.apply(w)), w)));
    }

    /**
     * An action that executes the given action, which returns a value and a
     * function, and returns the value, applying the function to the output.
     */
    @Override
    public <A> $<T, A> pass($<T, Tuple<A, Function<W, W>>> m) {
        return $(nm.bind(runWriter(m), (af, w) -> af.as((a, f) -> raw(a, f.apply(w)))));
    }

    /**
     * An action that executes the given action and applies the given function
     * to its output, leaving the return value unchanged.
     */
    @Override
    public <A> $<T, A> censor(Function<W, W> f, $<T, A> m) {
        return $(nm.bind(runWriter(m), (a, w) -> raw(a, f.apply(w))));
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
            return WriterTC.this;
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
            return mapWriter(m, v -> inner.local(f, v));
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
            return WriterTC.this;
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
            return $(inner.map(inner.listen(runWriter(m)), (Tuple<A, W> a, W1 w) ->
                a.mapFirst(r -> Tuple.of(r, w))));
        }

        @Override
        public <A> $<T, A> pass($<T, Tuple<A, Function<W1, W1>>> m) {
            return $(inner.pass(inner.map(runWriter(m), (Tuple<A, Function<W1, W1>> a, W w) ->
                a.mapFirst(r -> Tuple.of(r, w)))));
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
            return WriterTC.this;
        }

        @Override
        public <A> $<T, A> throwE(E e) {
            return lift(inner.throwE(e));
        }

        @Override
        public <A> $<T, A> catchE(Function<? super E, ? extends $<T, A>> h, $<T, A> m) {
            return $(inner.catchE(e -> runWriter(h.apply(e)), runWriter(m)));
        }
    }
}
