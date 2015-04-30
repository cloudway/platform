/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.control.monad.trans;

import java.util.function.Function;

import com.cloudway.fp.$;
import com.cloudway.fp.control.ForwardingMonad;
import com.cloudway.fp.control.Monad;
import com.cloudway.fp.control.MonadPlus;
import com.cloudway.fp.data.Fn;
import com.cloudway.fp.data.Maybe;
import com.cloudway.fp.data.Tuple;
import com.cloudway.fp.data.Unit;

/**
 * The {@link MaybeT} monad typeclass definition.
 *
 * @param <T> the maybe monad typeclass
 * @param <M> the inner monad typeclass
 */
public abstract class MaybeTC<T, M extends Monad<M>>
    implements MonadPlus<T>, MonadTrans<T, M>
{
    /**
     * The monadic type that encapsulate a Maybe computation.
     */
    public static abstract class Monadic<T, M, A> implements $<T, A> {
        final $<M, Maybe<A>> value;

        /**
         * Construct a maybe transformer monad.
         */
        protected Monadic($<M, Maybe<A>> value) {
            this.value = value;
        }
    }

    private final M nm;

    /**
     * Construct a maybe transformer monad typeclass.
     */
    protected MaybeTC(M nm) {
        this.nm = nm;
    }

    /**
     * Implemented by concrete class to instantiate a new maybe monad.
     */
    protected abstract <A> $<T, A> $($<M, Maybe<A>> value);

    /**
     * Unwrap the maybe computation.
     */
    @SuppressWarnings("unchecked")
    public <A> $<M, Maybe<A>> runMaybe($<T, A> m) {
        return ((Monadic<T,M,A>)m).value;
    }

    /**
     * Construct a pure computation that results in the given value.
     */
    @Override
    public <A> $<T, A> pure(A a) {
        return $(nm.pure(Maybe.of(a)));
    }

    /**
     * Fail with a message.
     */
    @Override
    public <A> $<T, A> fail(String msg) {
        return $(nm.pure(Maybe.empty()));
    }

    /**
     * Promote an inner monad to a maybe transformer monad.
     */
    @Override
    public <A> $<T, A> lift($<M, A> m) {
        return $(nm.map(m, Maybe::of));
    }

    /**
     * Returns the inner monad typeclass.
     */
    @Override
    public M inner() {
        return nm;
    }

    /**
     * Convert an {@link ExceptT} computation to {@code MaybeT}, discarding
     * the value of any exception.
     */
    public <A> $<T, A> fromExceptT(ExceptT.Monadic<?, ?, M, A> m) {
        return $(nm.map(m.value, v -> v.isLeft() ? Maybe.empty() : Maybe.of(v.right())));
    }

    /**
     * Transform the computation inside a {@code MaybeT}.
     */
    public <A, B> $<T, B>
    mapMaybe($<T, A> m, Function<? super $<M, Maybe<A>>, ? extends $<M, Maybe<B>>> f) {
        return $(f.apply(runMaybe(m)));
    }

    /**
     * Transfer a maybe computation by feeding the value to the given function
     * and wrap the result to a new computation.
     */
    @Override
    public <A, B> $<T, B> map($<T, A> m, Function<? super A, ? extends B> f) {
        return $(nm.map(runMaybe(m), v -> v.map(f)));
    }

    /**
     * Transfer a maybe computation by feeding the value to the given function
     * and wrap the result to a new computation.
     */
    @Override
    public <A, B> $<T, B> bind($<T, A> m, Function<? super A, ? extends $<T, B>> k) {
        return $(nm.bind(runMaybe(m), (Maybe<A> v) ->
            v.isPresent() ? runMaybe(k.apply(v.get()))
                          : nm.pure(Maybe.empty())));
    }

    /**
     * Returns the identity of monoid.
     */
    @Override
    public <A> $<T, A> mzero() {
        return $(nm.pure(Maybe.empty()));
    }

    /**
     * Combines two maybe computation.
     */
    @Override
    public <A> $<T, A> mplus($<T, A> x, $<T, A> y) {
        return $(nm.bind(runMaybe(x), v -> v.isPresent() ? nm.pure(v) : runMaybe(y)));
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
            return MaybeTC.this;
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
            return mapMaybe(m, v -> inner.local(f, v));
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
            return MaybeTC.this;
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
            MonadWriter<M, W> wt = this.inner;
            return $(wt.map(wt.listen(runMaybe(m)), (Tuple<Maybe<A>, W> t) ->
                t.as((a, w) -> a.map(r -> Tuple.of(r, w)))));
        }

        @Override
        public <A> $<T, A> pass($<T, Tuple<A, Function<W, W>>> m) {
            MonadWriter<M, W> wt = this.inner;
            return $(wt.pass(wt.map(runMaybe(m), (Maybe<Tuple<A, Function<W, W>>> a) ->
                a.isPresent() ? Tuple.of(Maybe.of(a.get().first()), a.get().second())
                              : Tuple.of(Maybe.empty(), Fn.id()))));
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
            return MaybeTC.this;
        }

        @Override
        public <A> $<T, A> throwE(E e) {
            return lift(inner.throwE(e));
        }

        @Override
        public <A> $<T, A> catchE(Function<? super E, ? extends $<T, A>> h, $<T, A> m) {
            return $(inner.catchE(e -> runMaybe(h.apply(e)), runMaybe(m)));
        }
    }
}
