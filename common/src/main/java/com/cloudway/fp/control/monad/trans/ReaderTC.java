/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.control.monad.trans;

import java.util.function.Function;
import java.util.function.Supplier;

import com.cloudway.fp.$;
import com.cloudway.fp.control.ForwardingMonad;
import com.cloudway.fp.control.Monad;
import com.cloudway.fp.data.Fn;
import com.cloudway.fp.data.Tuple;
import com.cloudway.fp.data.Unit;

/**
 * The {@link ReaderT} monad typeclass definition.
 *
 * @param <T> the reader monad typeclass
 * @param <R> the environment type
 * @param <M> the inner monad typeclass
 */
public abstract class ReaderTC<T, R, M extends Monad<M>>
    implements MonadTrans<T, M>, MonadReader<T, R>
{
    /**
     * The monadic data that encapsulate a computation transfer function.
     * This class should be implemented by concrete reader monad.
     */
    public static abstract class Monadic<T, R, M, A> implements $<T, A> {
        private final Function<? super R, ? extends $<M, A>> rf;

        /**
         * Construct a environment transformation monad.
         */
        protected Monadic(Function<? super R, ? extends $<M, A>> f) {
            this.rf = f;
        }
    }

    private final M nm;

    /**
     * Construct a environment transformer monad.
     *
     * @param m the inner monad
     */
    protected ReaderTC(M m) {
        this.nm = m;
    }

    /**
     * Implemented by concrete class to instantiate a new reader monad.
     */
    protected abstract <A> $<T, A> $(Function<? super R, ? extends $<M, A>> f);

    /**
     * Construct a pure computation that results in the given value.
     */
    @Override
    public <A> $<T, A> pure(A a) {
        return lift(nm.pure(a));
    }

    /**
     * Construct a pure computation that results in the given lazy evaluation
     * thunk.
     */
    @Override
    public <A> $<T, A> lazy(Supplier<A> a) {
        Supplier<A> t = Fn.lazy(a);
        return $(r -> nm.pure(t.get()));
    }

    /**
     * Fail with a message.
     */
    @Override
    public <A> $<T, A> fail(String msg) {
        return lift(nm.fail(msg));
    }

    /**
     * Promote an inner monad to a reader monad.
     */
    @Override
    public <A> $<T, A> lift($<M, A> m) {
        return $(Fn.pure(m));
    }

    /**
     * Returns the inner monad typeclass.
     */
    @Override
    public M inner() {
        return nm;
    }

    /**
     * Runs a reader and extracts the final value from it.
     */
    @SuppressWarnings("unchecked")
    public <A> $<M, A> runReader($<T, A> m, R r) {
        return ((Monadic<T,R,M,A>)m).rf.apply(r);
    }

    /**
     * Transform the computation inside a ReaderT.
     */
    public <A, B> $<T, B> mapReader($<T, A> m, Function<? super $<M, A>, ? extends $<M, B>> f) {
        return $(r -> f.apply(runReader(m, r)));
    }

    /**
     * Retrieve a function of the current environment.
     */
    @Override
    public <A> $<T, A> reader(Function<? super R, ? extends A> f) {
        return $(r -> nm.pure(f.apply(r)));
    }

    /**
     * Fetch the value of the environment.
     */
    @Override
    public $<T, R> ask() {
        return $(nm::pure);
    }

    /**
     * Execute a computation in a modified environment.
     */
    @Override
    public <A> $<T, A> local(Function<R, R> f, $<T, A> m) {
        return $(r -> runReader(m, f.apply(r)));
    }

    /**
     * Transfer a computation by feeding the value to the given function
     * and wrapping the result to new monadic.
     */
    @Override
    public <A, B> $<T, B> map($<T, A> m, Function<? super A, ? extends B> f) {
        return mapReader(m, x -> nm.map(x, f));
    }

    /**
     * Transfer a computation by feeding the value to the given function
     * and wrapping the result to new monadic.
     */
    @Override
    public <A, B> $<T, B> bind($<T, A> m, Function<? super A, ? extends $<T, B>> k) {
        return $(r -> nm.bind(runReader(m, r), a -> runReader(k.apply(a), r)));
    }

    /**
     * Sequential application.
     */
    @Override
    public <A, B> $<T, B> ap($<T, Function<? super A, ? extends B>> f, $<T, A> v) {
        return $(r -> nm.ap(runReader(f, r), runReader(v, r)));
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
            return ReaderTC.this;
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
            return mapReader(m, v -> inner.local(f, v));
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
            return ReaderTC.this;
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
            return mapReader(m, inner::listen);
        }

        @Override
        public <A> $<T, A> pass($<T, Tuple<A, Function<W, W>>> m) {
            return mapReader(m, inner::pass);
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
        return new LiftExcept<E>(inner);
    }

    private class LiftExcept<E> implements MonadExcept<T, E>, ForwardingMonad<T> {
        private final MonadExcept<M, E> inner;

        LiftExcept(MonadExcept<M, E> inner) {
            this.inner = inner;
        }

        @Override
        public Monad<T> delegate() {
            return ReaderTC.this;
        }

        @Override
        public <A> $<T, A> throwE(E e) {
            return lift(inner.throwE(e));
        }

        @Override
        public <A> $<T, A> catchE(Function<? super E, ? extends $<T, A>> h, $<T, A> m) {
            return $(r -> inner.catchE(e -> runReader(h.apply(e), r), runReader(m, r)));
        }
    }
}
