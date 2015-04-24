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

    <E, A> $<T, A> liftCatch(ExceptTC<M, E, ?> et, Function<? super E, ? extends $<T, A>> h, $<T, A> m) {
        return $(r -> et.catchE(e -> runReader(h.apply(e), r), runReader(m, r)));
    }
}
