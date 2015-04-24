/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.control.trans;

import java.util.function.Function;

import com.cloudway.platform.common.fp.$;
import com.cloudway.platform.common.fp.control.Monad;
import com.cloudway.platform.common.fp.data.Either;
import com.cloudway.platform.common.fp.data.Fn;
import com.cloudway.platform.common.fp.data.Tuple;

import static com.cloudway.platform.common.fp.data.Either.left;
import static com.cloudway.platform.common.fp.data.Either.right;

/**
 * The {@link ExceptT} monad typeclass definition.
 */
public abstract class ExceptTC<T, E, M extends Monad<M>> implements MonadTrans<T, M> {
    /**
     * The monadic data that encapsulate an exception computation.
     */
    public static abstract class Monadic<T, E, M, A> implements $<T, A> {
        final $<M, Either<E, A>> value;

        /**
         * Construct a exception transformer monad.
         */
        protected Monadic($<M, Either<E, A>> value) {
            this.value = value;
        }
    }

    private final M nm;

    /**
     * Construct a exception transformer monad typeclass.
     */
    protected ExceptTC(M nm) {
        this.nm = nm;
    }

    /**
     * Implemented by concrete class to instantiate a new exception monad.
     */
    protected abstract <A> $<T, A> $($<M, Either<E,A>> value);

    /**
     * Unwrap the exception computation.
     */
    @SuppressWarnings("unchecked")
    public <A> $<M, Either<E, A>> runExcept($<T, A> m) {
        return ((Monadic<T,E,M,A>)m).value;
    }

    /**
     * Constructor for computations in the exception monad.
     */
    public <A> $<T, A> except(Either<E, A> m) {
        return $(nm.pure(m));
    }

    /**
     * Construct a pure computation that results in the given value.
     */
    @Override
    public <A> $<T, A> pure(A a) {
        return $(nm.pure(right(a)));
    }

    /**
     * Fail with a message.
     */
    @Override
    public <A> $<T, A> fail(String msg) {
        return $(nm.fail(msg));
    }

    /**
     * Promote an inner monad to a exception transformer monad.
     */
    @Override
    public <A> $<T, A> lift($<M, A> m) {
        return $(nm.map(m, Either::right));
    }

    /**
     * Returns the inner monad typeclass.
     */
    @Override
    public M inner() {
        return nm;
    }

    /**
     * Convert a {@link MaybeT} computation to {@code ExceptT}, with a default
     * exception value.
     */
    public <A> $<T, A> fromMaybeT(E e, MaybeT.Monadic<?, M, A> m) {
        return $(nm.map(m.value, v -> v.isAbsent() ? left(e) : right(v.get())));
    }

    /**
     * Map the unwrapped computation using the given function.
     */
    public <A, B> $<T, B> mapExcept($<T, A> m,
            Function<? super $<M, Either<E, A>>, ? extends $<M, Either<E, B>>> f) {
        return $(f.apply(runExcept(m)));
    }

    /**
     * Transform any exceptions thrown by the computation using the given
     * function.
     */
    public <A> $<T, A> withExcept($<T, A> m, Function<E, E> f) {
        return $(nm.map(runExcept(m), v ->
            v.<Either<E,A>>either(e -> left(f.apply(e)), Either::right)));
    }

    /**
     * Signal an exception value.
     */
    public <A> $<T, A> throwE(E e) {
        return $(nm.pure(left(e)));
    }

    /**
     * Handle an exception.
     *
     * @param h a handler for exceptions in the inner computation
     * @param m the inner computation
     */
    public <A> $<T, A> catchE(Function<? super E, ? extends $<T, A>> h, $<T, A> m) {
        return $(nm.bind(runExcept(m), a ->
            a.either(l -> runExcept(h.apply(l)),
                     r -> nm.pure(Either.<E,A>right(r)))));
    }

    /**
     * Transfer a exception computation by feeding the value to the given
     * function and wrap the result to a new computation.
     */
    @Override
    public <A, B> $<T, B> map($<T, A> m, Function<? super A, ? extends B> f) {
        return $(nm.map(runExcept(m), v -> v.map(f)));
    }

    /**
     * Transfer a exception computation by feeding the value to the given
     * function and wrap the result to a new computation.
     */
    @Override
    public <A, B> $<T, B> bind($<T, A> m, Function<? super A, ? extends $<T, B>> k) {
        return $(nm.bind(runExcept(m), a ->
            a.either(e -> nm.pure(Either.<E,B>left(e)),
                     x -> runExcept(k.apply(x)))));
    }

    /**
     * Sequential application.
     */
    @Override
    public <A, B> $<T, B> ap($<T, Function<? super A, ? extends B>> f, $<T, A> v) {
        return $(nm.bind(runExcept(f), mf ->
            mf.<$<M, Either<E,B>>>either(
                e -> nm.pure(left(e)),
                k -> nm.bind(runExcept(v), mv ->
                    mv.either(e -> nm.pure(left(e)),
                              x -> nm.pure(right(k.apply(x))))))));
    }

    // Lifting other operations

    <W, A> $<T, Tuple<A, W>> liftListen(WriterTC<M, W, ?> wt, $<T, A> m) {
        return $(wt.bind(wt.listen(runExcept(m)), (Tuple<Either<E, A>, W> t) ->
            t.as((a, w) -> wt.pure(a.map(r -> Tuple.of(r, w))))));
    }

    <W, A> $<T, A> liftPass(WriterTC<M, W, ?> wt, $<T, Tuple<A, Function<W, W>>> m) {
        return $(wt.pass(wt.bind(runExcept(m), (Either<E, Tuple<A, Function<W, W>>> t) ->
            wt.pure(t.<Tuple<Either<E,A>, Function<W,W>>>either(
                l -> Tuple.of(left(l), Fn.id()),
                r -> r.mapFirst(Either::right))))));
    }
}
