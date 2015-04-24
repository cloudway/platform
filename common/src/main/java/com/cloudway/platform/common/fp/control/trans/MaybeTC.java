/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.control.trans;

import java.util.function.Function;

import com.cloudway.platform.common.fp.$;
import com.cloudway.platform.common.fp.control.Monad;
import com.cloudway.platform.common.fp.control.MonadPlus;
import com.cloudway.platform.common.fp.data.Fn;
import com.cloudway.platform.common.fp.data.Maybe;
import com.cloudway.platform.common.fp.data.Tuple;

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

    <W, A> $<T, Tuple<A, W>> liftListen(WriterTC<M, W, ?> wt, $<T, A> m) {
        return $(wt.map(wt.listen(runMaybe(m)), (Tuple<Maybe<A>, W> t) ->
            t.as((a, w) -> a.map(r -> Tuple.of(r, w)))));
    }

    <W, A> $<T, A> liftPass(WriterTC<M, W, ?> wt, $<T, Tuple<A, Function<W, W>>> m) {
        return $(wt.pass(wt.map(runMaybe(m), (Maybe<Tuple<A, Function<W, W>>> a) ->
            a.isPresent() ? Tuple.of(Maybe.of(a.get().first()), a.get().second())
                          : Tuple.of(Maybe.empty(), Fn.id()))));
    }

    <E, A> $<T, A> liftCatch(ExceptTC<M, E, ?> et, Function<? super E, ? extends $<T, A>> h, $<T, A> m) {
        return $(et.catchE(e -> runMaybe(h.apply(e)), runMaybe(m)));
    }
}
