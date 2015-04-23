/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.control;

import java.util.function.Function;

import com.cloudway.platform.common.fp.$;
import com.cloudway.platform.common.fp.data.Maybe;

/**
 * The {@code MaybeT} monad transformer extends a monad with the ability to
 * exit the computation without returning a value.
 *
 * <p>A sequence of actions produces a value only if all the actions in the
 * sequence do. If one exists, the rest of the sequence is skipped and the
 * composite action exits.
 *
 * <p>For a variant allowing a range of exception values, see {@link ExceptT}.
 *
 * @param <T> the maybe monad typeclass
 * @param <M> the inner monad typeclass
 */
public abstract class MaybeT<T extends MaybeT<T, M>, M extends Monad<M>>
    implements MonadPlus<T>, MonadTrans<T, M>
{
    /**
     * The monadic datatype that encapsulate a Maybe computation.
     *
     * @param <T> the maybe monad typeclass
     * @param <M> the inner monad typeclass
     * @param <A> the computation value type
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
    protected MaybeT(M nm) {
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
        return $(nm.bind(runMaybe(m), v ->
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

    // Monad Stacking

    /**
     * Stack maybe monad on another monad.
     */
    public static <M extends Monad<M>> On<M> on(M nm) {
        return new On<>(nm);
    }

    /**
     * The stacked monad typeclass.
     */
    public static final class On<M extends Monad<M>> extends MaybeT<On<M>, M> {
        private On(M nm) {
            super(nm);
        }

        @Override
        protected <A> $<On<M>, A> $($<M, Maybe<A>> value) {
            return new Monadic<On<M>, M, A>(value) {
                @Override public On<M> getTypeClass() {
                    return On.this;
                }
            };
        }
    }
}
