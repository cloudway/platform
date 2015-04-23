/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.control;

import java.util.function.Function;

import com.cloudway.platform.common.fp.$;
import com.cloudway.platform.common.fp.data.Either;
import static com.cloudway.platform.common.fp.data.Either.left;
import static com.cloudway.platform.common.fp.data.Either.right;

/**
 * This monad transformer extends a monad with the ability throw exceptions.
 *
 * <p>A sequence of actions terminates normally, producing a value, only if
 * none of the actions in the sequence throws an exception. If one throws
 * an exception, the rest of the sequence is skipped and the composite action
 * exists with that exception.
 *
 * <p>If the value of the exception is not required, the variant in {@link
 * MaybeT} may be used instead.
 *
 * @param <T> the exception monad typeclass
 * @param <E> the exception type
 * @param <M> the inner monad typeclass
 */
public abstract class ExceptT<T extends ExceptT<T, E, M>, E, M extends Monad<M>>
    implements Monad<T>, MonadTrans<T, M>
{
    /**
     * The monadic datatype that encapsulate a exception computation.
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
    protected ExceptT(M nm) {
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
                     r -> nm.pure(Either.<E, A>right(r)))));
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
            a.either(e -> nm.pure(Either.<E, B>left(e)),
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

    // Monad Stacking

    /**
     * Stack exception monad on another monad.
     */
    public static <E, M extends Monad<M>> On<E, M> on(M nm) {
        return new On<>(nm);
    }

    /**
     * The stacked monad typeclass.
     */
    public static final class On<E, M extends Monad<M>> extends ExceptT<On<E,M>, E, M> {
        private On(M nm) {
            super(nm);
        }

        @Override
        protected <A> $<On<E,M>, A> $($<M, Either<E, A>> value) {
            return new Monadic<On<E,M>, E, M, A>(value) {
                @Override public On<E,M> getTypeClass() {
                    return On.this;
                }
            };
        }
    }
}
