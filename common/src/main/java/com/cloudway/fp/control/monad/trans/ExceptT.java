/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.control.monad.trans;

import java.util.function.Function;

import com.cloudway.fp.$;
import com.cloudway.fp.control.Monad;
import com.cloudway.fp.data.Either;

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
 * @param <E> the exception type
 * @param <M> the inner monad typeclass
 */
public final class ExceptT<E, M extends Monad<M>>
    extends ExceptTC<ExceptT<E, M>, E, M>
{
    private ExceptT(M nm) {
        super(nm);
    }

    @Override
    protected <A> $<ExceptT<E, M>, A> $($<M, Either<E, A>> value) {
        return new Monadic<ExceptT<E,M>, E, M, A>(value) {
            @Override public ExceptT<E,M> getTypeClass() {
                return ExceptT.this;
            }
        };
    }

    /**
     * Stack exception monad on another monad.
     */
    public static <E, M extends Monad<M>> ExceptT<E, M> on(M nm) {
        return new ExceptT<>(nm);
    }

    public static <T, E, A, ET extends ExceptTC<ET, E, ?>>
    $<T, A> throwE(MonadTrans<T, ET> mt, E e) {
        return mt.lift(mt.inner().<A>throwE(e));
    }

    public static <R, E, A, ET extends ExceptTC<ET, E, ?>>
    $<ReaderT<R, ET>, A> catchE(ReaderT<R, ET> mt,
                                Function<? super E, ? extends $<ReaderT<R, ET>, A>> h,
                                $<ReaderT<R, ET>, A> m) {
        return mt.liftCatch(mt.inner(), h, m);
    }

    public static <W, E, A, ET extends ExceptTC<ET, E, ?>>
    $<WriterT<W, ET>, A> catchE(WriterT<W, ET> mt,
                                Function<? super E, ? extends $<WriterT<W, ET>, A>> h,
                                $<WriterT<W, ET>, A> m) {
        return mt.liftCatch(mt.inner(), h, m);
    }

    public static <S, E, A, ET extends ExceptTC<ET, E, ?>>
    $<StateT<S, ET>, A> catchE(StateT<S, ET> mt,
                               Function<? super E, ? extends $<StateT<S, ET>, A>> h,
                               $<StateT<S, ET>, A> m) {
        return mt.liftCatch(mt.inner(), h, m);
    }

    public static <R, W, S, E, A, ET extends ExceptTC<ET, E, ?>>
    $<RWST<R,W,S,ET>, A> catchE(RWST<R,W,S,ET> mt,
                                Function<? super E, ? extends $<RWST<R,W,S,ET>, A>> h,
                                $<RWST<R,W,S,ET>, A> m) {
        return mt.liftCatch(mt.inner(), h, m);
    }

    public static <E, A, ET extends ExceptTC<ET, E, ?>>
    $<ListT<ET>, A> catchE(ListT<ET> mt,
                           Function<? super E, ? extends $<ListT<ET>, A>> h,
                           $<ListT<ET>, A> m) {
        return mt.liftCatch(mt.inner(), h, m);
    }

    public static <E, A, ET extends ExceptTC<ET, E, ?>>
    $<MaybeT<ET>, A> catchE(MaybeT<ET> mt,
                            Function<? super E, ? extends $<MaybeT<ET>, A>> h,
                            $<MaybeT<ET>, A> m) {
        return mt.liftCatch(mt.inner(), h, m);
    }
}
