/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.control.trans;

import java.util.function.Function;

import com.cloudway.platform.common.fp.$;
import com.cloudway.platform.common.fp.control.Monad;

/**
 * The parameterizable reader monad.
 *
 * <p>Computations are functions of a shared environment.
 *
 * @param <R> the environment type
 * @param <M> the inner monad typeclass
 */
public final class ReaderT<R, M extends Monad<M>>
    extends ReaderTC<ReaderT<R, M>, R, M>
{
    private ReaderT(M nm) {
        super(nm);
    }

    @Override
    protected <A> $<ReaderT<R, M>, A> $(Function<? super R, ? extends $<M, A>> f) {
        return new Monadic<ReaderT<R, M>, R, M, A>(f) {
            @Override
            public ReaderT<R,M> getTypeClass() {
                return ReaderT.this;
            }
        };
    }

    /**
     * Stack monad on another monad.
     *
     * @param nm the inner monad
     * @return a stacked monad
     */
    public static <R, M extends Monad<M>> ReaderT<R, M> on(M nm) {
        return new ReaderT<>(nm);
    }

    public static <R, M extends Monad<M>, A> $<M, A> run($<ReaderT<R, M>, A> m, R r) {
        return m.getTypeClass().runReader(m, r);
    }

    public static <T, R, RT extends ReaderTC<RT, R, ?>>
    $<T, R> ask(MonadTrans<T, RT> mt) {
        return mt.lift(mt.inner().ask());
    }

    public static <T, R, A, RT extends ReaderTC<RT, R, ?>>
    $<T, A> asks(MonadTrans<T, RT> mt, Function<? super R, ? extends A> f) {
        return mt.lift(mt.inner().asks(f));
    }

    public static <R1, R, A, RT extends ReaderTC<RT, R, ?>>
    $<ReaderT<R1, RT>, A> local(ReaderT<R1, RT> mt, Function<R, R> f, $<ReaderT<R1, RT>, A> m) {
        return mt.mapReader(m, v -> mt.inner().local(f, v));
    }

    public static <W, R, A, RT extends ReaderTC<RT, R, ?>>
    $<WriterT<W, RT>, A> local(WriterT<W, RT> mt, Function<R, R> f, $<WriterT<W, RT>, A> m) {
        return mt.mapWriter(m, v -> mt.inner().local(f, v));
    }

    public static <S, R, A, RT extends ReaderTC<RT, R, ?>>
    $<StateT<S, RT>, A> local(StateT<S, RT> mt, Function<R, R> f, $<StateT<S, RT>, A> m) {
        return mt.mapState(m, v -> mt.inner().local(f, v));
    }

    public static <R1, W, S, R, A, RT extends ReaderTC<RT, R, ?>>
    $<RWST<R1,W,S,RT>, A> local(RWST<R1,W,S,RT> mt, Function<R, R> f, $<RWST<R1,W,S,RT>, A> m) {
        return mt.mapRWS(m, v -> mt.inner().local(f, v));
    }

    public static <R, A, RT extends ReaderTC<RT, R, ?>>
    $<ContT<RT>, A> local(ContT<RT> mt, Function<R, R> f, $<ContT<RT>, A> m) {
        return mt.liftLocal(mt.inner(), f, m);
    }

    public static <E, R, A, RT extends ReaderTC<RT, R, ?>>
    $<ExceptT<E, RT>, A> local(ExceptT<E, RT> mt, Function<R, R> f, $<ExceptT<E, RT>, A> m) {
        return mt.mapExcept(m, v -> mt.inner().local(f, v));
    }

    public static <R, A, RT extends ReaderTC<RT, R, ?>>
    $<ListT<RT>, A> local(ListT<RT> mt, Function<R, R> f, $<ListT<RT>, A> m) {
        return mt.mapList(m, v -> mt.inner().local(f, v));
    }

    public static <R, A, RT extends ReaderTC<RT, R, ?>>
    $<MaybeT<RT>, A> local(MaybeT<RT> mt, Function<R, R> f, $<MaybeT<RT>, A> m) {
        return mt.mapMaybe(m, v -> mt.inner().local(f, v));
    }
}
