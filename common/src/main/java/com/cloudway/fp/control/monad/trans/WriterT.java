/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.control.monad.trans;

import java.util.function.Function;

import com.cloudway.fp.$;
import com.cloudway.fp.control.Monad;
import com.cloudway.fp.data.Monoid;
import com.cloudway.fp.data.Tuple;
import com.cloudway.fp.data.Unit;

/**
 * The Writer monad adds collection of outputs (such as a count or string
 * output) to a given monad.
 *
 * <p>This monad transformer provides only limited access to the output
 * during the computation. For more general access, use {@link StateT}
 * instead.
 *
 * @param <W> the writer output type
 * @param <M> the inner monad typeclass
 */
public final class WriterT<W, M extends Monad<M>>
    extends WriterTC<WriterT<W, M>, W, M>
{
    private WriterT(Monoid<W> wm, M nm) {
        super(wm, nm);
    }

    @Override
    protected <A> $<WriterT<W, M>, A> $($<M, Tuple<A, W>> value) {
        return new Monadic<WriterT<W,M>, W, M, A>(value) {
            @Override public WriterT<W,M> getTypeClass() {
                return WriterT.this;
            }
        };
    }

    /**
     * Stack writer monad on another monad.
     *
     * @param wm the monoid used to collect output
     * @param nm the inner monad
     * @return a stacked writer monad
     */
    public static <W, M extends Monad<M>> WriterT<W, M> on(Monoid<W> wm, M nm) {
        return new WriterT<>(wm, nm);
    }

    public static <W, M extends Monad<M>, A> $<M, Tuple<A, W>> run($<WriterT<W, M>, A> m) {
        return m.getTypeClass().runWriter(m);
    }

    public static <T, W, WT extends WriterTC<WT, W, ?>>
    $<T, Unit> tell(MonadTrans<T, WT> mt, W w) {
        return mt.lift(mt.inner().tell(w));
    }

    public static <R, W, A, WT extends WriterTC<WT, W, ?>>
    $<ReaderT<R, WT>, Tuple<A, W>> listen(ReaderT<R, WT> mt, $<ReaderT<R, WT>, A> m) {
        return mt.mapReader(m, mt.inner()::listen);
    }

    public static <R, W, A, WT extends WriterTC<WT, W, ?>>
    $<ReaderT<R, WT>, A> pass(ReaderT<R, WT> mt, $<ReaderT<R, WT>, Tuple<A, Function<W, W>>> m) {
        return mt.mapReader(m, mt.inner()::pass);
    }

    public static <R, W, A, B, WT extends WriterTC<WT, W, ?>>
    $<ReaderT<R, WT>, Tuple<A, B>> listens(ReaderT<R, WT> mt,
           Function<? super W, ? extends B> f, $<ReaderT<R, WT>, A> m) {
        return mt.map(listen(mt, m), (Tuple<A, W> t) -> t.mapSecond(f));
    }

    public static <R, W, A, WT extends WriterTC<WT, W, ?>>
    $<ReaderT<R, WT>, A> censor(ReaderT<R, WT> mt, Function<W, W> f, $<ReaderT<R, WT>, A> m) {
        return pass(mt, mt.bind(m, a -> mt.pure(Tuple.of(a, f))));
    }

    public static <W1, W, A, WT extends WriterTC<WT, W, ?>>
    $<WriterT<W1, WT>, Tuple<A, W>> listen(WriterT<W1, WT> mt, $<WriterT<W1, WT>, A> m) {
        return mt.liftListen(mt.inner(), m);
    }

    public static <W1, W, A, WT extends WriterTC<WT, W, ?>>
    $<WriterT<W1, WT>, A> pass(WriterT<W1, WT> mt, $<WriterT<W1, WT>, Tuple<A, Function<W, W>>> m) {
        return mt.liftPass(mt.inner(), m);
    }

    public static <W1, W, A, B, WT extends WriterTC<WT, W, ?>>
    $<WriterT<W1, WT>, Tuple<A, B>> listens(WriterT<W1, WT> mt,
            Function<? super W, ? extends B> f, $<WriterT<W1, WT>, A> m) {
        return mt.map(listen(mt, m), (Tuple<A, W> t) -> t.mapSecond(f));
    }

    public static <W1, W, A, WT extends WriterTC<WT, W, ?>>
    $<WriterT<W1, WT>, A> censor(WriterT<W1, WT> mt, Function<W, W> f, $<WriterT<W1, WT>, A> m) {
        return pass(mt, mt.bind(m, a -> mt.pure(Tuple.of(a, f))));
    }

    public static <S, W, A, WT extends WriterTC<WT, W, ?>>
    $<StateT<S, WT>, Tuple<A, W>> listen(StateT<S, WT> mt, $<StateT<S, WT>, A> m) {
        return mt.liftListen(mt.inner(), m);
    }

    public static <S, W, A, WT extends WriterTC<WT, W, ?>>
    $<StateT<S, WT>, A> pass(StateT<S, WT> mt, $<StateT<S, WT>, Tuple<A, Function<W, W>>> m) {
        return mt.liftPass(mt.inner(), m);
    }

    public static <S, W, A, B, WT extends WriterTC<WT, W, ?>>
    $<StateT<S, WT>, Tuple<A, B>> listens(StateT<S, WT> mt,
            Function<? super W, ? extends B> f, $<StateT<S, WT>, A> m) {
        return mt.map(listen(mt, m), (Tuple<A, W> t) -> t.mapSecond(f));
    }

    public static <S, W, A, WT extends WriterTC<WT, W, ?>>
    $<StateT<S, WT>, A> censor(StateT<S, WT> mt, $<StateT<S, WT>, A> m, Function<W, W> f) {
        return pass(mt, mt.bind(m, a -> mt.pure(Tuple.of(a, f))));
    }

    public static <R, W1, S, W, A, WT extends WriterTC<WT, W, ?>>
    $<RWST<R,W1,S,WT>, Tuple<A, W>> listen(RWST<R,W1,S,WT> mt, $<RWST<R,W1,S,WT>, A> m) {
        return mt.liftListen(mt.inner(), m);
    }

    public static <R, W1, S, W, A, WT extends WriterTC<WT, W, ?>>
    $<RWST<R,W1,S,WT>, A> pass(RWST<R,W1,S,WT> mt, $<RWST<R,W1,S,WT>, Tuple<A, Function<W, W>>> m) {
        return mt.liftPass(mt.inner(), m);
    }

    public static <R, W1, S, W, A, B, WT extends WriterTC<WT, W, ?>>
    $<RWST<R,W1,S,WT>, Tuple<A, B>> listens(RWST<R,W1,S,WT> mt,
            Function<? super W, ? extends B> f, $<RWST<R,W1,S,WT>, A> m) {
        return mt.map(listen(mt, m), (Tuple<A, W> t) -> t.mapSecond(f));
    }

    public static <R, W1, S, W, A, WT extends WriterTC<WT, W, ?>>
    $<RWST<R,W1,S,WT>, A> censor(RWST<R,W1,S,WT> mt, Function<W, W> f, $<RWST<R,W1,S,WT>, A> m) {
        return pass(mt, mt.bind(m, a -> mt.pure(Tuple.of(a, f))));
    }

    public static <E, W, A, WT extends WriterTC<WT, W, ?>>
    $<ExceptT<E, WT>, Tuple<A, W>> listen(ExceptT<E, WT> mt, $<ExceptT<E, WT>, A> m) {
        return mt.liftListen(mt.inner(), m);
    }

    public static <E, W, A, WT extends WriterTC<WT, W, ?>>
    $<ExceptT<E, WT>, A> pass(ExceptT<E, WT> mt, $<ExceptT<E, WT>, Tuple<A, Function<W, W>>> m) {
        return mt.liftPass(mt.inner(), m);
    }

    public static <E, W, A, B, WT extends WriterTC<WT, W, ?>>
    $<ExceptT<E, WT>, Tuple<A, B>> listens(ExceptT<E, WT> mt,
            Function<? super W, ? extends B> f, $<ExceptT<E, WT>, A> m) {
        return mt.map(listen(mt, m), (Tuple<A, W> t) -> t.mapSecond(f));
    }

    public static <E, W, A, WT extends WriterTC<WT, W, ?>>
    $<ExceptT<E, WT>, A> censor(ExceptT<E, WT> mt, $<ExceptT<E, WT>, A> m, Function<W, W> f) {
        return pass(mt, mt.bind(m, a -> mt.pure(Tuple.of(a, f))));
    }

    public static <W, A, WT extends WriterTC<WT, W, ?>>
    $<MaybeT<WT>, Tuple<A, W>> listen(MaybeT<WT> mt, $<MaybeT<WT>, A> m) {
        return mt.liftListen(mt.inner(), m);
    }

    public static <W, A, WT extends WriterTC<WT, W, ?>>
    $<MaybeT<WT>, A> pass(MaybeT<WT> mt, $<MaybeT<WT>, Tuple<A, Function<W, W>>> m) {
        return mt.liftPass(mt.inner(), m);
    }

    public static <W, A, B, WT extends WriterTC<WT, W, ?>>
    $<MaybeT<WT>, Tuple<A, B>> listens(MaybeT<WT> mt,
            Function<? super W, ? extends B> f, $<MaybeT<WT>, A> m) {
        return mt.map(listen(mt, m), (Tuple<A, W> t) -> t.mapSecond(f));
    }

    public static <W, A, WT extends WriterTC<WT, W, ?>>
    $<MaybeT<WT>, A> censor(MaybeT<WT> mt, $<MaybeT<WT>, A> m, Function<W, W> f) {
        return pass(mt, mt.bind(m, a -> mt.pure(Tuple.of(a, f))));
    }
}
