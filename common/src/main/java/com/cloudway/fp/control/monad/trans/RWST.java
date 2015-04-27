/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.control.monad.trans;

import java.util.function.BiFunction;
import java.util.function.Function;

import com.cloudway.fp.$;
import com.cloudway.fp.control.Monad;
import com.cloudway.fp.data.Monoid;
import com.cloudway.fp.data.Triple;
import com.cloudway.fp.data.Tuple;
import com.cloudway.fp.data.Unit;

/**
 * A monad transformer that combines {@link ReaderT}, {@link WriterT} and
 * {@link StateT}.
 *
 * @param <R> the reader input type
 * @param <W> the writer output type
 * @param <S> the state type
 * @param <M> the inner monad typeclass
 */
public final class RWST<R, W, S, M extends Monad<M>>
    extends RWSTC<RWST<R, W, S, M>, R, W, S, M>
{
    private RWST(Monoid<W> wm, M nm) {
        super(wm, nm);
    }

    @Override
    protected <A> $<RWST<R, W, S, M>, A>
    $(BiFunction<? super R, ? super S, ? extends $<M, Triple<A,S,W>>> f) {
        return new Monadic<RWST<R,W,S,M>, R, W, S, M, A>(f) {
            @Override public RWST<R,W,S,M> getTypeClass() {
                return RWST.this;
            }
        };
    }

    /**
     * Stack RWS monad on another monad.
     *
     * @param wm the monoid used to collect output
     * @param nm the inner monad
     * @return a stacked monad
     */
    public static <R, W, S, M extends Monad<M>> RWST<R, W, S, M> on(Monoid<W> wm, M nm) {
        return new RWST<>(wm, nm);
    }

    public static <R, W, S, M extends Monad<M>, A> $<M, Triple<A, S, W>>
    run($<RWST<R, W, S, M>, A> m, R r, S s) {
        return m.getTypeClass().runRWS(m, r, s);
    }

    public static <R, W, S, M extends Monad<M>, A> $<M, Tuple<A, W>>
    eval($<RWST<R, W, S, M>, A> m, R r, S s) {
        return m.getTypeClass().evalRWS(m, r, s);
    }

    public static <R, W, S, M extends Monad<M>> $<M, Tuple<S, W>>
    exec($<RWST<R, W, S, M>, ?> m, R r, S s) {
        return m.getTypeClass().execRWS(m, r, s);
    }

    public static <T, R, MT extends RWSTC<MT,R,?,?,?>>
    $<T, R> ask(MonadTrans<T, MT> mt) {
        return mt.lift(mt.inner().ask());
    }

    public static <T, R, A, MT extends RWSTC<MT,R,?,?,?>>
    $<T, A> asks(MonadTrans<T, MT> mt, Function<? super R, ? extends A> f) {
        return mt.lift(mt.inner().asks(f));
    }

    public static <T, W, MT extends RWSTC<MT,?,W,?,?>>
    $<T, Unit> tell(MonadTrans<T, MT> mt, W w) {
        return mt.lift(mt.inner().tell(w));
    }

    public static <T, S, MT extends RWSTC<MT,?,?,S,?>>
    $<T, S> get(MonadTrans<T, MT> mt) {
        return mt.lift(mt.inner().get());
    }

    public static <T, S, MT extends RWSTC<MT,?,?,S,?>>
    $<T, Unit> put(MonadTrans<T, MT> mt, S s) {
        return mt.lift(mt.inner().put(s));
    }

    public static <T, S, MT extends RWSTC<MT,?,?,S,?>>
    $<T, Unit> modify(MonadTrans<T, MT> mt, Function<S, S> f) {
        return mt.lift(mt.inner().modify(f));
    }

    public static <T, S, A, MT extends RWSTC<MT,?,?,S,?>>
    $<T, A> gets(MonadTrans<T, MT> mt, Function<? super S, ? extends A> f) {
        return mt.lift(mt.inner().gets(f));
    }
}
