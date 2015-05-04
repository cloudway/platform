/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.control.monad.trans;

import com.cloudway.fp.$;
import com.cloudway.fp.control.Monad;
import com.cloudway.fp.data.Monoid;
import com.cloudway.fp.data.Triple;
import com.cloudway.fp.data.Tuple;

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
}
