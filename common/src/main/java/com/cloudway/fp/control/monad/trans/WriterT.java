/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.control.monad.trans;

import com.cloudway.fp.$;
import com.cloudway.fp.control.Monad;
import com.cloudway.fp.data.Monoid;
import com.cloudway.fp.data.Tuple;

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
}
