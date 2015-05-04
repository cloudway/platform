/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.control.monad.trans;

import com.cloudway.fp.$;
import com.cloudway.fp.control.Monad;

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
}
