/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.control.trans;

import com.cloudway.platform.common.fp.$;
import com.cloudway.platform.common.fp.control.Monad;

/**
 * The CPS transformer monad.
 *
 * @param <M> the inner monad typeclass
 */
public final class ContT<M extends Monad<M>> extends ContTC<ContT<M>, M>
{
    private ContT(M nm) {
        super(nm);
    }

    @Override
    protected <R, A> $<ContT<M>, A> $(Monadic.K<A, M, R> f) {
        return new Monadic<ContT<M>, M, A>(f) {
            @Override public ContT<M> getTypeClass() {
                return ContT.this;
            }
        };
    }

    /**
     * Stack monad on another monad.
     *
     * @param nm the inner monad
     * @return a stacked monad
     */
    public static <M extends Monad<M>> ContT<M> on(M nm) {
        return new ContT<>(nm);
    }
}
