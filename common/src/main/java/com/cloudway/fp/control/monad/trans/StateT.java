/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.control.monad.trans;

import com.cloudway.fp.$;
import com.cloudway.fp.control.Monad;
import com.cloudway.fp.data.Tuple;

/**
 * A state transformer monad.
 *
 * @param <S> the state type
 * @param <M> the inner monad typeclass
 */
public final class StateT<S, M extends Monad<M>>
    extends StateTC<StateT<S, M>, S, M>
{
    private StateT(M nm) {
        super(nm);
    }

    /**
     * Stack monad on another monad.
     *
     * @param nm the inner monad
     * @return a stacked monad
     */
    public static <S, M extends Monad<M>> StateT<S, M> on(M nm) {
        return new StateT<>(nm);
    }

    public static <S, M extends Monad<M>, A> $<M, Tuple<A,S>> run($<StateT<S, M>, A> m, S s) {
        return m.getTypeClass().runState(m, s);
    }

    public static <S, M extends Monad<M>, A> $<M, A> eval($<StateT<S, M>, A> m, S s) {
        return m.getTypeClass().evalState(m, s);
    }

    public static <S, M extends Monad<M>> $<M, S> exec($<StateT<S, M>, ?> m, S s) {
        return m.getTypeClass().execState(m, s);
    }
}
