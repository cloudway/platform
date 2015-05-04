/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.control.monad.trans;

import com.cloudway.fp.$;
import com.cloudway.fp.control.Monad;
import com.cloudway.fp.data.PMap;
import com.cloudway.fp.data.Tuple;

/**
 * A binding state transformer monad.
 *
 * @param <K> the type of keys maintained by binding state
 * @param <V> the type of mapped values in binding state
 * @param <M> the inner monad type class
 */
public final class BindT<K, V, M extends Monad<M>>
    extends StateTC<BindT<K, V, M>, PMap<K, V>, M>
    implements MonadBind<BindT<K, V, M>, K, V>
{
    private BindT(M nm) {
        super(nm);
    }

    /**
     * Stack transformer monad on another monad.
     *
     * @param nm the inner monad
     * @return a stacked monad
     */
    public static <K, V, M extends Monad<M>> BindT<K, V, M> on(M nm) {
        return new BindT<>(nm);
    }

    /**
     * Evaluate a state computation with the given initial state and return
     * a tuple of final value and final state.
     */
    public static <K, V, M extends Monad<M>, A> $<M, Tuple<A, PMap<K,V>>>
    run($<BindT<K, V, M>, A> m, PMap<K, V> s) {
        return m.getTypeClass().runState(m, s);
    }

    /**
     * Evaluate a state computation with the given initial state and return
     * the final result, discarding the final state.
     */
    public static <K, V, M extends Monad<M>, A> $<M, A>
    eval($<BindT<K, V, M>, A> m, PMap<K, V> s) {
        return m.getTypeClass().evalState(m, s);
    }

    /**
     * Evaluate state computation with the given initial state and return
     * the final state, discarding the final value.
     */
    public static <K, V, M extends Monad<M>, A> $<M, PMap<K, V>>
    exec($<BindT<K, V, M>, A> m, PMap<K, V> s) {
        return m.getTypeClass().execState(m, s);
    }
}
