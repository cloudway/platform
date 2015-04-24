/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.control.trans;

import java.util.function.BiFunction;
import java.util.function.Supplier;

import com.cloudway.platform.common.fp.$;
import com.cloudway.platform.common.fp.control.Monad;
import com.cloudway.platform.common.fp.data.Maybe;
import com.cloudway.platform.common.fp.data.Tuple;

/**
 * The {@code ListT} monad transformer, adding backtracking to a given monad,
 * which must be commutative.
 *
 * @param <M> the inner monad typeclass
 */
public class ListT<M extends Monad<M>> extends ListTC<ListT<M>, M> {
    public ListT(M nm) {
        super(nm);
    }

    @Override
    protected <A> $<ListT<M>, A> $(Supplier<$<M, MList<M, A>>> run) {
        return new Monadic<ListT<M>, M, A>(run) {
            @Override
            public ListT<M> getTypeClass() {
                return ListT.this;
            }
        };
    }

    public static <M extends Monad<M>> ListT<M> on(M nm) {
        return new ListT<>(nm);
    }

    public static <M extends Monad<M>, A>
    $<M, MList<M, A>> run($<ListT<M>, A> m) {
        return m.getTypeClass().runList(m);
    }

    public static <M extends Monad<M>, A>
    $<M, Maybe<Tuple<A, $<ListT<M>, A>>>> run_($<ListT<M>, A> m) {
        return m.getTypeClass().runList_(m);
    }

    public static <M extends Monad<M>, A, R>
    $<M, R> fold($<ListT<M>, A> m, R z, BiFunction<? super A, $<M, R>, R> f) {
        return m.getTypeClass().foldList(m, z, f);
    }
}
