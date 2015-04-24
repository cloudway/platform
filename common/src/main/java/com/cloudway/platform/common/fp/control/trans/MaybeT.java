/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.control.trans;

import com.cloudway.platform.common.fp.$;
import com.cloudway.platform.common.fp.control.Monad;
import com.cloudway.platform.common.fp.data.Maybe;

/**
 * The {@code MaybeT} monad transformer extends a monad with the ability to
 * exit the computation without returning a value.
 *
 * <p>A sequence of actions produces a value only if all the actions in the
 * sequence do. If one exists, the rest of the sequence is skipped and the
 * composite action exits.
 *
 * <p>For a variant allowing a range of exception values, see {@link ExceptT}.
 *
 * @param <M> the inner monad typeclass
 */
public final class MaybeT<M extends Monad<M>> extends MaybeTC<MaybeT<M>, M> {
    private MaybeT(M nm) {
        super(nm);
    }

    @Override
    protected <A> $<MaybeT<M>, A> $($<M, Maybe<A>> value) {
        return new Monadic<MaybeT<M>, M, A>(value) {
            @Override public MaybeT<M> getTypeClass() {
                return MaybeT.this;
            }
        };
    }

    /**
     * Stack maybe monad on another monad.
     */
    public static <M extends Monad<M>> MaybeT<M> on(M nm) {
        return new MaybeT<>(nm);
    }

    public static <M extends Monad<M>, A> $<M, Maybe<A>> run($<MaybeT<M>, A> m) {
        return m.getTypeClass().runMaybe(m);
    }
}
