/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.control.monad.trans;

import com.cloudway.fp.$;
import com.cloudway.fp.control.Monad;
import com.cloudway.fp.data.Either;

/**
 * This monad transformer extends a monad with the ability throw exceptions.
 *
 * <p>A sequence of actions terminates normally, producing a value, only if
 * none of the actions in the sequence throws an exception. If one throws
 * an exception, the rest of the sequence is skipped and the composite action
 * exists with that exception.
 *
 * <p>If the value of the exception is not required, the variant in {@link
 * MaybeT} may be used instead.
 *
 * @param <E> the exception type
 * @param <M> the inner monad typeclass
 */
public final class ExceptT<E, M extends Monad<M>>
    extends ExceptTC<ExceptT<E, M>, E, M>
{
    private ExceptT(M nm) {
        super(nm);
    }

    @Override
    protected <A> $<ExceptT<E, M>, A> $($<M, Either<E, A>> value) {
        return new Monadic<ExceptT<E,M>, E, M, A>(value) {
            @Override public ExceptT<E,M> getTypeClass() {
                return ExceptT.this;
            }
        };
    }

    /**
     * Stack exception monad on another monad.
     */
    public static <E, M extends Monad<M>> ExceptT<E, M> on(M nm) {
        return new ExceptT<>(nm);
    }

    public static <E, M extends Monad<M>, A> $<M, Either<E, A>> run($<ExceptT<E, M>, A> m) {
        return m.getTypeClass().runExcept(m);
    }
}
