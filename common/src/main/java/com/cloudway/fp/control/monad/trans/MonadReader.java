/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.control.monad.trans;

import java.util.function.Function;

import com.cloudway.fp.$;
import com.cloudway.fp.control.Monad;
import com.cloudway.fp.data.Fn;

/**
 * The {@code MonadReader} typeclass.
 *
 * <p>Minimal complete definition: ({@link #ask} | {@link #reader}), {@link #local}.
 */
public interface MonadReader<M, R> extends Monad<M> {
    /**
     * Retrieves a function of the current environment.
     */
    default <A> $<M, A> reader(Function<? super R, ? extends A> f) {
        return map(ask(), f);
    }

    /**
     * Retrieves the monad environment.
     */
    default $<M, R> ask() {
        return reader(Fn.id());
    }

    /**
     * Execute a computation in a modified environment.
     */
    <A> $<M, A> local(Function<R, R> f, $<M, A> m);

    /**
     * Retrieves a function of the current environment.
     */
    default <A> $<M, A> asks(Function<? super R, ? extends A> f) {
        return reader(f);
    }
}
