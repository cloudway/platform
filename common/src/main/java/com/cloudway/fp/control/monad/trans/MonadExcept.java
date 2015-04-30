/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.control.monad.trans;

import java.util.function.Function;

import com.cloudway.fp.$;
import com.cloudway.fp.control.Monad;

/**
 * The {@code MonadExcept} typeclass.
 */
public interface MonadExcept<M, E> extends Monad<M> {
    /**
     * Signal an exception value.
     */
    <A> $<M, A> throwE(E e);

    /**
     * Handle an exception.
     *
     * @param h a handler for exceptions in the inner computation
     * @param m the inner computation
     */
    <A> $<M, A> catchE(Function<? super E, ? extends $<M, A>> h, $<M, A> m);
}
