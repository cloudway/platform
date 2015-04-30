/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.control;

import java.util.function.Function;
import java.util.function.Supplier;

import com.cloudway.fp.$;
import com.cloudway.fp.data.Unit;

/**
 * A {@link Monad} which forwards all its method calls to another {@code Monad}.
 * Subclasses should override one or more methods to modify the behavior of the
 * backing monad as desired.
 */
public interface ForwardingMonad<M> extends Monad<M> {
    /**
     * Returns the delegate monad.
     *
     * @return the delegate monad
     */
    Monad<M> delegate();

    @Override
    default <A> $<M, A> pure(A a) {
        return delegate().pure(a);
    }

    @Override
    default <A> $<M, A> lazy(Supplier<A> a) {
        return delegate().lazy(a);
    }

    @Override
    default $<M, Unit> action(Runnable a) {
        return delegate().action(a);
    }

    @Override
    default <A> $<M, A> fail(String msg) {
        return delegate().fail(msg);
    }

    @Override
    default <A, B> $<M, B> map($<M, A> m, Function<? super A, ? extends B> f) {
        return delegate().map(m, f);
    }

    @Override
    default <A, B> $<M, B> bind($<M, A> m, Function<? super A, ? extends $<M, B>> k) {
        return delegate().bind(m, k);
    }

    @Override
    default <A, B> $<M, B> seqR($<M, A> a, $<M, B> b) {
        return delegate().seqR(a, b);
    }

    @Override
    default <A, B> $<M, B> seqR($<M, A> a, Supplier<? extends $<M, B>> b) {
        return delegate().seqR(a, b);
    }

    @Override
    default <A, B> $<M, B> ap($<M, Function<? super A, ? extends B>> f, $<M, A> v) {
        return delegate().ap(f, v);
    }
}
