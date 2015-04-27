/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.control.arrow;

import java.util.function.Function;

import com.cloudway.fp.$;
import com.cloudway.fp.π;
import com.cloudway.fp.control.Monad;
import com.cloudway.fp.control.MonadPlus;

/**
 * Kleisli arrows of a monad.
 */
public interface Kleisli<M extends Monad<M>, A, B>
    extends Function<A, $<M, B>>, π<KleisliArrow<M>, A, B>
{
    /**
     * Construct a Kleisli arrow typeclass.
     *
     * @param nm the inner monad typeclass
     * @return the Kleisli arrow typeclass
     */
    static <M extends Monad<M>> KleisliArrow<M> arrow(M nm) {
        return new KleisliArrow<>(nm);
    }

    /**
     * Construct a Kleisli arrow plus typeclass.
     */
    static <M extends MonadPlus<M>> KleisliArrowPlus<M> arrow(M nm) {
        return new KleisliArrowPlus<>(nm);
    }

    /**
     * Narrowing a generic arrow type.
     */
    static <M extends Monad<M>, A, B> Kleisli<M, A, B> narrow(π<KleisliArrow<M>, A, B> value) {
        return (Kleisli<M, A, B>)value;
    }

    /**
     * Execute the Kleisli arrow with the given input.
     */
    static <M extends Monad<M>, A, B> $<M, B> run(π<KleisliArrow<M>, A, B> k, A x) {
        return narrow(k).apply(x);
    }

    /**
     * The type safe arrow operation.
     */
    default <C> Kleisli<M, A, C> then(π<KleisliArrow<M>, B, C> next) {
        return getTypeClass().then(this, next);
    }
}
