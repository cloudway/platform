/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.control.arrow;

import java.util.function.Function;

import com.cloudway.fp.$;
import com.cloudway.fp.control.Monad;
import com.cloudway.fp.data.Fn;
import com.cloudway.fp.data.Tuple;
import com.cloudway.fp.data.Unit;
import com.cloudway.fp.π;

/**
 * The {@link ArrowApply} class is equivalent to {@link Monad}: any monad
 * gives rise to a {@link Kleisli} arrow, and any instance of
 * {@code ArrayApply} defines a monad.
 */
public class ArrowMonad<A extends ArrowApply<A>>
    implements Monad<ArrowMonad<A>>
{
    final A arrow;

    ArrowMonad(A arrow) {
        this.arrow = arrow;
    }

    public static <A extends ArrowApply<A>> ArrowMonad<A> create(A arrow) {
        return new ArrowMonad<>(arrow);
    }

    public static <A extends ArrowApply<A> & ArrowPlus<A>> ArrowMonadPlus<A> createPlus(A arrow) {
        return new ArrowMonadPlus<>(arrow);
    }

    private final class Monadic<B> implements $<ArrowMonad<A>, B> {
        final π<A, Unit, B> ar;

        Monadic(π<A, Unit, B> ar) {
            this.ar = ar;
        }

        @Override
        public ArrowMonad<A> getTypeClass() {
            return ArrowMonad.this;
        }
    }

    public <B> $<ArrowMonad<A>, B> monad(π<A, Unit, B> a) {
        return new Monadic<>(a);
    }

    public <B> π<A, Unit, B> unMonad($<ArrowMonad<A>, B> m) {
        return ((Monadic<B>)m).ar;
    }

    @Override
    public <B> $<ArrowMonad<A>, B> pure(B x) {
        return monad(arrow.arr(Fn.pure(x)));
    }

    @Override
    public <B, C> $<ArrowMonad<A>, C>
    map($<ArrowMonad<A>, B> m, Function<? super B, ? extends C> f) {
        return monad(arrow.then(unMonad(m), arrow.arr(f)));
    }

    @Override
    public <B, C> $<ArrowMonad<A>, C>
    bind($<ArrowMonad<A>, B> m, Function<? super B, ? extends $<ArrowMonad<A>, C>> k) {
        π<A, B, Tuple<π<A, Unit, C>, Unit>> arrK =
            arrow.arr(x -> Tuple.of(unMonad(k.apply(x)), Unit.U));
        return monad(arrow.then(unMonad(m), arrK, arrow.app()));
    }

    @Override
    public <B, C> $<ArrowMonad<A>, C>
    ap($<ArrowMonad<A>, Function<? super B, ? extends C>> f, $<ArrowMonad<A>, B> v) {
        π<A, Unit, Tuple<Function<? super B, ? extends C>, B>> split =
            arrow.fanout(unMonad(f), unMonad(v));
        π<A, Tuple<Function<? super B, ? extends C>, B>, C> merge =
            arrow.arr((k, x) -> k.apply(x));
        return monad(arrow.then(split, merge));
    }
}
