/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.control.arrow;


import java.util.function.BiFunction;
import java.util.function.Function;

import com.cloudway.fp.$;
import com.cloudway.fp.control.Monad;
import com.cloudway.fp.data.Either;
import com.cloudway.fp.data.Fn;
import com.cloudway.fp.data.Tuple;
import com.cloudway.fp.π;

import static com.cloudway.fp.control.arrow.Kleisli.narrow;
import static com.cloudway.fp.control.arrow.Kleisli.run;

/**
 * The Kleisli arrow typeclass definition.
 */
public class KleisliArrow<M extends Monad<M>>
    implements ArrowChoice<KleisliArrow<M>>, ArrowApply<KleisliArrow<M>>
{
    protected final M nm;

    protected KleisliArrow(M nm) {
        this.nm = nm;
    }

    /**
     * Construct a Kleisli arrow from given function.
     */
    public <A, B> Kleisli<M, A, B> kleisli(Function<? super A, ? extends $<M, B>> f) {
        return new Kleisli<M, A, B>() {
            @Override
            public $<M, B> apply(A a) {
                return f.apply(a);
            }

            @Override
            public KleisliArrow<M> getTypeClass() {
                return KleisliArrow.this;
            }
        };
    }

    /**
     * Construct a Kleisli arrow from given binary function.
     */
    public <A, B, C> Kleisli<M, Tuple<A, B>, C> kleisli(BiFunction<? super A, ? super B, ? extends $<M, C>> f) {
        return kleisli(t -> t.as(f));
    }

    @Override
    public <A> Kleisli<M, A, A> id() {
        return kleisli(nm::pure);
    }

    @Override
    public <A, B, C> Kleisli<M, A, C>
    compose(π<KleisliArrow<M>, B, C> f, π<KleisliArrow<M>, A, B> g) {
        return kleisli(b -> nm.bind(run(g, b), narrow(f)));
    }

    @Override
    public <A, B, C> Kleisli<M, A, C>
    then(π<KleisliArrow<M>, A, B> f, π<KleisliArrow<M>, B, C> g) {
        return narrow(ArrowChoice.super.then(f, g));
    }

    @Override
    public <B, C> Kleisli<M, B, C> arr(Function<? super B, ? extends C> f) {
        return kleisli(b -> nm.pure(f.apply(b)));
    }

    @Override
    public <B, C, D> Kleisli<M, Tuple<B,D>, Tuple<C,D>> first(π<KleisliArrow<M>, B, C> f) {
        return kleisli((b, d) -> nm.bind(run(f, b), (C c) -> nm.pure(Tuple.of(c, d))));
    }

    @Override
    public <B, C, D> Kleisli<M, Tuple<D,B>, Tuple<D,C>> second(π<KleisliArrow<M>, B, C> f) {
        return kleisli((d, b) -> nm.bind(run(f, b), (C c) -> nm.pure(Tuple.of(d, c))));
    }

    @Override
    public <B, C, B1, C1> Kleisli<M, Tuple<B, B1>, Tuple<C, C1>>
    split(π<KleisliArrow<M>, B, C> f, π<KleisliArrow<M>, B1, C1> g) {
        return narrow(ArrowChoice.super.split(f, g));
    }

    @Override
    public <B, C, C1> Kleisli<M, B, Tuple<C, C1>>
    fanout(π<KleisliArrow<M>, B, C> f, π<KleisliArrow<M>, B, C1> g) {
        return narrow(ArrowChoice.super.fanout(f, g));
    }

    @Override
    public <B> Kleisli<M, B, B> returnA() {
        return narrow(ArrowChoice.super.returnA());
    }

    @Override
    public <B, C, D> Kleisli<M, Either<B,D>, Either<C,D>> left(π<KleisliArrow<M>, B, C> f) {
        return merge(f, arr(Fn.id()));
    }

    @Override
    public <B, C, D> Kleisli<M, Either<D,B>, Either<D,C>> right(π<KleisliArrow<M>, B, C> f) {
        return merge(arr(Fn.id()), f);
    }

    @Override
    public <B, C, B1, C1> Kleisli<M, Either<B,B1>, Either<C,C1>>
    merge(π<KleisliArrow<M>, B, C> f, π<KleisliArrow<M>, B1, C1> g) {
        return fanin(then(f, arr((Function<C, Either<C,C1>>)Either::left)),
                     then(g, arr((Function<C1,Either<C,C1>>)Either::right)));
    }

    @Override
    public <B, C, D> Kleisli<M, Either<B, C>, D>
    fanin(π<KleisliArrow<M>, B, D> f, π<KleisliArrow<M>, C, D> g) {
        return kleisli(e -> e.<$<M,D>>either(narrow(f), narrow(g)));
    }

    @Override
    public <B, C> Kleisli<M, Tuple<π<KleisliArrow<M>, B, C>, B>, C> app() {
        return kleisli(t -> t.as(Kleisli::run));
    }
}
