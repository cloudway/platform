/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.control.arrow;

import java.util.function.Function;

import com.cloudway.fp.data.Either;
import com.cloudway.fp.data.Tuple;
import com.cloudway.fp.π;

/**
 * The singleton function arrow typeclass definition.
 */
public enum FunctionArrow implements ArrowChoice<FunctionArrow>, ArrowApply<FunctionArrow> {
    tclass;

    /**
     * The morphism function interface.
     */
    @FunctionalInterface
    public interface Func<A, B> extends Function<A, B>, π<FunctionArrow, A, B> {
        @Override
        default FunctionArrow getTypeClass() {
            return tclass;
        }

        default <C> Func<A, C> then(π<FunctionArrow, B, C> next) {
            return getTypeClass().then(this, next);
        }
    }

    /**
     * Narrowing generic function interface.
     */
    @SuppressWarnings("unchecked")
    public static <A, B> Func<A, B> narrow(π<FunctionArrow, A, B> f) {
        return (Func<A,B>)f;
    }

    // helper method to convert a lambda expression to function interface
    private static <A, B> Func<A, B> $(Func<A, B> f) {
        return f;
    }

    @Override
    public <A> Func<A, A> id() {
        return $(a -> a);
    }

    @Override
    public <A, B, C> Func<A, C> compose(π<FunctionArrow, B, C> f, π<FunctionArrow, A, B> g) {
        return $(a -> narrow(f).apply(narrow(g).apply(a)));
    }

    @Override
    public <A, B, C> Func<A, C> then(π<FunctionArrow, A, B> f, π<FunctionArrow, B, C> g) {
        return narrow(ArrowChoice.super.then(f, g));
    }

    @Override
    public <B, C> Func<B, C> arr(Function<? super B, ? extends C> f) {
        return $(f::apply);
    }

    @Override
    public <B, C, D> Func<Tuple<B, D>, Tuple<C, D>> first(π<FunctionArrow, B, C> f) {
        return split(f, id());
    }

    @Override
    public <B, C, D> Func<Tuple<D, B>, Tuple<D, C>> second(π<FunctionArrow, B, C> f) {
        return split(id(), f);
    }

    @Override
    public <B, C, B1, C1> Func<Tuple<B, B1>, Tuple<C, C1>>
    split(π<FunctionArrow, B, C> f, π<FunctionArrow, B1, C1> g) {
        return $(t -> t.map(narrow(f), narrow(g)));
    }

    @Override
    public <B, C, C1> Func<B, Tuple<C, C1>>
    fanout(π<FunctionArrow, B, C> f, π<FunctionArrow, B, C1> g) {
        return narrow(ArrowChoice.super.fanout(f, g));
    }

    @Override
    public <B, C, D> Func<Either<B, D>, Either<C, D>> left(π<FunctionArrow, B, C> f) {
        return merge(f, id());
    }

    @Override
    public <B, C, D> Func<Either<D, B>, Either<D, C>> right(π<FunctionArrow, B, C> f) {
        return merge(id(), f);
    }

    @Override
    public <B, C, B1, C1> Func<Either<B, B1>, Either<C, C1>>
    merge(π<FunctionArrow, B, C> f, π<FunctionArrow, B1, C1> g) {
        return fanin($(b -> Either.left(narrow(f).apply(b))),
                     $(b1 -> Either.right(narrow(g).apply(b1))));
    }

    @Override
    public <B, C, D> Func<Either<B, C>, D>
    fanin(π<FunctionArrow, B, D> f, π<FunctionArrow, C, D> g) {
        return $(e -> e.either(narrow(f), narrow(g)));
    }

    @Override
    public <B, C> Func<Tuple<π<FunctionArrow, B, C>, B>, C> app() {
        return $(fx -> fx.as((f, x) -> narrow(f).apply(x)));
    }
}
