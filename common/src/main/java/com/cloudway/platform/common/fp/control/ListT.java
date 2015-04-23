/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.control;

import java.util.function.BiFunction;
import java.util.function.Function;

import com.cloudway.platform.common.fp.$;
import com.cloudway.platform.common.fp.data.Seq;

/**
 * The ListT monad transformer, adding backtracking to a given monad, which
 * must be commutative.
 *
 * @param <T> the transformer monad typeclass
 * @param <M> the inner monad typeclass
 */
public abstract class ListT<T extends ListT<T, M>, M extends Monad<M>>
    implements MonadPlus<T>, MonadTrans<T, M>
{
    /**
     * The monadic datatype that encapsulate a monadic state.
     */
    public static abstract class Monadic<T, M, A> implements $<T, A> {
        final $<M, Seq<A>> value;

        /**
         * Construct a {@code ListT} monad.
         */
        protected Monadic($<M, Seq<A>> value) {
            this.value = value;
        }
    }

    private final M nm;

    /**
     * Construct a {@code ListT} transformer monad typeclass.
     */
    protected ListT(M nm) {
        this.nm = nm;
    }

    /**
     * Implemented by concrete class to instantiate a new {@code ListT} monad.
     */
    protected abstract <A> $<T, A> $($<M, Seq<A>> value);

    /**
     * Unwrap the {@code LisT} computation.
     */
    @SuppressWarnings("unchecked")
    public <A> $<M, Seq<A>> runList($<T, A> m) {
        return ((Monadic<T,M,A>)m).value;
    }

    /**
     * Construct a {@code ListT} monad from a list.
     */
    public <A> $<T, A> list(Seq<A> xs) {
        return $(nm.pure(xs));
    }

    /**
     * Construct a pure computation that results in the given value.
     */
    @Override
    public <A> $<T, A> pure(A a) {
        return $(nm.pure(Seq.of(a)));
    }

    /**
     * Fail with a message.
     */
    @Override
    public <A> $<T, A> fail(String msg) {
        return $(nm.pure(Seq.nil()));
    }

    /**
     * Promote an inner monad to a {@code ListT} transformer monad.
     */
    @Override
    public <A> $<T, A> lift($<M, A> m) {
        return $(nm.bind(m, a -> nm.pure(Seq.of(a))));
    }

    /**
     * Returns the inner monad typeclass.
     */
    @Override
    public M inner() {
        return nm;
    }

    /**
     * Map between {@code ListT} computations.
     */
    public <A, B> $<T, B> mapList($<T, A> m, Function<? super $<M, Seq<A>>, ? extends $<M, Seq<B>>> f) {
        return $(f.apply(runList(m)));
    }

    /**
     * Transform a {@code ListT} computation by feeding the value to the given
     * function and wrap the result to a new computation.
     */
    @Override
    public <A, B> $<T, B> map($<T, A> m, Function<? super A, ? extends B> f) {
        return $(nm.map(runList(m), v -> v.map(f)));
    }

    /**
     * Transform a {@code ListT} computation by feeding the value to the given
     * function and wrap the result to a new computation.
     */
    @Override
    public <A, B> $<T, B> bind($<T, A> m, Function<? super A, ? extends $<T, B>> k) {
        return $(nm.bind(runList(m), (Seq<A> a) ->
                 nm.bind(a.traverse(nm, x -> runList(k.apply(x))), (Seq<Seq<B>> b) ->
                 nm.pure(Seq.flatten(b)))));
    }

    /**
     * Sequential application.
     */
    @Override
    public <A, B> $<T, B> ap($<T, Function<? super A, ? extends B>> f, $<T, A> v) {
        BiFunction<Seq<Function<? super A, ? extends B>>, Seq<A>, Seq<B>> k =
            (fs, xs) -> fs.flatMap(xs::map);
        return $(nm.ap2(k, runList(f), runList(v)));
    }

    /**
     * Returns the monoid identity.
     */
    @Override
    public <A> $<T, A> mzero() {
        return $(nm.pure(Seq.nil()));
    }

    /**
     * Combines two computations.
     */
    @Override
    public <A> $<T, A> mplus($<T, A> m, $<T, A> n) {
        return $(nm.bind(runList(m), a -> nm.bind(runList(n), b -> nm.pure(a.append(b)))));
    }

    // Monad Stacking

    public static <M extends Monad<M>> On<M> on(M nm) {
        return new On<>(nm);
    }

    public static final class On<M extends Monad<M>> extends ListT<On<M>, M> {
        private On(M nm) {
            super(nm);
        }

        @Override
        protected <A> $<On<M>, A> $($<M, Seq<A>> value) {
            return new Monadic<On<M>, M, A>(value) {
                @Override public On<M> getTypeClass() {
                    return On.this;
                }
            };
        }
    }
}
