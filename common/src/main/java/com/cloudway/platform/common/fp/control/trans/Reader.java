/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.control.trans;

import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;

import com.cloudway.platform.common.fp.$;
import com.cloudway.platform.common.fp.data.Foldable;
import com.cloudway.platform.common.fp.data.Identity;
import com.cloudway.platform.common.fp.data.Seq;
import com.cloudway.platform.common.fp.data.Traversable;
import com.cloudway.platform.common.fp.data.Unit;

/**
 * The {@code Reader} monad (also called the Environment monad). Represents a
 * computation, which can read values from a shared environment, pass values
 * from function to function, and execute sub-computations in a modified
 * environment. Using {@code Reader} monad for such computations is often
 * clearer and easier than using the {@code State} monad.
 * 
 * @param <R> the environment type
 * @param <A> the computation type
 */
public final class Reader<R, A> extends ReaderTC.Monadic<Reader.µ<R>, R, Identity.µ, A> {
    private Reader(Function<? super R, ? extends $<Identity.µ, A>> f) {
        super(f);
    }

    /**
     * Construct a pure computation that results in the given value.
     */
    public static <R, A> Reader<R, A> pure(A a) {
        return narrow(Reader.<R>tclass().pure(a));
    }

    /**
     * Returns a do nothing computation.
     */
    public static <R> Reader<R, Unit> unit() {
        return pure(Unit.U);
    }

    /**
     * Construct a pure computation that results in the given lazy evaluation
     * thunk.
     */
    public static <R, A> Reader<R, A> lazy(Supplier<A> a) {
        return narrow(Reader.<R>tclass().lazy(a));
    }
    
    /**
     * Construct a computation that performs the given action with no result.
     */
    public Reader<R, Unit> action(Runnable a) {
        return narrow(Reader.<R>tclass().action(a));
    }

    /**
     * Runs a reader and extracts the final value from it.
     */
    public A runReader(R r) {
        return Identity.run(Reader.<R>tclass().runReader(this, r));
    }

    /**
     * Transform the computation inside a ReaderT.
     */
    public <B> Reader<R, B> mapReader(Function<? super A, ? extends B> f) {
        return narrow(Reader.<R>tclass().mapReader(this, Identity.mapIdentity(f)));
    }

    /**
     * Fetch the value of the environment.
     */
    public static <R> Reader<R, R> ask() {
        return narrow(Reader.<R>tclass().ask());
    }

    /**
     * Execute a computation in a modified environment.
     */
    public static <R, A> Reader<R, A> local(Function<R, R> f, $<µ<R>, A> m) {
        return narrow(Reader.<R>tclass().local(f, m));
    }

    /**
     * Retrieve a function of the current environment.
     */
    public static <R, A> Reader<R, A> asks(Function<? super R, ? extends A> f) {
        return narrow(Reader.<R>tclass().asks(f));
    }

    /**
     * Transfer a computation by feeding the value to the given function
     * and wrapping the result to new monadic.
     */
    public <B> Reader<R, B> map(Function<? super A, ? extends B> f) {
        return narrow(Reader.<R>tclass().map(this, f));
    }

    /**
     * Transfer a computation by feeding the value to the given function
     * and wrapping the result to new monadic.
     */
    public <B> Reader<R, B> bind(Function<? super A, ? extends $<µ<R>, B>> f) {
        return narrow(Reader.<R>tclass().bind(this, f));
    }

    /**
     * Transfer a state computation by discarding the intermediate value.
     */
    public <B> Reader<R, B> then(Supplier<? extends $<µ<R>, B>> next) {
        return narrow(Reader.<R>tclass().seqR(this, next));
    }

    /**
     * Transfer a state computation by discarding the intermediate value.
     */
    public <B> Reader<R, B> then($<µ<R>, B> next) {
        return narrow(Reader.<R>tclass().seqR(this, next));
    }

    // Type Class

    public static final class µ<R> extends ReaderTC<µ<R>, R, Identity.µ> {
        private µ() {
            super(Identity.tclass);
        }

        @Override
        protected <A> Reader<R, A> $(Function<? super R, ? extends $<Identity.µ, A>> f) {
            return new Reader<>(f);
        }
    }

    private static final µ<?> _TCLASS = new µ<>();

    @SuppressWarnings("unchecked")
    public static <R> µ<R> tclass() {
        return (µ<R>)_TCLASS;
    }

    @Override
    public µ<R> getTypeClass() {
        return tclass();
    }

    public static <R, A> Reader<R, A> narrow($<µ<R>, A> value) {
        return (Reader<R,A>)value;
    }

    public static <R, A> A run($<µ<R>, A> m, R r) {
        return narrow(m).runReader(r);
    }

    // Convenient static monad methods

    public static <T, R, A> Reader<R, ? extends Traversable<T, A>>
    flatM(Traversable<T, ? extends $<µ<R>, A>> ms) {
        return narrow(Reader.<R>tclass().flatM(ms));
    }

    @SuppressWarnings("unchecked")
    public static <R, A> Reader<R, Seq<A>>
    flatM(Seq<? extends $<µ<R>, A>> ms) {
        return (Reader<R, Seq<A>>)Reader.<R>tclass().flatM(ms);
    }

    public static <T, R, A, B> Reader<R, ? extends Traversable<T, B>>
    mapM(Traversable<T, A> xs, Function<? super A, ? extends $<µ<R>, B>> f) {
        return narrow(Reader.<R>tclass().mapM(xs, f));
    }

    @SuppressWarnings("unchecked")
    public static <R, A, B> Reader<R, Seq<B>>
    mapM(Seq<A> xs, Function<? super A, ? extends $<µ<R>, B>> f) {
        return (Reader<R, Seq<B>>)Reader.<R>tclass().mapM(xs, f);
    }

    public static <R, A> Reader<R, Unit>
    sequence(Foldable<? extends $<µ<R>, A>> ms) {
        return narrow(Reader.<R>tclass().sequence(ms));
    }

    public static <R, A, B> Reader<R, Unit>
    mapM_(Foldable<A> xs, Function<? super A, ? extends $<µ<R>, B>> f) {
        return narrow(Reader.<R>tclass().mapM_(xs, f));
    }

    public static <R, A> Reader<R, Seq<A>>
    filterM(Seq<A> xs, Function<? super A, ? extends $<µ<R>, Boolean>> p) {
        return narrow(Reader.<R>tclass().filterM(xs, p));
    }

    public static <R, A, B> Reader<R, B>
    foldM(B r0, Foldable<A> xs, BiFunction<B, ? super A, ? extends $<µ<R>, B>> f) {
        return narrow(Reader.<R>tclass().foldM(r0, xs, f));
    }

    public static <R, A> Reader<R, Seq<A>> replicateM(int n, $<µ<R>, A> a) {
        return narrow(Reader.<R>tclass().replicateM(n, a));
    }

    public static <R, A> Reader<R, Unit> replicateM_(int n, $<µ<R>, A> a) {
        return narrow(Reader.<R>tclass().replicateM_(n, a));
    }
}
