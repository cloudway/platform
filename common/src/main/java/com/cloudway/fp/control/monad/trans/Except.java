/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.control.monad.trans;

import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;

import com.cloudway.fp.$;
import com.cloudway.fp.data.Either;
import com.cloudway.fp.data.Foldable;
import com.cloudway.fp.data.Identity;
import com.cloudway.fp.data.Seq;
import com.cloudway.fp.data.Traversable;
import com.cloudway.fp.data.Unit;

/**
 * The parameterizable exception monad.  Computations are either exceptions
 * or normal values.
 *
 * @param <E> the exception type
 * @param <A> the computation value type
 */
public final class Except<E, A> extends ExceptTC.Monadic<Except.µ<E>, E, Identity.µ, A> {
    private Except($<Identity.µ, Either<E, A>> value) {
        super(value);
    }

    /**
     * Construct a pure computation that results in the given value.
     *
     * @param a the final result
     * @return the monad that hold the final result
     */
    public static <E, A> Except<E, A> pure(A a) {
        return narrow(Except.<E>tclass().pure(a));
    }

    /**
     * Returns a do nothing computation.
     */
    public static <E> Except<E, Unit> unit() {
        return pure(Unit.U);
    }

    /**
     * Construct a pure computation that results in the given lazy evaluation
     * thunk.
     */
    public static <E, A> Except<E, A> lazy(Supplier<A> a) {
        return narrow(Except.<E>tclass().lazy(a));
    }

    /**
     * Construct a computation that performs the given action with no result.
     */
    public static <E> Except<E, Unit> action(Runnable a) {
        return narrow(Except.<E>tclass().action(a));
    }

    /**
     * Evaluate the exception computation and returns either the result or
     * an exception.
     */
    public Either<E, A> runExcept() {
        return Identity.run(Except.<E>tclass().runExcept(this));
    }

    /**
     * Maps the unwrapped computation using the given function.
     */
    public <B> Except<E, B> mapExcept(Function<Either<E, A>, Either<E, B>> f) {
        return narrow(Except.<E>tclass().mapExcept(this, Identity.mapIdentity(f)));
    }

    /**
     * Transfer any exception thrown by the computation using the given
     * function.
     */
    public Except<E, A> withExcept(Function<E, E> f) {
        return narrow(Except.<E>tclass().withExcept(this, f));
    }

    /**
     * Signal an exception value.
     */
    public static <E, A> Except<E, A> throwE(E e) {
        return narrow(Except.<E>tclass().throwE(e));
    }

    /**
     * Handle an exception.
     */
    public static <E, A> Except<E, A> catchE(Function<? super E, ? extends $<µ<E>, A>> h, $<µ<E>, A> m) {
        return narrow(Except.<E>tclass().catchE(h, m));
    }

    /**
     * Transfer an exception computation by feeding the value to the given
     * function.
     */
    public <B> Except<E, B> map(Function<? super A, ? extends B> f) {
        return narrow(Except.<E>tclass().map(this, f));
    }

    /**
     * Transfer an exception computation by feeding the value to the given
     * function.
     */
    public <B> Except<E, B> bind(Function<? super A, ? extends $<µ<E>, B>> f) {
        return narrow(Except.<E>tclass().bind(this, f));
    }

    /**
     * Transfer an exception computation by discarding the intermediate value.
     */
    public <B> Except<E, B> then(Supplier<? extends $<µ<E>, B>> next) {
        return narrow(Except.<E>tclass().seqR(this, next));
    }
    
    /**
     * Transfer an exception computation by discarding the intermediate value.
     */
    public <B> Except<E, B> then($<µ<E>, B> next) {
        return narrow(Except.<E>tclass().seqR(this, next));
    }
    
    // Type Class
    
    public static final class µ<E> extends ExceptTC<µ<E>, E, Identity.µ> {
        public µ() {
            super(Identity.tclass);
        }

        @Override
        protected <A> $<µ<E>, A> $($<Identity.µ, Either<E, A>> value) {
            return new Except<>(value);
        }
    }
    
    private static final µ<?> _TCLASS = new µ<>();
    
    @SuppressWarnings("unchecked")
    public static <E> µ<E> tclass() {
        return (µ<E>)_TCLASS;
    }

    @Override
    public µ<E> getTypeClass() {
        return tclass();
    }
    
    @SuppressWarnings("unchecked")
    public static <E, A> Except<E, A> narrow($<µ<E>, A> value) {
        return (Except<E,A>)value;
    }
    
    public static <E, A> Either<E, A> runExcept($<µ<E>, A> m) {
        return narrow(m).runExcept();
    }

    // Convenient static monad methods

    public static <T, E, A> Except<E, ? extends Traversable<T, A>>
    flatM(Traversable<T, ? extends $<µ<E>, A>> ms) {
        return narrow(Except.<E>tclass().flatM(ms));
    }

    @SuppressWarnings("unchecked")
    public static <E, A> Except<E, Seq<A>>
    flatM(Seq<? extends $<µ<E>, A>> ms) {
        return (Except<E, Seq<A>>)Except.<E>tclass().flatM(ms);
    }

    public static <T, E, A, B> Except<E, ? extends Traversable<T, B>>
    mapM(Traversable<T, A> xs, Function<? super A, ? extends $<µ<E>, B>> f) {
        return narrow(Except.<E>tclass().mapM(xs, f));
    }

    @SuppressWarnings("unchecked")
    public static <E, A, B> Except<E, Seq<B>>
    mapM(Seq<A> xs, Function<? super A, ? extends $<µ<E>, B>> f) {
        return (Except<E, Seq<B>>)Except.<E>tclass().mapM(xs, f);
    }

    public static <E, A> Except<E, Unit>
    sequence(Foldable<? extends $<µ<E>, A>> ms) {
        return narrow(Except.<E>tclass().sequence(ms));
    }

    public static <E, A, B> Except<E, Unit>
    mapM_(Foldable<A> xs, Function<? super A, ? extends $<µ<E>, B>> f) {
        return narrow(Except.<E>tclass().mapM_(xs, f));
    }

    public static <E, A> Except<E, Seq<A>>
    filterM(Seq<A> xs, Function<? super A, ? extends $<µ<E>, Boolean>> p) {
        return narrow(Except.<E>tclass().filterM(xs, p));
    }

    public static <E, A, B> Except<E, B>
    foldM(B r0, Foldable<A> xs, BiFunction<B, ? super A, ? extends $<µ<E>, B>> f) {
        return narrow(Except.<E>tclass().foldM(r0, xs, f));
    }

    public static <E, A> Except<E, Seq<A>> replicateM(int n, $<µ<E>, A> a) {
        return narrow(Except.<E>tclass().replicateM(n, a));
    }

    public static <E, A> Except<E, Unit> replicateM_(int n, $<µ<E>, A> a) {
        return narrow(Except.<E>tclass().replicateM_(n, a));
    }
}
