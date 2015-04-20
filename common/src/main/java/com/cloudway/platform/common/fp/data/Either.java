/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.data;

import java.util.NoSuchElementException;
import java.util.function.BiFunction;
import java.util.function.Function;

import com.cloudway.platform.common.fp.function.ExceptionFunction;
import com.cloudway.platform.common.fp.control.ConditionCase;
import com.cloudway.platform.common.fp.$;
import com.cloudway.platform.common.fp.control.Applicative;
import com.cloudway.platform.common.fp.control.Monad;

/**
 * The {@code Either} class represents values with two possibilities: a value of
 * type {@code Either a b} is either {@code Left a} or {@code Right b}.
 *
 * <p>The {@code Either} type is sometimes used to represent a value which is
 * either correct or an error; by convention, the 'Left' constructor is used
 * to hold an error value and the 'Right' constructor is used to hold a correct
 * value (mnemonic: "right" also means "correct").</p>
 *
 * @param <A> the type of the left value
 * @param <B> the type of the right value
 */
public abstract class Either<A, B> implements $<Either.µ<A>, B>, Traversable<Either.µ<A>, B> {
    private static class Left<A, B> extends Either<A, B> {
        private final A a;

        Left(A a) {
            this.a = a;
        }

        @Override
        public boolean isLeft() {
            return true;
        }

        @Override
        public A left() {
            return a;
        }

        public String toString() {
            return "Left(" + a + ")";
        }
    }

    private static class Right<A, B> extends Either<A, B> {
        private final B b;

        Right(B b) {
            this.b = b;
        }

        @Override
        public boolean isRight() {
            return true;
        }

        @Override
        public B right() {
            return b;
        }

        public String toString() {
            return "Right(" + b + ")";
        }
    }

    /**
     * Construct a 'Left' value.
     */
    public static <A, B> Either<A, B> left(A left) {
        return new Left<>(left);
    }

    /**
     * Construct a 'Right' value.
     */
    public static <A, B> Either<A, B> right(B right) {
        return new Right<>(right);
    }

    /**
     * Deconstruct if the given value is 'Left'.
     */
    public static <A, R, X extends Throwable> ConditionCase<Either<A,?>, R, X>
    Left(ExceptionFunction<? super A, ? extends R, X> mapper) {
        return e -> e.isLeft()
                    ? () -> mapper.evaluate(e.left())
                    : null;
    }

    /**
     * Deconstruct if the given value is 'Right'.
     */
    public static <B, R, X extends Throwable> ConditionCase<Either<?,B>, R, X>
    Right(ExceptionFunction<? super B, ? extends R, X> mapper) {
        return e -> e.isRight()
                    ? () -> mapper.evaluate(e.right())
                    : null;
    }

    /**
     * Returns true if the given value is a 'Left'-value, false otherwise.
     */
    public boolean isLeft() {
        return false;
    }

    /**
     * Returns true if the given value is a 'Right'-value, false otherwise.
     */
    public boolean isRight() {
        return false;
    }

    /**
     * Returns the 'Left' value.
     */
    public A left() {
        throw new NoSuchElementException();
    }

    /**
     * Returns the 'Right' value.
     */
    public B right() {
        throw new NoSuchElementException();
    }

    /**
     * If the given value is 'Right', apply the provided mapping function to it,
     * otherwise return the 'Left' value.
     */
    @SuppressWarnings("unchecked")
    public <C> Either<A, C> map(Function<? super B, ? extends C> f) {
        return isLeft() ? (Either<A,C>)this
                        : right(f.apply(right()));
    }

    /**
     * If the given value is 'Right', apply the provided mapping function to it,
     * return that result, otherwise return the 'Left' value.  This method is
     * similar to {@link #map(Function)}, but the provided mapper is one whose
     * result is already an {@code Either}, and if invoked, {@code flatMap} does
     * not wrap it with an additional {@code Either}.
     */
    @SuppressWarnings("unchecked")
    public <C> Either<A, C> flatMap(Function<? super B, ? extends $<µ<A>, C>> f) {
        return isLeft() ? (Either<A,C>)this
                        : narrow(f.apply(right()));
    }

    /**
     * Map each element of a structure to an action, evaluate these actions from
     * left to right, and collect the results.
     */
    @Override
    @SuppressWarnings("unchecked")
    public <F, C> $<F, Either<A, C>> traverse(Applicative<F> m, Function<? super B, ? extends $<F,C>> f) {
        return isLeft() ? m.pure((Either<A,C>)this)
                        : m.map(f.apply(right()), Either::right);
    }

    /**
     * Case analysis for the Either type. If the value is Left a, apply the first
     * function to a; if it is Right b, apply the second function b.
     */
    public <C> C either(Function<? super A, ? extends C> af, Function<? super B, ? extends C> bf) {
        return isLeft() ? af.apply(left()) : bf.apply(right());
    }

    /**
     * Returns the 'Right' value, if present, otherwise throw an exception
     * to be created by the provided supplier.
     */
    public <X extends Throwable> B getOrThrow(Function<? super A, ? extends X> exceptionSupplier) throws X {
        if (isRight()) {
            return right();
        } else {
            throw exceptionSupplier.apply(left());
        }
    }

    // Type Classes

    public static final class µ<A> implements Monad<µ<A>> {
        @Override
        public <B> Either<A, B> pure(B b) {
            return right(b);
        }

        @Override
        public <B, C> Either<A, C> map($<µ<A>, B> a, Function<? super B, ? extends C> f) {
            return narrow(a).map(f);
        }

        @Override
        public <B, C> Either<A, C> bind($<µ<A>, B> a, Function<? super B, ? extends $<µ<A>, C>> k) {
            return narrow(a).flatMap(k);
        }
    }

    public static <A, B> Either<A, B> narrow($<µ<A>, B> value) {
        return (Either<A,B>)value;
    }

    private static final µ<?> _TCLASS = new µ<>();

    @SuppressWarnings("unchecked")
    public static <A> µ<A> tclass() {
        return (µ<A>)_TCLASS;
    }

    @Override
    public µ<A> getTypeClass() {
        return tclass();
    }

    // Convenient static monad methods

    public static <T, E, A> Either<E, ? extends Traversable<T, A>>
    flatM(Traversable<T, ? extends $<µ<E>, A>> ms) {
        return narrow(Either.<E>tclass().flatM(ms));
    }

    @SuppressWarnings("unchecked")
    public static <E, A> Either<E, Seq<A>> flatM(Seq<? extends $<µ<E>, A>> ms) {
        return (Either<E, Seq<A>>)Either.<E>tclass().flatM(ms);
    }

    public static <T, E, A, B> Either<E, ? extends Traversable<T, B>>
    mapM(Traversable<T, A> xs, Function<? super A, ? extends $<µ<E>, B>> f) {
        return narrow(Either.<E>tclass().mapM(xs, f));
    }

    @SuppressWarnings("unchecked")
    public static <E, A, B> Either<E, Seq<B>>
    mapM(Seq<A> xs, Function<? super A, ? extends $<µ<E>, B>> f) {
        return (Either<E, Seq<B>>)Either.<E>tclass().mapM(xs, f);
    }

    public static <E, A> Either<E, Unit> sequence(Foldable<? extends $<µ<E>, A>> ms) {
        return narrow(Either.<E>tclass().sequence(ms));
    }

    public static <E, A, B> Either<E, Unit>
    mapM_(Foldable<A> xs, Function<? super A, ? extends $<µ<E>, A>> f) {
        return narrow(Either.<E>tclass().mapM_(xs, f));
    }

    public static <E, A> Either<E, Seq<A>>
    filterM(Seq<A> xs, Function<? super A, ? extends $<µ<E>, Boolean>> p) {
        return narrow(Either.<E>tclass().filterM(xs, p));
    }

    public static <E, A, B> Either<E, B>
    foldM(B r0, Foldable<A> xs, BiFunction<B, ? super A, ? extends $<µ<E>, B>> f) {
        return narrow(Either.<E>tclass().foldM(r0, xs, f));
    }

    public static <E, A> Either<E, Seq<A>> replicateM(int n, $<µ<E>, A> a) {
        return narrow(Either.<E>tclass().replicateM(n, a));
    }

    public static <E, A> Either<E, Unit> replicateM_(int n, $<µ<E>, A> a) {
        return narrow(Either.<E>tclass().replicateM_(n, a));
    }
}
