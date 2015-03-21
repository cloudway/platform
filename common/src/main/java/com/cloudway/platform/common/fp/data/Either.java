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
import com.cloudway.platform.common.fp.function.TriFunction;

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
public abstract class Either<A, B> {
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
        if (isLeft()) {
            return (Either<A,C>)this;
        } else {
            return right(f.apply(right()));
        }
    }

    /**
     * If the given value is 'Right', apply the provided mapping function to it,
     * return that result, otherwise return the 'Left' value.  This method is
     * similar to {@link #map(Function)}, but the provided mapper is one whose
     * result is already an {@code Either}, and if invoked, {@code flatMap} does
     * not wrap it with an additional {@code Either}.
     */
    @SuppressWarnings("unchecked")
    public <C> Either<A, C> flatMap(Function<? super B, ? extends Either<A, C>> f) {
        if (isLeft()) {
            return (Either<A,C>)this;
        } else {
            return f.apply(right());
        }
    }

    /**
     * Case analysis for the Either type. If the value is Left a, apply the first
     * function to a; if it is Right b, apply the second function b.
     */
    public <C> C either(Function<? super A, ? extends C> fa, Function<? super B, ? extends C> fb) {
        return isLeft() ? fa.apply(left()) : fb.apply(right());
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

    /**
     * Evaluate each action in the sequence from left to right, and collect
     * the result.
     */
    public static <A, B> Either<A, Seq<B>> flatM(Seq<Either<A, B>> ms) {
        return ms.foldRight_(right(Seq.nil()), liftM2(Seq::cons));
    }

    /**
     * The {@code mapM} is analogous to {@link Seq#map(Function) map} except that
     * its result is encapsulated in an {@code Either}.
     */
    public static <A, B, T> Either<A, Seq<B>> mapM(Seq<T> xs, Function<? super T, ? extends Either<A,B>> f) {
        return flatM(xs.map(f));
    }

    /**
     * Generalizes {@link Seq#zip(Seq,BiFunction)} to arbitrary monads.
     * Bind the given function to the given computations with a final join.
     */
    public static <T, A, B, C> Either<T, Seq<C>>
    zipM(Seq<A> xs, Seq<B> ys, BiFunction<? super A, ? super B, Either<T, C>> f) {
        return flatM(Seq.zip(xs, ys, f));
    }

    /**
     * This generalizes the list-based filter function.
     */
    public static <A, B> Either<A, Seq<B>> filterM(Seq<B> xs, Function<? super B, Either<A, Boolean>> p) {
        return xs.isEmpty()
            ? right(Seq.nil())
            : p.apply(xs.head()).flatMap(flg ->
              filterM(xs.tail(), p).flatMap(ys ->
              right(flg ? Seq.cons(xs.head(), ys) : ys)));
    }

    /**
     * The {@code foldM} is analogous to {@link Seq#foldLeft(Object,BiFunction) foldLeft},
     * except that its result is encapsulated in an {@code Either}. Note that
     * {@code foldM} works from left-to-right over the lists arguments. If right-to-left
     * evaluation is required, the input list should be reversed.
     */
    public static <A, T, R> Either<A, R> foldM(R r0, Seq<T> xs, BiFunction<R, ? super T, Either<A, R>> f) {
        return xs.foldLeft(right(r0), (m, x) -> m.flatMap(r -> f.apply(r, x)));
    }

    /**
     * Kleisli composition of monads.
     */
    public static <A, B, C, X> Function<A, Either<X, C>>
    kleisli(Function<A, Either<X, B>> f, Function<B, Either<X, C>> g) {
        return x -> f.apply(x).flatMap(g);
    }

    /**
     * Promote a function to an either function.
     */
    public static <A, T, R> Function<Either<A, T>, Either<A, R>>
    liftM(Function<? super T, ? extends R> f) {
        return m -> m.map(f);
    }

    /**
     * Promote a function to an either function.
     */
    public static <A, T, U, R> BiFunction<Either<A, T>, Either<A, U>, Either<A, R>>
    liftM2(BiFunction<? super T, ? super U, ? extends R> f) {
        return (m1, m2) -> m1.flatMap(x1 -> m2.map(x2 -> f.apply(x1, x2)));
    }

    /**
     * Promote a function to an either function.
     */
    public static <A, T, U, V, R> TriFunction<Either<A, T>, Either<A, U>, Either<A, V>, Either<A, R>>
    liftM3(TriFunction<? super T, ? super U, ? super V, ? extends R> f) {
        return (m1, m2, m3) -> m1.flatMap(x1 -> m2.flatMap(x2 -> m3.map(x3 -> f.apply(x1, x2, x3))));
    }
}
