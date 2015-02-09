/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util;

import java.io.Serializable;
import java.util.Comparator;
import java.util.Objects;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;

import com.cloudway.platform.common.util.function.ExceptionBiFunction;
import com.cloudway.platform.common.util.function.ExceptionSupplier;

/**
 * A tuple with two elements.
 */
public class Tuple<T, U> implements Serializable, Cloneable
{
    private static final long serialVersionUID = 6021746465072972306L;

    private final T left;
    private final U right;

    /**
     * Construct a new Tuple with two arguments.
     *
     * @param left the first argument
     * @param right the second argument
     */
    public Tuple(T left, U right) {
        this.left = left;
        this.right = right;
    }

    /**
     * Construct a new Tuple with two arguments.
     *
     * @param left the first argument
     * @param right the second argument
     */
    public static <T, U> Tuple<T, U> of(T left, U right) {
        return new Tuple<>(left, right);
    }

    /**
     * Returns the first element.
     */
    public T left() {
        return left;
    }

    /**
     * Returns the second element.
     */
    public U right() {
        return right;
    }

    /**
     * Get a tuple with two elements swapped.
     */
    public Tuple<U, T> swap() {
        return new Tuple<>(right, left);
    }

    /**
     * Apply this tuple as arguments to a function.
     */
    public <R> R map(BiFunction<? super T, ? super U, ? extends R> fn) {
        return fn.apply(left, right);
    }

    /**
     * Apply this tuple as arguments to a curried function.
     */
    public <R> R map(Function<? super T, Function<? super U, ? extends R>> fn) {
        return fn.apply(left).apply(right);
    }

    /**
     * Apply first element as argument to a function and return a new tuple
     * with the substituted argument.
     */
    public <R> Tuple<R, U> mapLeft(Function<? super T, ? extends R> fn) {
        return of(fn.apply(left), right);
    }

    /**
     * Apply second element as argument to a function and return a new tuple
     * with the substituted argument.
     */
    public <R> Tuple<T, R> mapRight(Function<? super U, ? extends R> fn) {
        return of(left, fn.apply(right));
    }

    /**
     * Returns a predicate that evaluate first element as argument to the
     * given predicate.
     */
    public static <T, U> Predicate<Tuple<T,U>> left(Predicate<? super T> p) {
        return t -> p.test(t.left());
    }

    /**
     * Returns a predicate that evaluate second element as argument to the
     * given predicate.
     */
    public static <T, U> Predicate<Tuple<T,U>> right(Predicate<? super U> p) {
        return t -> p.test(t.right());
    }

    public boolean equals(Object obj) {
        if (obj == this)
            return true;
        if (!(obj instanceof Tuple))
            return false;

        @SuppressWarnings("rawtypes") Tuple other = (Tuple)obj;
        return Objects.equals(left, other.left)
            && Objects.equals(right, other.right);
    }

    public int hashCode() {
        return 31 * (31 + Objects.hashCode(left)) + Objects.hashCode(right);
    }

    public String toString() {
        return "Tuple(" + left + ", " + right + ")";
    }

    public static <T extends Comparable<T>, U extends Comparable<U>>
    Comparator<Tuple<T,U>> comparator() {
        return Comparator.<Tuple<T,U>,T>comparing(Tuple::left).thenComparing(Tuple::right);
    }

    public static <T> Comparator<Tuple<T,T>>
    comparator(Comparator<? super T> comparator) {
        return Comparator.<Tuple<T,T>,T>comparing(Tuple::left, comparator)
            .thenComparing(Tuple::right, comparator);
    }

    public static <T,U> Comparator<Tuple<T,U>>
    comparator(Comparator<? super T> leftComparator, Comparator<? super U> rightComparator) {
        return Comparator.<Tuple<T,U>,T>comparing(Tuple::left, leftComparator)
            .thenComparing(Tuple::right, rightComparator);
    }

    @Override
    @SuppressWarnings({"rawtypes", "unchecked", "CloneDoesntDeclareCloneNotSupportedException"})
    public Tuple<T, U> clone() {
        try {
            return (Tuple)super.clone();
        } catch (CloneNotSupportedException ex) {
            throw new InternalError();
        }
    }

    // Pattern Matching Helper Methods --------------------------------------

    /**
     * Returns a conditional case to deconstruct elements of a tuple.
     *
     * <p>Usage:</p>
     * <pre>
     *     when(Tuple((m, v) -> m*v*v/2))
     * </pre>
     */
    @SuppressWarnings("MethodNameSameAsClassName")
    public static <T, U, R, X extends Throwable> ConditionCase<Tuple<T,U>, R, X>
    Tuple(ExceptionBiFunction<? super T, ? super U, ? extends R, X> mapper) {
        return t -> () -> mapper.evaluate(t.left(), t.right());
    }

    /**
     * Returns a conditional case to deconstruct elements from a tuple and
     * evaluate the given mapper function when both of elements satisfy the
     * given predicates.
     *
     * <p>Usage:</p>
     * <pre>
     *     when(Tuple(massive(), highSpeed(), (m,v) -> new BlackHole(m,v))
     * </pre>
     */
    @SuppressWarnings("MethodNameSameAsClassName")
    public static <T, U, R, X extends Throwable> ConditionCase<Tuple<T,U>, R, X>
    Tuple(Predicate<? super T> p1, Predicate<? super U> p2, ExceptionBiFunction<? super T, ? super U, ? extends R, X> mapper) {
        return t -> p1.test(t.left()) && p2.test(t.right())
            ? () -> mapper.evaluate(t.left(), t.right())
            : null;
    }

    /**
     * Returns a conditional case to deconstruct elements from a tuple and
     * get result from the given supplier when both of elements satisfy the
     * given predicates.
     *
     * <p>Usage:</p>
     * <pre>
     *     when(Tuple(massive(), highSpeed(), () -> new BlackHole())
     * </pre>
     */
    @SuppressWarnings("MethodNameSameAsClassName")
    public static <T, U, R, X extends Throwable> ConditionCase<Tuple<T,U>, R, X>
    Tuple(Predicate<? super T> p1, Predicate<? super U> p2, ExceptionSupplier<R, X> supplier) {
        return t -> p1.test(t.left()) && p2.test(t.right()) ? supplier : null;
    }

    /**
     * Returns a conditional case to deconstruct elements from a tuple and
     * evaluate the given mapper function when elements are instance of
     * given types.  Elements are explicitly casted to the given type when
     * evaluating the mapper function.
     */
    @SuppressWarnings("MethodNameSameAsClassName")
    public static <T, U, T1, U1, R, X extends Throwable> ConditionCase<Tuple<T,U>, R, X>
    Tuple(Class<T1> c1, Class<U1> c2, ExceptionBiFunction<? super T1, ? super U1, ? extends R, X> mapper) {
        return t -> c1.isInstance(t.left()) && c2.isInstance(t.right())
            ? () -> mapper.evaluate(c1.cast(t.left()), c2.cast(t.right()))
            : null;
    }
}
