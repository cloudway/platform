/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.data;

import java.io.Serializable;
import java.util.Comparator;
import java.util.Objects;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;

import com.cloudway.platform.common.fp.function.ExceptionBiFunction;
import com.cloudway.platform.common.fp.control.ConditionCase;
import com.cloudway.platform.common.fp.function.ExceptionTriFunction;

/**
 * A tuple with two elements.
 */
public class Tuple<T, U> implements Serializable, Cloneable
{
    private static final long serialVersionUID = 6021746465072972306L;

    private final T first;
    private final U second;

    /**
     * Construct a new Tuple with two arguments.
     *
     * @param first the first argument
     * @param second the second argument
     */
    public Tuple(T first, U second) {
        this.first = first;
        this.second = second;
    }

    /**
     * Construct a new Tuple with two arguments.
     *
     * @param first the first argument
     * @param second the second argument
     */
    public static <T, U> Tuple<T, U> of(T first, U second) {
        return new Tuple<>(first, second);
    }

    /**
     * Construct a new Tuple with two elements of same type.
     *
     * @param first the first argument
     * @param second the second argument
     */
    public static <T> Pair<T> pair(T first, T second) {
        return new Pair<>(first, second);
    }

    /**
     * Construct a new Tuple with three elements.
     *
     * @param a the first argument
     * @param b the second argument
     * @param c the third argument
     */
    public static <A, B, C> Triple<A, B, C> of(A a, B b, C c) {
        return new Triple<>(a, b, c);
    }

    /**
     * Returns the first element.
     */
    public T first() {
        return first;
    }

    /**
     * Returns the second element.
     */
    public U second() {
        return second;
    }

    /**
     * Get a tuple with two elements swapped.
     */
    public Tuple<U, T> swap() {
        return new Tuple<>(second, first);
    }

    /**
     * Apply this tuple as arguments to a function.
     */
    public <R> R as(BiFunction<? super T, ? super U, ? extends R> fn) {
        return fn.apply(first, second);
    }

    /**
     * Applies two elements as arguments to two given functions and return
     * a new tuple with the substituted arguments.
     */
    public <X, Y> Tuple<X, Y> map(Function<? super T, ? extends X> f,
                                  Function<? super U, ? extends Y> g) {
        return of(f.apply(first), g.apply(second));
    }

    /**
     * Apply first element as argument to a function and return a new tuple
     * with the substituted argument.
     */
    public <R> Tuple<R, U> mapFirst(Function<? super T, ? extends R> fn) {
        return of(fn.apply(first), second);
    }

    /**
     * Apply second element as argument to a function and return a new tuple
     * with the substituted argument.
     */
    public <R> Tuple<T, R> mapSecond(Function<? super U, ? extends R> fn) {
        return of(first, fn.apply(second));
    }

    /**
     * Returns a predicate that evaluate first element as argument to the
     * given predicate.
     */
    public static <T, U> Predicate<Tuple<T, U>> first(Predicate<? super T> p) {
        return t -> p.test(t.first());
    }

    /**
     * Returns a predicate that evaluate second element as argument to the
     * given predicate.
     */
    public static <T, U> Predicate<Tuple<T, U>> second(Predicate<? super U> p) {
        return t -> p.test(t.second());
    }

    /**
     * Return the monoid for a tuple.
     */
    public static <T, U> Monoid<Tuple<T, U>> monoid(Monoid<T> mt, Monoid<U> mu) {
        return Monoid.monoid_(of(mt.empty(), mu.empty()), (a1, a2) ->
            of(mt.append(a1.first(), a2.first()), mu.append(a1.second(), a2.second())));
    }

    public boolean equals(Object obj) {
        if (obj == this)
            return true;
        if (!(obj instanceof Tuple))
            return false;

        @SuppressWarnings("rawtypes") Tuple other = (Tuple)obj;
        return Objects.equals(first, other.first)
            && Objects.equals(second, other.second);
    }

    public int hashCode() {
        return 31 * (31 + Objects.hashCode(first)) + Objects.hashCode(second);
    }

    public String toString() {
        return "(" + first + "," + second + ")";
    }

    public static <T extends Comparable<T>, U extends Comparable<U>>
    Comparator<Tuple<T, U>> comparator() {
        return Comparator.<Tuple<T, U>, T>comparing(Tuple::first).thenComparing(Tuple::second);
    }

    public static <T> Comparator<Tuple<T, T>>
    comparator(Comparator<? super T> comparator) {
        return Comparator.<Tuple<T, T>, T>comparing(Tuple::first, comparator)
            .thenComparing(Tuple::second, comparator);
    }

    public static <T, U> Comparator<Tuple<T, U>>
    comparator(Comparator<? super T> firstComparator, Comparator<? super U> secondComparator) {
        return Comparator.<Tuple<T, U>, T>comparing(Tuple::first, firstComparator)
            .thenComparing(Tuple::second, secondComparator);
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
     * <pre>{@code
     *     when(Tuple((m, v) -> m*v*v/2))
     * }</pre>
     */
    public static <T, U, R, X extends Throwable> ConditionCase<Tuple<T, U>, R, X>
    Tuple_(ExceptionBiFunction<? super T, ? super U, ? extends R, X> mapper) {
        return t -> () -> mapper.evaluate(t.first(), t.second());
    }

    /**
     * Returns a conditional case to deconstruct elements of a triple.
     */
    public static <A, B, C, R, X extends Throwable> ConditionCase<Triple<A, B, C>, R, X>
    Triple_(ExceptionTriFunction<A, B, C, R, X> mapper) {
        return t -> () -> mapper.evaluate(t._1(), t._2(), t._3());
    }
}
