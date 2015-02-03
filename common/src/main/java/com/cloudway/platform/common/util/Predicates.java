/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util;

import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import java.util.Objects;
import java.util.function.BiFunction;
import java.util.function.BiPredicate;
import java.util.function.Function;
import java.util.function.Predicate;
import static java.util.Objects.requireNonNull;

/**
 * This utility class contains various {@link Predicate} factories.
 */
public final class Predicates
{
    private Predicates() {}

    /**
     * A sugar that surround a predicate to make code readable.
     *
     * @param p a predicate
     * @return the predicate
     */
    public static <T> Predicate<T> is(Predicate<T> p) {
        return p;
    }

    /**
     * Returns a predicate that evaluate to {@code true} if the object being
     * tested {@code equals()} the given value or both are null.
     */
    public static <T> Predicate<T> is(Object value) {
        return equalTo(value);
    }

    /**
     * Returns a predicate that evaluate to {@code true} if the object being
     * tested is identical to the given enum value.
     */
    public static <T extends Enum<T>> Predicate<T> is(T value) {
        return sameAs(value);
    }

    /**
     * Returns a predicate that evaluate to {@code true} if the object being
     * tested is an instance of the given class.
     */
    public static <T> Predicate<T> is(Class<?> type) {
        return instanceOf(type);
    }

    /**
     * Returns the composition of a function and a predicate. For every {@code x}
     * the generated predicate returns {@code predicate(function(x))}.
     *
     * @param fn the function being composed
     * @param p the predicate that evaluate on result of the function
     * @return the composition of the provided function and predicate
     */
    public static <T, R> Predicate<T>
    having(Function<? super T, ? extends R> fn, Predicate<? super R> p) {
        requireNonNull(fn);
        requireNonNull(p);
        return t -> p.test(fn.apply(t));
    }

    /**
     * Returns the composition of a function and a predicate. For given {@code y}
     * and every {@code x} the generated predicate returns {@code predicate(function(x, y))}.
     *
     * @param fn the function that takes two arguments
     * @param u the value to the second argument of the function
     * @param p a predicate that evaluate on result of the function
     * @return the composition of the provided function and predicate
     */
    public static <T, U, R> Predicate<T>
    having(BiFunction<? super T, ? super U, ? extends R> fn, U u, Predicate<? super R> p) {
        requireNonNull(fn);
        requireNonNull(p);
        return t -> p.test(fn.apply(t, u));
    }

    /**
     * Returns the composition of a function and a predicate. For given {@code x}
     * and every {@code y} the generated predicate returns {@code predicate(function(x, y))}.
     *
     * @param fn the function that takes two arguments
     * @param t the value to the first argument of the function
     * @param p a predicate that evaluate on result of the function
     * @return the composition of the provided function and predicate
     */
    public static <T, U, R> Predicate<U>
    having(T t, BiFunction<? super T, ? super U, ? extends R> fn, Predicate<? super R> p) {
        requireNonNull(fn);
        requireNonNull(p);
        return u -> p.test(fn.apply(t, u));
    }

    /**
     * A sugar that surround a predicate to make code readable.
     *
     * @param p a predicate
     * @return the predicate
     */
    public static <T> Predicate<T> on(Predicate<T> p) {
        return p;
    }

    /**
     * Returns a curried predicate that applying the given value as the second argument
     * to the predicate.
     *
     * @param p a predicate with two arguments
     * @param u the value applied to the second argument of the predicate
     * @return the curried predicate
     */
    public static <T, U> Predicate<T> on(BiPredicate<? super T,? super U> p, U u) {
        requireNonNull(p);
        return t -> p.test(t, u);
    }

    /**
     * Returns a curried predicate that applying the given value as the first argument
     * to the predicate.
     *
     * @param p a predicate with two arguments
     * @param t the value applied to the first argument of the predicate
     * @return the curried predicate
     */
    public static <T, U> Predicate<U> on(T t, BiPredicate<? super T,? super U> p) {
        requireNonNull(p);
        return u -> p.test(t, u);
    }

    /**
     * Returns a predicate that evaluate to {@code true} if the given predicate
     * evaluate to {@code false}.
     */
    public static <T> Predicate<T> not(Predicate<? super T> p) {
        requireNonNull(p);
        return t -> !p.test(t);
    }

    /**
     * Returns a curried predicate that applying the given value as the second argument
     * to the negative evaluation of the given predicate.
     */
    public static <T, U> Predicate<T> not(BiPredicate<? super T, ? super U> p, U u) {
        return not(on(p, u));
    }

    /**
     * Returns a curried predicate that applying the given value as the first argument
     * to the negative evaluation of the given predicate.
     */
    public static <T, U> Predicate<U> not(T t, BiPredicate<? super T, ? super U> p) {
        return not(on(t, p));
    }

    /**
     * Returns a predicate that evaluate to {@code true} if the object being
     * tested not {@code equals()} the given value.
     */
    public static <T> Predicate<T> not(Object value) {
        return not(equalTo(value));
    }

    /**
     * Returns a predicate that evaluate to {@code true} if both of two given
     * predicates evaluate to {@code true}. The predicates are evaluated in
     * order, and evaluation will be "short-circuited" as soon as a false
     * predicate is found.
     */
    public static <T> Predicate<T> both(Predicate<? super T> p1, Predicate<? super T> p2) {
        requireNonNull(p1);
        requireNonNull(p2);
        return t -> p1.test(t) && p2.test(t);
    }

    /**
     * Returns a predicate that evaluate to {@code true} if either of two given
     * predicates evaluate to {@code true}. The predicates are evaluated in
     * order, and evaluation will be "short-circuited" as soon as a true
     * predicate is found.
     */
    public static <T> Predicate<T> either(Predicate<? super T> p1, Predicate<? super T> p2) {
        requireNonNull(p1);
        requireNonNull(p2);
        return t -> p1.test(t) || p2.test(t);
    }

    /**
     * Returns a predicate that evaluate to {@code false} if neither of two given
     * predicates evaluate to {@code true}. The predicates are evaluated in
     * order, and evaluation will be "short-circuited" as soon as a false
     * predicate is found.
     */
    public static <T> Predicate<T> neither(Predicate<? super T> p1, Predicate<? super T> p2) {
        return not(either(p1, p2));
    }

    /**
     * Returns a predicate that always evaluate to {@code true}.
     */
    public static <T> Predicate<T> any() {
        return t -> true;
    }

    /**
     * Returns a predicate that evaluate to {@code true} if the object being
     * tested {@code equals()} the given value or both are null.
     */
    public static <T> Predicate<T> equalTo(Object value) {
        return (value == null) ? Objects::isNull : t -> value.equals(t);
    }

    /**
     * Returns a predicate that evaluate to {@code true} if the object being
     * tested the same instance as the specified target object.
     */
    public static <T> Predicate<T> sameAs(Object value) {
        return t -> t == value;
    }

    /**
     * Returns a predicate that evaluate to {@code true} if the object being
     * tested is an instance of the given class.
     */
    public static <T> Predicate<T> instanceOf(Class<?> type) {
        requireNonNull(type);
        return t -> type.isInstance(t);
    }

    /**
     * Returns a predicate that evaluate to {@code true} if the object being
     * tested is an array.
     */
    public static <T> Predicate<T> isArray() {
        return t -> t.getClass().isArray();
    }

    /**
     * Returns a predicate that evaluates to {@code true} if the object reference
     * being tested is a member of the given collection.
     *
     * @param c the collection that may contain the function input
     */
    public static <T> Predicate<T> in(Collection<? extends T> c) {
        requireNonNull(c);
        return t -> c.contains(t);
    }

    /**
     * Returns a predicate that evaluates to {@code true} if the object reference
     * being tested is a member of the given array.
     *
     * @param a the array that may contain the function input
     */
    @SafeVarargs
    @SuppressWarnings("varargs")
    public static <T> Predicate<T> in(T... a) {
        return in(Arrays.asList(a));
    }

    /**
     * Returns a predicate that evaluates to {@code true} if the object reference
     * being tested is a member of the given map.
     *
     * @param m the map that may contain the function input
     */
    public static <T> Predicate<T> in(Map<? extends T, ?> m) {
        requireNonNull(m);
        return t -> m.containsKey(t);
    }

    /**
     * Returns a predicate that evaluates to {@code true} if the object reference
     * being tested is a member of the given collection.
     *
     * @param c the collection that may contain the function input
     */
    public static <T> Predicate<T> oneOf(Collection<? extends T> c) {
        return in(c);
    }

    /**
     * Returns a predicate that evaluates to {@code true} if the object reference
     * being tested is a member of the given array.
     *
     * @param a the array that may contain the function input
     */
    @SafeVarargs
    @SuppressWarnings("varargs")
    public static <T> Predicate<T> oneOf(T... a) {
        return in(a);
    }

    /**
     * Returns a predicate that evaluates to {@code true} if the object reference
     * being tested is not a member of the given collection.
     *
     * @param c the collection that may contain the function input
     */
    public static <T> Predicate<T> noneOf(Collection<? extends T> c) {
        return not(in(c));
    }

    /**
     * Returns a predicate that evaluates to {@code true} if the object reference
     * being tested is not a member of the given array.
     *
     * @param a the array that may contain the function input
     */
    @SafeVarargs
    @SuppressWarnings("varargs")
    public static <T> Predicate<T> noneOf(T... a) {
        return not(in(a));
    }
}
