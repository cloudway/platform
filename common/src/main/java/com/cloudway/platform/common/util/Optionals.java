/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util;

import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

public final class Optionals
{
    private Optionals() {}

    /**
     * Returns the first of two given parameters that is not {@code null},
     * if either is, or otherwise throws a {@link NullPointerException}.
     *
     * @return {@code first} if it is non-null; otherwise {@code second} if it is non-null
     * @throws NullPointerException if both {@code first} and {@code second} are null.
     */
    public static <T> T firstNonNull(T first, T second) {
        return first != null ? first : Objects.requireNonNull(second);
    }

    /**
     * Given two parameters, returns the first parameter if it is non-null,
     * or return the second parameter if the first parameter is {@code null}.
     *
     * @return {@code first} if it is non-null; otherwise {@code second} if
     * {@code first} is {@code null}.
     */
    public static <T> T or(T first, T second) {
        return first != null ? first : second;
    }

    /**
     * Returns the first parameter if it is non-null, otherwise invoke {@code second}
     * and return the result of that invocation.
     *
     * @return {@code first} if it is non-null; otherwise invoke {@code second} and
     * return the result of that invocation.
     */
    public static <T> T or(T first, Supplier<? extends T> second) {
        return first != null ? first : second.get();
    }

    /**
     * Adapts a {@link Supplier} to return optional value where the invocation of supplier
     * may return null value or throws exception.
     *
     * @return an adapted supplier
     */
    public static <T> Supplier<Optional<T>> of(Supplier<? extends T> supplier) {
        return () -> {
            try {
                return Optional.ofNullable(supplier.get());
            } catch (Exception ex) {
                return Optional.empty();
            }
        };
    }

    /**
     * Adapts a {@link Function} to return optional value where the invocation of function
     * may return null value or throws exception.
     *
     * @return an adapted function
     */
    public static <T, R> Function<T, Optional<R>> of(Function<? super T, ? extends R> f) {
        return (t) -> {
            try {
                return Optional.ofNullable(f.apply(t));
            } catch (Exception ex) {
                return Optional.empty();
            }
        };
    }

    /**
     * Returns a predicate that evaluate to {@code true} if the optional being
     * tested is {@link Optional#isPresent() presents} a value and the given
     * predicate evaluate to {@code true} to the value.
     */
    public static <T> Predicate<Optional<T>> just(Predicate<? super T> p) {
        return opt -> opt.isPresent() && p.test(opt.get());
    }

    /**
     * Returns a predicate that evaluate to {@code true} if the optional being
     * tested is {@link Optional#isPresent() presents} a value and the value
     * {@code equals()} to the given value.
     */
    public static <T> Predicate<Optional<T>> just(T value) {
        return just(Predicate.isEqual(value));
    }

    /**
     * Returns a predicate that evaluate to {@code true} if the optional being
     * tested does not {@link Optional#isPresent() presents} a value.}
     */
    public static Predicate<Optional<?>> nothing() {
        return opt -> !opt.isPresent();
    }

    /**
     * Returns a conditional case that evaluate to perform action on the optional
     * being tested is {@link Optional#isPresent() presents} a value.
     */
    public static <T, R, X extends Throwable> ConditionCase<Optional<T>, R, X>
    Just(ExceptionFunction<? super T, ? extends R, X> mapper) {
        return opt -> opt.isPresent()
            ? () -> mapper.evaluate(opt.get())
            : null;
    }

    /**
     * Returns a conditional case that evaluate to perform action of the giving
     * inner conditional case, while the optional being tested is
     * {@link Optional#isPresent() presents)} a value.
     *
     * <p>The following example illustrated a typical pattern matching usage
     * case that deconstruct the element contained in an Optional. In this
     * example the instance of a Tuple class contains two elements. The Tuple()
     * method deconstruct it into arguments passed to a lambda expression.</p>
     *
     * <pre>
     *     void test(Optional&lt;Tuple&gt; obj) {
     *         with(obj)
     *           .when(JustIn(Tuple((x, y) -> ...)));
     *     }
     * </pre>
     */
    public static <T, R, X extends Throwable> ConditionCase<Optional<T>, R, X>
    JustIn(ConditionCase<? super T, ? extends R, X> mapper) {
        return opt -> opt.isPresent()
            ? mapper.evaluate(opt.get())
            : null;
    }

    /**
     * Returns a conditional case that evaluate to perform action on the optional
     * being tested is not {@link Optional#isPresent() presents} a value.
     */
    public static <T, R, X extends Throwable> ConditionCase<Optional<T>, R, X>
    Nothing(ExceptionSupplier<R, X> mapper) {
        return opt -> opt.isPresent() ? null : mapper;
    }
}
