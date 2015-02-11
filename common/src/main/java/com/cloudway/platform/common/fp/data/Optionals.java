/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.data;

import java.util.Objects;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

import com.cloudway.platform.common.fp.control.ConditionCase;
import com.cloudway.platform.common.fp.function.ExceptionFunction;
import com.cloudway.platform.common.fp.function.ExceptionSupplier;

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
     * Evaluate each action in the sequence from left to right, and collect
     * the result.
     */
    public static <T> Optional<Seq<T>> flatM(Seq<Optional<T>> xs) {
        Seq<T> res = Seq.nil();
        for (; !xs.isEmpty(); xs = xs.tail()) {
            Optional<T> x = xs.head();
            if (x.isPresent()) {
                res = Seq.cons(x.get(), res);
            } else {
                return Optional.empty();
            }
        }
        return Optional.of(res.reverse());
    }

    /**
     * The {@code mapM} is analogous to {@link Seq#map(Function) map} except that
     * its result is encapsulated in an {@code Optional}.
     */
    public static <T, R> Optional<Seq<R>> mapM(Seq<T> xs, Function<? super T, Optional<R>> f) {
        return flatM(xs.map(f));
    }

    /**
     * This generalizes the list-based filter function.
     */
    public static <T> Optional<Seq<T>> filterM(Seq<T> xs, Function<? super T, Optional<Boolean>> p) {
        return xs.isEmpty()
            ? Optional.of(Seq.nil())
            : p.apply(xs.head()).flatMap(flg ->
              filterM(xs.tail(), p).flatMap(ys ->
              Optional.of(flg ? Seq.cons(xs.head(), ys) : ys)));
    }

    /**
     * The {@code foldM} is analogous to {@link Seq#foldLeft(Object,BiFunction) foldLeft},
     * except that its result is encapsulated in an {@code Optional}. Note that
     * {@code foldM} works from left-to-right over the list arguments. If right-to-left
     * evaluation is required, the input list should be reversed.
     */
    public static <T, R> Optional<R> foldM(R r0, Seq<T> xs, BiFunction<R, ? super T, Optional<R>> f) {
        Optional<R> r = Optional.of(r0);
        while (!xs.isEmpty()) {
            r = f.apply(r.get(), xs.head());
            if (!r.isPresent())
                break;
            xs = xs.tail();
        }
        return r;
    }

    /**
     * Kleisli composition of monads.
     */
    public static <A, B, C> Function<A, Optional<C>>
    compose(Function<A, Optional<B>> f, Function<B, Optional<C>> g) {
        return x -> f.apply(x).flatMap(g);
    }

    /**
     * Promote a function to an optional function.
     */
    public static <T, R> Function<Optional<T>, Optional<R>>
    liftM(Function<? super T, ? extends R> f) {
        return m -> m.map(f);
    }

    /**
     * Promote a function to an optional function.
     */
    public static <T, U, R> BiFunction<Optional<T>, Optional<U>, Optional<R>>
    liftM2(BiFunction<? super T, ? super U, ? extends R> f) {
        return (m1,m2) -> m1.flatMap(x1 -> m2.map(x2 -> f.apply(x1, x2)));
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
     * Returns a conditional case that evaluate to perform action on the optional
     * being tested is not {@link Optional#isPresent() presents} a value.
     */
    public static <R, X extends Throwable> ConditionCase<Optional<?>, R, X>
    Nothing(ExceptionSupplier<R, X> mapper) {
        return opt -> opt.isPresent() ? null : mapper;
    }
}
