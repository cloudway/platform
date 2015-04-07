/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.data;

import java.math.BigInteger;
import java.util.Comparator;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.function.Supplier;

import static com.cloudway.platform.common.fp.control.Conditionals.Any;
import static com.cloudway.platform.common.fp.control.Conditionals.with;
import static com.cloudway.platform.common.fp.data.Optionals.Just;
import static com.cloudway.platform.common.fp.data.Optionals.Nothing;

/**
 * A class for monoids (types with an associative binary operation that has
 * an identity) with various general-purpose instances. Instances should satisfy
 * the following laws:
 *
 * <pre>{@code
 *      0 + x = x
 *      x + 0 = x
 *      x + (y + z) = (x + y) + z
 * }</pre>
 */
public abstract class Monoid<A> {
    /**
     * Returns the identity value for this monoid.
     *
     * @return the identity value for this monoid
     */
    public abstract A empty();

    /**
     * Appends a strict value and a lazy value.
     *
     * @param a1 a strict value to append with another
     * @param a2 a lazy value to append with another
     * @return the concatenation of the two given values
     */
    public abstract A append(A a1, Supplier<A> a2);

    /**
     * Appends the two given strict values.
     *
     * @param a1 a strict value to append with another
     * @param a2 a strict value to append with another
     * @return the concatenation of the two given strict values
     */
    public abstract A append(A a1, A a2);

    /**
     * Fold a data structure using the monoid.
     *
     * <p>For most types, the default definition for 'concat' will be used,
     * but an optimized version can be provided for specific types.
     */
    public A concat(Foldable<A> xs) {
        return xs.foldRight(empty(), this::append);
    }

    /**
     * Maps each element of a structure to this monoid, and combine the results.
     * This method can be optimized by subclasses.
     *
     * @param xs the structure in which element is mapped and results combined
     * @param f the mapping function
     * @return the reduced result
     * @see Foldable#foldMap
     */
    protected abstract <T> A foldMap(Foldable<T> xs, Function<? super T, ? extends A> f);

    static class Lazy<A> extends Monoid<A> {
        private final A empty;
        private final BiFunction<A, Supplier<A>, A> append;

        Lazy(A empty, BiFunction<A, Supplier<A>, A> append) {
            this.empty = empty;
            this.append = append;
        }

        @Override
        public A empty() {
            return empty;
        }

        @Override
        public A append(A a1, Supplier<A> a2) {
            return append.apply(a1, a2);
        }

        @Override
        public A append(A a1, A a2) {
            return append.apply(a1, () -> a2);
        }

        @Override
        protected <T> A foldMap(Foldable<T> xs, Function<? super T, ? extends A> f) {
            return xs.foldRight(empty, (x, r) -> append.apply(f.apply(x), r));
        }
    }

    static class Strict<A> extends Monoid<A> {
        private final A empty;
        private final BinaryOperator<A> append;

        Strict(A empty, BinaryOperator<A> append) {
            this.empty = empty;
            this.append = append;
        }

        @Override
        public A empty() {
            return empty;
        }

        @Override
        public A append(A a1, Supplier<A> a2) {
            return append.apply(a1, a2.get());
        }

        @Override
        public A append(A a1, A a2) {
            return append.apply(a1, a2);
        }

        @Override
        protected <T> A foldMap(Foldable<T> xs, Function<? super T, ? extends A> f) {
            return xs.foldRight_(empty, (x, r) -> append.apply(f.apply(x), r));
        }
    }

    /**
     * Construct a monoid from the given append function and empty value, which
     * must follow the monoidal laws.
     *
     * @param empty the empty for the monoid
     * @param append the append function for the monoid
     * @return a monoid instance that uses the given empty value and append function
     */
    public static <A> Monoid<A> monoid(A empty, BiFunction<A, Supplier<A>, A> append) {
        return new Lazy<>(empty, append);
    }

    /**
     * Construct a monoid from the given append function and empty value, which
     * must follow the monoidal laws.
     *
     * @param empty the empty for the monoid
     * @param append the append function for the monoid
     * @return a monoid instance that uses the given empty value and append function
     */
    public static <A> Monoid<A> monoid_(A empty, BinaryOperator<A> append) {
        return new Strict<>(empty, append);
    }

    // Monoid instances

    /**
     * A monoid that adds integers.
     */
    public static final Monoid<Integer> intSum = new Monoid<Integer>() {
        @Override
        public Integer empty() {
            return 0;
        }

        @Override
        public Integer append(Integer a1, Supplier<Integer> a2) {
            return a1 + a2.get();
        }

        @Override
        public Integer append(Integer a1, Integer a2) {
            return a1 + a2;
        }

        @Override
        protected <T> Integer foldMap(Foldable<T> xs, Function<? super T, ? extends Integer> f) {
            return xs.foldLeft(0, (s, x) -> s + f.apply(x));
        }
    };

    /**
     * A monoid that multiplies integers.
     */
    public static final Monoid<Integer> intProduct =
        monoid_(1, (a, b) -> a * b);

    /**
     * A monoid that adds long integers.
     */
    public static final Monoid<Long> longSum =
        monoid_(0L, Long::sum);

    /**
     * A monoid that multiplies integers.
     */
    public static final Monoid<Long> longProduct =
        monoid_(1L, (a, b) -> a * b);

    /**
     * A monoid that adds big integers.
     */
    public static final Monoid<BigInteger> bigIntSum =
        monoid_(BigInteger.ZERO, BigInteger::add);

    /**
     * A monoid that multiplies integers.
     */
    public static final Monoid<BigInteger> bigIntProduct =
        monoid_(BigInteger.ONE, BigInteger::multiply);

    /**
     * A monoid that ANDs booleans.
     */
    public static final Monoid<Boolean> conjunction =
        monoid(true, (a, b) -> a && b.get());

    /**
     * A monoid that ORs booleans.
     */
    public static final Monoid<Boolean> disjunction =
        monoid(false, (a, b) -> a || b.get());

    /**
     * A monoid that XORs booleans.
     */
    public static final Monoid<Boolean> exclusiveDisjunction =
        monoid_(false, (a, b) -> a ^ b);

    /**
     * A monoid that appends strings.
     */
    public static final Monoid<String> stringConcat =
        monoid_("", String::concat);

    /**
     * A monoid for functions.
     */
    public static <A, B> Monoid<Function<A, B>> ofFunction(Monoid<B> mb) {
        return monoid(Fn.pure(mb.empty()), (f, gs) ->
            x -> mb.append(f.apply(x), Fn.map(gs, g -> g.apply(x))));
    }

    /**
     * A monoid for optional values.
     */
    public static <A> Monoid<Optional<A>> ofOptional(Monoid<A> ma) {
        return monoid_(Optional.empty(), (a1, a2) ->
            with(a1, a2).<Optional<A>>get()
              .when(Any(x -> Nothing(() -> x)))
              .when(Nothing(() -> Any(y -> y)))
              .when(Just(x -> Just(y -> Optional.of(ma.append(x, y)))))
              .get());
    }

    /**
     * A monoid for optionals that take the first available value.
     */
    public static <A> Monoid<Optional<A>> first() {
        return monoid_(Optional.empty(), (a1, a2) -> a1.isPresent() ? a1 : a2);
    }

    /**
     * A monoid for optionals that take the last available value.
     */
    public static <A> Monoid<Optional<A>> last() {
        return monoid_(Optional.empty(), (a1, a2) -> a2.isPresent() ? a2 : a1);
    }

    /**
     * A monoid for optionals that take the maximum value.
     */
    public static <A> Monoid<Optional<A>> max(Comparator<? super A> c) {
        return monoid_(Optional.empty(), (a1, a2) ->
            with(a1, a2).<Optional<A>>get()
              .when(Any(x -> Nothing(() -> x)))
              .when(Nothing(() -> Any(y -> y)))
              .when(Just(x -> Just(y -> c.compare(x, y) >= 0 ? a1 : a2)))
              .get());
    }

    /**
     * A monoid for optionals that take the minimum value.
     */
    public static <A> Monoid<Optional<A>> min(Comparator<? super A> c) {
        return monoid_(Optional.empty(), (a1, a2) ->
            with(a1, a2).<Optional<A>>get()
              .when(Any(x -> Nothing(() -> x)))
              .when(Nothing(() -> Any(y -> y)))
              .when(Just(x -> Just(y -> c.compare(x, y) <= 0 ? a1 : a2)))
              .get());
    }

    /**
     * The monoid of endomorphisms under composition.
     */
    public static <A> Monoid<Function<Supplier<A>, A>> endo() {
        return monoid(Supplier::get, (f, g) -> x -> f.apply(() -> g.get().apply(x)));
    }
}
