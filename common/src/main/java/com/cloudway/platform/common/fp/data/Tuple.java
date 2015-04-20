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
import java.util.function.Supplier;

import com.cloudway.platform.common.fp.$;
import com.cloudway.platform.common.fp.control.Applicative;
import com.cloudway.platform.common.fp.function.ExceptionBiFunction;
import com.cloudway.platform.common.fp.control.ConditionCase;
import com.cloudway.platform.common.fp.function.ExceptionTriFunction;

/**
 * A tuple with two elements.
 */
public class Tuple<A, B> implements
    $<Tuple.µ<A>, B>, Foldable<B>, Traversable<Tuple.µ<A>, B>,
    Serializable, Cloneable
{
    private static final long serialVersionUID = 6021746465072972306L;

    private final A first;
    private final B second;

    /**
     * Construct a new Tuple with two arguments.
     *
     * @param first the first argument
     * @param second the second argument
     */
    public Tuple(A first, B second) {
        this.first = first;
        this.second = second;
    }

    /**
     * Construct a new Tuple with two arguments.
     *
     * @param first the first argument
     * @param second the second argument
     */
    public static <A, B> Tuple<A, B> of(A first, B second) {
        return new Tuple<>(first, second);
    }

    /**
     * Construct a new Tuple with two elements of same type.
     *
     * @param first the first argument
     * @param second the second argument
     */
    public static <A> Pair<A> pair(A first, A second) {
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
    public A first() {
        return first;
    }

    /**
     * Returns the second element.
     */
    public B second() {
        return second;
    }

    /**
     * Get a tuple with two elements swapped.
     */
    public Tuple<B, A> swap() {
        return new Tuple<>(second, first);
    }

    /**
     * Apply this tuple as arguments to a function.
     */
    public <R> R as(BiFunction<? super A, ? super B, ? extends R> fn) {
        return fn.apply(first, second);
    }

    /**
     * Applies two elements as arguments to two given functions and return
     * a new tuple with the substituted arguments.
     */
    public <X, Y> Tuple<X, Y> map(Function<? super A, ? extends X> f,
                                  Function<? super B, ? extends Y> g) {
        return of(f.apply(first), g.apply(second));
    }

    /**
     * Apply first element as argument to a function and return a new tuple
     * with the substituted argument.
     */
    public <R> Tuple<R, B> mapFirst(Function<? super A, ? extends R> fn) {
        return of(fn.apply(first), second);
    }

    /**
     * Apply second element as argument to a function and return a new tuple
     * with the substituted argument.
     */
    public <R> Tuple<A, R> mapSecond(Function<? super B, ? extends R> fn) {
        return of(first, fn.apply(second));
    }

    /**
     * Returns a predicate that evaluate first element as argument to the
     * given predicate.
     */
    public static <A, B> Predicate<Tuple<A, B>> first(Predicate<? super A> p) {
        return t -> p.test(t.first());
    }

    /**
     * Returns a predicate that evaluate second element as argument to the
     * given predicate.
     */
    public static <A, B> Predicate<Tuple<A, B>> second(Predicate<? super B> p) {
        return t -> p.test(t.second());
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

    public static <A extends Comparable<A>, B extends Comparable<B>>
    Comparator<Tuple<A, B>> comparator() {
        return Comparator.<Tuple<A, B>, A>comparing(Tuple::first).thenComparing(Tuple::second);
    }

    public static <A> Comparator<Tuple<A, A>>
    comparator(Comparator<? super A> comparator) {
        return Comparator.<Tuple<A, A>, A>comparing(Tuple::first, comparator)
            .thenComparing(Tuple::second, comparator);
    }

    public static <A, B> Comparator<Tuple<A, B>>
    comparator(Comparator<? super A> firstComparator, Comparator<? super B> secondComparator) {
        return Comparator.<Tuple<A, B>, A>comparing(Tuple::first, firstComparator)
            .thenComparing(Tuple::second, secondComparator);
    }

    @Override
    @SuppressWarnings({"rawtypes", "unchecked", "CloneDoesntDeclareCloneNotSupportedException"})
    public Tuple<A, B> clone() {
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
    public static <A, B, R, X extends Throwable> ConditionCase<Tuple<A, B>, R, X>
    Tuple_(ExceptionBiFunction<? super A, ? super B, ? extends R, X> mapper) {
        return t -> () -> mapper.evaluate(t.first(), t.second());
    }

    /**
     * Returns a conditional case to deconstruct elements of a triple.
     */
    public static <A, B, C, R, X extends Throwable> ConditionCase<Triple<A, B, C>, R, X>
    Triple_(ExceptionTriFunction<A, B, C, R, X> mapper) {
        return t -> () -> mapper.evaluate(t._1(), t._2(), t._3());
    }

    // Type Classes -----------------------------------------------------------

    @Override
    public <R> R foldMap(Monoid<R> monoid, Function<? super B, ? extends R> f) {
        return f.apply(second);
    }

    @Override
    public <R> R foldRight(BiFunction<? super B, Supplier<R>, R> f, Supplier<R> r) {
        return f.apply(second, r);
    }

    @Override
    public <R> R foldRight_(R z, BiFunction<? super B, R, R> f) {
        return f.apply(second, z);
    }

    @Override
    public Maybe<B> foldRight(BiFunction<B, B, B> f) {
        return Maybe.of(second);
    }

    @Override
    public <R> R foldLeft(BiFunction<Supplier<R>, ? super B, R> f, Supplier<R> r) {
        return f.apply(r, second);
    }

    @Override
    public <R> R foldLeft(R z, BiFunction<R, ? super B, R> f) {
        return f.apply(z, second);
    }

    @Override
    public Maybe<B> foldLeft(BiFunction<B, B, B> f) {
        return Maybe.of(second);
    }

    @Override
    public <F, B1> $<F, Tuple<A, B1>>
    traverse(Applicative<F> m, Function<? super B, ? extends $<F, B1>> f) {
        return m.map(f.apply(second), y -> of(first, y));
    }

    /**
     * Return the monoid for a tuple.
     */
    public static <A, B> Monoid<Tuple<A,B>> monoid(Monoid<A> ma, Monoid<B> mb) {
        return Monoid.monoid_(of(ma.empty(), mb.empty()), (a1, a2) ->
            of(ma.append(a1.first(), a2.first()), mb.append(a1.second(), a2.second())));
    }

    /**
     * The typeclass definition for Tuple.
     */
    public static final class µ<A> implements Functor<µ<A>> {
        @Override
        public <B, C> Tuple<A, C> map($<µ<A>, B> a, Function<? super B, ? extends C> f) {
            return narrow(a).mapSecond(f);
        }
    }

    public static <A, B> Tuple<A, B> narrow($<µ<A>, B> value) {
        return (Tuple<A,B>)value;
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

    /**
     * Returns a typeclass that implement {@link Applicative} by giving
     * a {@link Monoid} for the first element of the tuple.
     */
    public static <A> Applicative<µ<A>> tclass(Monoid<A> m) {
        return new Applicative<µ<A>>() {
            @Override
            public <B> Tuple<A, B> pure(B b) {
                return of(m.empty(), b);
            }

            @Override
            public <B, C> Tuple<A, C> map($<µ<A>, B> a, Function<? super B, ? extends C> f) {
                return narrow(a).mapSecond(f);
            }

            @Override
            public <B, C> $<µ<A>, C> ap($<µ<A>, Function<? super B, ? extends C>> fs, $<µ<A>, B> a) {
                Tuple<A, Function<? super B, ? extends C>> f = narrow(fs);
                Tuple<A, B> t = narrow(a);
                return of(m.append(f.first, t.first), f.second.apply(t.second));
            }
        };
    }
}
