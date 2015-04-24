/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.data;

import java.util.Comparator;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.StringJoiner;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Collector;
import static java.util.Objects.requireNonNull;

import com.cloudway.platform.common.fp.control.trans.Cont;
import com.cloudway.platform.common.fp.control.Trampoline;
import static com.cloudway.platform.common.fp.control.trans.Cont.generator;
import static com.cloudway.platform.common.fp.control.trans.Cont.yield;
import static com.cloudway.platform.common.fp.control.Trampoline.suspend;

/**
 * Class of data structures that can be folded to a summary value.
 *
 * <p><strong>Minimal complete definition</strong></p>
 * <p>
 * {@link #foldMap(Monoid,Function) foldMap} |
 * {@link #foldRight(BiFunction,Supplier) foldRight}
 * </p>
 *
 * @param <T> the type of the data structure element
 */
public interface Foldable<T> extends Iterable<T> {
    /**
     * Convenient functional interface used to declare a complex fold function.
     */
    @FunctionalInterface
    interface RightFoldFunction<T, R> extends BiFunction<T, Supplier<R>, R> {
    }

    /**
     * Convenient functional interface used to declare a complex fold function.
     */
    @FunctionalInterface
    interface LeftFoldFunction<T, R> extends BiFunction<Supplier<R>, T, R> {
    }

    /**
     * Reduce the data structure using the binary operator, from right to left.
     * This is a lazy operation so the accumulator accept a delay evaluation of
     * reduced result instead of a strict value.
     *
     * @param <R> the type of the result
     * @param f an associative non-interfering function for combining two values
     * @param r the partial reduction result to the accumulator
     * @return the result of the reduction
     */
    default <R> R foldRight(BiFunction<? super T, Supplier<R>, R> f, Supplier<R> r) {
        return foldMap(Monoid.<R>endo(), Fn.curry(f)).apply(r);
    }

    /**
     * Reduce the data structure using the binary operator, from right to left.
     * This is a lazy operation so the accumulator accept a delay evaluation of
     * reduced result instead of a strict value.
     *
     * @param <R> the type of the result
     * @param z the identity of the result
     * @param f an associative non-interfering function for combining
     * two values
     * @return the result of the reduction
     */
    default <R> R foldRight(R z, BiFunction<? super T, Supplier<R>, R> f) {
        return foldRight(f, () -> z);
    }

    /**
     * The strict version of {@link #foldRight(Object,BiFunction) foldRight}.
     *
     * @param <R> the type of the result
     * @param z the identity of the result
     * @param f an associative non-interfering function for combining
     * two values
     * @return the result of the reduction
     */
    default <R> R foldRight_(R z, BiFunction<? super T, R, R> f) {
        RightFoldFunction<T, Trampoline<R>> mf = (x, t) ->
            suspend(() -> t.get().map(r -> f.apply(x, r)));
        return foldRight(mf, () -> Trampoline.pure(z)).run();
    }

    /**
     * A variant of {@link #foldRight(Object,BiFunction) foldRight} that has no
     * starting value argument
     *
     * @param f an associative non-interfering function for combining two values
     * @return the result of the reduction
     */
    default Maybe<T> foldRight(BiFunction<T,T,T> f) {
        requireNonNull(f);
        BiFunction<T, Maybe<T>, Maybe<T>> mf = (x, r) ->
            r.isPresent() ? Maybe.of(f.apply(x, r.get()))
                          : Maybe.of(x);
        return foldRight_(Maybe.empty(), mf);
    }

    /**
     * Reduce the data structure using the binary operator, from left to right.
     * This is a lazy operation so the accumulator accept a delay evaluation of
     * reduced result instead of a strict value.
     *
     * @param <R> the type the result
     * @param f an associative non-interfering function for combining two values
     * @param r the partial reduction result to the accumulator
     * @return the result of the reduction
     */
    default <R> R foldLeft(BiFunction<Supplier<R>, ? super T, R> f, Supplier<R> r) {
        RightFoldFunction<T, Function<Supplier<R>, Trampoline<R>>>
            mf = (x, g) -> a -> suspend(() -> g.get().apply(() -> f.apply(a, x)));
        return foldRight(a -> Trampoline.pure(a.get()), mf).apply(r).run();
    }

    /**
     * Reduce the data structure using the binary operator, from left to right.
     *
     * @param <R> the type of the result
     * @param z the identity of the result
     * @param f an associative non-interfering function for combining two values
     * @return the result of the reduction
     */
    default <R> R foldLeft(R z, BiFunction<R, ? super T, R> f) {
        RightFoldFunction<T, Function<R, Trampoline<R>>>
            mf = (x, g) -> a -> suspend(() -> g.get().apply(f.apply(a, x)));
        return foldRight(Trampoline::pure, mf).apply(z).run();
    }

    /**
     * A variant of {@link #foldLeft(Object,BiFunction) foldLeft} that has no
     * starting value argument.
     *
     * @param f an associative non-interfering function for combining
     * two values
     * @return the result of the reduction
     */
    default Maybe<T> foldLeft(BiFunction<T,T,T> f) {
        requireNonNull(f);
        BiFunction<Maybe<T>, T, Maybe<T>> mf = (r, x) ->
            r.isPresent() ? Maybe.of(f.apply(r.get(), x))
                          : Maybe.of(x);
        return foldLeft(Maybe.empty(), mf);
    }

    /**
     * Combine the elements of a structure using a monoid.
     *
     * @param monoid the monoid used to combine the elements
     */
    default T fold(Monoid<T> monoid) {
        return foldMap(monoid, Fn.id());
    }

    /**
     * Map each element of the structure to a monoid, and combine the results.
     *
     * @param monoid the monoid used to combine the elements
     * @param f the mapping function
     */
    default <R> R foldMap(Monoid<R> monoid, Function<? super T, ? extends R> f) {
        return monoid.foldMap(this, f);
    }

    /**
     * Returns the largest element of the structure.
     *
     * @param c the comparator that will be used to order elements
     * @return the largest element of the structure
     * @throws NoSuchElementException if structure is empty
     */
    default T maximum(Comparator<? super T> c) {
        return foldMap(Monoid.max(c), Maybe::of).get();
    }

    /**
     * Returns the least element of the structure.
     *
     * @param c the comparator that will be used to order elements
     * @return the largest element of the structure, or nothing for empty structure
     * @throws NoSuchElementException if structure is empty
     */
    default T minimum(Comparator<? super T> c) {
        return foldMap(Monoid.min(c), Maybe::of).get();
    }

    /**
     * Returns the count of elements in the structure.
     *
     * @return the count of elements in the structure
     */
    default long count() {
        return foldMap(Monoid.longSum, Fn.pure(1L));
    }

    /**
     * Search the first occurrence of an element that satisfy the given
     * predicate.
     *
     * @param predicate the predicate to be tested on element
     * @return the first occurrence of element that satisfy the given predicate,
     * or empty {@code Maybe} if element not found
     */
    default Maybe<T> find(Predicate<? super T> predicate) {
        RightFoldFunction<T, Trampoline<Maybe<T>>> g = (x, r) ->
            predicate.test(x) ? Trampoline.pure(Maybe.of(x)) : suspend(r);
        return foldRight(g, () -> Trampoline.pure(Maybe.empty())).run();
    }

    /**
     * Search for the first occurrence of an element that satisfy the given
     * predicate.
     *
     * @param predicate the predicate to be tested on element
     * @return the first occurrence of element that satisfy the given predicate,
     * or empty {@code Maybe} if element not found
     */
    default Maybe<T> findFirst(Predicate<? super T> predicate) {
        return find(predicate);
    }

    /**
     * Search for the last occurrence of an element that satisfy the given
     * predicate.
     *
     * @param predicate the predicate to be tested on element
     * @return the last occurrence of element that satisfy the given predicate,
     * or empty {@code Maybe} if element not found
     */
    default Maybe<T> findLast(Predicate<? super T> predicate) {
        LeftFoldFunction<T, Trampoline<Maybe<T>>> g = (r, x) ->
            predicate.test(x) ? Trampoline.pure(Maybe.of(x)) : suspend(r);
        return foldLeft(g, () -> Trampoline.pure(Maybe.empty())).run();
    }

    /**
     * Returns whether any elements of structure match the provided
     * predicate. May not evaluate the predicate on all elements if not
     * necessary for determining the result. If the structure is empty then
     * {@code false} is returned and the predicate is not evaluated.
     *
     * @param predicate a predicate to apply to elements of structure
     * @return {@code true} if any elements of the structure match the provided
     * predicate, other {@code false}
     */
    default boolean anyMatch(Predicate<? super T> predicate) {
        return find(predicate).isPresent();
    }

    /**
     * Returns whether all elements of structure match the provided predicate.
     * May not evaluate the predicate on all elements if not necessary for
     * determining the result. If the structure is empty then {@code true} is
     * returned and the predicate is not evaluated.
     *
     * @param predicate a predicate to apply to elements of structure
     * @return {@code true} if either all elements of the structure match the
     * provided predicate or the structure is empty, otherwise {@code false}
     */
    default boolean allMatch(Predicate<? super T> predicate) {
        return !anyMatch(predicate.negate());
    }

    /**
     * Returns whether no elements of structure match the provided predicate.
     * May not evaluate the predicate on all elements if not necessary for
     * determining the result. If the structure is empty then {@code true} is
     * returned and the predicate is not evaluated.
     *
     * @param predicate a predicate to apply to elements of structure
     * @return {@code true} if either no elements of the stream match the
     * provided predicate or the structure is empty, otherwise {@code false}
     */
    default boolean noneMatch(Predicate<? super T> predicate) {
        return !anyMatch(predicate);
    }

    /**
     * Returns an iterator over elements of the data structure.
     *
     * @return an Iterator
     */
    @Override
    default Iterator<T> iterator() {
        return generator(foldRight(Cont.<T>finish(), (x, r) -> yield(x).then(r))).iterator();
    }

    /**
     * Performs the given action for each element of the {@code Iterable}
     * until all elements have been processed or the action throws an
     * exception.
     *
     * @param action The action to be performed for each element
     * @throws NullPointerException if the specified action is null
     */
    @Override
    default void forEach(Consumer<? super T> action) {
        requireNonNull(action);
        foldLeft(Unit.U, (u, x) -> { action.accept(x); return u; });
    }

    /**
     * Performs a reduction operation on the elements of data structure using a
     * {@code Collector}.  A {@code Collector} is borrowed from Stream API that
     * encapsulates the functions used as arguments to reduction operation.
     *
     * @param <R> the type of the result
     * @param <A> the intermediate accumulation type of the {@code Collector}
     * @param collector the {@code Collector} describing the reduction
     * @return the result of the reduction
     */
    @SuppressWarnings("unchecked")
    default <R, A> R collect(Collector<? super T, A, R> collector) {
        A container = collector.supplier().get();
        BiConsumer<A, ? super T> accumulator = collector.accumulator();
        forEach(x -> accumulator.accept(container, x));
        return collector.characteristics().contains(Collector.Characteristics.IDENTITY_FINISH)
            ? (R) container
            : collector.finisher().apply(container);
    }

    /**
     * Fold elements to a list.
     *
     * @return a list of elements contained in the data structure
     */
    default Seq<T> asList() {
        return foldRight(Seq.nil(), Seq::cons);
    }

    /**
     * Returns the string representation of the data structure.
     *
     * @param delimiter the sequence of character to be used between element
     * @param prefix the sequence of characters to be used at the beginning
     * @param suffix the sequence of characters to be used at the end
     */
    default String show(CharSequence delimiter, CharSequence prefix, CharSequence suffix) {
        return foldLeft(new StringJoiner(delimiter, prefix, suffix),
                        (sj, x) -> sj.add(String.valueOf(x))).toString();
    }
}
