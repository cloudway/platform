/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.data;

import java.util.Comparator;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collector;
import static java.util.Objects.requireNonNull;

import com.cloudway.platform.common.fp.control.Cont;
import static com.cloudway.platform.common.fp.control.Cont.generator;
import static com.cloudway.platform.common.fp.control.Cont.yield;

/**
 * Class of data structures that can be folded to a summary value.
 *
 * @param <T> the type of the data structure element
 */
public interface Foldable<T> extends Iterable<T> {
    /**
     * Reduce the data structure using the binary operator, from right to left.
     * This is a lazy operation so the accumulator accept a delay evaluation of
     * reduced result instead of a strict value.
     *
     * @param <R> the type of the result
     * @param identity the identity of the result
     * @param accumulator an associative non-interfering function for combining
     * two values
     * @return the result of the reduction
     */
    <R> R foldRight(R identity, BiFunction<? super T, Supplier<R>, R> accumulator);

    /**
     * The strict version of {@link #foldRight(Object,BiFunction) foldRight}.
     *
     * @param <R> the type of the result
     * @param identity the identity of the result
     * @param accumulator an associative non-interfering function for combining
     * two values
     * @return the result of the reduction
     */
    default <R> R foldRight_(R identity, BiFunction<? super T, R, R> accumulator) {
        return asReverseList().foldLeft(identity, (r, x) -> accumulator.apply(x, r));
    }

    /**
     * A variant of {@link #foldRight(Object,BiFunction) foldRight} that has no
     * starting value argument
     *
     * @param accumulator an associative non-interfering function for combining
     * two values
     * @return the result of the reduction
     */
    default Optional<T> foldRight(BiFunction<T,T,T> accumulator) {
        requireNonNull(accumulator);
        BiFunction<T, Optional<T>, Optional<T>> mf = (x, r) ->
            r.isPresent() ? Optional.of(accumulator.apply(x, r.get()))
                          : Optional.of(x);
        return foldRight_(Optional.empty(), mf);
    }

    /**
     * Reduce the list using the binary operator, from left to right.
     *
     * @param <R> the type of the result
     * @param identity the identity of the result
     * @param accumulator an associative non-interfering function for combining
     * two values
     * @return the result of the reduction
     */
    <R> R foldLeft(R identity, BiFunction<R, ? super T, R> accumulator);

    /**
     * A variant of {@link #foldLeft(Object,BiFunction) foldLeft} that has no
     * starting value argument.
     *
     * @param accumulator an associative non-interfering function for combining
     * two values
     * @return the result of the reduction
     */
    default Optional<T> foldLeft(BiFunction<T,T,T> accumulator) {
        requireNonNull(accumulator);
        BiFunction<Optional<T>, T, Optional<T>> mf = (r, x) ->
            r.isPresent() ? Optional.of(accumulator.apply(r.get(), x))
                          : Optional.of(x);
        return foldLeft(Optional.empty(), mf);
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
        return foldMap(Monoid.max(c), Optional::of).get();
    }

    /**
     * Returns the least element of the structure.
     *
     * @param c the comparator that will be used to order elements
     * @return the largest element of the structure, or nothing for empty structure
     * @throws NoSuchElementException if structure is empty
     */
    default T minimum(Comparator<? super T> c) {
        return foldMap(Monoid.min(c), Optional::of).get();
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
     * Fold elements as a reversed list.
     *
     * @return a list of elements contained in the data structure in reversed order
     */
    default Seq<T> asReverseList() {
        return foldLeft(Seq.nil(), Fn.flip(Seq::cons));
    }
}
