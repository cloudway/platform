/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.data;

import java.util.Comparator;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

/**
 * <p>An efficient implementation of sets</p>
 *
 * <p>The implementation of set is based on size balanced binary trees (or trees
 * of bounded balance) as described by:</p>
 *
 * <ul>
 * <li>Stephen Adams, <a href="http://www.swiss.ai.mit.edu/~adams/BB/">
 *     "Efficient sets: a balancing act"</a>, Journal of Functional Programming
 *     3(4):553-562, October 1993,</li>
 * <li>J. Nievergelt and E.M. Reingold, "Binary search trees of bounded balance",
 *     SIAM journal of computing 2(1), March 1973.</li>
 * </ul>
 *
 * @param <E> the type of set elements
 */
public interface TreeSet<E> {
    // Construction

    /**
     * Construct an empty set, sorted according to the natural ordering
     * of its elements.
     */
    @SuppressWarnings("unchecked")
    static <E extends Comparable<E>> TreeSet<E> empty() {
        return Tree.EMPTY_SET;
    }

    /**
     * Construct an empty set, sorted according to the specified comparator.
     *
     * @param c the comparator that will be used to order this set
     * @throws NullPointerException if <tt>c</tt> is null
     */
    static <E> TreeSet<E> empty(Comparator<? super E> c) {
        Objects.requireNonNull(c);
        return new Tree.SetTip<>(c);
    }

    /**
     * Construct a set with a single element, sorted according to the natural
     * ordering of its elements.
     */
    static <E extends Comparable<E>> TreeSet<E> singleton(E value) {
        return TreeSet.<E>empty().add(value);
    }

    /**
     * Construct a set with a single element, sorted according to the specified
     * comparator.
     *
     * @param c the comparator that will be used to order this set
     * @throws NullPointerException if <tt>c</tt> is null
     */
    static <E> TreeSet<E> singleton(Comparator<? super E> c, E value) {
        return empty(c).add(value);
    }

    /**
     * Construct a set with given elements, sorted according to the natural
     * ordering of its elements.
     */
    @SafeVarargs
    static <E extends Comparable<E>> TreeSet<E> of(E... elements) {
        TreeSet<E> res = empty();
        for (E e : elements) {
            res = res.add(e);
        }
        return res;
    }

    /**
     * Construct a set with given elements, sorted according to the specified
     * comparator.
     *
     * @param c the comparator that will be used to order this set
     * @throws NullPointerException if <tt>c</tt> is null
     */
    @SafeVarargs
    static <E> TreeSet<E> of(Comparator<? super E> c, E... elements) {
        TreeSet<E> res = empty(c);
        for (E e : elements) {
            res = res.add(e);
        }
        return res;
    }

    /**
     * Construct a set from a list, sorted according to the natural ordering
     * of its elements.
     */
    static <E extends Comparable<E>> TreeSet<E> fromList(Seq<E> list) {
        return list.foldLeft(empty(), TreeSet::add);
    }

    /**
     * Construct a set from a list, sorted according to the specified comparator.
     *
     * @param c the comparator that will be used to order this set
     * @throws NullPointerException if <tt>c</tt> is null
     */
    static <E> TreeSet<E> fromList(Comparator<? super E> c, Seq<E> list) {
        return list.foldLeft(empty(c), TreeSet::add);
    }

    // Query Operations

    /**
     * Returns {@code true} if this set contains no elements.
     *
     * @return {@code true} if this set contains no elements
     */
    boolean isEmpty();

    /**
     * Returns the number of elements in this set.
     *
     * @return the number of elements in this set
     */
    int size();

    /**
     * Returns {@code true} if this set contains the specified element.
     *
     * @param e element whose presence in this set is to be tested
     * @return {@code true} if this set contains the specified element
     */
    boolean contains(E e);

    // Modification Operations

    /**
     * Adds the specified element to this set if it is not already present.
     *
     * @param e element to be added to this set
     */
    TreeSet<E> add(E e);

    /**
     * Removes the specified element from this set if it is present.
     *
     * @param e element to be removed from this set, if present
     */
    TreeSet<E> remove(E e);

    /**
     * Returns <tt>true</tt> if this set contains all of the elements of the
     * specified set.
     *
     * @param s set to be checked for containment in this set
     * @return <tt>true</tt> if this set contains all of the elements of the
     *         specified set
     */
    boolean containsAll(TreeSet<E> s);

    /**
     * Returns a set consisting of the results of applying the given function
     * to the elements of this set.
     *
     * @param mapper a function to apply to each element
     * @return the mapped set
     */
    <R> TreeSet<R> map(Function<? super E, ? extends R> mapper);

    /**
     * Returns a set consisting of the elements of this set that match the given
     * predicate.
     *
     * @param predicate a predicate to apply to each element to determine if it
     * should be included
     * @return the filtered set
     */
    TreeSet<E> filter(Predicate<? super E> predicate);

    /**
     * Perform the given action for each element in this set until all elements
     * have been processed or the action throws an exception.
     *
     * @param action an action to perform on each elements
     */
    default void forEach(Consumer<? super E> action) {
        foldLeft(Unit.U, (z, e) -> { action.accept(e); return z; });
    }

    /**
     * Reduce the set elements using the accumulator function, from left to right.
     */
    <R> R foldLeft(R seed, BiFunction<R, ? super E, R> accumulator);

    /**
     * Reduce the map elements using the accumulator function, from right to left.
     * This is a lazy operation so the accumulator accept a delay evaluation of
     * reduced result instead of a strict value.
     */
    <R> R foldRight(R seed, BiFunction<? super E, Supplier<R>, R> accumulator);

    /**
     * The strict version of {@link #foldRight(Object,BiFunction) foldRight}.
     */
    <R> R foldRight_(R seed, BiFunction<? super E, R, R> accumulator);

    /**
     * Returns a list of elements contained in this set.
     *
     * @return a list of elements contained in this set.
     */
    default Seq<E> toList() {
        return foldRight(Seq.nil(), Seq::cons);
    }

    /**
     * Unions all of the elements from the specified set to this set.
     *
     * @param s set to be union in this set
     * @return the union of two sets
     */
    TreeSet<E> union(TreeSet<E> s);

    /**
     * Difference of two sets. Return elements of the first set not existing
     * in the second set.
     *
     * @param s set to be difference in this set
     * @return the difference of two sets
     */
    TreeSet<E> difference(TreeSet<E> s);

    /**
     * Intersection of two sets. Return elements existing in both sets.
     *
     * @param s set to be intersect in this set
     * @return the intersection of two sets
     */
    TreeSet<E> intersection(TreeSet<E> s);

    /**
     * Returns the greatest element in this set strictly less than the given
     * element, or {@code Optional.empty()} if there is no such element.
     *
     * @param e the value to match
     * @return the greatest element less than {@code e} or {@code Optional.empty()}
     *         if there is no such element
     */
    Optional<E> lower(E e);

    /**
     * Returns the greatest element in this set less than or equal to the given
     * element, or {@code Optional.empty()} if there is no such element.
     *
     * @param e the value to match
     * @return the greatest element less than or equal to {@code e}, or
     *         {@code Optional.empty()} if there is no such element
     */
    Optional<E> floor(E e);

    /**
     * Returns the least element in this set greater than or equal to the given
     * element, or {@code Optional.empty()} if there is no such element.
     *
     * @param e the value to match
     * @return the least element greater than or equal to {@code e}, or
     *         {@code Optional.empty()} if there is no such element
     */
    Optional<E> ceiling(E e);

    /**
     * Returns the least element in this set strictly greater than the given
     * element, or {@code Optional.empty()} if there is no such element.
     *
     * @param e the value to match
     * @return the least element greater than {@code e}, or {@code Optional.empty()}
     *         if there is no such element
     */
    Optional<E> higher(E e);

    /**
     * Returns the first (lowest) element in this set.
     *
     * @return the first (lowest) element in this set
     * @throws NoSuchElementException if this set is empty
     */
    E first();

    /**
     * Returns the last (highest) element in this set.
     *
     * @return the last (highest) element in this set
     * @throws NoSuchElementException if this set is empty
     */
    E last();
}
