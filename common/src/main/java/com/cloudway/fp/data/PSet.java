/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.data;

import java.util.function.Predicate;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

/**
 * The PSet (P stands for Pure or Persistent) is an analogy of java.util.Set.
 *
 * @param <E> the type of set elements
 */
public interface PSet<E> extends Foldable<E> {

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
    boolean contains(Object e);

    /**
     * Returns <tt>true</tt> if this set contains all of the elements of the
     * specified set.
     *
     * @param s set to be checked for containment in this set
     * @return <tt>true</tt> if this set contains all of the elements of the
     *         specified set
     */
    boolean containsAll(PSet<? extends E> s);

    // Modification Operations

    /**
     * Adds the specified element to this set if it is not already present.
     *
     * @param e element to be added to this set
     */
    PSet<E> add(E e);

    /**
     * Removes the specified element from this set if it is present.
     *
     * @param e element to be removed from this set, if present
     */
    PSet<E> remove(Object e);

    /**
     * Returns an empty set.
     *
     * @return an empty set
     */
    PSet<E> clear();

    /**
     * Returns a set consisting of the elements of this set that match the given
     * predicate.
     *
     * @param predicate a predicate to apply to each element to determine if it
     * should be included
     * @return the filtered set
     */
    PSet<E> filter(Predicate<? super E> predicate);

    /**
     * Removes all of the elements of this set that satisfy the given predicate.
     *
     * @param filter a predicate which returns {@code true} for elements to be
     *        removed
     */
    default PSet<E> removeIf(Predicate<? super E> filter) {
        return filter(filter.negate());
    }

    /**
     * Unions all of the elements from the specified set to this set.
     *
     * @param s set to be union in this set
     * @return the union of two sets
     */
    PSet<E> union(PSet<E> s);

    /**
     * Difference of two sets. Return elements of the first set not existing
     * in the second set.
     *
     * @param s set to be difference in this set
     * @return the difference of two sets
     */
    PSet<E> difference(PSet<E> s);

    /**
     * Intersection of two sets. Return elements existing in both sets.
     *
     * @param s set to be intersect in this set
     * @return the intersection of two sets
     */
    PSet<E> intersection(PSet<E> s);

    /**
     * Returns a sequential {@code Stream} with this set as its source.
     *
     * @return a sequential {@code Stream} over the elements in this set
     */
    default Stream<E> stream() {
        return StreamSupport.stream(spliterator(), false);
    }
}
