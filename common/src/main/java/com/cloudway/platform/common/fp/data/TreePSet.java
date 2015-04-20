/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.data;

import java.util.Comparator;
import java.util.NoSuchElementException;
import java.util.Objects;

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
public interface TreePSet<E> extends PSet<E> {
    // Construction

    /**
     * Construct an empty set, sorted according to the natural ordering
     * of its elements.
     */
    @SuppressWarnings("unchecked")
    static <E extends Comparable<E>> PSet<E> empty() {
        return Tree.EMPTY_SET;
    }

    /**
     * Construct an empty set, sorted according to the specified comparator.
     *
     * @param c the comparator that will be used to order this set
     * @throws NullPointerException if <tt>c</tt> is null
     */
    static <E> PSet<E> empty(Comparator<? super E> c) {
        Objects.requireNonNull(c);
        return new Tree.SetTip<>(c);
    }

    /**
     * Construct a set with a single element, sorted according to the natural
     * ordering of its elements.
     */
    static <E extends Comparable<E>> PSet<E> singleton(E value) {
        return TreePSet.<E>empty().add(value);
    }

    /**
     * Construct a set with a single element, sorted according to the specified
     * comparator.
     *
     * @param c the comparator that will be used to order this set
     * @throws NullPointerException if <tt>c</tt> is null
     */
    static <E> PSet<E> singleton(Comparator<? super E> c, E value) {
        return empty(c).add(value);
    }

    /**
     * Construct a set with given elements, sorted according to the natural
     * ordering of its elements.
     */
    @SafeVarargs
    static <E extends Comparable<E>> PSet<E> of(E... elements) {
        PSet<E> res = empty();
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
    static <E> PSet<E> of(Comparator<? super E> c, E... elements) {
        PSet<E> res = empty(c);
        for (E e : elements) {
            res = res.add(e);
        }
        return res;
    }

    /**
     * Construct a set from a list, sorted according to the natural ordering
     * of its elements.
     */
    static <E extends Comparable<E>> PSet<E> fromList(Seq<E> list) {
        return list.foldLeft(empty(), PSet::add);
    }

    /**
     * Construct a set from a list, sorted according to the specified comparator.
     *
     * @param c the comparator that will be used to order this set
     * @throws NullPointerException if <tt>c</tt> is null
     */
    static <E> PSet<E> fromList(Comparator<? super E> c, Seq<E> list) {
        return list.foldLeft(empty(c), PSet::add);
    }

    /**
     * Returns the set monoid.
     *
     * @return the set monoid
     */
    static <E extends Comparable<E>> Monoid<PSet<E>> monoid() {
        return Monoid.monoid_(empty(), PSet::union);
    }

    /**
     * Returns the set monoid.
     *
     * @param c the comparator that will be used to order set elements
     * @return the set monoid
     */
    static <E> Monoid<PSet<E>> monoid(Comparator<? super E> c) {
        return Monoid.monoid_(empty(c), PSet::union);
    }

    // Navigation

    /**
     * Returns the greatest element in this set strictly less than the given
     * element, or an empty {@code Maybe} if there is no such element.
     *
     * @param e the value to match
     * @return the greatest element less than {@code e} or an empty {@code Maybe}
     *         if there is no such element
     */
    Maybe<E> lower(E e);

    /**
     * Returns the greatest element in this set less than or equal to the given
     * element, or an empty {@code Maybe} if there is no such element.
     *
     * @param e the value to match
     * @return the greatest element less than or equal to {@code e}, or
     *         an empty {@code Maybe} if there is no such element
     */
    Maybe<E> floor(E e);

    /**
     * Returns the least element in this set greater than or equal to the given
     * element, or an empty {@code Maybe} if there is no such element.
     *
     * @param e the value to match
     * @return the least element greater than or equal to {@code e}, or
     *         an empty {@code Maybe} if there is no such element
     */
    Maybe<E> ceiling(E e);

    /**
     * Returns the least element in this set strictly greater than the given
     * element, or an empty {@code Maybe} if there is no such element.
     *
     * @param e the value to match
     * @return the least element greater than {@code e}, or an empty {@code Maybe}
     *         if there is no such element
     */
    Maybe<E> higher(E e);

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
