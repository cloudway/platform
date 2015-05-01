/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.data;

import java.util.NoSuchElementException;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.UnaryOperator;

/**
 * Provides 2-3 finger trees, a functional representation of persistent sequences
 * supporting access to the ends in amortized O(1) time. Concatenation and
 * splitting time is O(log n) in the size of the smaller piece. A general purpose
 * data structure that can serve as a sequence, priority queue, search tree,
 * priority search queue and more.
 *
 * <p>This class serves as a data structure construction kit, rather than a data
 * structure in its own right. By supplying a monoid, a measurement function,
 * insertion, deletion, and so forth, any purely functional data structure can
 * be emulated.
 *
 * <p>Based on "Finger trees: a simple general-purpose data structure", by Ralf
 * Hinze and Rose Paterson.
 *
 * @param <V> The monoidal type with which to annotate nodes
 * @param <A> The type of the tree's elements
 */
public interface FingerTree<V, A> extends Foldable<A>, Forcible<FingerTree<V,A>> {
    /**
     * This interface has two purposes:
     *
     * <ul>
     * <li>Construct FingerTree instances.</li>
     * <li>Determines how the elements of a tree are measured and how measures are
     * summed. Consists of a monoid and a measuring function. Different instances
     * of this class will result in different behaviours for the tree.</li>
     * </ul>
     */
    interface Maker<V, A> {
        /**
         * Measure a given element.
         *
         * @param a an element to measure
         * @return the element's measurement
         */
        V measure(A a);

        /**
         * Returns the identity measurement for the monoid.
         *
         * @return the identity measurement for the monoid
         */
        V zero();

        /**
         * Sums the given measurements with the monoid.
         *
         * @param a a measurement to add to another
         * @param b a measurement to add to another
         * @return the sum of the two measurements
         */
        V sum(V a, V b);

        /**
         * Construct an empty tree.
         *
         * @return the empty tree
         */
        default FingerTree<V, A> empty() {
            throw new UnsupportedOperationException();
        }

        /**
         * Construct a singleton tree.
         *
         * @param value the single element for the tree
         * @return a tree with the given value as the single element
         */
        default FingerTree<V, A> singleton(A value) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * Construct a Maker instance for the element type, given a monoid and
     * a measuring function.
     *
     * @param monoid a monoid for the measures
     * @param measure a function with which to measure element values
     * @return a Maker instance for the given element type, that uses the given
     * measuring function.
     */
    static <V, A> Maker<V, A> maker(Monoid<V> monoid, Function<? super A, ? extends V> measure) {
        return new FingerTreeImpl.MonoidMaker<>(monoid, measure);
    }

    /**
     * Construct a Maker instance for the element type, given a Maker instance
     * as delegate.
     *
     * @param delegate a delegate maker object
     * @return a Maker instance for the given element type, the uses the the given
     * maker as delegate
     */
    static <V, A> Maker<V, A> maker(Maker<V, A> delegate) {
        return new FingerTreeImpl.DelegateMaker<>(delegate);
    }

    /**
     * Indicates whether this tree is empty.
     *
     * @return true if this tree is the empty tree, otherwise false.
     */
    boolean isEmpty();

    /**
     * Returns the sum of this tree's annotations.
     *
     * @return the sum of this tree's annotations
     */
    V measure();

    /**
     * Returns the first element of the tree, which must be non-empty.
     *
     * @return the first element of the tree
     * @throws NoSuchElementException if the tree is empty
     */
    A head();

    /**
     * Returns the last element of the tree, which must be non-empty.
     *
     * @return the last element of the tree
     * @throws NoSuchElementException if the tree is empty
     */
    A last();

    /**
     * Returns the elements after the head of the tree, which must be non-empty.
     *
     * @return the elements after the head of the tree
     * @throws NoSuchElementException if the tree is empty
     */
    FingerTree<V, A> tail();

    /**
     * Returns all the elements of the tree except the last one. The tree must be
     * non-empty.
     *
     * @return all the elements of the tree except the last one
     * @throws NoSuchElementException if the tree is empty
     */
    FingerTree<V, A> init();

    /**
     * Adds the given element to this tree as the first element.
     *
     * @param a the element to add to the front of this tree
     * @return a new tree with the given element at the front.
     */
    FingerTree<V, A> cons(A a);

    /**
     * Adds the given element to this tree as the last element.
     *
     * @param a the element to add to the end of this tree
     * @return a new tree with the given element at the end
     */
    FingerTree<V, A> snoc(A a);

    /**
     * Appends one finger tree to another.
     *
     * @param that a finger tree to append to this one
     * @return a new finger tree which is a concatenation of this tree and
     * the given tree
     */
    FingerTree<V, A> append(FingerTree<V, A> that);

    /**
     * Lookup an element in the tree at a point where the predicate on the
     * accumulated measure changes from 'false' to 'true'.
     *
     * @param p the predicate to test on the accumulated measure
     * @return an element at the satisfied point in the tree
     * @throws NoSuchElementException if no such point found
     */
    Maybe<A> lookup(Predicate<? super V> p);

    /**
     * Replaces the element in the tree at a point where the predicate on the
     * accumulated measure changes from 'false' to 'true'.
     *
     * <p>This method must be used caution where the measurement of the replaced
     * element must be identical to the old one.
     *
     * @param p the predicate to test on the accumulated measure
     * @param f a function that compute from old value to new value
     * @return a new tree with the given element replaced at the satisfied point
     * @throws NoSuchElementException if no such point found
     */
    FingerTree<V, A> modify(Predicate<? super V> p, UnaryOperator<A> f);

    /**
     * Split a tree at a point where the predicate on the accumulated
     * measure changes from 'false' to 'true'.
     *
     * @param p the predicate to test on the accumulated measure
     * @return a pair of splits tree
     */
    Pair<FingerTree<V, A>> split(Predicate<? super V> p);

    /**
     * Returns a tree with all elements skipped for which a predicate evaluate
     * to 'false'.
     *
     * @param p the predicate to test on the accumulated measure
     * @return a tree will all elements skipped for which a predicate evaluate
     * to 'false'.
     */
    default FingerTree<V, A> takeUntil(Predicate<? super V> p) {
        return split(p).first();
    }

    /**
     * Returns a tree with all elements dropped for which a predicate evaluate
     * to 'false'.
     *
     * @param p the predicate to test on the accumulated measure
     * @return a tree will all elements dropped for which a predicate evaluate
     * to 'false'.
     */
    default FingerTree<V, A> dropUntil(Predicate<? super V> p) {
        return split(p).second();
    }

    /**
     * Maps the given function across this tree, measuring with the given Measured
     * instance.
     *
     * @param f a function to map across the values of this tree
     * @param m a measuring with which to annotate the tree
     */
    <B> FingerTree<V, B> map(Function<? super A, ? extends B> f, Maker<V, B> m);

    /**
     * Reverse elements in this tree.
     *
     * @return a reversed tree
     */
    FingerTree<V, A> reverse();
}
