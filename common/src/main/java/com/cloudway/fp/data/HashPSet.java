/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.data;

/**
 * A set implementation based on hash trie.
 *
 * @param <E> the type of set elements
 */
public interface HashPSet<E> extends PSet<E> {
    /**
     * Construct an empty set.
     */
    static <E> PSet<E> empty() {
        return TSetImpl.empty();
    }

    /**
     * Construct a set with a single element.
     */
    static <E> PSet<E> singleton(E e) {
        return TSetImpl.singleton(e);
    }

    /**
     * Construct a set with given elements.
     */
    @SafeVarargs
    static <E> PSet<E> of(E... elements) {
        PSet<E> res = empty();
        for (E e : elements) {
            res = res.add(e);
        }
        return res;
    }

    /**
     * Construct a set from a list.
     */
    static <E> PSet<E> fromList(Seq<E> list) {
        return list.foldLeft(empty(), PSet::add);
    }

    /**
     * Returns the set monoid.
     */
    static <E> Monoid<PSet<E>> monoid() {
        return Monoid.monoid_(empty(), PSet::union);
    }
}
