/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.data;

import java.util.Comparator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.function.BiFunction;

/**
 * <p>An efficient implementation of maps from keys to values.</p>
 *
 * <p>The implementation of map is based on size balanced binary trees (or trees
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
 * @param <K> the type of keys maintained by this map
 * @param <V> the type of mapped values
 */
public interface TreePMap<K, V> extends PMap<K, V> {
    // Construction

    /**
     * Construct an empty map, sorted according to the natural ordering
     * of its elements.
     */
    @SuppressWarnings("unchecked")
    static <K extends Comparable<K>, V> PMap<K, V> empty() {
        return Tree.EMPTY_MAP;
    }

    /**
     * Construct an empty map, sorted according to the specified comparator.
     *
     * @param c the comparator that will be used to order this map
     * @throws NullPointerException if {@code c} is null
     */
    static <K, V> PMap<K, V> empty(Comparator<? super K> c) {
        Objects.requireNonNull(c);
        return new Tree.MapTip<>(c);
    }

    /**
     * Construct a map with a single element, sorted according to the natural
     * ordering of its elements.
     */
    static <K extends Comparable<K>, V> PMap<K, V> singleton(K key, V value) {
        return TreePMap.<K,V>empty().put(key, value);
    }

    /**
     * Construct a map with a single element, sorted according to the specified
     * comparator.
     *
     * @param c the comparator that will be used to order this map
     * @throws NullPointerException if {@code c} is null
     */
    static <K, V> PMap<K, V> singleton(Comparator<? super K> c, K key, V value) {
        return TreePMap.<K,V>empty(c).put(key, value);
    }

    // Navigation

    /**
     * Returns a key-value mapping associated with the greatest key strictly
     * less than the given key, or an empty {@code Maybe} if there is no
     * such key.
     *
     * @param key the key
     * @return an entry with the greatest key less than {@code key}, or
     *         an {@code Maybe} if there is no such key
     */
    Maybe<Map.Entry<K, V>> lowerEntry(K key);

    /**
     * Returns the greatest key strictly less than the given key, or
     * an empty {@code Maybe} if there is no such key.
     *
     * @param key the key
     * @return the greatest key less than {@code key}, or an empty {@code Maybe}
     *         if there is no such key
     */
    Maybe<K> lowerKey(K key);

    /**
     * Returns a key-value mapping associated with the greatest key less than
     * or equal to the given key, or an empty {@code Maybe} if there is no
     * such key.
     *
     * @param key the key
     * @return an entry with the greatest key less than or equal to {@code key},
     *         or an empty {@code Maybe} if there is no such key
     */
    Maybe<Map.Entry<K, V>> floorEntry(K key);

    /**
     * Returns the greatest key less than or equal to the given key, or
     * an {@code Maybe} if there is no such key.
     *
     * @param key the key
     * @return the greatest key less than or equal to {@code key}, or
     *         an empty {@code Maybe} if there is no such key
     */
    Maybe<K> floorKey(K key);

    /**
     * Returns a key-value mapping associated with the least key greater than
     * or equal to the given key, or an empty {@code Maybe} if there is no
     * such key.
     *
     * @param key the key
     * @return an entry with the least key greater than or equal to {@code key},
     *         or an empty {@code Maybe} if there is no such key
     */
    Maybe<Map.Entry<K, V>> ceilingEntry(K key);

    /**
     * Returns the least key greater than or equal to the given key, or
     * an empty {@code Maybe} if there is no such key.
     *
     * @param key the key
     * @return the least key greater than or equal to {@code key}, or
     *         an empty {@code Maybe} if there is no such key
     */
    Maybe<K> ceilingKey(K key);

    /**
     * Returns a key-value mapping associated with the least key strictly greater
     * than the given key, or an empty {@code Maybe} if there is no such key.
     *
     * @param key the key
     * @return an entry with the least key greater than {@code key}, or
     *         an empty {@code Maybe} if there is no such key
     */
    Maybe<Map.Entry<K, V>> higherEntry(K key);

    /**
     * Returns the least key strictly greater than the given key, or
     * an empty {@code Maybe} if there is no such key.
     *
     * @param key the key
     * @return the least key greater than {@code key}, or an empty {@code Maybe}
     *         if there is no such key
     */
    Maybe<K> higherKey(K key);

    /**
     * Returns a key-value mapping associated with the least key in this map.
     *
     * @return an entry with the least key
     * @throws NoSuchElementException if this map is empty
     */
    Map.Entry<K, V> firstEntry();

    /**
     * Returns the first (lowest) key currently in this map.
     *
     * @return the first (lowest) key currently in this map
     * @throws NoSuchElementException if this map is empty
     */
    K firstKey();

    /**
     * Returns a key-value mapping associated with the greatest key in this map.
     *
     * @return an entry with the greatest key
     * @throws NoSuchElementException if this map is empty
     */
    Map.Entry<K, V> lastEntry();

    /**
     * Returns the last (highest) key currently in this map.
     *
     * @return the last (highest) key currently in this map
     * @throws NoSuchElementException if this map is empty
     */
    K lastKey();

    // Debugging

    /**
     * Show the tree that implements the map. This method is used for debugging
     * purposes only.
     */
    String showTree();

    /**
     * Shows the tree that implements the map. Elements are shown using the
     * {@code showElem} function. This method is used for debugging purposes only.
     */
    String showTree(BiFunction<? super K, ? super V, String> showElem);

    /**
     * Test if the internal map structure is valid.
     */
    boolean valid();
}
