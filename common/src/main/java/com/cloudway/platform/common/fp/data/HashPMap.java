/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.data;

/**
 * A map implementation based on hash trie.
 *
 * @param <K> the type of keys maintained by this map
 * @param <V> the type of mapped values
 */
public interface HashPMap<K, V> extends PMap<K, V> {
    /**
     * Construct an empty hash trie map.
     *
     * @return an empty map
     */
    static <K, V> PMap<K, V> empty() {
        return TMapImpl.empty();
    }

    /**
     * Construct a map with a single element.
     *
     * @param key the element key
     * @param value the element value
     * @return a map with a single element
     */
    static <K, V> PMap<K, V> singleton(K key, V value) {
        return TMapImpl.singleton(key, value);
    }

    /**
     * A collision resolution function.
     */
    interface MergeFunction<K, V> {
        V apply(K key, V oldValue, V newValue);
    }

    /**
     * Merges all of the mappings from the specified map to this map, using the
     * given remapping function.
     *
     * @param that mappings to be merged in this map
     * @param mergeFunction the function to compute a value if key collision from maps
     * @return the merged map
     */
    PMap<K,V> merge(PMap<K,V> that, MergeFunction<K,V> mergeFunction);
}
