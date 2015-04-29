/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.control.monad.trans;

import java.util.function.Function;

import com.cloudway.fp.$;
import com.cloudway.fp.data.Maybe;
import com.cloudway.fp.data.PMap;
import com.cloudway.fp.data.Tuple;
import com.cloudway.fp.data.Unit;

/**
 * Shallow monad transformer for dealing with bindings.
 */
public interface MonadBind<M, K, V> extends MonadState<M, PMap<K, V>> {
    /**
     * Returns monadic {@code true} if the state contains a mapping for the
     * specified key.
     */
    default $<M, Boolean> conatainsKey(K key) {
        return gets(b -> b.containsKey(key));
    }

    /**
     * Lookup the monadic value to which the specified key is mapped.
     */
    default $<M, Maybe<V>> lookup(K key) {
        return gets(b -> b.lookup(key));
    }

    /**
     * Returns the monadic value to which the specified key is mapped. If the
     * state contains no mapping for the key, {@link #fail} is returned.
     */
    default $<M, V> get(K key) {
        return bind(lookup(key), v ->
            v.isPresent() ? pure(v.get())
                          : fail(key + ": no such element"));
    }

    /**
     * Returns the monadic value to which the specified key is mapped, or default
     * value if the state contains no mapping for the key.
     */
    default $<M, V> getOrDefault(K key, V defaultValue) {
        return gets(b -> b.getOrDefault(key, defaultValue));
    }

    /**
     * Insert a new key and value in the state.
     */
    default $<M, Unit> put(K key, V value) {
        return modify(b -> b.put(key, value));
    }

    /**
     * Removes the mapping for a key from state if it is present.
     */
    default $<M, Unit> remove(K key) {
        return modify(b -> b.remove(key));
    }

    /**
     * Removes all mappings from state.
     */
    default $<M, Unit> clear() {
        return modify(PMap::clear);
    }

    /**
     * If the specified key is not already associated with a value, attempt to
     * compute its value using the given mapping function and enters it into
     * bindings.
     */
    default $<M, V> memo(K key, Function<? super K, ? extends $<M, V>> f) {
        return bind(lookup(key),  old_v -> old_v.isPresent() ? pure(old_v.get()) :
               bind(f.apply(key), new_v -> state(b -> Tuple.of(new_v, b.put(key, new_v)))));
    }
}
