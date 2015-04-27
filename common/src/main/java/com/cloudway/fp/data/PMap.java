/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.data;

import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.BiPredicate;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

import com.cloudway.fp.function.TriFunction;
import com.cloudway.fp.control.Applicative;
import com.cloudway.fp.$;

/**
 * The PMap (P stands for Pure or Persistent) is an analogy of java.util.Map.
 *
 * @param <K> the type of keys maintained by this map
 * @param <V> the type of mapped values
 */
public interface PMap<K, V> extends Iterable<Map.Entry<K, V>> {

    // Query Operations

    /**
     * Returns {@code true} if this map contains no key-value mappings.
     *
     * @return {@code true} if this map contains no key-value mappings
     */
    boolean isEmpty();

    /**
     * Returns the number of key-value mappings in this map.
     *
     * @return the number of key-value mappings in this map
     */
    int size();

    /**
     * Returns {@code true} if this map contains a mapping for the specified
     * key.
     *
     * @param key key whose presence in this map is to be tested
     * @return {@code true} if this map contains a mapping for the specified key
     */
    default boolean containsKey(Object key) {
        return lookup(key).isPresent();
    }

    /**
     * Lookup the value to which the specified key is mapped.  Returns
     * an empty {@code Maybe} if this map contains no mapping for the key.
     *
     * @param key the key whose associated value is to be returned
     * @return the value to which the specified key is mapped, or
     *         an empty {@code Maybe} if this map contains no mapping
     *         for the key
     */
    Maybe<V> lookup(Object key);

    /**
     * Returns the value to which the specified key is mapped. If this map contains
     * no mapping for the key, a NoSuchElementException is thrown.
     *
     * @param key the key whose associated value is to returned
     * @return the value to which specified key is mapped
     * @throws NoSuchElementException if this map contains no mapping for the key
     */
    default V get(Object key) {
        return lookup(key).get();
    }

    /**
     * Returns the value to which the specified key is mapped, or default value
     * if this map contains no mapping for the key.
     *
     * @param key the key whose associated value is to be returned
     * @param def the default mapping of the key
     * @return the map that contains new mappings, the original map is unchanged
     */
    default V getOrDefault(Object key, V def) {
        return lookup(key).orElse(def);
    }

    /**
     * Search for an element that satisfy the given predicate.
     *
     * @param predicate the predicate to be tested on element
     * @return an empty {@code Maybe} if element not found in the map, otherwise
     * a {@code Maybe} wrapping the found element
     */
    Maybe<Map.Entry<K,V>> find(BiPredicate<? super K, ? super V> predicate);

    /**
     * Returns whether any elements of this map match the provided
     * predicate. May not evaluate the predicate on all elements if not
     * necessary for determining the result. If the map is empty then
     * {@code false} is returned and the predicate is not evaluated.
     *
     * @param predicate a predicate to apply to elements of this map
     * @return {@code true} if any elements of the map match the provided
     * predicate, other {@code false}
     */
    default boolean anyMatch(BiPredicate<? super K, ? super V> predicate) {
        return find(predicate).isPresent();
    }

    /**
     * Returns whether all elements of this map match the provided predicate.
     * May not evaluate the predicate on all elements if not necessary for
     * determining the result. If the map is empty then {@code true} is returned
     * and the predicate is not evaluated.
     *
     * @param predicate a predicate to apply to elements of this map
     * @return {@code true} if either all elements of the map match the
     * provided predicate or the set is empty, otherwise {@code false}
     */
    default boolean allMatch(BiPredicate<? super K, ? super V> predicate) {
        return !anyMatch(predicate.negate());
    }

    /**
     * Returns whether no elements of this map match the provided predicate.
     * May not evaluate the predicate on all elements if not necessary for
     * determining the result. If the map is empty then {@code true} is returned
     * and the predicate is not evaluated.
     *
     * @param predicate a predicate to apply to elements of this map
     * @return {@code true} if either no elements of the stream match the
     * provided predicate or the map is empty, otherwise {@code false}
     */
    default boolean noneMatch(BiPredicate<? super K, ? super V> predicate) {
        return !anyMatch(predicate);
    }

    // Modification Operations

    /**
     * Insert a new key and value in the map. If the key is already present
     * in the map, the associated value is replaced with the supplied value.
     *
     * @param key key with which the specified value is to be associated
     * @param value value to be associated with the specified key
     * @return the map that contains new mappings, the original map is unchanged
     */
    PMap<K, V> put(K key, V value);

    /**
     * Removes the mapping for a key from this map if it is present.
     *
     * @param key key with which the specified value is associated
     * @return the map that contains new mappings, the original map is unchanged
     */
    PMap<K, V> remove(Object key);

    /**
     * Replaces the entry for the specified key only if currently mapped to the
     * specified value.
     *
     * @param key key with which the specified value is associated
     * @param oldValue value expected to be associated with the specified key
     * @param newValue value to be associated with the specified key
     */
    default PMap<K, V> replace(K key, V oldValue, V newValue) {
        return computeIfPresent(key, (k, v) ->
            Maybe.of(v.equals(oldValue) ? newValue : oldValue));
    }

    /**
     * Replaces the entry for the specified key only if it is currently mapped
     * to some value.
     *
     * @param key key with which the specified value is associated
     * @param value value to be associated with the specified key
     */
    default PMap<K, V> replace(K key, V value) {
        return computeIfPresent(key, (k, v) -> Maybe.of(value));
    }

    /**
     * Insert a new key and value in the map if it is not already present.
     *
     * @param key key with which the specified value is to be associated
     * @param value value to be associated with the specified key
     * @return the map that contains new mappings, the original map is unchanged
     */
    default PMap<K, V> putIfAbsent(K key, V value) {
        return containsKey(key) ? this : put(key, value);
    }

    /**
     * If the specified key is not already associated with a value, attempt to
     * compute its value using the given mapping function and enters it into
     * this map.
     *
     * @param key key with which the specified value is to be associated
     * @param mappingFunction the function to compute a value
     * @return the map that contains new mappings, the original map is unchanged
     */
    default PMap<K, V> computeIfAbsent(K key, Function<? super K, ? extends V> mappingFunction) {
        if (containsKey(key)) {
            return this;
        } else {
            return put(key, mappingFunction.apply(key));
        }
    }

    /**
     * If the value for the specified key is present, attempt to compute a new
     * mapping given the key and its current mapped value.
     *
     * <p>If the function returns an empty {@code Maybe}, the mapping is removed.
     * If the function itself throws an (unchecked) exception, the exception
     * is rethrown.</p>
     *
     * @param key key with which the specified value is to be associated
     * @param remappingFunction the function to compute a value
     */
    default PMap<K, V> computeIfPresent(K key,
            BiFunction<? super K, ? super V, Maybe<? extends V>> remappingFunction) {
        Maybe<V> oldValue = lookup(key);
        if (oldValue.isPresent()) {
            Maybe<? extends V> newValue = remappingFunction.apply(key, oldValue.get());
            if (newValue.isPresent()) {
                return put(key, newValue.get());
            } else {
                return remove(key);
            }
        } else {
            return this;
        }
    }

    /**
     * Attempts to compute a mapping for the specified key and its current mapped
     * mapped value.
     *
     * <p>If the function returns an empty {@code Maybe}, the mapping is removed
     * (or remains absent if initially absent). If the function itself throws an
     * (unchecked) exception, the exception is rethrown.</p>
     *
     * @param key key with which the specified value is to be associated
     * @param remappingFunction the function to compute a value
     */
    default PMap<K, V> compute(K key,
            BiFunction<? super K, Maybe<V>, Maybe<? extends V>> remappingFunction) {
        Maybe<V> oldValue = lookup(key);
        Maybe<? extends V> newValue = remappingFunction.apply(key, oldValue);
        if (newValue.isPresent()) {
            return put(key, newValue.get());
        } else if (oldValue.isPresent()) {
            return remove(key);
        } else {
            return this;
        }
    }

    /**
     * If the specified key is not already associated with a value, associate
     * it with the given value. Otherwise, replaces the associated value with
     * the result of the given remapping function. This method may be of use
     * when combining multiple mapped values for a key. For example, to either
     * create or append a {@code String msg} to a value mapping:
     *
     * <pre> {@code
     * map.merge(key, msg, String::concat)
     * }</pre>
     *
     * @param key key with which the resulting value is to be associated
     * @param value the non-null value to be merged with the existing value
     *        associated with the key or, if no existing value is associated
     *        with the key, to be associated with the key
     * @param remappingFunction the function to recompute a value if present
     * @return the map that contains new mappings, the original map is unchanged
     */
    default PMap<K, V> merge(K key, V value,
            BiFunction<? super V, ? super V, ? extends V> remappingFunction) {
        Maybe<V> oldValue = lookup(key);
        V newValue = oldValue.isPresent()
            ? remappingFunction.apply(oldValue.get(), value)
            : value;
        return put(key, newValue);
    }

    // Bulk Operations

    /**
     * Returns <tt>true</tt> if this map contains all of the mappings of the
     * specified map.
     *
     * @param m map to be checked for containment in this map
     * @return <tt>true</tt> if this map contains all of the elements of the
     *         specified map
     */
    boolean containsAll(PMap<? extends K, ? extends V> m);

    /**
     * Unions all of the mappings from the specified map to this map.
     *
     * @param m mappings to be union in this map
     * @return the union of two maps
     */
    PMap<K, V> putAll(PMap<? extends K, ? extends V> m);

    /**
     * Returns an empty map.
     *
     * @return an empty map
     */
    PMap<K, V> clear();

    /**
     * Replaces each entry's value with the result of invoking the given
     * function on that entry's value until all entries have been processed.
     *
     * @param f the function to apply to each entry value
     */
    default <R> PMap<K,R> map(Function<? super V, ? extends R> f) {
        return mapKV((k, v) -> f.apply(v));
    }

    /**
     * Replaces each entry's value with the result of invoking the given
     * function on that entry's key and value until all entries have been
     * processed.
     *
     * @param f the function to apply to each entry
     */
    <R> PMap<K,R> mapKV(BiFunction<? super K, ? super V, ? extends R> f);

    /**
     * Map each element of this map to an action, evaluate these actions from
     * left to right, and collect the results.  (Optional operation).
     *
     * @param m an {@code Applicative} to collect the result
     * @param f a function maps key and value to an action
     * @throws UnsupportedOperationException if this operation is not supported
     */
    default <R, T> $<T, PMap<K,R>> traverse(Applicative<T> m,
            BiFunction<? super K, ? super V, ? extends $<T, R>> f) {
        throw new UnsupportedOperationException();
    }

    /**
     * Returns a map consisting of the mappings of this map that matches
     * the given predicate.
     *
     * @param predicate a predicate to apply to each map entry value to
     * determine if it should be included
     */
    default PMap<K,V> filter(Predicate<? super V> predicate) {
        return filterKV((k, v) -> predicate.test(v));
    }

    /**
     * Returns a map consisting of the mappings of this map that matches
     * the given predicate.
     *
     * @param predicate a predicate to apply to each map entry key and value
     * to determine if it should be included
     */
    PMap<K,V> filterKV(BiPredicate<? super K, ? super V> predicate);

    /**
     * Removes all of the mappings of this map that satisfy the given predicate.
     *
     * @param predicate a predicate which returns {@code true} for mappings to
     * be removed
     */
    default PMap<K,V> removeIf(BiPredicate<? super K, ? super V> predicate) {
        return filterKV(predicate.negate());
    }

    /**
     * Removes all of the mappings of this map that the key satisfy the given
     * predicate.
     *
     * @param predicate a predicate which returns {@code true} for keys to be
     * removed
     */
    default PMap<K,V> removeKeys(Predicate<? super K> predicate) {
        return filterKV((k, v) -> !predicate.test(k));
    }

    /**
     * Removes all of the mappings of this map that the value satisfy the given
     * predicate.
     *
     * @param predicate a predicate which returns {@code true} for values to be
     * removed
     */
    default PMap<K,V> removeValues(Predicate<? super V> predicate) {
        return filter(predicate.negate());
    }

    /**
     * Removes all of the mappings of this map that the value equals to the given
     * value.
     *
     * @param value a value for which to be removed
     */
    default PMap<K,V> removeValues(V value) {
        return filter(x -> !Objects.equals(x, value));
    }

    /**
     * Perform the given action for each entry in this map until all entries
     * have been processed or the action throws an exception.
     *
     * @param action an action to perform on each elements
     */
    default void forEach(BiConsumer<? super K, ? super V> action) {
        foldLeftKV(Unit.U, (z, k, v) -> { action.accept(k, v); return z; });
    }

    // Views

    /**
     * Returns a set of the keys contained in this map.
     *
     * @return a set of the keys contained in this map
     */
    PSet<K> keySet();

    /**
     * Returns a list of the keys contained in this map.
     *
     * @return a list of the keys contained in this map
     */
    default Seq<K> keys() {
        return foldRightKV(Seq.nil(), (k, v, ks) -> Seq.cons(k, ks));
    }

    /**
     * Returns a list of of the values contained in this map.
     *
     * @return a list of of the values contained in this map
     */
    default Seq<V> values() {
        return foldRight(Seq.nil(), Seq::cons);
    }

    /**
     * Returns a list of the mappings contained in this map.
     *
     * @return a list of the mappings contained in this map
     */
    Seq<Map.Entry<K,V>> entries();

    // Folding

    /**
     * Reduce the map elements using the accumulator function that accept
     * element value, from left to right.
     */
    <R> R foldLeft(R seed, BiFunction<R, ? super V, R> accumulator);

    /**
     * Reduce the map elements using the accumulator function that accept
     * element key and value, from left to right.
     */
    <R> R foldLeftKV(R seed, TriFunction<R, ? super K, ? super V, R> accumulator);

    /**
     * Reduce the map elements using the accumulator function that accept
     * element value, from right to left. This is a lazy operation so the
     * accumulator accept a delay evaluation of reduced result instead of
     * a strict value.
     */
    <R> R foldRight(BiFunction<? super V, Supplier<R>, R> accumulator, Supplier<R> partial);

    /**
     * Reduce the map elements using the accumulator function that accept
     * element value, from right to left. This is a lazy operation so the
     * accumulator accept a delay evaluation of reduced result instead of
     * a strict value.
     */
    default <R> R foldRight(R seed, BiFunction<? super V, Supplier<R>, R> accumulator) {
        return foldRight(accumulator, () -> seed);
    }

    /**
     * Reduce the map elements using the accumulator function that accept
     * element key and value, from right to left. This is a lazy operation
     * so the accumulator accept a delay evaluation of reduced result instead
     * of a strict value.
     */
    <R> R foldRightKV(TriFunction<? super K, ? super V, Supplier<R>, R> accumulator, Supplier<R> partial);

    /**
     * Reduce the map elements using the accumulator function that accept
     * element key and value, from right to left. This is a lazy operation
     * so the accumulator accept a delay evaluation of reduced result instead
     * of a strict value.
     */
    default <R> R foldRightKV(R seed, TriFunction<? super K, ? super V, Supplier<R>, R> accumulator) {
        return foldRightKV(accumulator, () -> seed);
    }

    /**
     * The strict version of {@link #foldRight(Object,BiFunction) foldRight}.
     */
    default <R> R foldRight_(R seed, BiFunction<? super V, R, R> accumulator) {
        Seq<V> rev = foldLeft(Seq.nil(), Fn.flip(Seq::cons));
        return rev.foldLeft(seed, (r, x) -> accumulator.apply(x, r));
    }

    /**
     * the strict version of {@link #foldRightKV(Object,TriFunction) foldRightKV}.
     */
    default <R> R foldRightKV_(R seed, TriFunction<? super K, ? super V, R, R> accumulator) {
        Seq<Tuple<K,V>> rev = foldLeftKV(Seq.nil(), (r, k, v) -> Seq.cons(Tuple.of(k, v), r));
        return rev.foldLeft(seed, (r, t) -> accumulator.apply(t.first(), t.second(), r));
    }
}
