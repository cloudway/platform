/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util;

import java.util.HashMap;
import java.util.Map;
import java.util.function.BiConsumer;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.function.ToIntFunction;
import java.util.stream.Collector;
import static java.util.stream.Collector.Characteristics.*;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableMultimap;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Multimap;

/**
 * Implementations of {@link Collector} that implement various useful reduction
 * operations, such as accumulating elements into immutable collections.
 *
 * <p>The following are examples of using the predefined collectors to perform
 * common immutable reduction tasks:
 *
 * <pre>{@code
 *     // Accumulate names into a ImmutableList
 *     List<String> list = people.stream().map(Person::getName).collect(MoreCollectors.toImmutableList());
 *
 *     // Accumulate names into a ImmutableSortedSet
 *     Set<String> set = people.stream().map(Person::getName).collect(
 *         MoreCollectors.toImmutableCollection(ImmutableSortedSet::<String>naturalOrder);
 * }</pre>
 */
public final class MoreCollectors
{
    private MoreCollectors() {}

    /**
     * Returns a merge function which always throws {@code IllegalStateException}.
     * This can be used to enforce the assumption that the elements being collected
     * are distinct.
     *
     * @param <T> the type of input arguments to the merge function
     * @return a merge function which always throw {@code IllegalStateException}
     */
    @SuppressWarnings("unchecked")
    private static <T> BinaryOperator<T> throwingMerger() {
        return (u,v) -> { throw new IllegalStateException(String.format("Duplicate key %s", u)); };
    }

    /**
     * Returns a {@code Collector} that accumulates the input elements into a
     * new {@code ImmutableCollection}, in encounter order. The {@code ImmutableCollection}
     * is created by the provided builder factory.
     *
     * @param <T> the type of the input elements
     * @param <B> the type of the immutable collection builder
     * @param <C> the type of the resulting immutable collection
     * @param builderSupplier a {@code Supplier} which returns a immutable collection builder
     * @return a {@code Collector} which collects all the input elements into a
     * {@code ImmutableCollection}, in encounter order
     */
    @SuppressWarnings("unchecked")
    public static <T, B extends ImmutableCollection.Builder<T>, C extends ImmutableCollection<T>>
    Collector<T, B, C> toImmutableCollection(Supplier<B> builderSupplier) {
        return Collector.of(builderSupplier, ImmutableCollection.Builder::add,
                            (l, r) -> (B)l.addAll(r.build()),
                            b -> (C)b.build());
    }

    /**
     * Returns a {@code Collector} that accumulates the input elements into a
     * new {@code ImmutableList}.
     *
     * @param <T> the type of the input elements
     * @return a {@code Collector} which collects all the input elements into a
     * {@code ImmutableList}, in encounter order
     */
    public static <T> Collector<T, ImmutableList.Builder<T>, ImmutableList<T>> toImmutableList() {
        return Collector.of(ImmutableList::builder, ImmutableList.Builder::add,
                            (l, r) -> l.addAll(r.build()),
                            ImmutableList.Builder::build);
    }

    /**
     * Returns a {@code Collector} that accumulates the input elements into a
     * new {@code ImmutableSet}.
     *
     * @param <T> the type of the input elements
     * @return a {@code Collector} which collects all the input elements into a
     * {@code ImmutableSet}
     */
    public static <T> Collector<T, ImmutableSet.Builder<T>, ImmutableSet<T>> toImmutableSet() {
        return Collector.of(ImmutableSet::builder, ImmutableSet.Builder::add,
                            (l, r) -> l.addAll(r.build()),
                            ImmutableSet.Builder::build);
    }

    /**
     * Returns a {@code Collector} that accumulates elements into a
     * {@code ImmutableMap} whose keys and values are the result of applying
     * the provided mapping functions to the input elements.
     *
     * @param <T> the type of the input elements
     * @param <K> the output type of the key mapping function
     * @param <U> the output type of the value mapping function
     * @param keyMapper a mapping function to produce keys
     * @param valueMapper a mapping function to produce values
     * @return a {@code Collector} which collects elements into a {@code ImmutableMap}
     * whose keys are the result of applying a key mapping function to the input
     * elements, and whose values are the result of applying a value mapping
     * function to all input elements equal to the key
     */
    public static <T, K, U> Collector<T, ?, ImmutableMap<K,U>>
    toImmutableMap(Function<? super T, ? extends K> keyMapper,
                   Function<? super T, ? extends U> valueMapper) {
        return toImmutableMap(keyMapper, valueMapper, ImmutableMap::builder);
    }

    /**
     * Returns a {@code Collector} that accumulates elements into a
     * {@code ImmutableMap} whose keys and values are the result of applying
     * the provided mapping functions to the input elements.
     *
     * @param <T> the type of the input elements
     * @param <K> the output type of the key mapping function
     * @param <U> the output type of the value mapping function
     * @param <M> the type of the resulting {@code ImmutableMap}
     * @param keyMapper a mapping function to produce keys
     * @param valueMapper a mapping function to produce values
     * @param builderSupplier a function which returns a new map builder
     * @return a {@code Collector} which collects elements into a {@code ImmutableMap}
     * whose keys are the result of applying a key mapping function to the input
     * elements, and whose values are the result of applying a value mapping
     * function to all input elements equal to the key
     */
    @SuppressWarnings("unchecked")
    public static <T, K, U, B extends ImmutableMap.Builder<K,U>, M extends ImmutableMap<K,U>>
    Collector<T, B, M> toImmutableMap(Function<? super T, ? extends K> keyMapper,
                                      Function<? super T, ? extends U> valueMapper,
                                      Supplier<B> builderSupplier) {
        return Collector.of(builderSupplier,
                            (b, t) -> b.put(keyMapper.apply(t), valueMapper.apply(t)),
                            (l, r) -> (B)l.putAll(r.build()),
                            b -> (M)b.build());
    }

    /**
     * Returns a {@code Collector} implementing a "group by" operation on
     * input elements of type {@code T}, grouping elements according to a
     * classification function, and returning the results in a {@code Multimap}.
     *
     * <p>The classification function maps elements to some key type {@code K}.
     * The collector produces a {@code Multimap&lt;K, T&gt;} whose keys are the
     * values resulting from applying the classification function to the input
     * elements, and whose corresponding values are input elements which map to
     * the associated key under the classification function.
     *
     * @param <T> the type of the input elements
     * @param <K> the type of the keys
     * @param classifier the classifier function mapping input elements to keys
     */
    public static <T, K> Collector<T, ?, Multimap<K, T>>
    toMultimap(Function<? super T, ? extends K> classifier) {
        return toMultimap(classifier, Function.identity(), HashMultimap::<K,T>create);
    }

    /**
     * Returns a {@code Collector} that accumulates elements into a
     * {@code Multimap} whose keys and values are the result of applying
     * the provided mapping functions to the input elements.
     *
     * @param <T> the type of the input elements
     * @param <K> the output type of the key mapping function
     * @param <U> the output type of the value mapping function
     * @param keyMapper a mapping function to produce keys
     * @param valueMapper a mapping function to produce values
     * @return a {@code Collector} which collects elements into a {@code Multimap}
     * whose keys are the result of applying a key mapping function to the input
     * elements, and whose values are the result of applying a value mapping
     * function to all input elements equal to the key
     */
    public static <T, K, U> Collector<T, ?, Multimap<K,U>>
    toMultimap(Function<? super T, ? extends K> keyMapper,
               Function<? super T, ? extends U> valueMapper) {
        return toMultimap(keyMapper, valueMapper, HashMultimap::<K,U>create);
    }

    /**
     * Returns a {@code Collector} that accumulates elements into a
     * {@code Multimap} whose keys and values are the result of applying
     * the provided mapping functions to the input elements.
     *
     * @param <T> the type of the input elements
     * @param <K> the output type of the key mapping function
     * @param <U> the output type of the value mapping function
     * @param <M> the type of the resulting {@code Multimap}
     * @param keyMapper a mapping function to produce keys
     * @param valueMapper a mapping function to produce values
     * @param mapSupplier a function which returns a new Multimap
     * @return a {@code Collector} which collects elements into a {@code Multimap}
     * whose keys are the result of applying a key mapping function to the input
     * elements, and whose values are the result of applying a value mapping
     * function to all input elements equal to the key
     */
    public static <T, K, U, M extends Multimap<K,U>>
    Collector<T, ?, M> toMultimap(Function<? super T, ? extends K> keyMapper,
                                  Function<? super T, ? extends U> valueMapper,
                                  Supplier<M> mapSupplier) {
        return Collector.of(mapSupplier,
                            (m, t) -> m.put(keyMapper.apply(t), valueMapper.apply(t)),
                            (l, r) -> { l.putAll(r); return l; },
                            IDENTITY_FINISH);
    }

    /**
     * Returns a {@code Collector} implementing a "group by" operation on
     * input elements of type {@code T}, grouping elements according to a
     * classification function, and returning the results in a
     * {@code ImmutableMultimap}.
     *
     * <p>The classification function maps elements to some key type {@code K}.
     * The collector produces a {@code ImmutableMultimap&lt;K, T&gt;} whose keys are the
     * values resulting from applying the classification function to the input
     * elements, and whose corresponding values are input elements which map to
     * the associated key under the classification function.
     *
     * @param <T> the type of the input elements
     * @param <K> the type of the keys
     * @param classifier the classifier function mapping input elements to keys
     */
    public static <T, K> Collector<T, ?, ImmutableMultimap<K, T>>
    toImmutableMultimap(Function<? super T, ? extends K> classifier) {
        return toImmutableMultimap(classifier, Function.identity(), ImmutableMultimap::builder);
    }

    /**
     * Returns a {@code Collector} that accumulates elements into a
     * {@code ImmutableMultimap} whose keys and values are the result of applying
     * the provided mapping functions to the input elements.
     *
     * @param <T> the type of the input elements
     * @param <K> the output type of the key mapping function
     * @param <U> the output type of the value mapping function
     * @param keyMapper a mapping function to produce keys
     * @param valueMapper a mapping function to produce values
     * @return a {@code Collector} which collects elements into a {@code ImmutableMultimap}
     * whose keys are the result of applying a key mapping function to the input
     * elements, and whose values are the result of applying a value mapping
     * function to all input elements equal to the key
     */
    public static <T, K, U> Collector<T, ?, ImmutableMultimap<K,U>>
    toImmutableMultimap(Function<? super T, ? extends K> keyMapper,
                        Function<? super T, ? extends U> valueMapper) {
        return toImmutableMultimap(keyMapper, valueMapper, ImmutableMultimap::builder);
    }

    /**
     * Returns a {@code Collector} that accumulates elements into a
     * {@code ImmutableMultimap} whose keys and values are the result of applying
     * the provided mapping functions to the input elements.
     *
     * @param <T> the type of the input elements
     * @param <K> the output type of the key mapping function
     * @param <U> the output type of the value mapping function
     * @param <M> the type of the resulting {@code ImmutableMultimap}
     * @param keyMapper a mapping function to produce keys
     * @param valueMapper a mapping function to produce values
     * @param builderSupplier a function which returns a new ImmutableMultimap
     * @return a {@code Collector} which collects elements into a {@code ImmutableMultimap}
     * whose keys are the result of applying a key mapping function to the input
     * elements, and whose values are the result of applying a value mapping
     * function to all input elements equal to the key
     */
    @SuppressWarnings({"unchecked", "SerializableClassWithUnconstructableAncestor"})
    public static <T, K, U, B extends ImmutableMultimap.Builder<K,U>, M extends ImmutableMultimap<K,U>>
    Collector<T, B, M> toImmutableMultimap(Function<? super T, ? extends K> keyMapper,
                                           Function<? super T, ? extends U> valueMapper,
                                           Supplier<B> builderSupplier) {
        return Collector.of(builderSupplier,
                            (b, t) -> b.put(keyMapper.apply(t), valueMapper.apply(t)),
                            (l, r) -> (B)l.putAll(r.build()),
                            b -> (M)b.build());
    }

    /**
     * Returns a {@code Collector} that accumulates string elements into a
     * {@code Map} whose keys and values are the result of applying the provided
     * splitter and mapping functions to the input elements.
     *
     * @param <K> the output type of the key mapping function
     * @param <V> the output type of the value mapping function
     * @param <M> the type of the resulting {@code Map}
     * @param splitter a function to split input string into key and value
     * @param keyMapper a mapping function to produce keys
     * @param valueMapper a mapping function to produce values
     * @param mergeFunction a merge function, used to resolve collisions between
     *                      values associated with the same key, as supplied
     *                      to {@link Map#merge(Object, Object, java.util.function.BiFunction)}
     * @param mapSupplier a function which returns a new, empty {@code Map} into
     *                    which the results will be inserted
     * @return a {@code Collector} which collects string elements into a {@code Map}
     */
    public static <K, V, M extends Map<K,V>>
    Collector<String, ?,M> toSplittingMap(ToIntFunction<String> splitter,
                                          Function<String, ? extends K> keyMapper,
                                          Function<String, ? extends V> valueMapper,
                                          BinaryOperator<V> mergeFunction,
                                          Supplier<M> mapSupplier) {
        BiConsumer<M, String> accumulator =
            (map, element) -> {
                int i = splitter.applyAsInt(element);
                if (i != -1) {
                    K k = keyMapper.apply(element.substring(0, i));
                    V v = valueMapper.apply(element.substring(i + 1));
                    if (k != null && v != null) {
                        map.merge(k, v, mergeFunction);
                    }
                }
            };

        BinaryOperator<M> mapMerger =
            (m1, m2) -> {
                for (Map.Entry<K,V> e : m2.entrySet())
                    m1.merge(e.getKey(), e.getValue(), mergeFunction);
                return m1;
            };

        return Collector.of(mapSupplier, accumulator, mapMerger, IDENTITY_FINISH);
    }

    /**
     * The simplest implementation of a splitting map by split the string by the
     * given character.
     *
     * @param c the character to split string into key and value
     * @return a splitting map
     */
    public static Collector<String, ?, Map<String,String>> toSplittingMap(char c) {
        return toSplittingMap(splitByChar(c), String::trim, String::trim, throwingMerger(), HashMap::new);
    }

    /**
     * Returns a {@code Collector} that accumulates string elements into a
     * {@code ImmutableMap} whose keys and values are the result of applying the provided
     * splitter and mapping functions to the input elements.
     *
     * @param <K> the output type of the key mapping function
     * @param <V> the output type of the value mapping function
     * @param <M> the type of the resulting {@code ImmutableMap}
     * @param splitter a function to split input string into key and value
     * @param keyMapper a mapping function to produce keys
     * @param valueMapper a mapping function to produce values
     * @param builderSupplier a function which returns a new {@code ImmutableMap.Builder} into
     *                    which the results will be inserted
     * @return a {@code Collector} which collects string elements into a {@code ImmutableMap}
     */
    @SuppressWarnings("unchecked")
    public static <K, V, B extends ImmutableMap.Builder<K,V>, M extends ImmutableMap<K,V>>
    Collector<String, B, M> toImmutableSplittingMap(ToIntFunction<String> splitter,
                                                    Function<String, ? extends K> keyMapper,
                                                    Function<String, ? extends V> valueMapper,
                                                    Supplier<B> builderSupplier) {
        BiConsumer<B, String> accumulator =
            (builder, element) -> {
                int i = splitter.applyAsInt(element);
                if (i != -1) {
                    K k = keyMapper.apply(element.substring(0, i));
                    V v = valueMapper.apply(element.substring(i + 1));
                    if (k != null && v != null) {
                        builder.put(k, v);
                    }
                }
            };

        return Collector.of(builderSupplier, accumulator,
                            (l,r) -> (B)l.putAll(r.build()),
                            b -> (M)b.build());
    }

    /**
     * The simplest implementation of a immutable splitting map by split the string
     * by the given character.
     *
     * @param c the character to split string into key and value
     * @return an immutable splitting map
     */
    public static Collector<String, ?, ImmutableMap<String,String>> toImmutableSplittingMap(char c) {
        return toImmutableSplittingMap(splitByChar(c), String::trim, String::trim, ImmutableMap::builder);
    }

    private static ToIntFunction<String> splitByChar(char c) {
        return s -> s.indexOf(c);
    }
}
