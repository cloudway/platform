/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.data;

import java.util.AbstractCollection;
import java.util.AbstractMap;
import java.util.AbstractSet;
import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.UnaryOperator;

/**
 * A thread-safe variant of {@link java.util.TreeMap} which backed by a purely
 * functional tree map.
 */
public class CopyOnWriteTreeMap<K,V> extends AbstractMap<K,V> implements java.io.Serializable {
    private static final long serialVersionUID = 1498708605633959313L;

    /**
     * The comparator used to maintain order in this tree map.
     *
     * @serial
     */
    private final Comparator<? super K> comparator;

    /**
     * The underlying purely functional tree map that accessed atomically.
     */
    private final transient AtomicReference<TreeMap<K,V>> tree = new AtomicReference<>();

    /**
     * Returns the backing tree that can be used purely.
     */
    public final TreeMap<K,V> getTree() {
        return tree.get();
    }

    /**
     * Update tree atomically.
     */
    final TreeMap<K,V> updateTree(UnaryOperator<TreeMap<K,V>> updater) {
        return tree.updateAndGet(updater);
    }

    /**
     * Update tree atomically and return previous value associated to the given key.
     */
    final V updateTree(K key, UnaryOperator<TreeMap<K,V>> updater) {
        TreeMap<K,V> prev = tree.getAndUpdate(updater);
        return prev.getOrDefault(key, null);
    }

    /**
     * Construct a new, empty tree map, using the natural ordering of its keys.
     * All keys inserted into the map must implement the {@link Comparable}
     * interface.  Furthermore, all such keys must be <em>mutually comparable</em>:
     * {@code k1.compareTo(k2)} must not throw a {@code ClassCastException} for
     * any keys {@code k1} and {@code k2} in the map.  If the user attempts to
     * put a key into the map that violates this constraint (for example, the
     * user attempts to put a string key into a map whose keys are integers),
     * the {@code put(Object key, Object value)} call will throw a
     * {@code ClassCastException}.
     */
    @SuppressWarnings("unchecked")
    public CopyOnWriteTreeMap() {
        this((Comparator<? super K>)Comparator.naturalOrder());
    }

    /**
     * Constructs a new, empty tree map, ordered according to the given comparator.
     * All keys inserted into the map must be <em>mutually comparable</em> by the
     * given comparator: {@code comparator.compare(k1,k2)} must not throw a
     * {@code ClassCastException} for any keys {@code k1} and {@code k2} in the map.
     * If the user attempts to put a key into the map that violates this constraint,
     * the {@code put(Object key, Object value)} call will throw a
     * {@code ClassCastException}.
     *
     * @param comparator the comparator that will be used to order this map.
     */
    public CopyOnWriteTreeMap(Comparator<? super K> comparator) {
        Objects.requireNonNull(comparator);
        this.comparator = comparator;
        tree.set(TreeMap.empty(comparator));
    }

    @Override
    public boolean isEmpty() {
        return getTree().isEmpty();
    }

    @Override
    public int size() {
        return getTree().size();
    }

    @Override
    @SuppressWarnings("unchecked")
    public boolean containsKey(Object key) {
        return getTree().containsKey((K)key);
    }

    @Override
    @SuppressWarnings("unchecked")
    public boolean containsValue(Object value) {
        return getTree().values().contains((V)value);
    }

    @Override
    @SuppressWarnings("unchecked")
    public V get(Object key) {
        return getTree().getOrDefault((K)key, null);
    }

    @Override
    @SuppressWarnings("unchecked")
    public V getOrDefault(Object key, V defaultValue) {
        return getTree().getOrDefault((K)key, defaultValue);
    }

    @Override
    public V put(K key, V value) {
        return updateTree(key, t -> t.put(key, value));
    }

    @Override
    @SuppressWarnings("unchecked")
    public V remove(Object key) {
        return updateTree((K)key, t -> t.remove((K)key));
    }

    @Override
    public void clear() {
        updateTree(t -> TreeMap.empty(comparator));
    }

    @Override
    public boolean replace(K key, V oldValue, V newValue) {
        TreeMap<K,V> oldTree = getTree();
        TreeMap<K,V> newTree = updateTree(t -> t.replace(key, oldValue, newValue));
        return oldTree != newTree;
    }

    @Override
    public V replace(K key, V value) {
        return updateTree(key, t -> t.replace(key, value));
    }

    @Override
    public V putIfAbsent(K key, V value) {
        return updateTree(key, t -> t.putIfAbsent(key, value));
    }

    @Override
    public V computeIfAbsent(K key, Function<? super K, ? extends V> f) {
        return updateTree(key, t -> t.computeIfAbsent(key, f));
    }

    @Override
    public V computeIfPresent(K key, BiFunction<? super K, ? super V, ? extends V> f) {
        return updateTree(key, t -> t.computeIfPresent(key, (k, v) -> Optional.ofNullable(f.apply(k, v))));
    }

    @Override
    public V compute(K key, BiFunction<? super K, ? super V, ? extends V> f) {
        return updateTree(key, t -> t.compute(key, (k, v) -> Optional.ofNullable(f.apply(k, v.orElse(null)))));
    }

    @Override
    public V merge(K key, V value, BiFunction<? super V, ? super V, ? extends V> f) {
        return updateTree(key, t -> t.merge(key, value, f));
    }

    @Override
    public void forEach(BiConsumer<? super K, ? super V> action) {
        getTree().forEach(action);
    }

    @Override
    public void replaceAll(BiFunction<? super K, ? super V, ? extends V> function) {
        updateTree(t -> t.mapKV(function));
    }

    // Views

    private transient volatile KeySet keySet;
    private transient volatile Values values;
    private transient volatile EntrySet entrySet;

    @Override
    public Set<K> keySet() {
        KeySet ks = keySet;
        return (ks != null) ? ks : (keySet = new KeySet());
    }

    @Override
    public Collection<V> values() {
        Values vs = values;
        return (vs != null) ? vs : (values = new Values());
    }

    @Override
    public Set<Map.Entry<K,V>> entrySet() {
        EntrySet es = entrySet;
        return (es != null) ? es : (entrySet = new EntrySet());
    }

    class KeySet extends AbstractSet<K> {
        @Override
        public Iterator<K> iterator() {
            return getTree().keys().iterator();
        }

        @Override
        public boolean isEmpty() {
            return CopyOnWriteTreeMap.this.isEmpty();
        }

        @Override
        public int size() {
            return CopyOnWriteTreeMap.this.size();
        }

        @Override
        public boolean contains(Object o) {
            return CopyOnWriteTreeMap.this.containsKey(o);
        }

        @Override
        public boolean remove(Object o) {
            int oldSize = size();
            CopyOnWriteTreeMap.this.remove(o);
            return size() != oldSize;
        }

        @Override
        public void clear() {
            CopyOnWriteTreeMap.this.clear();
        }
    }

    class Values extends AbstractCollection<V> {
        @Override
        public Iterator<V> iterator() {
            return getTree().values().iterator();
        }

        @Override
        public boolean isEmpty() {
            return CopyOnWriteTreeMap.this.isEmpty();
        }

        @Override
        public int size() {
            return CopyOnWriteTreeMap.this.size();
        }

        @Override
        public boolean contains(Object o) {
            return CopyOnWriteTreeMap.this.containsValue(o);
        }

        @Override
        public boolean remove(Object o) {
            int oldSize = size();
            updateTree(t -> t.filter(x -> !Objects.equals(x, o)));
            return size() != oldSize;
        }

        @Override
        public void clear() {
            CopyOnWriteTreeMap.this.clear();
        }
    }

    class EntrySet extends AbstractSet<Map.Entry<K,V>> {
        @Override
        public Iterator<Map.Entry<K,V>> iterator() {
            return getTree().iterator();
        }

        @Override
        public boolean isEmpty() {
            return CopyOnWriteTreeMap.this.isEmpty();
        }

        @Override
        public int size() {
            return CopyOnWriteTreeMap.this.size();
        }

        @Override
        @SuppressWarnings("unchecked")
        public boolean contains(Object o) {
            if (!(o instanceof Map.Entry))
                return false;
            Map.Entry<?,?> entry = (Entry<?,?>)o;
            Object value = entry.getValue();
            return getTree().lookup((K)entry.getKey())
                            .filter(x -> Objects.equals(x, value))
                            .isPresent();
        }

        @Override
        @SuppressWarnings("unchecked")
        public boolean remove(Object o) {
            if (!(o instanceof Map.Entry))
                return false;
            Map.Entry<?,?> entry = (Map.Entry<?,?>)o;
            Object value = entry.getValue();
            int oldSize = size();
            updateTree(t -> t.computeIfPresent((K)entry.getKey(), (k, v) ->
                Objects.equals(v, value) ? Optional.empty() : Optional.of(v)));
            return size() != oldSize;
        }

        @Override
        public void clear() {
            CopyOnWriteTreeMap.this.clear();
        }
    }
}
