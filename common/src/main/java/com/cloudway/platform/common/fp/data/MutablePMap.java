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
import java.util.Iterator;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.UnaryOperator;

public class MutablePMap<K, V> extends AbstractMap<K, V> implements java.io.Serializable {
    private static final long serialVersionUID = -5019230762019800566L;

    /**
     * The underlying persistent map that accessed atomically.
     */
    private final transient AtomicReference<PMap<K,V>> pure = new AtomicReference<>();

    /**
     * Construct a mutable map from a persistent map.
     */
    public MutablePMap(PMap<K,V> init) {
        pure.set(Objects.requireNonNull(init));
    }

    /**
     * Returns the backing persistent map that can be used purely.
     */
    public final PMap<K,V> snapshot() {
        return pure.get();
    }

    /**
     * Update persistent map atomically.
     */
    private PMap<K,V> update(UnaryOperator<PMap<K,V>> updater) {
        return pure.updateAndGet(updater);
    }

    /**
     * Update persistent map atomically and return previous value associated
     * to the given key.
     */
    private V update(Object key, UnaryOperator<PMap<K,V>> updater) {
        PMap<K,V> prev = pure.getAndUpdate(updater);
        return prev.getOrDefault(key, null);
    }

    @Override
    public boolean isEmpty() {
        return snapshot().isEmpty();
    }

    @Override
    public int size() {
        return snapshot().size();
    }

    @Override
    public boolean containsKey(Object key) {
        return snapshot().containsKey(key);
    }

    @Override
    @SuppressWarnings("unchecked")
    public boolean containsValue(Object value) {
        return snapshot().values().contains((V)value);
    }

    @Override
    public V get(Object key) {
        return snapshot().getOrDefault(key, null);
    }

    @Override
    public V getOrDefault(Object key, V defaultValue) {
        return snapshot().getOrDefault(key, defaultValue);
    }

    @Override
    public V put(K key, V value) {
        return update(key, t -> t.put(key, value));
    }

    @Override
    public void putAll(Map<? extends K, ? extends V> m) {
        PMap<K,V> pm = pureFor(m);
        if (pm != null) {
            update(t -> t.putAll(pm));
        } else {
            super.putAll(m);
        }
    }

    @Override
    public V remove(Object key) {
        return update(key, t -> t.remove(key));
    }

    @Override
    public void clear() {
        update(PMap::clear);
    }

    @Override
    public boolean replace(K key, V oldValue, V newValue) {
        PMap<K,V> oldTrie = snapshot();
        PMap<K,V> newTrie = update(t -> t.replace(key, oldValue, newValue));
        return oldTrie != newTrie;
    }

    @Override
    public V replace(K key, V value) {
        return update(key, t -> t.replace(key, value));
    }

    @Override
    public V putIfAbsent(K key, V value) {
        return update(key, t -> t.putIfAbsent(key, value));
    }

    @Override
    public V computeIfAbsent(K key, Function<? super K, ? extends V> f) {
        return update(key, t -> t.computeIfAbsent(key, f));
    }

    @Override
    public V computeIfPresent(K key, BiFunction<? super K, ? super V, ? extends V> f) {
        return update(key, t -> t.computeIfPresent(key, (k, v) -> Maybe.ofNullable(f.apply(k, v))));
    }

    @Override
    public V compute(K key, BiFunction<? super K, ? super V, ? extends V> f) {
        return update(key, t -> t.compute(key, (k, v) -> Maybe.ofNullable(f.apply(k, v.orElse(null)))));
    }

    @Override
    public V merge(K key, V value, BiFunction<? super V, ? super V, ? extends V> f) {
        return update(key, t -> t.merge(key, value, f));
    }

    @Override
    public void forEach(BiConsumer<? super K, ? super V> action) {
        snapshot().forEach(action);
    }

    @Override
    public void replaceAll(BiFunction<? super K, ? super V, ? extends V> f) {
        update(t -> t.mapKV(f));
    }

    protected PMap<K,V> pureFor(Map<?,?> m) {
        if (m instanceof MutablePMap) {
            @SuppressWarnings("unchecked")
            MutablePMap<K,V> pm = (MutablePMap<K,V>)m;
            return pm.snapshot();
        }
        return null;
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
            return snapshot().keys().iterator();
        }

        @Override
        public boolean isEmpty() {
            return MutablePMap.this.isEmpty();
        }

        @Override
        public int size() {
            return MutablePMap.this.size();
        }

        @Override
        public boolean contains(Object o) {
            return MutablePMap.this.containsKey(o);
        }

        @Override
        public boolean remove(Object o) {
            int oldSize = size();
            MutablePMap.this.remove(o);
            return size() != oldSize;
        }

        @Override
        public void clear() {
            MutablePMap.this.clear();
        }
    }

    class Values extends AbstractCollection<V> {
        @Override
        public Iterator<V> iterator() {
            return snapshot().values().iterator();
        }

        @Override
        public boolean isEmpty() {
            return MutablePMap.this.isEmpty();
        }

        @Override
        public int size() {
            return MutablePMap.this.size();
        }

        @Override
        public boolean contains(Object o) {
            return MutablePMap.this.containsValue(o);
        }

        @Override
        public boolean remove(Object o) {
            int oldSize = size();
            update(t -> t.filter(x -> !Objects.equals(x, o)));
            return size() != oldSize;
        }

        @Override
        public void clear() {
            MutablePMap.this.clear();
        }
    }

    class EntrySet extends AbstractSet<Map.Entry<K,V>> {
        @Override
        public Iterator<Map.Entry<K,V>> iterator() {
            return snapshot().iterator();
        }

        @Override
        public boolean isEmpty() {
            return MutablePMap.this.isEmpty();
        }

        @Override
        public int size() {
            return MutablePMap.this.size();
        }

        @Override
        @SuppressWarnings("unchecked")
        public boolean contains(Object o) {
            if (!(o instanceof Map.Entry))
                return false;
            Map.Entry<?,?> entry = (Entry<?,?>)o;
            Object value = entry.getValue();
            return snapshot().lookup(entry.getKey())
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
            update(t -> t.computeIfPresent((K)entry.getKey(), (k, v) ->
                Objects.equals(v, value) ? Maybe.empty() : Maybe.of(v)));
            return size() != oldSize;
        }

        @Override
        public void clear() {
            MutablePMap.this.clear();
        }
    }
}
