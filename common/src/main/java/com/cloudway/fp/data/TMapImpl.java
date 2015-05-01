/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.data;

import java.util.Arrays;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.StringJoiner;
import java.util.function.BiFunction;
import java.util.function.BiPredicate;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

import com.cloudway.fp.data.HashPMap.MergeFunction;
import com.cloudway.fp.function.TriFunction;

@SuppressWarnings("EqualsAndHashcode")
final class TMapImpl {
    private TMapImpl() {}

    private static final Empty<?,?> EMPTY = new Empty<>();

    @SuppressWarnings("unchecked")
    static <K,V> TMap<K,V> empty() {
        return (TMap<K,V>)EMPTY;
    }

    static <K,V> TMap<K,V> singleton(K k, V v) {
        return new Singleton<>(k, v, hash(k));
    }

    static int hash(Object key) {
        int h = key.hashCode();
        return h ^ (h >>> 16);
    }

    static <K,V> NodeList<K,V> cons(K k, V v, NodeList<K,V> t) {
        return new NodeList<>(k, v, t);
    }

    @SuppressWarnings("unchecked")
    static <K,V> TMap<K,V>[] newBuffer(int minSize) {
        int bufferSize = Math.min(minSize + 6, 32 * 7);
        return new TMap[bufferSize];
    }

    static class Merger<K,V> implements MergeFunction<K,V> {
        final MergeFunction<K,V> mergef;
        final Merger<K,V> invert;

        Merger(MergeFunction<K,V> mergef) {
            this.mergef = mergef;
            this.invert = new InvertMerger<>(mergef, this);
        }

        Merger(MergeFunction<K,V> mergef, Merger<K,V> invert) {
            this.mergef = mergef;
            this.invert = invert;
        }

        @Override
        public V apply(K k, V v1, V v2) {
            return mergef.apply(k, v1, v2);
        }

        public Merger<K,V> invert() {
            return invert;
        }
    }

    static class InvertMerger<K,V> extends Merger<K,V> {
        InvertMerger(MergeFunction<K,V> mergef, Merger<K,V> origin) {
            super(mergef, origin);
        }

        @Override
        public V apply(K k, V v1, V v2) {
            return mergef.apply(k, v2, v1);
        }
    }

    static <K,V> Merger<K,V> liftMerger(MergeFunction<K,V> f) {
        return new Merger<>(f);
    }

    interface Traverser<K,V> {
        Traverser<K,V> succ();
        Node<K,V> cursor();
    }

    interface TMap<K,V> extends HashPMap<K,V> {
        Node<K,V> get0(Object key, int hash, int level);
        TMap<K,V> put0(K key, V value, int hash, int level, boolean onlyIfAbsent, Merger<K,V> merger);
        TMap<K,V> remove0(Object key, int hash, int level);
        TMap<K,V> replace0(K key, V oldValue, V newValue, int hash, int level, boolean anyValue);
        <U> TMap<K,U> map0(Function<Node<K,V>, ? extends U> f);
        TMap<K,V> filter0(BiPredicate<? super K, ? super V> p, int level, TMap<K,V>[] buffer, int offset0);
        TMap<K,V> merge0(TMap<K,V> that, int level, Merger<K,V> merger);
        boolean submapOf(TMap<K,V> that, int level);
        <R> R foldl(R z, BiFunction<R, Node<K,V>, R> f);
        <R> R foldr(BiFunction<Node<K,V>, Supplier<R>, R> f, Supplier<R> r);
        Traverser<K,V> traverser();

        @Override
        default boolean containsKey(Object key) {
            return get0(key, hash(key), 0) != null;
        }

        @Override
        default Maybe<V> lookup(Object key) {
            Node<K,V> kv = get0(key, hash(key), 0);
            return kv != null ? Maybe.of(kv.value) : Maybe.empty();
        }

        @Override
        default V get(Object key) {
            Node<K,V> kv = get0(key, hash(key), 0);
            if (kv == null)
                throw new NoSuchElementException();
            return kv.value;
        }

        @Override
        default V getOrDefault(Object key, V def) {
            Node<K,V> kv = get0(key, hash(key), 0);
            return kv != null ? kv.value : def;
        }

        @Override
        default PMap<K,V> put(K key, V value) {
            return put0(key, value, hash(key), 0, false, null);
        }

        @Override
        default PMap<K,V> putIfAbsent(K key, V value) {
            return put0(key, value, hash(key), 0, true, null);
        }

        @Override
        default PMap<K,V> remove(Object key) {
            return remove0(key, hash(key), 0);
        }

        @Override
        default PMap<K,V> replace(K key, V oldValue, V newValue) {
            return replace0(key, oldValue, newValue, hash(key), 0, false);
        }

        @Override
        default PMap<K,V> replace(K key, V value) {
            return replace0(key, null, value, hash(key), 0, true);
        }

        @Override
        default <U> PMap<K,U> map(Function<? super V, ? extends U> f) {
            return map0(kv -> f.apply(kv.value));
        }

        @Override
        default <U> PMap<K,U> mapKV(BiFunction<? super K, ? super V, ? extends U> f) {
            return map0(kv -> f.apply(kv.key, kv.value));
        }

        @Override
        default PMap<K,V> filterKV(BiPredicate<? super K, ? super V> p) {
            TMap<K,V> buffer[] = newBuffer(size());
            return filter0(p, 0, buffer, 0);
        }

        @Override
        @SuppressWarnings("unchecked")
        default boolean containsAll(PMap<? extends K, ? extends V> that) {
            if (size() < that.size()) {
                return false;
            } else if (that instanceof TMap) {
                return ((TMap<K,V>)that).submapOf(this, 0);
            } else {
                return that.allMatch((k, v) -> {
                    Node<K,V> kv = get0(k, hash(k), 0);
                    return kv != null && Objects.equals(kv.value, v);
                });
            }
        }

        @Override
        @SuppressWarnings("unchecked")
        default PMap<K,V> putAll(PMap<? extends K, ? extends V> that) {
            if (that instanceof TMap) {
                return merge0((TMap<K,V>)that, 0, liftMerger((k, a, b) -> b));
            } else {
                return that.foldLeftKV((PMap<K,V>)this, PMap::put);
            }
        }

        @Override
        default PMap<K,V> merge(PMap<K,V> that, MergeFunction<K,V> f) {
            return merge0((TMap<K,V>)that, 0, liftMerger(f));
        }

        @Override
        default PMap<K,V> clear() {
            return TMapImpl.empty();
        }

        @Override
        default <R> R foldLeft(R z, BiFunction<R, ? super V, R> f) {
            return foldl(z, (r, e) -> f.apply(r, e.value));
        }

        @Override
        default <R> R foldLeftKV(R z, TriFunction<R, ? super K, ? super V, R> f) {
            return foldl(z, (r, e) -> f.apply(r, e.key, e.value));
        }

        @Override
        default <R> R foldRight(BiFunction<? super V, Supplier<R>, R> f, Supplier<R> sr) {
            return foldr((e, r) -> f.apply(e.value, r), sr);
        }

        @Override
        default <R> R foldRightKV(TriFunction<? super K, ? super V, Supplier<R>, R> f, Supplier<R> sr) {
            return foldr((e, r) -> f.apply(e.key, e.value, r), sr);
        }

        @Override
        default PSet<K> keySet() {
            return new KeySetView<>(this);
        }

        PSet<K> toKeySet();

        @Override
        default Seq<Map.Entry<K,V>> entries() {
            return foldr(Seq::cons, Seq::nil);
        }

        @Override
        default Iterator<Map.Entry<K,V>> iterator() {
            return new MapIterator<>(this);
        }

        @Override
        default void forEach(Consumer<? super Map.Entry<K,V>> action) {
            foldl(Unit.U, (u, e) -> { action.accept(e); return u; });
        }

        default boolean equals0(Object obj) {
            if (this == obj)
                return true;
            if (!(obj instanceof TMap))
                return false;
            @SuppressWarnings("unchecked")
            TMap<K,V> that = (TMap<K,V>)obj;
            return size() == that.size() && submapOf(that, 0);
        }

        default String show() {
            return foldl(new StringJoiner(",", "{", "}"),
                         (sj, e) -> sj.add(e.key + ":" + e.value))
                  .toString();
        }
    }

    static class Node<K,V> implements Map.Entry<K,V> {
        final K key;
        final V value;

        Node(K k, V v) {
            this.key = k;
            this.value = v;
        }

        @Override
        public K getKey() {
            return key;
        }

        @Override
        public V getValue() {
            return value;
        }

        @Override
        public V setValue(V value) {
            throw new UnsupportedOperationException();
        }

        public boolean equals(Object obj) {
            if (obj == this)
                return true;
            if (!(obj instanceof Node))
                return false;
            @SuppressWarnings("unchecked")
            Node<K,V> e = (Node<K,V>)obj;
            return key.equals(e.key) && Objects.equals(value, e.value);
        }

        public int hashCode() {
            return 31 * key.hashCode() + Objects.hashCode(value);
        }

        public String toString() {
            return key + ":" + value;
        }
    }

    static class NodeList<K,V> extends Node<K,V> implements Traverser<K,V> {
        final NodeList<K,V> next;

        NodeList(K key, V value, NodeList<K,V> next) {
            super(key, value);
            this.next = next;
        }

        int size() {
            int sz = 0;
            NodeList<K,V> p = this;
            while (p != null) {
                sz++;
                p = p.next;
            }
            return sz;
        }

        NodeList<K,V> find(BiPredicate<? super K, ? super V> pred) {
            NodeList<K,V> p = this;
            while (p != null) {
                if (pred.test(p.key, p.value))
                    return p;
                p = p.next;
            }
            return null;
        }

        boolean any(BiPredicate<? super K, ? super V> pred) {
            return find(pred) != null;
        }

        boolean all(BiPredicate<? super K, ? super V> pred) {
            return find(pred.negate()) == null;
        }

        private NodeList<K,V> consTo(NodeList<K,V> end, NodeList<K,V> res) {
            NodeList<K,V> p = this;
            while (p != end) {
                res = cons(p.key, p.value, res);
                p = p.next;
            }
            return res;
        }

        Node<K,V> lookup(Object key) {
            return find((k,v) -> k.equals(key));
        }

        NodeList<K,V> insert(K key, V value) {
            return cons(key, value, delete(key));
        }

        NodeList<K,V> delete(Object key) {
            NodeList<K,V> t = find((k,v) -> k.equals(key));
            return t == null ? this : consTo(t, t.next);
        }

        NodeList<K,V> replace(K key, V oldValue, V newValue, boolean anyValue) {
            NodeList<K,V> t = find((k,v) ->
                k.equals(key) && (anyValue || Objects.equals(v, oldValue)));
            return t == null ? this : consTo(t, cons(key, newValue, t.next));
        }

        <U> NodeList<K,U> map(Function<Node<K,V>, ? extends U> f) {
            NodeList<K,V> p = this;
            NodeList<K,U> r = null;
            while (p != null) {
                r = cons(p.key, f.apply(p), r);
                p = p.next;
            }
            return r;
        }

        NodeList<K,V> filter(BiPredicate<? super K, ? super V> pred) {
            NodeList<K,V> t = find(pred.negate());
            if (t == null) {
                return this;
            } else {
                NodeList<K,V> r = consTo(t, null);
                while ((t = t.next) != null) {
                    if (pred.test(t.key, t.value)) {
                        r = cons(t.key, t.value, r);
                    }
                }
                return r;
            }
        }

        <R> R foldl(R z, BiFunction<R, Node<K,V>, R> f) {
            NodeList<K,V> p = this;
            while (p != null) {
                z = f.apply(z, p);
                p = p.next;
            }
            return z;
        }

        <R> R foldr(BiFunction<Node<K,V>, Supplier<R>, R> f, Supplier<R> r) {
            if (next == null) {
                return f.apply(this, r);
            } else {
                return f.apply(this, () -> next.foldr(f, r));
            }
        }

        @Override
        public Traverser<K,V> succ() {
            return next;
        }

        @Override
        public Node<K,V> cursor() {
            return this;
        }

        NodeList<K,V> force() {
            NodeList<K,V> p = this;
            while (p != null) {
                Forcible.force(p.key);
                Forcible.force(p.value);
                p = p.next;
            }
            return this;
        }
    }

    static class Empty<K,V> implements TMap<K,V> {
        @Override
        public boolean isEmpty() {
            return true;
        }

        @Override
        public int size() {
            return 0;
        }

        @Override
        public Node<K,V> get0(Object k, int hash, int level) {
            return null;
        }

        @Override
        public Maybe<Map.Entry<K,V>> find(BiPredicate<? super K, ? super V> p) {
            return Maybe.empty();
        }

        @Override
        public boolean submapOf(TMap<K,V> that, int level) {
            return true;
        }

        @Override
        public TMap<K,V> put0(K key, V value, int hash, int level, boolean onlyIfAbsent, Merger<K,V> merger) {
            return new Singleton<>(key, value, hash);
        }

        @Override
        public TMap<K,V> remove0(Object key, int hash, int level) {
            return this;
        }

        @Override
        public TMap<K,V> replace0(K key, V oldValue, V newValue, int hash, int level, boolean anyValue) {
            return this;
        }

        @Override
        public <U> TMap<K,U> map0(Function<Node<K,V>, ? extends U> f) {
            return TMapImpl.empty();
        }

        @Override
        public TMap<K,V> filter0(BiPredicate<? super K, ? super V> p, int level, TMap<K,V>[] buffer, int offset0) {
            return this;
        }

        @Override
        public TMap<K,V> merge0(TMap<K,V> that, int level, Merger<K,V> merger) {
            return that;
        }

        @Override
        public <R> R foldl(R z, BiFunction<R, Node<K,V>, R> f) {
            return z;
        }

        @Override
        public <R> R foldr(BiFunction<Node<K,V>, Supplier<R>, R> f, Supplier<R> r) {
            return r.get();
        }

        @Override
        public Traverser<K,V> traverser() {
            return null;
        }

        @Override
        public PMap<K,V> force() {
            return this;
        }

        @Override
        public PSet<K> toKeySet() {
            return TSetImpl.empty();
        }

        @Override
        public boolean equals(Object obj) {
            return equals0(obj);
        }

        @Override
        public int hashCode() {
            return 0;
        }

        @Override
        public String toString() {
            return "{}";
        }
    }

    static class Singleton<K,V> extends Node<K,V> implements TMap<K,V>, Traverser<K,V> {
        private final int hash;

        Singleton(K key, V value, int hash) {
            super(key, value);
            this.hash = hash;
        }

        @Override
        public int size() {
            return 1;
        }

        @Override
        public boolean isEmpty() {
            return false;
        }

        private boolean equiv(Object key, int hash) {
            return this.hash == hash && this.key.equals(key);
        }

        @Override
        public Node<K,V> get0(Object key, int hash, int level) {
            return equiv(key, hash) ? this : null;
        }

        @Override
        public Maybe<Map.Entry<K,V>> find(BiPredicate<? super K, ? super V> p) {
            return p.test(key, value) ? Maybe.of(this) : Maybe.empty();
        }

        @Override
        public boolean submapOf(TMap<K,V> that, int level) {
            Node<K,V> node = that.get0(key, hash, level);
            return node != null && Objects.equals(value, node.value);
        }

        @Override
        public TMap<K,V> put0(K key, V value, int hash, int level, boolean onlyIfAbsent, Merger<K,V> merger) {
            if (equiv(key, hash)) {
                if (onlyIfAbsent) {
                    return this;
                } else if (merger == null) {
                    return new Singleton<>(key, value, hash);
                } else {
                    return new Singleton<>(key, merger.apply(this.key, this.value, value), hash);
                }
            } else {
                if (this.hash != hash) {
                    TMap<K,V> that = new Singleton<>(key, value, hash);
                    return Trie.make(this.hash, this, hash, that, level, 2);
                } else {
                    return new Collision<>(hash, cons(this.key, this.value, cons(key, value, null)));
                }
            }
        }

        @Override
        public TMap<K,V> remove0(Object key, int hash, int level) {
            return equiv(key, hash) ? TMapImpl.empty() : this;
        }

        @Override
        public TMap<K,V> replace0(K key, V oldValue, V newValue, int hash, int level, boolean anyValue) {
            if (equiv(key, hash) && (anyValue || Objects.equals(this.value, oldValue))) {
                return new Singleton<>(key, newValue, hash);
            } else {
                return this;
            }
        }

        @Override
        public <U> TMap<K,U> map0(Function<Node<K,V>, ? extends U> f) {
            return new Singleton<>(key, f.apply(this), hash);
        }

        @Override
        public TMap<K,V> filter0(BiPredicate<? super K, ? super V> p, int level, TMap<K,V>[] buffer, int offset0) {
            return p.test(key, value) ? this : TMapImpl.empty();
        }

        @Override
        public TMap<K,V> merge0(TMap<K,V> that, int level, Merger<K,V> merger) {
            return that.put0(key, value, hash, level, false, merger.invert());
        }

        @Override
        public <R> R foldl(R z, BiFunction<R, Node<K,V>, R> f) {
            return f.apply(z, this);
        }

        @Override
        public <R> R foldr(BiFunction<Node<K,V>, Supplier<R>, R> f, Supplier<R> r) {
            return f.apply(this, r);
        }

        @Override
        public Traverser<K,V> traverser() {
            return this;
        }

        @Override
        public Traverser<K,V> succ() {
            return null;
        }

        @Override
        public Node<K,V> cursor() {
            return this;
        }

        @Override
        public PMap<K,V> force() {
            Forcible.force(key);
            Forcible.force(value);
            return this;
        }

        @Override
        public PSet<K> toKeySet() {
            return new TSetImpl.Singleton<>(key, hash);
        }

        @Override
        public boolean equals(Object obj) {
            return equals0(obj);
        }

        @Override
        public int hashCode() {
            return hash * 31 + Objects.hash(value);
        }

        @Override
        public String toString() {
            return show();
        }
    }

    static class Collision<K,V> implements TMap<K,V> {
        private final int hash;
        private final NodeList<K,V> kvs;
        private final int size;

        Collision(int hash, NodeList<K,V> kvs) {
            this.hash = hash;
            this.kvs  = kvs;
            this.size = kvs.size();
        }

        @Override
        public int size() {
            return size;
        }

        @Override
        public boolean isEmpty() {
            return false;
        }

        @Override
        public Node<K,V> get0(Object key, int hash, int level) {
            return this.hash == hash ? kvs.lookup(key) : null;
        }

        @Override
        public Maybe<Map.Entry<K,V>> find(BiPredicate<? super K, ? super V> p) {
            return Maybe.ofNullable(kvs.find(p));
        }

        @Override
        public boolean submapOf(TMap<K,V> that, int level) {
            if (size() > that.size()) {
                return false;
            } else {
                return kvs.all((k, v) -> {
                    Node<K,V> kv = that.get0(k, hash, level);
                    return kv != null && Objects.equals(v, kv.value);
                });
            }
        }

        @Override
        public TMap<K,V> put0(K key, V value, int hash, int level, boolean onlyIfAbsent, Merger<K,V> merger) {
            if (hash == this.hash) {
                Node<K,V> kv = kvs.lookup(key);
                if (kv != null && onlyIfAbsent) {
                    return this;
                } else if (merger == null || kv == null) {
                    return new Collision<>(hash, kvs.insert(key, value));
                } else {
                    return new Collision<>(hash, kvs.insert(key, merger.apply(kv.key, kv.value, value)));
                }
            } else {
                TMap<K,V> that = new Singleton<>(key, value, hash);
                return Trie.make(this.hash, this, hash, that, level, size + 1);
            }
        }

        @Override
        public TMap<K,V> remove0(Object key, int hash, int level) {
            if (hash == this.hash) {
                return afterRemove(kvs.delete(key));
            } else {
                return this;
            }
        }

        private TMap<K,V> afterRemove(NodeList<K, V> kvs1) {
            if (kvs1 == null) {
                return TMapImpl.empty();
            } else if (kvs1 == this.kvs) {
                return this;
            } else if (kvs1.next == null) {
                return new Singleton<>(kvs1.key, kvs1.value, hash);
            } else {
                return new Collision<>(hash, kvs1);
            }
        }

        @Override
        public TMap<K,V> replace0(K key, V oldValue, V newValue, int hash, int level, boolean anyValue) {
            if (hash == this.hash) {
                NodeList<K,V> kvs1 = kvs.replace(key, oldValue, newValue, anyValue);
                if (kvs1 == this.kvs) {
                    return this;
                } else {
                    return new Collision<>(hash, kvs1);
                }
            } else {
                return this;
            }
        }

        @Override
        public <U> TMap<K,U> map0(Function<Node<K,V>, ? extends U> f) {
            return new Collision<>(hash, kvs.map(f));
        }

        @Override
        public TMap<K,V> filter0(BiPredicate<? super K, ? super V> p, int level, TMap<K, V>[] buffer, int offset0) {
            return afterRemove(kvs.filter(p));
        }

        @Override
        public TMap<K, V> merge0(TMap<K,V> that, int level, Merger<K,V> merger) {
            Merger<K,V> invert = merger.invert();
            return kvs.foldl(that, (m, kv) -> m.put0(kv.key, kv.value, hash, level, false, invert));
        }

        @Override
        public <R> R foldl(R z, BiFunction<R, Node<K,V>, R> f) {
            return kvs.foldl(z, f);
        }

        @Override
        public <R> R foldr(BiFunction<Node<K,V>, Supplier<R>, R> f, Supplier<R> r) {
            return kvs.foldr(f, r);
        }

        @Override
        public Traverser<K,V> traverser() {
            return kvs;
        }

        @Override
        public PMap<K,V> force() {
            kvs.force();
            return this;
        }

        @Override
        public PSet<K> toKeySet() {
            TSetImpl.List<K> keys = kvs.foldl(null, (ks, kv) -> TSetImpl.cons(kv.key, ks));
            return new TSetImpl.Collision<>(hash, keys);
        }

        @Override
        public boolean equals(Object obj) {
            return equals0(obj);
        }

        @Override
        public int hashCode() {
            return kvs.foldl(hash * 31, (Integer h, Node<K,V> kv) -> h * 31 + Objects.hashCode(kv.value));
        }

        @Override
        public String toString() {
            return show();
        }
    }

    static class Trie<K,V> implements TMap<K,V> {
        static <K,V> Trie<K,V> make(int hash0, TMap<K,V> elem0, int hash1, TMap<K,V> elem1, int level, int size) {
            int index0 = (hash0 >>> level) & 0x1f;
            int index1 = (hash1 >>> level) & 0x1f;
            if (index0 != index1) {
                @SuppressWarnings("unchecked")
                TMap<K,V> elems[] = new TMap[2];
                int bitmap = (1 << index0) | (1 << index1);
                if (index0 < index1) {
                    elems[0] = elem0;
                    elems[1] = elem1;
                } else {
                    elems[0] = elem1;
                    elems[1] = elem0;
                }
                return new Trie<>(bitmap, elems, size);
            } else {
                @SuppressWarnings("unchecked")
                TMap<K,V> elems[] = new TMap[1];
                int bitmap = (1 << index0);
                elems[0] = make(hash0, elem0, hash1, elem1, level + 5, size);
                return new Trie<>(bitmap, elems, size);
            }
        }

        private final int bitmap;
        private final TMap<K,V> elems[];
        private final int size;

        Trie(int bitmap, TMap<K,V> elems[], int size) {
            this.bitmap = bitmap;
            this.elems = elems;
            this.size = size;
        }

        @Override
        public int size() {
            return size;
        }

        @Override
        public boolean isEmpty() {
            return false;
        }

        @Override
        public Node<K,V> get0(Object key, int hash, int level) {
            TMap<K,V> sub = this;
            do {
                Trie<K,V> trie = (Trie<K,V>)sub;
                int bits = trie.bitmap;
                int index = (hash >>> level) & 0x1f;
                int mask = (1 << index);
                if (bits == -1) {
                    sub = trie.elems[index];
                } else if ((bits & mask) != 0) {
                    sub = trie.elems[Integer.bitCount(bits & (mask-1))];
                } else {
                    return null;
                }
                level += 5;
            } while (sub instanceof Trie);
            return sub.get0(key, hash, level);
        }

        @Override
        public Maybe<Map.Entry<K,V>> find(BiPredicate<? super K, ? super V> p) {
            for (TMap<K,V> el : elems) {
                Maybe<Map.Entry<K,V>> kv = el.find(p);
                if (kv.isPresent())
                    return kv;
            }
            return Maybe.empty();
        }

        @Override
        public boolean submapOf(TMap<K,V> that, int level) {
            if (size() > that.size()) {
                return false;
            } else if (that instanceof Trie) {
                return submap2((Trie<K,V>)that, level);
            } else {
                return allMatch((k, v) -> {
                    Node<K,V> kv = that.get0(k, hash(k), level);
                    return kv != null && Objects.equals(v, kv.value);
                });
            }
        }

        private boolean submap2(Trie<K,V> that, int level) {
            TMap<K,V>[] xs = this.elems;
            TMap<K,V>[] ys = that.elems;
            int xbm = this.bitmap;
            int ybm = that.bitmap;

            if ((xbm & ybm) != xbm) {
                return false;
            }

            int ix = 0, iy = 0;
            while (ix < xs.length && iy < ys.length) {
                int xlsb = xbm ^ (xbm & (xbm - 1));
                int ylsb = ybm ^ (ybm & (ybm - 1));
                if (xlsb == ylsb) {
                    if (!xs[ix].submapOf(ys[iy], level + 5))
                        return false;
                    xbm &= ~xlsb;
                    ybm &= ~ylsb;
                    ix++;
                    iy++;
                } else if (unsignedCompare(xlsb - 1, ylsb - 1)) {
                    return false;
                } else {
                    ybm &= ~ylsb;
                    iy++;
                }
            }
            return true;
        }

        @Override
        public TMap<K,V> put0(K key, V value, int hash, int level, boolean onlyIfAbsent, Merger<K,V> merger) {
            int index = (hash >>> level) & 0x1f;
            int mask = (1 << index);
            int offset = Integer.bitCount(bitmap & (mask-1));
            if ((bitmap & mask) != 0) {
                TMap<K,V> sub = elems[offset];
                TMap<K,V> subNew = sub.put0(key, value, hash, level + 5, onlyIfAbsent, merger);
                if (subNew == sub) {
                    return this;
                } else {
                    TMap<K,V> elemsNew[] = elems.clone();
                    elemsNew[offset] = subNew;
                    return new Trie<>(bitmap, elemsNew, size + (subNew.size() - sub.size()));
                }
            } else {
                @SuppressWarnings("unchecked")
                TMap<K,V> elemsNew[] = new TMap[elems.length + 1];
                System.arraycopy(elems, 0, elemsNew, 0, offset);
                elemsNew[offset] = new Singleton<>(key, value, hash);
                System.arraycopy(elems, offset, elemsNew, offset + 1, elems.length - offset);
                return new Trie<>(bitmap | mask, elemsNew, size + 1);
            }
        }

        @Override
        public TMap<K,V> remove0(Object key, int hash, int level) {
            int index = (hash >>> level) & 0x1f;
            int mask = (1 << index);
            int offset = Integer.bitCount(bitmap & (mask-1));
            if ((bitmap & mask) != 0) {
                TMap<K,V> sub = elems[offset];
                TMap<K,V> subNew = sub.remove0(key, hash, level + 5);
                if (subNew == sub) {
                    return this;
                } else if (subNew.isEmpty()) {
                    int bitmapNew = bitmap ^ mask;
                    if (bitmapNew != 0) {
                        @SuppressWarnings("unchecked")
                        TMap<K,V> elemsNew[] = new TMap[elems.length - 1];
                        System.arraycopy(elems, 0, elemsNew, 0, offset);
                        System.arraycopy(elems, offset + 1, elemsNew, offset, elems.length - offset - 1);
                        int sizeNew = size - sub.size();
                        if (elemsNew.length == 1 && !(elemsNew[0] instanceof Trie)) {
                            return elemsNew[0];
                        } else {
                            return new Trie<>(bitmapNew, elemsNew, sizeNew);
                        }
                    } else {
                        return TMapImpl.empty();
                    }
                } else if (elems.length == 1 && !(subNew instanceof Trie)) {
                    return subNew;
                } else {
                    TMap<K,V> elemsNew[] = elems.clone();
                    elemsNew[offset] = subNew;
                    int sizeNew = size + (subNew.size() - sub.size());
                    return new Trie<>(bitmap, elemsNew, sizeNew);
                }
            } else {
                return this;
            }
        }

        @Override
        public TMap<K,V> replace0(K key, V oldValue, V newValue, int hash, int level, boolean anyValue) {
            int index = (hash >>> level) & 0x1f;
            int mask = (1 << index);
            int offset = Integer.bitCount(bitmap & (mask-1));
            if ((bitmap & mask) != 0) {
                TMap<K,V> sub = elems[offset];
                TMap<K,V> subNew = sub.replace0(key, oldValue, newValue, hash, level + 5, anyValue);
                if (subNew == sub) {
                    return this;
                } else {
                    TMap<K,V> elemsNew[] = elems.clone();
                    elemsNew[offset] = subNew;
                    return new Trie<>(bitmap, elemsNew, size);
                }
            } else {
                return this;
            }
        }

        @Override
        public <U> TMap<K,U> map0(Function<Node<K,V>, ? extends U> f) {
            @SuppressWarnings("unchecked")
            TMap<K,U> elemsNew[] = new TMap[elems.length];
            for (int i = 0; i < elemsNew.length; i++) {
                elemsNew[i] = elems[i].map0(f);
            }
            return new Trie<>(bitmap, elemsNew, size);
        }

        @Override
        public TMap<K,V> filter0(BiPredicate<? super K, ? super V> p, int level, TMap<K,V>[] buffer, int offset0) {
            int offset = offset0;
            int rs = 0;
            int bm = bitmap;
            int rbm = 0;

            for (TMap<K,V> el : elems) {
                TMap<K,V> result = el.filter0(p, level + 5, buffer, offset);
                int lsb = bm ^ (bm & (bm - 1));
                if (!result.isEmpty()) {
                    buffer[offset++] = result;
                    rs += result.size();
                    rbm |= lsb;
                }
                bm &= ~lsb;
            }

            if (offset == offset0) {
                return TMapImpl.empty();
            } else if (rs == this.size) {
                return this;
            } else if (offset == offset0 + 1 && !(buffer[offset0] instanceof Trie)) {
                return buffer[offset0];
            } else {
                int length = offset - offset0;
                @SuppressWarnings("unchecked")
                TMap<K,V>[] elems = new TMap[length];
                System.arraycopy(buffer, offset0, elems, 0, length);
                return new Trie<>(rbm, elems, rs);
            }
        }

        @Override
        public TMap<K,V> merge0(TMap<K,V> that, int level, Merger<K,V> merger) {
            if (that.isEmpty()) {
                return this;
            } else if (that instanceof Trie) {
                return merge2((Trie<K,V>)that, level, merger);
            } else {
                return that.merge0(this, level, merger.invert());
            }
        }

        private TMap<K,V> merge2(Trie<K,V> that, int level, Merger<K,V> merger) {
            TMap<K,V>[] xs = this.elems;
            TMap<K,V>[] ys = that.elems;
            int xbm = this.bitmap;
            int ybm = that.bitmap;

            // determine the necessary size for the array
            int subcount = Integer.bitCount(xbm | ybm);

            // construct a new array of appropriate size
            @SuppressWarnings("unchecked")
            TMap<K,V>[] buffer = new TMap[subcount];
            int totalsize = 0;

            // run through both bitmaps and add elements to it
            int ix = 0, iy = 0;
            for (int i = 0; i < subcount; i++) {
                int xlsb = xbm ^ (xbm & (xbm - 1));
                int ylsb = ybm ^ (ybm & (ybm - 1));
                if (xlsb == ylsb) {
                    TMap<K,V> sub = xs[ix++].merge0(ys[iy++], level + 5, merger);
                    buffer[i] = sub;
                    totalsize += sub.size();
                    xbm &= ~xlsb;
                    ybm &= ~ylsb;
                } else if (unsignedCompare(xlsb - 1, ylsb - 1)) {
                    TMap<K,V> sub = xs[ix++];
                    buffer[i] = sub;
                    totalsize += sub.size();
                    xbm &= ~xlsb;
                } else {
                    TMap<K,V> sub = ys[iy++];
                    buffer[i] = sub;
                    totalsize += sub.size();
                    ybm &= ~ylsb;
                }
            }
            return new Trie<>(this.bitmap | that.bitmap, buffer, totalsize);
        }

        @Override
        public <R> R foldl(R z, BiFunction<R, Node<K,V>, R> f) {
            for (TMap<K,V> elem : elems) {
                z = elem.foldl(z, f);
            }
            return z;
        }

        @Override
        public <R> R foldr(BiFunction<Node<K,V>, Supplier<R>, R> f, Supplier<R> r) {
            return _foldr(0, f, r);
        }

        private <R> R _foldr(int i, BiFunction<Node<K,V>, Supplier<R>, R> f, Supplier<R> r) {
            if (i == elems.length - 1) {
                return elems[i].foldr(f, r);
            } else {
                return elems[i].foldr(f, () -> _foldr(i+1, f, r));
            }
        }

        @Override
        public Traverser<K,V> traverser() {
            return new TrieTraverser<>(this, null).first();
        }

        static class TrieTraverser<K,V> implements Traverser<K,V> {
            private final Trie<K,V> trie;
            private final TrieTraverser<K,V> parent;
            private Traverser<K,V> current;
            private int index;

            TrieTraverser(Trie<K,V> trie, TrieTraverser<K,V> parent) {
                this.trie = trie;
                this.parent = parent;
            }

            private Traverser<K,V> first() {
                return first(trie.elems[0]);
            }

            private Traverser<K,V> first(TMap<K,V> t) {
                if (t instanceof Trie) {
                    return new TrieTraverser<>((Trie<K,V>)t, this).first();
                } else {
                    current = t.traverser();
                    return this;
                }
            }

            @Override
            public Traverser<K,V> succ() {
                if (current != null && (current = current.succ()) != null) {
                    return this;
                } else if (++index < trie.elems.length) {
                    return first(trie.elems[index]);
                } else if (parent != null) {
                    return parent.succ();
                } else {
                    return null;
                }
            }

            @Override
            public Node<K,V> cursor() {
                return current.cursor();
            }
        }

        @Override
        public PMap<K,V> force() {
            for (TMap<K,V> elem : elems) {
                elem.force();
            }
            return this;
        }

        @Override
        public PSet<K> toKeySet() {
            @SuppressWarnings("unchecked")
            TSetImpl.TSet<K>[] ks = new TSetImpl.TSet[elems.length];
            for (int i = 0; i < ks.length; i++) {
                ks[i] = (TSetImpl.TSet<K>)elems[i].keySet();
            }
            return new TSetImpl.Trie<>(bitmap, ks, size);
        }

        private static boolean unsignedCompare(int i, int j) {
            return (i < j) ^ (i < 0) ^ (j < 0);
        }

        @Override
        public boolean equals(Object obj) {
            return equals0(obj);
        }

        @Override
        public int hashCode() {
            return Arrays.hashCode(elems);
        }

        @Override
        public String toString() {
            return show();
        }
    }

    static class MapIterator<K,V> implements Iterator<Map.Entry<K,V>> {
        private Traverser<K,V> current;

        MapIterator(TMap<K, V> t) {
            this.current = t.traverser();
        }

        @Override
        public boolean hasNext() {
            return current != null;
        }

        @Override
        public Map.Entry<K,V> next() {
            if (current == null)
                throw new NoSuchElementException();
            Map.Entry<K,V> result = current.cursor();
            current = current.succ();
            return result;
        }
    }

    static class KeyIterator<K,V> implements Iterator<K> {
        private Traverser<K,V> current;

        KeyIterator(TMap<K,V> t) {
            this.current = t.traverser();
        }

        @Override
        public boolean hasNext() {
            return current != null;
        }

        @Override
        public K next() {
            if (current == null)
                throw new NoSuchElementException();
            Map.Entry<K,V> result = current.cursor();
            current = current.succ();
            return result.getKey();
        }
    }

    @SuppressWarnings("override")
    static class KeySetView<K,V> implements PSet<K> {
        private final TMap<K,V> m;
        KeySetView(TMap<K,V> m) {
            this.m = m;
        }

        private PSet<K> copy() {
            return m.toKeySet();
        }

        public int size()                   { return m.size(); }
        public boolean isEmpty()            { return m.isEmpty(); }
        public boolean contains(Object o)   { return m.containsKey(o); }
        public PSet<K> add(K k)             { return copy().add(k); }
        public PSet<K> remove(Object o)     { return copy().remove(o); }
        public PSet<K> clear()              { return HashPSet.empty(); }

        public Maybe<K> find(Predicate<? super K> p) {
            return m.find((k, v) -> p.test(k)).map(Map.Entry::getKey);
        }
        public PSet<K> filter(Predicate<? super K> p) {
            return copy().filter(p);
        }
        public boolean containsAll(PSet<? extends K> s) {
            return copy().containsAll(s);
        }
        public PSet<K> union(PSet<K> s) {
            return copy().union(s);
        }
        public PSet<K> difference(PSet<K> s) {
            return copy().difference(s);
        }
        public PSet<K> intersection(PSet<K> s) {
            return copy().intersection(s);
        }

        public <R> R foldRight(BiFunction<? super K, Supplier<R>, R> f, Supplier<R> sr) {
            return m.foldRightKV((k, v, r) -> f.apply(k, r), sr);
        }
        public <R> R foldRight_(R z, BiFunction<? super K, R, R> f) {
            return m.foldRightKV_(z, (k, v, r) -> f.apply(k, r));
        }
        public <R> R foldLeft(R z, BiFunction<R, ? super K, R> f) {
            return m.foldLeftKV(z, (r, k, v) -> f.apply(r, k));
        }

        public Iterator<K> iterator() {
            return new KeyIterator<>(m);
        }
        public void forEach(Consumer<? super K> action) {
            m.foldLeftKV(Unit.U, (z, k, v) -> { action.accept(k); return z; });
        }
    }
}
