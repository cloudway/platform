/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.data;

import java.util.Arrays;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.Optional;
import java.util.StringJoiner;
import java.util.function.BiFunction;
import java.util.function.BiPredicate;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

import com.cloudway.platform.common.fp.control.Cont;
import com.cloudway.platform.common.fp.function.TriFunction;

@SuppressWarnings("EqualsAndHashcode")
final class TMapImpl {
    private TMapImpl() {}

    private static final Empty<?,?> EMPTY = new Empty<>();

    @SuppressWarnings("unchecked")
    static <K,V> TMap<K,V> empty() {
        return (TMap<K,V>)EMPTY;
    }

    static <K,V> TMap<K,V> singleton(K k, V v) {
        return new Singleton<>(node(k, v), hashing(k));
    }

    static int hashing(Object key) {
        int h = key.hashCode();
        return h ^ (h >>> 16);
    }

    static <K,V> Node<K,V> node(K k, V v) {
        return new Node<>(k, v);
    }

    static <K,V> NodeList<K,V> pair(Node<K,V> a, Node<K,V> b) {
        return new NodeList<>(a, new NodeList<>(b, null));
    }

    static <K,V> NodeList<K,V> cons(Node<K,V> h, NodeList<K,V> t) {
        return new NodeList<>(h, t);
    }

    interface Merger<K,V> {
        Node<K,V> apply(Node<K,V> kv1, Node<K,V> kv2);
        Merger<K,V> invert();
    }

    static class LiftMerger<K,V> implements Merger<K,V> {
        private final BiFunction<Node<K,V>, Node<K,V>, Node<K,V>> mergef;

        LiftMerger(BiFunction<Node<K,V>, Node<K,V>, Node<K,V>> mergef) {
            this.mergef = mergef;
        }

        @Override
        public Node<K,V> apply(Node<K,V> kv1, Node<K,V> kv2) {
            return mergef.apply(kv1, kv2);
        }

        @Override
        public Merger<K,V> invert() {
            return new Merger<K,V>() {
                @Override
                public Node<K, V> apply(Node<K,V> kv1, Node<K,V> kv2) {
                    return mergef.apply(kv2, kv1);
                }
                @Override
                public Merger<K, V> invert() {
                    return LiftMerger.this;
                }
            };
        }
    }

    static <K,V> Merger<K,V> liftMerger(BiFunction<Node<K,V>, Node<K,V>, Node<K,V>> mergef) {
        return new LiftMerger<>(mergef);
    }

    static abstract class TMap<K,V> implements HashPMap<K,V> {
        abstract Node<K,V> get0(Object key, int hash, int level);
        abstract TMap<K,V> put0(Node<K,V> kv, int hash, int level, boolean onlyIfAbsent, Merger<K,V> merger);
        abstract TMap<K,V> remove0(Object key, int hash, int level);
        abstract TMap<K,V> replace0(K key, V oldValue, V newValue, int hash, int level, boolean anyValue);
        abstract <U> TMap<K,U> map0(Function<Node<K,V>, ? extends U> f);
        abstract TMap<K,V> filter0(BiPredicate<? super K, ? super V> p, int level, TMap<K,V>[] buffer, int offset0);
        abstract TMap<K,V> merge0(TMap<K,V> that, int level, Merger<K,V> merger);
        abstract boolean submapOf(TMap<K,V> that, int level);
        abstract <R> R foldl(R z, BiFunction<R, Node<K,V>, R> f);
        abstract <R> R foldr(BiFunction<Node<K,V>, Supplier<R>, R> f, Supplier<R> r);

        Node<K,V> get0(Object key) {
            return get0(key, hashing(key), 0);
        }

        @Override
        public boolean containsKey(Object key) {
            return get0(key) != null;
        }

        @Override
        public Optional<V> lookup(Object key) {
            Node<K,V> kv = get0(key);
            return kv != null ? Optional.of(kv.value) : Optional.empty();
        }

        @Override
        public V get(Object key) {
            Node<K,V> kv = get0(key);
            if (kv == null)
                throw new NoSuchElementException();
            return kv.value;
        }

        @Override
        public V getOrDefault(Object key, V def) {
            Node<K,V> kv = get0(key);
            return kv != null ? kv.value : def;
        }

        @Override
        public PMap<K,V> put(K key, V value) {
            return put0(node(key, value), hashing(key), 0, false, null);
        }

        @Override
        public PMap<K,V> putIfAbsent(K key, V value) {
            return put0(node(key, value), hashing(key), 0, true, null);
        }

        @Override
        public PMap<K,V> remove(Object key) {
            return remove0(key, hashing(key), 0);
        }

        @Override
        public PMap<K,V> replace(K key, V oldValue, V newValue) {
            return replace0(key, oldValue, newValue, hashing(key), 0, false);
        }

        @Override
        public PMap<K,V> replace(K key, V value) {
            return replace0(key, null, value, hashing(key), 0, true);
        }

        @Override
        public <U> PMap<K,U> mapKV(BiFunction<? super K, ? super V, ? extends U> f) {
            return map0(kv -> f.apply(kv.key, kv.value));
        }

        @SuppressWarnings("unchecked")
        private static <K,V> TMap<K,V>[] newBuffer(int minSize) {
            int bufferSize = Math.min(minSize + 6, 32 * 7);
            return new TMap[bufferSize];
        }

        @Override
        public PMap<K,V> filterKV(BiPredicate<? super K, ? super V> p) {
            TMap<K,V> buffer[] = newBuffer(size());
            return filter0(p, 0, buffer, 0);
        }

        @Override
        @SuppressWarnings("unchecked")
        public boolean containsAll(PMap<? extends K, ? extends V> that) {
            if (size() < that.size()) {
                return false;
            } else if (that instanceof TMap) {
                return ((TMap<K,V>)that).submapOf(this, 0);
            } else {
                return that.allMatch((k, v) -> {
                    Node<K,V> kv;
                    return (kv = get0(k)) != null && Objects.equals(kv.value, v);
                });
            }
        }

        @Override
        @SuppressWarnings("unchecked")
        public PMap<K,V> putAll(PMap<? extends K, ? extends V> that) {
            if (that instanceof TMap) {
                return merge0((TMap<K,V>)that, 0, liftMerger((a, b) -> b));
            } else {
                return that.foldLeftKV((PMap<K,V>)this, PMap::put);
            }
        }

        @Override
        public PMap<K,V> merge(PMap<K,V> that, TriFunction<? super K, ? super V, ? super V, ? extends V> f) {
            Merger<K,V> merger = liftMerger((a, b) -> node(a.key, f.apply(a.key, a.value, b.value)));
            return merge0((TMap<K,V>)that, 0, merger);
        }

        @Override
        public PMap<K,V> clear() {
            return TMapImpl.empty();
        }

        @Override
        public <R> R foldLeft(R z, BiFunction<R, ? super V, R> f) {
            return foldl(z, (r, e) -> f.apply(r, e.getValue()));
        }

        @Override
        public <R> R foldLeftKV(R z, TriFunction<R, ? super K, ? super V, R> f) {
            return foldl(z, (r, e) -> f.apply(r, e.key, e.value));
        }

        @Override
        public <R> R foldRight(R z, BiFunction<? super V, Supplier<R>, R> f) {
            return foldr((e, r) -> f.apply(e.value, r), () -> z);
        }

        @Override
        public <R> R foldRightKV(R z, TriFunction<? super K, ? super V, Supplier<R>, R> f) {
            return foldr((e, r) -> f.apply(e.key, e.value, r), () -> z);
        }

        @Override
        public Seq<Map.Entry<K,V>> entries() {
            return foldr(Seq::cons, Seq::nil);
        }

        @Override
        public Iterator<Map.Entry<K,V>> iterator() {
            return Cont.generator(
                foldr((e, r) -> Cont.yield(e).then(r), Cont::<Map.Entry<K,V>>finish))
                .iterator();
        }

        @Override
        public void forEach(Consumer<? super Map.Entry<K,V>> action) {
            foldl(Unit.U, (u, e) -> { action.accept(e); return u; });
        }

        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (!(obj instanceof TMap))
                return false;
            @SuppressWarnings("unchecked")
            TMap<K,V> that = (TMap<K,V>)obj;
            return size() == that.size() && submapOf(that, 0);
        }

        public abstract int hashCode();

        public String toString() {
            return foldl(new StringJoiner(",", "{", "}"),
                         (sj, e) -> sj.add(e.toString()))
                  .toString();
        }
    }

    static final class Node<K,V> implements Map.Entry<K,V> {
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

    static class NodeList<K,V> {
        final Node<K,V> head;
        final NodeList<K,V> tail;

        NodeList(Node<K,V> head, NodeList<K,V> tail) {
            this.head = head;
            this.tail = tail;
        }

        int size() {
            int sz = 0;
            NodeList<K,V> p = this;
            while (p != null) {
                sz++;
                p = p.tail;
            }
            return sz;
        }

        NodeList<K,V> find(BiPredicate<? super K, ? super V> pred) {
            NodeList<K,V> p = this;
            while (p != null) {
                if (pred.test(p.head.key, p.head.value))
                    return p;
                p = p.tail;
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
                res = cons(p.head, res);
                p = p.tail;
            }
            return res;
        }

        Node<K,V> lookup(Object key) {
            NodeList<K,V> p = find((k,v) -> k.equals(key));
            return p != null ? p.head : null;
        }

        NodeList<K,V> insert(Node<K,V> kv) {
            return cons(kv, delete(kv.key));
        }

        NodeList<K,V> delete(Object key) {
            NodeList<K,V> t = find((k,v) -> k.equals(key));
            return t == null ? this : consTo(t, t.tail);
        }

        NodeList<K,V> replace(K key, V oldValue, V newValue, boolean anyValue) {
            NodeList<K,V> t = find((k,v) ->
                k.equals(key) && (anyValue || Objects.equals(v, oldValue)));
            return t == null ? this : consTo(t, cons(node(key, newValue), t.tail));
        }

        <U> NodeList<K,U> map(Function<Node<K,V>, ? extends U> f) {
            NodeList<K,V> p = this;
            NodeList<K,U> r = null;
            while (p != null) {
                r = cons(node(p.head.key, f.apply(p.head)), r);
                p = p.tail;
            }
            return r;
        }

        NodeList<K,V> filter(BiPredicate<? super K, ? super V> pred) {
            NodeList<K,V> t = find(pred.negate());
            if (t == null) {
                return this;
            } else {
                NodeList<K,V> r = consTo(t, null);
                while ((t = t.tail) != null) {
                    if (pred.test(t.head.key, t.head.value)) {
                        r = cons(t.head, r);
                    }
                }
                return r;
            }
        }

        <R> R foldl(R z, BiFunction<R, Node<K,V>, R> f) {
            NodeList<K,V> p = this;
            while (p != null) {
                z = f.apply(z, p.head);
                p = p.tail;
            }
            return z;
        }

        <R> R foldr(BiFunction<Node<K,V>, Supplier<R>, R> f, Supplier<R> r) {
            if (tail == null) {
                return f.apply(head, r);
            } else {
                return f.apply(head, () -> tail.foldr(f, r));
            }
        }
    }

    static class Empty<K,V> extends TMap<K,V> {
        @Override
        public boolean isEmpty() {
            return true;
        }

        @Override
        public int size() {
            return 0;
        }

        @Override
        Node<K,V> get0(Object k, int hash, int level) {
            return null;
        }

        @Override
        public Optional<Map.Entry<K,V>> find(BiPredicate<? super K, ? super V> p) {
            return Optional.empty();
        }

        @Override
        boolean submapOf(TMap<K,V> that, int level) {
            return true;
        }

        @Override
        TMap<K,V> put0(Node<K,V> kv, int hash, int level, boolean onlyIfAbsent, Merger<K,V> merger) {
            return new Singleton<>(kv, hash);
        }

        @Override
        TMap<K,V> remove0(Object key, int hash, int level) {
            return this;
        }

        @Override
        TMap<K,V> replace0(K key, V oldValue, V newValue, int hash, int level, boolean anyValue) {
            return this;
        }

        @Override
        <U> TMap<K,U> map0(Function<Node<K,V>, ? extends U> f) {
            return TMapImpl.empty();
        }

        @Override
        TMap<K,V> filter0(BiPredicate<? super K, ? super V> p, int level, TMap<K,V>[] buffer, int offset0) {
            return this;
        }

        @Override
        TMap<K,V> merge0(TMap<K,V> that, int level, Merger<K,V> merger) {
            return that;
        }

        @Override
        <R> R foldl(R z, BiFunction<R, Node<K,V>, R> f) {
            return z;
        }

        @Override
        <R> R foldr(BiFunction<Node<K,V>, Supplier<R>, R> f, Supplier<R> r) {
            return r.get();
        }

        @Override
        public PSet<K> keySet() {
            return TSetImpl.empty();
        }

        @Override
        public int hashCode() {
            return 0;
        }
    }

    static class Singleton<K,V> extends TMap<K,V> {
        private final Node<K,V> kv;
        private final int hash;

        Singleton(Node<K,V> kv, int hash) {
            this.kv = kv;
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
            return hash == this.hash && kv.key.equals(key);
        }

        @Override
        Node<K,V> get0(Object key, int hash, int level) {
            return equiv(key, hash) ? kv : null;
        }

        @Override
        public Optional<Map.Entry<K,V>> find(BiPredicate<? super K, ? super V> p) {
            return p.test(kv.key, kv.value) ? Optional.of(kv) : Optional.empty();
        }

        @Override
        boolean submapOf(TMap<K,V> that, int level) {
            Node<K,V> node = that.get0(kv.key, hash, level);
            return node != null && Objects.equals(kv.value, node.value);
        }

        @Override
        TMap<K,V> put0(Node<K,V> nkv, int hash, int level, boolean onlyIfAbsent, Merger<K,V> merger) {
            if (equiv(nkv.key, hash)) {
                if (onlyIfAbsent) {
                    return this;
                } else if (merger == null) {
                    return new Singleton<>(nkv, hash);
                } else {
                    return new Singleton<>(merger.apply(kv, nkv), hash);
                }
            } else {
                if (this.hash != hash) {
                    TMap<K,V> that = new Singleton<>(nkv, hash);
                    return Trie.make(this.hash, this, hash, that, level, 2);
                } else {
                    return new Collision<>(hash, pair(kv, nkv));
                }
            }
        }

        @Override
        TMap<K,V> remove0(Object key, int hash, int level) {
            return equiv(key, hash) ? TMapImpl.empty() : this;
        }

        @Override
        TMap<K,V> replace0(K key, V oldValue, V newValue, int hash, int level, boolean anyValue) {
            if (equiv(key, hash) && (anyValue || Objects.equals(kv.value, oldValue))) {
                return new Singleton<>(node(key, newValue), hash);
            } else {
                return this;
            }
        }

        @Override
        <U> TMap<K,U> map0(Function<Node<K,V>, ? extends U> f) {
            return new Singleton<>(node(kv.key, f.apply(kv)), hash);
        }

        @Override
        TMap<K,V> filter0(BiPredicate<? super K, ? super V> p, int level, TMap<K,V>[] buffer, int offset0) {
            return p.test(kv.key, kv.value) ? this : TMapImpl.empty();
        }

        @Override
        TMap<K,V> merge0(TMap<K,V> that, int level, Merger<K,V> merger) {
            return that.put0(kv, hash, level, false, merger.invert());
        }

        @Override
        <R> R foldl(R z, BiFunction<R, Node<K,V>, R> f) {
            return f.apply(z, kv);
        }

        @Override
        <R> R foldr(BiFunction<Node<K,V>, Supplier<R>, R> f, Supplier<R> r) {
            return f.apply(kv, r);
        }

        @Override
        public PSet<K> keySet() {
            return new TSetImpl.Singleton<>(kv.key, hash);
        }

        @Override
        public int hashCode() {
            return hash * 31 + Objects.hash(kv.value);
        }
    }

    static class Collision<K,V> extends TMap<K,V> {
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
        Node<K,V> get0(Object key, int hash, int level) {
            return hash == this.hash ? kvs.lookup(key) : null;
        }

        @Override
        public Optional<Map.Entry<K,V>> find(BiPredicate<? super K, ? super V> p) {
            NodeList<K,V> kv = kvs.find(p);
            return kv != null ? Optional.of(kv.head) : Optional.empty();
        }

        @Override
        boolean submapOf(TMap<K,V> that, int level) {
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
        TMap<K,V> put0(Node<K,V> nkv, int hash, int level, boolean onlyIfAbsent, Merger<K,V> merger) {
            if (hash == this.hash) {
                Node<K,V> kv = kvs.lookup(nkv.key);
                if (kv != null && onlyIfAbsent) {
                    return this;
                } else if (merger == null || kv == null) {
                    return new Collision<>(hash, kvs.insert(nkv));
                } else {
                    return new Collision<>(hash, kvs.insert(merger.apply(kv, nkv)));
                }
            } else {
                TMap<K,V> that = new Singleton<>(nkv, hash);
                return Trie.make(this.hash, this, hash, that, level, size + 1);
            }
        }

        @Override
        TMap<K,V> remove0(Object key, int hash, int level) {
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
            } else if (kvs1.tail == null) {
                return new Singleton<>(kvs1.head, hash);
            } else {
                return new Collision<>(hash, kvs1);
            }
        }

        @Override
        TMap<K,V> replace0(K key, V oldValue, V newValue, int hash, int level, boolean anyValue) {
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
        <U> TMap<K,U> map0(Function<Node<K,V>, ? extends U> f) {
            return new Collision<>(hash, kvs.map(f));
        }

        @Override
        TMap<K,V> filter0(BiPredicate<? super K, ? super V> p, int level, TMap<K, V>[] buffer, int offset0) {
            return afterRemove(kvs.filter(p));
        }

        @Override
        TMap<K, V> merge0(TMap<K,V> that, int level, Merger<K,V> merger) {
            Merger<K,V> invert = merger.invert();
            return kvs.foldl(that, (m, kv) -> m.put0(kv, hash, level, false, invert));
        }

        @Override
        <R> R foldl(R z, BiFunction<R, Node<K,V>, R> f) {
            return kvs.foldl(z, f);
        }

        @Override
        <R> R foldr(BiFunction<Node<K,V>, Supplier<R>, R> f, Supplier<R> r) {
            return kvs.foldr(f, r);
        }

        @Override
        public PSet<K> keySet() {
            TSetImpl.List<K> keys = kvs.foldl(null, (ks, kv) -> TSetImpl.cons(kv.key, ks));
            return new TSetImpl.Collision<>(hash, keys);
        }

        @Override
        public int hashCode() {
            return kvs.foldl(hash * 31, (Integer h, Node<K,V> kv) -> h * 31 + Objects.hashCode(kv.value));
        }
    }

    static class Trie<K,V> extends TMap<K,V> {
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
        Node<K,V> get0(Object key, int hash, int level) {
            int index = (hash >>> level) & 0x1f;
            int mask = (1 << index);
            if (bitmap == -1) {
                return elems[index].get0(key, hash, level + 5);
            } else if ((bitmap & mask) != 0) {
                int offset = Integer.bitCount(bitmap & (mask-1));
                return elems[offset].get0(key, hash, level + 5);
            } else {
                return null;
            }
        }

        @Override
        public Optional<Map.Entry<K,V>> find(BiPredicate<? super K, ? super V> p) {
            for (TMap<K,V> el : elems) {
                Optional<Map.Entry<K,V>> kv = el.find(p);
                if (kv.isPresent())
                    return kv;
            }
            return Optional.empty();
        }

        @Override
        boolean submapOf(TMap<K,V> that, int level) {
            if (that instanceof Trie) {
                return submap2((Trie<K,V>)that, level);
            } else if (size() <= that.size()) {
                return allMatch((k, v) -> {
                    Node<K,V> kv = that.get0(k, hashing(k), level);
                    return kv != null && Objects.equals(v, kv.value);
                });
            } else {
                return false;
            }
        }

        private boolean submap2(Trie<K,V> that, int level) {
            TMap<K,V>[] xels = this.elems;
            TMap<K,V>[] yels = that.elems;
            int xbm = this.bitmap;
            int ybm = that.bitmap;

            if ((xbm & ybm) != xbm) {
                return false;
            }

            int ix = 0, iy = 0;
            while (ix < xels.length && iy < yels.length) {
                int xlsb = xbm ^ (xbm & (xbm - 1));
                int ylsb = ybm ^ (ybm & (ybm - 1));
                if (xlsb == ylsb) {
                    if (!xels[ix].submapOf(yels[iy], level + 5))
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
        TMap<K,V> put0(Node<K,V> nkv, int hash, int level, boolean onlyIfAbsent, Merger<K,V> merger) {
            int index = (hash >>> level) & 0x1f;
            int mask = (1 << index);
            int offset = Integer.bitCount(bitmap & (mask-1));
            if ((bitmap & mask) != 0) {
                TMap<K,V> sub = elems[offset];
                TMap<K,V> subNew = sub.put0(nkv, hash, level + 5, onlyIfAbsent, merger);
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
                elemsNew[offset] = new Singleton<>(nkv, hash);
                System.arraycopy(elems, offset, elemsNew, offset + 1, elems.length - offset);
                return new Trie<>(bitmap | mask, elemsNew, size + 1);
            }
        }

        @Override
        TMap<K,V> remove0(Object key, int hash, int level) {
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
        TMap<K,V> replace0(K key, V oldValue, V newValue, int hash, int level, boolean anyValue) {
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
        <U> TMap<K,U> map0(Function<Node<K,V>, ? extends U> f) {
            @SuppressWarnings("unchecked")
            TMap<K,U> elemsNew[] = new TMap[elems.length];
            for (int i = 0; i < elemsNew.length; i++) {
                elemsNew[i] = elems[i].map0(f);
            }
            return new Trie<>(bitmap, elemsNew, size);
        }

        @Override
        TMap<K,V> filter0(BiPredicate<? super K, ? super V> p, int level, TMap<K,V>[] buffer, int offset0) {
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
        TMap<K,V> merge0(TMap<K,V> that, int level, Merger<K,V> merger) {
            if (that.isEmpty()) {
                return this;
            } else if (that instanceof Trie) {
                return merge2((Trie<K,V>)that, level, merger);
            } else {
                return that.merge0(this, level, merger.invert());
            }
        }

        private TMap<K,V> merge2(Trie<K,V> that, int level, Merger<K,V> merger) {
            TMap<K,V>[] xels = this.elems;
            TMap<K,V>[] yels = that.elems;
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
                    TMap<K,V> sub = xels[ix++].merge0(yels[iy++], level + 5, merger);
                    buffer[i] = sub;
                    totalsize += sub.size();
                    xbm &= ~xlsb;
                    ybm &= ~ylsb;
                } else if (unsignedCompare(xlsb - 1, ylsb - 1)) {
                    TMap<K,V> sub = xels[ix++];
                    buffer[i] = sub;
                    totalsize += sub.size();
                    xbm &= ~xlsb;
                } else {
                    TMap<K,V> sub = yels[iy++];
                    buffer[i] = sub;
                    totalsize += sub.size();
                    ybm &= ~ylsb;
                }
            }
            return new Trie<>(this.bitmap | that.bitmap, buffer, totalsize);
        }

        @Override
        <R> R foldl(R z, BiFunction<R, Node<K,V>, R> f) {
            for (TMap<K,V> elem : elems) {
                z = elem.foldl(z, f);
            }
            return z;
        }

        @Override
        <R> R foldr(BiFunction<Node<K,V>, Supplier<R>, R> f, Supplier<R> r) {
            return _foldr(0, f, r);
        }

        <R> R _foldr(int i, BiFunction<Node<K,V>, Supplier<R>, R> f, Supplier<R> r) {
            if (i == elems.length - 1) {
                return elems[i].foldr(f, r);
            } else {
                return elems[i].foldr(f, () -> _foldr(i+1, f, r));
            }
        }

        @Override
        public PSet<K> keySet() {
            @SuppressWarnings("unchecked")
            TSetImpl.TSet<K>[] kels = new TSetImpl.TSet[elems.length];
            for (int i = 0; i < kels.length; i++) {
                kels[i] = (TSetImpl.TSet<K>)elems[i].keySet();
            }
            return new TSetImpl.Trie<>(bitmap, kels, size);
        }

        @Override
        public int hashCode() {
            return Arrays.hashCode(elems);
        }

        private static boolean unsignedCompare(int i, int j) {
            return (i < j) ^ (i < 0) ^ (j < 0);
        }
    }
}
