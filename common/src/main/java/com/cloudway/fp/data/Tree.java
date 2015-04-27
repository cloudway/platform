/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.data;

import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.StringJoiner;
import java.util.function.BiFunction;
import java.util.function.BiPredicate;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

import com.cloudway.fp.function.TriFunction;
import com.cloudway.fp.control.Applicative;
import com.cloudway.fp.$;

import static com.cloudway.fp.control.Syntax.*;

// @formatter:off

/**
 * The underlying implementation for TreeMap and TreeSet.
 */
final class Tree {
    private Tree() {}

    /**
     * The common operations of a balanced binary tree.
     */
    interface Node<K,V> {
        boolean isEmpty();
        int size();
        V __lookup(K k);
        Maybe<Map.Entry<K,V>> __find(BiPredicate<? super K, ? super V> p);

        Node<K,V> __put(K k, V v);
        Node<K,V> __remove(K k);
        Node<K,V> __putIfAbsent(K k, V v);
        Node<K,V> __computeIfAbsent(K k, Function<? super K, ? extends V> f);
        Node<K,V> __computeIfPresent(K k, BiFunction<? super K, ? super V, Maybe<? extends V>> f);
        Node<K,V> __compute(K k, BiFunction<? super K, Maybe<V>, Maybe<? extends V>> f);
        Node<K,V> __merge(K k, V v, BiFunction<? super V, ? super V, ? extends V> f);

        Node<K,V> __filter(Predicate<Map.Entry<K,V>> p);
        <R> Node<K,R> __map(Function<Map.Entry<K,V>, ? extends R> f);
        <R,T> $<T, Node<K,R>> __traverse(Applicative<T> m, Function<Map.Entry<K,V>, $<T,R>> f);

        boolean valid();
    }
    
    /**
     * An empty tree and responsible to compare elements and construct
     * tree node.
     */
    static abstract class Tip<K,V> implements Node<K,V> {
        final Comparator<? super K> comparator;

        protected Tip(Comparator<? super K> comparator) {
            this.comparator = comparator;
        }

        int compare(K k1, K k2) {
            return comparator.compare(k1, k2);
        }

        abstract Node<K,V> cons(int sz, K k, V v, Node<K,V> l, Node<K,V> r);

        @Override
        public boolean isEmpty() {
            return true;
        }

        @Override
        public int size() {
            return 0;
        }

        @Override
        public V __lookup(K k) {
            return null;
        }

        @Override
        public Maybe<Map.Entry<K,V>> __find(BiPredicate<? super K, ? super V> p) {
            return Maybe.empty();
        }

        @Override
        public Node<K,V> __put(K k, V v) {
            return cons(1, k, v, this, this);
        }

        @Override
        public Node<K,V> __remove(K k) {
            return this;
        }

        @Override
        public Node<K,V> __putIfAbsent(K k, V v) {
            return __put(k, v);
        }

        @Override
        public Node<K,V> __computeIfAbsent(K k, Function<? super K, ? extends V> f) {
            return __put(k, f.apply(k));
        }

        @Override
        public Node<K,V> __computeIfPresent(K k, BiFunction<? super K, ? super V, Maybe<? extends V>> f) {
            return this;
        }

        @Override
        public Node<K,V> __compute(K k, BiFunction<? super K, Maybe<V>, Maybe<? extends V>> f) {
            return f.apply(k, Maybe.empty())
                    .map(x -> __put(k, x))
                    .orElse(this);
        }

        @Override
        public Node<K,V> __merge(K k, V v, BiFunction<? super V, ? super V, ? extends V> f) {
            return __put(k, v);
        }

        @Override
        @SuppressWarnings("unchecked")
        public <R> Node<K,R> __map(Function<Map.Entry<K,V>, ? extends R> f) {
            return (Node<K,R>)this;
        }

        @Override
        public Node<K,V> __filter(Predicate<Map.Entry<K,V>> p) {
            return this;
        }

        @Override
        @SuppressWarnings("unchecked")
        public <R,T> $<T, Node<K,R>> __traverse(Applicative<T> m, Function<Map.Entry<K,V>, $<T,R>> f) {
            return m.pure((Node<K,R>)this);
        }

        @Override
        public boolean valid() {
            return true;
        }
    }

    /**
     * A tree node that contains data and child nodes.
     */
    static class Bin<K,V> implements Node<K,V>, Map.Entry<K,V> {
        final Tip<K,V> tip;
        final int size;
        final K key;
        final V value;
        final Node<K,V> left;
        final Node<K,V> right;

        protected Bin(Tip<K,V> tip, int size, K key, V value, Node<K,V> left, Node<K,V> right) {
            this.tip   = tip;
            this.size  = size;
            this.key   = key;
            this.value = value;
            this.left  = left;
            this.right = right;
        }

        int compare(K k1, K k2) {
            return tip.compare(k1, k2);
        }

        @Override
        public boolean isEmpty() {
            return false;
        }

        @Override
        public int size() {
            return size;
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

        @Override
        public V __lookup(K k) {
            Node<K,V> t = this;
            while (!t.isEmpty()) {
                Bin<K,V> tb = (Bin<K,V>)t;
                int cmp = compare(k, tb.key);
                if (cmp < 0) {
                    t = tb.left;
                } else if (cmp > 0) {
                    t = tb.right;
                } else {
                    return tb.value;
                }
            }
            return null;
        }

        @Override
        public Maybe<Map.Entry<K,V>> __find(BiPredicate<? super K, ? super V> p) {
            if (p.test(key, value)) {
                return Maybe.of(this);
            } else {
                Maybe<Map.Entry<K,V>> e = left.__find(p);
                return e.isPresent() ? e : right.__find(p);
            }
        }

        @Override
        public Node<K,V> __put(K k, V v) {
            int cmp = compare(k, key);
            return cmp < 0 ? balanceL(left.__put(k, v), right) :
                   cmp > 0 ? balanceR(left, right.__put(k, v))
                           : modify(v);
        }

        @Override
        public Node<K,V> __remove(K k) {
            int cmp = compare(k, key);
            return cmp < 0 ? balanceR(left.__remove(k), right) :
                   cmp > 0 ? balanceL(left, right.__remove(k))
                           : delete();
        }

        @Override
        public Node<K,V> __putIfAbsent(K k, V v) {
            int cmp = compare(k, key);
            return cmp < 0 ? balanceL(left.__putIfAbsent(k, v), right) :
                   cmp > 0 ? balanceR(left, right.__putIfAbsent(k, v))
                           : this;
        }

        @Override
        public Node<K,V> __computeIfAbsent(K k, Function<? super K, ? extends V> f) {
            int cmp = compare(k, key);
            return cmp < 0 ? balanceL(left.__computeIfAbsent(k, f), right) :
                   cmp > 0 ? balanceR(left, right.__computeIfAbsent(k, f))
                           : this;
        }

        @Override
        public Node<K,V> __computeIfPresent(K k, BiFunction<? super K, ? super V, Maybe<? extends V>> f) {
            int cmp = compare(k, key);
            return cmp < 0 ? balanceL(left.__computeIfPresent(k, f), right) :
                   cmp > 0 ? balanceR(left, right.__computeIfPresent(k, f))
                           : f.apply(key, value).map(this::modify).orElseGet(this::delete);
        }

        @Override
        public Node<K,V> __compute(K k, BiFunction<? super K, Maybe<V>, Maybe<? extends V>> f) {
            int cmp = compare(k, key);
            return cmp < 0 ? balance(left.__compute(k, f), right) :
                   cmp > 0 ? balance(left, right.__compute(k, f))
                           : f.apply(key, Maybe.of(value)).map(this::modify).orElseGet(this::delete);
        }

        @Override
        public Node<K,V> __merge(K k, V v, BiFunction<? super V, ? super V, ? extends V> f) {
            int cmp = compare(k, key);
            return cmp < 0 ? balanceL(left.__merge(k, v, f), right) :
                   cmp > 0 ? balanceR(left, right.__merge(k, v, f))
                           : modify(f.apply(value, v));
        }

        @Override
        public <R> Node<K,R> __map(Function<Map.Entry<K,V>, ? extends R> f) {
            @SuppressWarnings("unchecked") Tip<K,R> t = (Tip<K,R>)tip;
            return t.cons(size, key, f.apply(this), left.__map(f), right.__map(f));
        }

        @Override
        public Node<K,V> __filter(Predicate<Map.Entry<K,V>> p) {
            if (p.test(this)) {
                return link(left.__filter(p), right.__filter(p));
            } else {
                return merge(left.__filter(p), right.__filter(p));
            }
        }

        @Override
        public <R,T> $<T, Node<K,R>> __traverse(Applicative<T> m, Function<Map.Entry<K,V>, $<T, R>> f) {
            @SuppressWarnings("unchecked") Tip<K,R> t = (Tip<K,R>)tip;
            if (size == 1) {
                return m.map(f.apply(this), v -> t.cons(1, key, v, t, t));
            } else {
                return m.ap3((Node<K,R> l, R v, Node<K,R> r) -> t.cons(size, key, v, l, r),
                             left.__traverse(m, f), f.apply(this), right.__traverse(m, f));
            }
        }

        static <K,V> Maybe<Map.Entry<K,V>> lookupLT(Node<K,V> t, K k) {
            Bin<K,V> lt = null;
            while (!t.isEmpty()) {
                Bin<K,V> b = (Bin<K,V>)t;
                if (b.compare(k, b.key) <= 0) {
                    t = b.left;
                } else {
                    lt = b;
                    t = b.right;
                }
            }
            return Maybe.ofNullable(lt);
        }

        static <K,V> Maybe<Map.Entry<K,V>> lookupGT(Node<K,V> t, K k) {
            Bin<K,V> gt = null;
            while (!t.isEmpty()) {
                Bin<K,V> b = (Bin<K,V>)t;
                if (b.compare(k, b.key) >= 0) {
                    t = b.right;
                } else {
                    gt = b;
                    t = b.left;
                }
            }
            return Maybe.ofNullable(gt);
        }

        static <K,V> Maybe<Map.Entry<K,V>> lookupLE(Node<K,V> t, K k) {
            Bin<K,V> le = null;
            while (!t.isEmpty()) {
                Bin<K,V> b = (Bin<K,V>)t;
                int cmp = b.compare(k, b.key);
                if (cmp < 0) {
                    t = b.left;
                } else if (cmp > 0) {
                    le = b;
                    t = b.right;
                } else {
                    le = b;
                    break;
                }
            }
            return Maybe.ofNullable(le);
        }

        static <K,V> Maybe<Map.Entry<K,V>> lookupGE(Node<K,V> t, K k) {
            Bin<K,V> ge = null;
            while (!t.isEmpty()) {
                Bin<K,V> b = (Bin<K,V>)t;
                int cmp = b.compare(k, b.key);
                if (cmp > 0) {
                    t = b.right;
                } else if (cmp < 0) {
                    ge = b;
                    t = b.left;
                } else {
                    ge = b;
                    break;
                }
            }
            return Maybe.ofNullable(ge);
        }

        static <K,V> Map.Entry<K,V> findFirst(Node<K,V> t) {
            if (t.isEmpty()) {
                throw new NoSuchElementException();
            } else {
                Bin<K,V> b = (Bin<K,V>)t;
                while (!b.left.isEmpty()) {
                    b = (Bin<K,V>)b.left;
                }
                return b;
            }
        }

        static <K,V> Map.Entry<K,V> findLast(Node<K,V> t) {
            if (t.isEmpty()) {
                throw new NoSuchElementException();
            } else {
                Bin<K,V> b = (Bin<K,V>)t;
                while (!b.right.isEmpty()) {
                    b = (Bin<K,V>)b.right;
                }
                return b;
            }
        }

        static <K,V> Node<K,V> union(Node<K,V> t1, Node<K,V> t2) {
            if (t1.isEmpty()) {
                return t2;
            } else if (t2.isEmpty()) {
                return t1;
            } else {
                return hedgeUnion(t1, t2, null, null);
            }
        }

        static <K,V> Node<K,V> hedgeUnion(Node<K,V> t1, Node<K,V> t2, K lo, K hi) {
            if (t2.isEmpty()) {
                return t1;
            } else if (t1.isEmpty()) {
                Bin<K,V> t2b = (Bin<K,V>)t2;
                return t2b.link(filterGt(t2b.left, lo), filterLt(t2b.right, hi));
            } else {
                Bin<K,V> t1b = (Bin<K,V>)t1;
                Bin<K,V> t2b = (Bin<K,V>)t2;
                if (t2b.left.isEmpty() && t2b.right.isEmpty()) {
                    return t1b.__putIfAbsent(t2b.key, t2b.value);
                } else {
                    K mi = t1b.key;
                    return t1b.link(hedgeUnion(t1b.left, t2b.trim(lo, mi), lo, mi),
                                    hedgeUnion(t1b.right, t2b.trim(mi, hi), mi, hi));
                }
            }
        }

        static <K,V> Node<K,V> difference(Node<K,V> t1, Node<K,V> t2) {
            if (t1.isEmpty() || t2.isEmpty()) {
                return t1;
            } else {
                return hedgeDiff(t1, t2, null, null);
            }
        }

        static <K,V> Node<K,V> hedgeDiff(Node<K,V> t1, Node<K,V> t2, K lo, K hi) {
            if (t1.isEmpty()) {
                return t1;
            } else if (t2.isEmpty()) {
                Bin<K,V> t1b = (Bin<K,V>)t1;
                return t1b.link(filterGt(t1b.left, lo), filterLt(t1b.right, hi));
            } else {
                Bin<K,V> t1b = (Bin<K,V>)t1;
                Bin<K,V> t2b = (Bin<K,V>)t2;
                K mi = t2b.key;
                return merge(hedgeDiff(t1b.trim(lo, mi), t2b.left, lo, mi),
                             hedgeDiff(t1b.trim(mi, hi), t2b.right, mi, hi));
            }
        }

        static <K,V> Node<K,V> intersection(Node<K,V> t1, Node<K,V> t2) {
            if (t1.isEmpty()) {
                return t1;
            } else if (t2.isEmpty()) {
                return t2;
            } else {
                return hedgeInt(t1, t2, null, null);
            }
        }

        static <K,V> Node<K,V> hedgeInt(Node<K,V> t1, Node<K,V> t2, K lo, K hi) {
            if (t1.isEmpty()) {
                return t1;
            } else if (t2.isEmpty()) {
                return t2;
            } else {
                Bin<K,V> t1b = (Bin<K,V>)t1;
                Bin<K,V> t2b = (Bin<K,V>)t2;
                K mi = t1b.key;
                Node<K,V> l = hedgeInt(t1b.left, t2b.trim(lo, mi), lo, mi);
                Node<K,V> r = hedgeInt(t1b.right, t2b.trim(mi, hi), mi, hi);
                return (t2.__lookup(t1b.key) != null) ? t1b.link(l, r) : merge(l, r);
            }
        }

        static <K,V,R> R foldLeft(Node<K,V> t, R z, BiFunction<R, Map.Entry<K,V>, R> f) {
            if (!t.isEmpty()) {
                PreOrder<K,V> st = new PreOrder<>();
                for (Bin<K,V> p = st.first((Bin<K,V>)t); p != null; p = st.succ(p)) {
                    z = f.apply(z, p);
                }
            }
            return z;
        }

        static <K,V,R> R foldRight(Node<K,V> t, BiFunction<Map.Entry<K,V>, Supplier<R>, R> f, Supplier<R> r) {
            if (t.isEmpty()) {
                return r.get();
            } else {
                PreOrder<K,V> st = new PreOrder<>();
                return foldr(st.first((Bin<K,V>)t), st, f, r);
            }
        }

        private static <K,V,R> R foldr(Bin<K,V> t, PreOrder<K,V> st,
                BiFunction<Map.Entry<K,V>, Supplier<R>, R> f, Supplier<R> r) {
            if (t == null) {
                return r.get();
            } else {
                return f.apply(t, () -> foldr(st.succ(t), st, f, r));
            }
        }

        static <K,V,R> R foldRight_(Node<K,V> t, R z, BiFunction<Map.Entry<K,V>, R, R> f) {
            if (!t.isEmpty()) {
                PostOrder<K,V> st = new PostOrder<>();
                for (Bin<K,V> p = st.last((Bin<K,V>)t); p != null; p = st.pred(p)) {
                    z = f.apply(p, z);
                }
            }
            return z;
        }

        // Helper methods

        Node<K,V> modify(V v) {
            return (v == value) ? this : tip.cons(size, key, v, left, right);
        }

        Node<K,V> delete() {
            return glue(tip, left, right);
        }

        Node<K,V> balance(Node<K,V> l, Node<K,V> r) {
            return (l == left && r == right) ? this : balance(tip, key, value, l, r);
        }

        Node<K,V> balanceL(Node<K,V> l, Node<K,V> r) {
            return (l == left && r == right) ? this : balanceL(tip, key, value, l, r);
        }

        Node<K,V> balanceR(Node<K,V> l, Node<K,V> r) {
            return (l == left && r == right) ? this : balanceR(tip, key, value, l, r);
        }

        // Utility methods that return sub-ranges of the original tree

        /**
         * Trims away all subtrees that surely contain no values between the
         * range [lk] and [hk]. The returned tree is either empty or the key
         * of the root is between lk and hk.
         */
        Node<K,V> trim(K lk, K hk) {
            if (lk == null && hk == null) {
                return this;
            }

            Node<K,V> t = this;
            if (hk == null) {
                while (!t.isEmpty()) {
                    Bin<K,V> tb = (Bin<K,V>)t;
                    if (compare(tb.key, lk) > 0)
                        break;
                    t = tb.right;
                }
            } else if (lk == null) {
                while (!t.isEmpty()) {
                    Bin<K,V> tb = (Bin<K,V>)t;
                    if (compare(tb.key, hk) < 0)
                        break;
                    t = tb.left;
                }
            } else {
                while (!t.isEmpty()) {
                    Bin<K,V> tb = (Bin<K,V>)t;
                    if (compare(tb.key, lk) <= 0) {
                        t = tb.right;
                    } else if (compare(tb.key, hk) >= 0) {
                        t = tb.left;
                    } else {
                        break;
                    }
                }
            }
            return t;
        }

        /*
         * Filter all keys >k from tree
         */
        static <K,V> Node<K,V> filterGt(Node<K,V> t, K k) {
            if (k == null || t.isEmpty()) {
                return t;
            }

            while (!t.isEmpty()) {
                Bin<K,V> tb = (Bin<K,V>)t;
                int cmp = tb.compare(k, tb.key);
                if (cmp < 0) {
                    t = tb.link(filterGt(tb.left, k), tb.right);
                    break;
                } else if (cmp > 0) {
                    t = tb.right;
                } else {
                    t = tb.right;
                    break;
                }
            }
            return t;
        }

        /*
         * Filter all keys <k from tree.
         */
        static <K,V> Node<K,V> filterLt(Node<K,V> t, K k) {
            if (k == null || t.isEmpty()) {
                return t;
            }

            while (!t.isEmpty()) {
                Bin<K,V> tb = (Bin<K,V>)t;
                int cmp = tb.compare(tb.key, k);
                if (cmp < 0) {
                    t = tb.link(tb.left, filterLt(tb.right, k));
                    break;
                } else if (cmp > 0) {
                    t = tb.left;
                } else {
                    t = tb.left;
                    break;
                }
            }
            return t;
        }

        /**
         * Performs a split and also returns whether the pivot element was found
         * in the original tree.
         */
        static <K,V> Triple<V, Node<K,V>, Node<K,V>> splitMember(Node<K,V> t, K k) {
            if (t.isEmpty()) {
                return Tuple.of(null, t, t);
            } else {
                Bin<K,V> b = (Bin<K,V>)t;
                int cmp = b.compare(k, b.key);
                return cmp < 0 ? splitMember(b.left, k).as((z, lt, gt) ->
                                    Tuple.of(z, lt, b.link(gt, b.right))) :
                       cmp > 0 ? splitMember(b.right, k).as((z, lt, gt) ->
                                    Tuple.of(z, b.link(b.left, lt), gt))
                               : Tuple.of(b.value, b.left, b.right);
            }
        }

        /**
         * Returns true if all keys in t1 are in t2, and p returns true when
         * applied to their respective values.
         */
        static <K,V> boolean isSubsetOf(Node<K,V> t1, Node<K,V> t2, BiPredicate<? super V, ? super V> p) {
            if (t1.isEmpty()) {
                return true;
            } else if (t2.isEmpty()) {
                return false;
            } else {
                Bin<K,V> t1b = (Bin<K,V>)t1;
                return splitMember(t2, t1b.key).as((z, lt, gt) ->
                    z != null && p.test(t1b.value, z) &&
                    isSubsetOf(t1b.left, lt, p) &&
                    isSubsetOf(t1b.right, gt, p));
            }
        }

        // Utility methods that maintain the balance properties of the tree.

        /**
         * Restores balance and size.
         */
        Node<K,V> link(Node<K,V> l, Node<K,V> r) {
            if (l.isEmpty()) {
                return insertMin(key, value, r);
            } else if (r.isEmpty()) {
                return insertMax(key, value, l);
            } else {
                Bin<K,V> lb = (Bin<K,V>)l, rb = (Bin<K,V>)r;
                if (lb.size * DELTA < rb.size) {
                    return rb.balanceL(link(l, rb.left), rb.right);
                } else if (rb.size * DELTA < lb.size) {
                    return lb.balanceR(lb.left, link(lb.right, r));
                } else {
                    return tip.cons(l.size() + r.size() + 1, key, value, l, r);
                }
            }
        }

        static <K,V> Node<K,V> insertMax(K k, V v, Node<K,V> t) {
            if (t.isEmpty()) {
                return t.__put(k, v);
            } else {
                Bin<K,V> b = (Bin<K,V>)t;
                return b.balanceR(b.left, insertMax(k, v, b.right));
            }
        }

        static <K,V> Node<K,V> insertMin(K k, V v, Node<K,V> t) {
            if (t.isEmpty()) {
                return t.__put(k, v);
            } else {
                Bin<K,V> b = (Bin<K,V>)t;
                return b.balanceL(insertMin(k, v, b.left), b.right);
            }
        }

        /**
         * Merge two trees.
         */
        static <K,V> Node<K,V> merge(Node<K,V> l, Node<K,V> r) {
            if (l.isEmpty()) {
                return r;
            } else if (r.isEmpty()) {
                return l;
            } else {
                Bin<K,V> lb = (Bin<K,V>)l, rb = (Bin<K,V>)r;
                if (lb.size * DELTA < rb.size) {
                    return rb.balanceL(merge(l, rb.left), rb.right);
                } else if (rb.size * DELTA < lb.size) {
                    return lb.balanceR(lb.left, merge(lb.right, r));
                } else {
                    return glue(lb.tip, l, r);
                }
            }
        }

        /**
         * Glues two trees together. Assumes that 'l' and 'r' are already
         * balanced with respect to each other.
         */
        static <K,V> Node<K,V> glue(Tip<K,V> tip, Node<K,V> l, Node<K,V> r) {
            if (l.isEmpty()) {
                return r;
            } else if (r.isEmpty()) {
                return l;
            } else if (l.size() > r.size()) {
                return deleteFindMax(l, (k, v, t) -> balanceR(tip, k, v, t, r));
            } else {
                return deleteFindMin(r, (k, v, t) -> balanceL(tip, k, v, l, t));
            }
        }

        /**
         * Delete and find the minimal element.
         */
        static <K, V, R> R deleteFindMin(Node<K,V> t, TriFunction<K, V, Node<K,V>, R> f) {
            if (t.isEmpty()) {
                throw new IllegalStateException("Cannot return the minimal element of an empty map");
            }

            Seq<Bin<K,V>> stack = Seq.nil();
            Bin<K,V> b = (Bin<K,V>)t;
            while (!b.left.isEmpty()) {
                stack = Seq.cons(b, stack);
                b = (Bin<K,V>)b.left;
            }

            return f.apply(b.key, b.value, stack.foldLeft(b.right,
                        (min, x) -> x.balanceR(min, x.right)));
        }

        /**
         * Delete and find the maximal element.
         */
        static <K, V, R> R deleteFindMax(Node<K,V> t, TriFunction<K, V, Node<K,V>, R> f) {
            if (t.isEmpty()) {
                throw new IllegalStateException("Cannot return the maximal element of an empty map");
            }

            Seq<Bin<K,V>> stack = Seq.nil();
            Bin<K,V> b = (Bin<K,V>)t;
            while (!b.right.isEmpty()) {
                stack = Seq.cons(b, stack);
                b = (Bin<K,V>)b.right;
            }

            return f.apply(b.key, b.value, stack.foldLeft(b.left,
                        (max, x) -> x.balanceL(x.left, max)));
        }

        // Balance trees

        private static final int DELTA = 3;
        private static final int RATIO = 2;

        /**
         * Balance two trees with value x.  The sizes of the trees should balance
         * after decreasing the size of one of them (a rotation).
         *
         * <ul>
         * <li>{@code DELTA} is the maximum relative difference between the sizes of
         * two trees, it corresponds with the [w] in Adams' paper.</li>
         * <li>{@code RATIO} is the ratio between an outer and inner sibling of the
         * heavier subtree in an unbalanced setting. It determines whether a double
         * or single rotation should be performed to restore balance.  It is corresponds
         * with the inverse of {@code &alpha;} in Adam's article.</li>
         * </ul>
         */
        static <K, V> Node<K,V> balance(Tip<K,V> tip, K k, V v, Node<K,V> l, Node<K,V> r) {
            if (l.isEmpty()) {
                if (r.isEmpty()) {
                    return tip.cons(1, k, v, tip, tip);
                }
                Bin<K,V> rb = (Bin<K,V>)r;
                if (rb.left.isEmpty() && rb.right.isEmpty()) {
                    return tip.cons(2, k, v, tip, r);
                } else if (rb.left.isEmpty()) {
                    return tip.cons(3, rb.key, rb.value,
                        tip.cons(1, k, v, tip, tip),
                        rb.right);
                } else if (rb.right.isEmpty()) {
                    Bin<K,V> rlb = (Bin<K,V>)rb.left;
                    return tip.cons(3, rlb.key, rlb.value,
                        tip.cons(1, k, v, tip, tip),
                        tip.cons(1, rb.key, rb.value, tip, tip));
                } else {
                    Bin<K,V> rlb = (Bin<K,V>)rb.left;
                    Bin<K,V> rrb = (Bin<K,V>)rb.right;
                    if (rlb.size < RATIO * rrb.size) {
                        return tip.cons(1 + rb.size, rb.key, rb.value,
                            tip.cons(1 + rlb.size, k, v, tip, rlb),
                            rrb);
                    } else {
                        return tip.cons(1 + rb.size, rlb.key, rlb.value,
                            tip.cons(1 + rlb.left.size(), k, v, tip, rlb.left),
                            tip.cons(1 + rrb.size + rlb.right.size(), rb.key, rb.value, rlb.right, rrb));
                    }
                }
            } else {
                Bin<K,V> lb = (Bin<K,V>)l;
                if (r.isEmpty()) {
                    if (lb.left.isEmpty() && lb.right.isEmpty()) {
                        return tip.cons(2, k, v, l, tip);
                    } else if (lb.left.isEmpty()) {
                        Bin<K,V> lrb = (Bin<K,V>)lb.right;
                        return tip.cons(3, lrb.key, lrb.value,
                            tip.cons(1, lb.key, lb.value, tip, tip),
                            tip.cons(1, k, v, tip, tip));
                    } else if (lb.right.isEmpty()) {
                        return tip.cons(3, lb.key, lb.value, lb.left,
                            tip.cons(1, k, v, tip, tip));
                    } else {
                        Bin<K,V> llb = (Bin<K,V>)lb.left;
                        Bin<K,V> lrb = (Bin<K,V>)lb.right;
                        if (lrb.size < RATIO * llb.size) {
                            return tip.cons(1 + lb.size, lb.key, lb.value, llb,
                                tip.cons(1 + lrb.size, k, v, lrb, tip));
                        } else {
                            return tip.cons(1 + lb.size, lrb.key, lrb.value,
                                tip.cons(1 + llb.size + lrb.left.size(), lb.key, lb.value, llb, lrb.left),
                                tip.cons(1 + lrb.right.size(), k, v, lrb.right, tip));
                        }
                    }
                } else {
                    Bin<K,V> rb = (Bin<K,V>)r;
                    if (rb.size > DELTA * lb.size) {
                        if (rb.left.isEmpty() || rb.right.isEmpty()) {
                            throw new IllegalStateException("Failure in balance");
                        }
                        Bin<K,V> rlb = (Bin<K,V>)rb.left;
                        Bin<K,V> rrb = (Bin<K,V>)rb.right;
                        if (rlb.size < RATIO * rrb.size) {
                            return tip.cons(1 + lb.size + rb.size, rb.key, rb.value,
                                tip.cons(1 + lb.size + rlb.size, k, v, l, rlb),
                                rrb);
                        } else {
                            return tip.cons(1 + lb.size + rb.size, rlb.key, rlb.value,
                                tip.cons(1 + lb.size + rlb.left.size(), k, v, l, rlb.left),
                                tip.cons(1 + rrb.size + rlb.right.size(), rb.key, rb.value, rlb.right, rrb));
                        }
                    } else if (lb.size > DELTA * rb.size) {
                        if (lb.left.isEmpty() || lb.right.isEmpty()) {
                            throw new IllegalStateException("Failure in balance");
                        }
                        Bin<K,V> llb = (Bin<K,V>)lb.left;
                        Bin<K,V> lrb = (Bin<K,V>)lb.right;
                        if (lrb.size < RATIO * llb.size) {
                            return tip.cons(1 + lb.size + rb.size, lb.key, lb.value, llb,
                                tip.cons(1 + rb.size + lrb.size, k, v, lrb, r));
                        } else {
                            return tip.cons(1 + lb.size + rb.size, lrb.key, lrb.value,
                                tip.cons(1 + llb.size + lrb.left.size(), lb.key, lb.value, llb, lrb.left),
                                tip.cons(1 + rb.size + lrb.right.size(), k, v, lrb.right, r));
                        }
                    } else {
                        return tip.cons(1 + lb.size + rb.size, k, v, l, r);
                    }
                }
            }
        }

        // balanceL and balanceR are specialized versions of balance.
        // balanceL only checks whether the left subtree is too big,
        // balanceR only checks whether the right subtree is too big.

        /**
         * balanceL is called when left subtree might have been inserted to or when
         * right subtree might have been deleted from.
         */
        static <K, V> Node<K,V> balanceL(Tip<K,V> tip, K k, V v, Node<K,V> l, Node<K,V> r) {
            if (r.isEmpty()) {
                if (l.isEmpty()) {
                    return tip.cons(1, k, v, tip, tip);
                }
                Bin<K,V> lb = (Bin<K,V>)l;
                if (lb.left.isEmpty() && lb.right.isEmpty()) {
                    return tip.cons(2, k, v, l, tip);
                } else if (lb.left.isEmpty()) {
                    Bin<K,V> lrb = (Bin<K,V>)lb.right;
                    return tip.cons(3, lrb.key, lrb.value,
                        tip.cons(1, lb.key, lb.value, tip, tip),
                        tip.cons(1, k, v, tip, tip));
                } else if (lb.right.isEmpty()) {
                    return tip.cons(3, lb.key, lb.value, lb.left,
                        tip.cons(1, k, v, tip, tip));
                } else {
                    Bin<K,V> llb = (Bin<K,V>)lb.left;
                    Bin<K,V> lrb = (Bin<K,V>)lb.right;
                    if (lrb.size < RATIO * llb.size) {
                        return tip.cons(1 + lb.size, lb.key, lb.value, llb,
                            tip.cons(1 + lrb.size, k, v, lrb, tip));
                    } else {
                        return tip.cons(1 + lb.size, lrb.key, lrb.value,
                            tip.cons(1 + llb.size + lrb.left.size(), lb.key, lb.value, llb, lrb.left),
                            tip.cons(1 + lrb.right.size(), k, v, lrb.right, tip));
                    }
                }
            } else {
                if (l.isEmpty()) {
                    return tip.cons(1 + r.size(), k, v, tip, r);
                }
                Bin<K,V> lb = (Bin<K,V>)l;
                Bin<K,V> rb = (Bin<K,V>)r;
                if (lb.size > DELTA * rb.size) {
                    if (lb.left.isEmpty() || lb.right.isEmpty()) {
                        throw new IllegalStateException("Failure in balanceL");
                    }
                    Bin<K,V> llb = (Bin<K,V>)lb.left;
                    Bin<K,V> lrb = (Bin<K,V>)lb.right;
                    if (lrb.size < RATIO * llb.size) {
                        return tip.cons(1 + lb.size + rb.size, lb.key, lb.value, llb,
                            tip.cons(1 + rb.size + lrb.size, k, v, lrb, r));
                    } else {
                        return tip.cons(1 + lb.size + rb.size, lrb.key, lrb.value,
                            tip.cons(1 + llb.size + lrb.left.size(), lb.key, lb.value, llb, lrb.left),
                            tip.cons(1 + rb.size + lrb.right.size(), k, v, lrb.right, r));
                    }
                } else {
                    return tip.cons(1 + lb.size + rb.size, k, v, l, r);
                }
            }
        }

        /**
         * balanceR is called when right subtree might have been inserted to or when
         * left subtree might have been deleted from.
         */
        static <K, V> Node<K,V> balanceR(Tip<K,V> tip, K k, V v, Node<K,V> l, Node<K,V> r) {
            if (l.isEmpty()) {
                if (r.isEmpty()) {
                    return tip.cons(1, k, v, tip, tip);
                }
                Bin<K,V> rb = (Bin<K,V>)r;
                if (rb.left.isEmpty() && rb.right.isEmpty()) {
                    return tip.cons(2, k, v, tip, r);
                } else if (rb.left.isEmpty()) {
                    return tip.cons(3, rb.key, rb.value,
                        tip.cons(1, k, v, tip, tip),
                        rb.right);
                } else if (rb.right.isEmpty()) {
                    Bin<K,V> rlb = (Bin<K,V>)rb.left;
                    return tip.cons(3, rlb.key, rlb.value,
                        tip.cons(1, k, v, tip, tip),
                        tip.cons(1, rb.key, rb.value, tip, tip));
                } else {
                    Bin<K,V> rlb = (Bin<K,V>)rb.left;
                    Bin<K,V> rrb = (Bin<K,V>)rb.right;
                    if (rlb.size < RATIO * rrb.size) {
                        return tip.cons(1 + rb.size, rb.key, rb.value,
                            tip.cons(1 + rlb.size, k, v, tip, rlb),
                            rrb);
                    } else {
                        return tip.cons(1 + rb.size, rlb.key, rlb.value,
                            tip.cons(1 + rlb.left.size(), k, v, tip, rlb.left),
                            tip.cons(1 + rrb.size + rlb.right.size(), rb.key, rb.value, rlb.right, rrb));
                    }
                }
            } else {
                if (r.isEmpty()) {
                    return tip.cons(1 + l.size(), k, v, l, tip);
                }
                Bin<K,V> lb = (Bin<K,V>)l;
                Bin<K,V> rb = (Bin<K,V>)r;
                if (rb.size > DELTA * lb.size) {
                    if (rb.left.isEmpty() || rb.right.isEmpty()) {
                        throw new IllegalStateException("Failure in balanceR");
                    }
                    Bin<K,V> rlb = (Bin<K,V>)rb.left;
                    Bin<K,V> rrb = (Bin<K,V>)rb.right;
                    if (rlb.size < RATIO * rrb.size) {
                        return tip.cons(1 + lb.size + rb.size, rb.key, rb.value,
                            tip.cons(1 + lb.size + rlb.size, k, v, l, rlb),
                            rrb);
                    } else {
                        return tip.cons(1 + lb.size + rb.size, rlb.key, rlb.value,
                            tip.cons(1 + lb.size + rlb.left.size(), k, v, l, rlb.left),
                            tip.cons(1 + rrb.size + rlb.right.size(), rb.key, rb.value, rlb.right, rrb));
                    }
                } else {
                    return tip.cons(1 + lb.size + rb.size, k, v, l, r);
                }
            }
        }

        // Show

        static <K, V> String showTree(Node<K,V> t, BiFunction<? super K, ? super V, String> elem, Seq<String> bars) {
            if (t.isEmpty()) {
                return showBars(bars) + "@\n";
            }

            Bin<K,V> tb = (Bin<K,V>)t;
            if (tb.left.isEmpty() && tb.right.isEmpty()) {
                return showBars(bars) + elem.apply(tb.key, tb.value) + "\n";
            } else {
                return showBars(bars) + elem.apply(tb.key, tb.value) + "\n"
                     + showTree(tb.left, elem, withBar(bars))
                     + showTree(tb.right, elem, withEmpty(bars));
            }
        }

        static Seq<String> withBar(Seq<String> bars) {
            return Seq.cons("|  ", bars);
        }

        static Seq<String> withEmpty(Seq<String> bars) {
            return Seq.cons("   ", bars);
        }

        static String showBars(Seq<String> bars) {
            return bars.isEmpty() ? "" : bars.tail().reverse().show("", "", "") + "+--";
        }

        // Assertions

        @Override
        public boolean valid() {
            return balanced(this) && ordered(this) && validsize(this);
        }

        private static <K,V> boolean balanced(Node<K,V> t) {
            if (t.isEmpty()) {
                return true;
            } else {
                Node<K,V> l = ((Bin<K,V>)t).left, r = ((Bin<K,V>)t).right;
                return (l.size() + r.size() <= 1 || (l.size() <= DELTA * r.size() &&
                                                     r.size() <= DELTA * l.size()))
                    && balanced(l) && balanced(r);
            }
        }

        private static <K, V> boolean ordered(Node<K,V> t) {
            return bounded(t, Fn.pure(true), Fn.pure(true));
        }

        private static <K, V> boolean bounded(Node<K,V> t, Function<K, Boolean> lo, Function<K, Boolean> hi) {
            if (t.isEmpty()) {
                return true;
            } else {
                Bin<K,V> b = (Bin<K,V>)t;
                return lo.apply(b.key) && hi.apply(b.key)
                    && bounded(b.left, lo, k -> b.compare(k, b.key) < 0)
                    && bounded(b.right, k -> b.compare(k, b.key) > 0, hi);
            }
        }

        private static <K, V> boolean validsize(Node<K,V> t) {
            return realsize(t).filter(sz -> sz == t.size()).isPresent();
        }

        private static <K, V> Maybe<Integer> realsize(Node<K,V> t) {
            if (t.isEmpty()) {
                return Maybe.of(0);
            } else {
                Bin<K,V> b = (Bin<K,V>)t;
                return select.from(realsize(b.left), n ->
                              from(realsize(b.right), m ->
                              where(n + m + 1 == b.size,
                              yield(b.size))));
            }
        }
    }

    static class PreOrder<K,V> {
        Seq<Bin<K,V>> stack = Seq.nil();

        final Bin<K,V> first(Bin<K,V> t) {
            while (!t.left.isEmpty()) {
                stack = Seq.cons(t, stack);
                t = (Bin<K,V>)t.left;
            }
            return t;
        }

        final Bin<K,V> succ(Bin<K,V> t) {
            if (!t.right.isEmpty()) {
                return first((Bin<K,V>)t.right);
            } else if (!stack.isEmpty()) {
                Bin<K,V> up = stack.head();
                stack = stack.tail();
                return up;
            } else {
                return null;
            }
        }
    }

    static class PostOrder<K,V> {
        Seq<Bin<K,V>> stack = Seq.nil();

        final Bin<K,V> last(Bin<K,V> t) {
            while (!t.right.isEmpty()) {
                stack = Seq.cons(t, stack);
                t = (Bin<K,V>)t.right;
            }
            return t;
        }

        final Bin<K,V> pred(Bin<K,V> t) {
            if (!t.left.isEmpty()) {
                return last((Bin<K,V>)t.left);
            } else if (!stack.isEmpty()) {
                Bin<K,V> up = stack.head();
                stack = stack.tail();
                return up;
            } else {
                return null;
            }
        }
    }

    static class TreeIterator<K,V> extends PreOrder<K,V> implements Iterator<K> {
        Bin<K,V> current;

        public TreeIterator(Bin<K,V> tree) {
            this.current = first(tree);
        }

        @Override
        public boolean hasNext() {
            return current != null;
        }

        @Override
        public K next() {
            if (current == null)
                throw new NoSuchElementException();
            K result = current.key;
            current = succ(current);
            return result;
        }
    }

    static class MapIterator<K,V> extends PreOrder<K,V> implements Iterator<Map.Entry<K,V>> {
        Bin<K,V> current;

        public MapIterator(Bin<K,V> tree) {
            this.current = first(tree);
        }

        @Override
        public boolean hasNext() {
            return current != null;
        }

        @Override
        public Map.Entry<K,V> next() {
            if (current == null)
                throw new NoSuchElementException();
            Map.Entry<K,V> result = current;
            current = succ(current);
            return result;
        }
    }

    /**
     * Iteration starts at a given origin and continues up to but not
     * including a given fence (or null for end).
     */
    static class TreeSpliterator<K,V> extends PreOrder<K,V> implements Spliterator<K> {
        final Bin<K,V> tree;
        Bin<K,V> current;      // traverser; initially first node in range
        Bin<K,V> fence;        // one past last, or null
        int side;              // 0: top, -1: is a left split, +1: right
        int est;               // size estimate

        public TreeSpliterator(Bin<K,V> tree) {
            this(tree, null, null, Seq.nil(), 0, -1);
        }

        private TreeSpliterator(Bin<K,V> tree, Bin<K,V> origin, Bin<K,V> fence,
                                Seq<Bin<K,V>> stack, int side, int est) {
            this.tree = tree;
            this.current = origin;
            this.fence = fence;
            this.stack = stack;
            this.side = side;
            this.est = est;
        }

        final int getEstimate() { // force initialization
            if (est < 0) {
                current = first(tree);
                est = tree.size();
            }
            return est;
        }

        @Override
        public final long estimateSize() {
            return getEstimate();
        }

        @Override
        public TreeSpliterator<K,V> trySplit() {
            if (est < 0)
                getEstimate(); // force initialization
            int d = side;
            Bin<K,V> e = current, f = fence;
            Node<K,V>
                s = ((e == null || e == f) ? null :     // empty
                     (d == 0)              ? tree :     // was top
                     (d >  0)              ? e.right :  // was right
                     (d <  0 && f != null) ? f.left :   // was left
                     null);
            if (s != null && !s.isEmpty() && s != e && s != f) {
                Bin<K,V> t = (Bin<K,V>)s;
                if (tree.compare(e.key, t.key) < 0) { // e not already past s
                    Seq<Bin<K,V>> st = stack;
                    stack = Seq.nil();
                    current = t;
                    side = 1;
                    return new TreeSpliterator<>(tree, e, t, st, -1, est >>>= 1);
                }
            }
            return null;
        }

        @Override
        public void forEachRemaining(Consumer<? super K> action) {
            if (est < 0)
                getEstimate(); // force initialization
            Bin<K,V> f = fence, e;
            if ((e = current) != null && e != f) {
                current = f; // exhaust
                do {
                    action.accept(e.key);
                    e = succ(e);
                } while (e != null && e != f);
            }
        }

        @Override
        public boolean tryAdvance(Consumer<? super K> action) {
            Bin<K,V> e;
            if (est < 0)
                getEstimate(); // force initialization
            if ((e = current) == null || e == fence)
                return false;
            current = succ(e);
            action.accept(e.key);
            return true;
        }

        @Override
        public int characteristics() {
            return (side == 0 ? Spliterator.SIZED : 0) |
                Spliterator.DISTINCT | Spliterator.SORTED |
                Spliterator.ORDERED | Spliterator.IMMUTABLE;
        }

        @Override
        public final Comparator<? super K> getComparator() {
            return tree.tip.comparator;
        }
    }

    // ------------------------------------------------------------------------

    @SuppressWarnings({"rawtypes", "unchecked"})
    static final MapTip EMPTY_MAP = new MapTip(Comparator.naturalOrder());

    // mixin interface to convert generic tree node to map node
    @SuppressWarnings("unchecked")
    interface MapNode<K,V> extends Node<K,V>, TreePMap<K,V> {
        @Override
        default boolean containsKey(Object k) {
            return __lookup((K)k) != null;
        }

        @Override
        default Maybe<V> lookup(Object k) {
            return Maybe.ofNullable(__lookup((K)k));
        }

        @Override
        default V get(Object k) {
            V v = __lookup((K)k);
            if (v == null)
                throw new NoSuchElementException();
            return v;
        }

        @Override
        default V getOrDefault(Object k, V d) {
            V v = __lookup((K)k);
            return v != null ? v : d;
        }

        @Override
        default Maybe<Map.Entry<K,V>> find(BiPredicate<? super K, ? super V> p) {
            return __find(p);
        }

        @Override
        default PMap<K,V> put(K k, V v) {
            Objects.requireNonNull(v);
            return (PMap<K,V>)__put(k, v);
        }

        @Override
        default PMap<K,V> remove(Object k) {
            return (PMap<K,V>)__remove((K)k);
        }

        @Override
        default PMap<K,V> putIfAbsent(K k, V v) {
            Objects.requireNonNull(v);
            return (PMap<K,V>)__putIfAbsent(k, v);
        }

        @Override
        default PMap<K,V> computeIfAbsent(K k, Function<? super K, ? extends V> f) {
            return (PMap<K,V>)__computeIfAbsent(k, f);
        }

        @Override
        default PMap<K,V> computeIfPresent(K k, BiFunction<? super K, ? super V, Maybe<? extends V>> f) {
            return (PMap<K,V>)__computeIfPresent(k, f);
        }

        @Override
        default PMap<K,V> compute(K k, BiFunction<? super K, Maybe<V>, Maybe<? extends V>> f) {
            return (PMap<K,V>)__compute(k, f);
        }

        @Override
        default PMap<K,V> merge(K k, V v, BiFunction<? super V, ? super V, ? extends V> f) {
            Objects.requireNonNull(v);
            return (PMap<K,V>)__merge(k, v, f);
        }

        @Override
        default boolean containsAll(PMap<? extends K, ? extends V> that) {
            if (this.size() < that.size()) {
                return false;
            } else if (that instanceof Node) {
                return Bin.isSubsetOf((Node<K,V>)that, this, Objects::equals);
            } else {
                return that.allMatch((k, v) -> {
                    V v1;
                    return (v1 = __lookup(k)) != null && v1.equals(v);
                });
            }
        }

        @Override
        default PMap<K,V> putAll(PMap<? extends K, ? extends V> that) {
            if (that instanceof Node) {
                return (PMap<K,V>)Bin.union((Node<K,V>)that, this);
            } else {
                return that.foldLeftKV((PMap<K,V>)this, PMap::put);
            }
        }

        @Override
        default PMap<K,V> filter(Predicate<? super V> p) {
            return (PMap<K,V>)__filter(b -> p.test(b.getValue()));
        }

        @Override
        default PMap<K,V> filterKV(BiPredicate<? super K, ? super V> p) {
            return (PMap<K,V>)__filter(b -> p.test(b.getKey(), b.getValue()));
        }

        @Override
        default <R> PMap<K,R> map(Function<? super V, ? extends R> f) {
            return (PMap<K,R>)__map(b -> f.apply(b.getValue()));
        }

        @Override
        default <R> PMap<K,R> mapKV(BiFunction<? super K, ? super V, ? extends R> f) {
            return (PMap<K,R>)__map(b -> f.apply(b.getKey(), b.getValue()));
        }

        @Override
        default <R,T> $<T, PMap<K,R>>
        traverse(Applicative<T> m, BiFunction<? super K, ? super V, ? extends $<T,R>> f) {
            return ($)__traverse(m, b -> f.apply(b.getKey(), b.getValue()));
        }

        @Override
        default <R> R foldLeft(R z, BiFunction<R, ? super V, R> f) {
            return Bin.foldLeft(this, z, (r, b) -> f.apply(r, b.getValue()));
        }

        @Override
        default <R> R foldLeftKV(R z, TriFunction<R, ? super K, ? super V, R> f) {
            return Bin.foldLeft(this, z, (r, b) -> f.apply(r, b.getKey(), b.getValue()));
        }

        @Override
        default <R> R foldRight(BiFunction<? super V, Supplier<R>, R> f, Supplier<R> sr) {
            return Bin.foldRight(this, (b, r) -> f.apply(b.getValue(), r), sr);
        }

        @Override
        default <R> R foldRightKV(TriFunction<? super K, ? super V, Supplier<R>, R> f, Supplier<R> sr) {
            return Bin.foldRight(this, (b, r) -> f.apply(b.getKey(), b.getValue(), r), sr);
        }

        @Override
        default <R> R foldRight_(R z, BiFunction<? super V, R, R> f) {
            return Bin.foldRight_(this, z, (b, r) -> f.apply(b.getValue(), r));
        }

        @Override
        default <R> R foldRightKV_(R z, TriFunction<? super K, ? super V, R, R> f) {
            return Bin.foldRight_(this, z, (b, r) -> f.apply(b.getKey(), b.getValue(), r));
        }

        @Override
        default void forEach(Consumer<? super Map.Entry<K,V>> action) {
            Bin.foldLeft(this, Unit.U, (z, e) -> { action.accept(e); return z; });
        }

        @Override
        default Seq<Map.Entry<K,V>> entries() {
            return Bin.foldRight(this, Seq::cons, Seq::nil);
        }

        @Override
        default Maybe<Map.Entry<K, V>> lowerEntry(K k) {
            return Bin.lookupLT(this, k);
        }

        @Override
        default Maybe<K> lowerKey(K k) {
            return Bin.lookupLT(this, k).map(Map.Entry::getKey);
        }

        @Override
        default Maybe<Map.Entry<K,V>> floorEntry(K k) {
            return Bin.lookupLE(this, k);
        }

        @Override
        default Maybe<K> floorKey(K k) {
            return Bin.lookupLE(this, k).map(Map.Entry::getKey);
        }

        @Override
        default Maybe<Map.Entry<K,V>> ceilingEntry(K k) {
            return Bin.lookupGE(this, k);
        }

        @Override
        default Maybe<K> ceilingKey(K k) {
            return Bin.lookupGE(this, k).map(Map.Entry::getKey);
        }

        @Override
        default Maybe<Map.Entry<K,V>> higherEntry(K k) {
            return Bin.lookupGT(this, k);
        }

        @Override
        default Maybe<K> higherKey(K k) {
            return Bin.lookupGT(this, k).map(Map.Entry::getKey);
        }

        @Override
        default Map.Entry<K,V> firstEntry() {
            return Bin.findFirst(this);
        }

        @Override
        default K firstKey() {
            return Bin.findFirst(this).getKey();
        }

        @Override
        default Map.Entry<K,V> lastEntry() {
            return Bin.findLast(this);
        }

        @Override
        default K lastKey() {
            return Bin.findLast(this).getKey();
        }

        @Override
        default String showTree() {
            return showTree((k, v) -> "(" + k + "," + v + ")");
        }

        @Override
        default String showTree(BiFunction<? super K, ? super V, String> showelem) {
            return Bin.showTree(this, showelem, Seq.nil());
        }
    }

    static class MapTip<K,V> extends Tip<K,V> implements MapNode<K,V> {
        MapTip(Comparator<? super K> cmp) {
            super(cmp);
        }

        @Override
        Node<K,V> cons(int sz, K k, V v, Node<K,V> l, Node<K,V> r) {
            return new MapBin<>(this, sz, k, v, l, r);
        }

        @Override
        public PMap<K,V> clear() {
            return this;
        }

        @Override
        public PSet<K> keySet() {
            return toKeySet();
        }

        @SuppressWarnings("unchecked")
        final SetTip<K> toKeySet() {
            return this == EMPTY_MAP ? EMPTY_SET : new SetTip<>(comparator);
        }

        @Override
        public Iterator<Map.Entry<K,V>> iterator() {
            return Collections.emptyIterator();
        }

        public boolean equals(Object obj) {
            return (obj instanceof PMap) && ((PMap)obj).isEmpty();
        }

        public int hashCode() {
            return 0;
        }

        public String toString() {
            return "[]";
        }
    }

    static class MapBin<K,V> extends Bin<K,V> implements MapNode<K,V> {
        MapBin(MapTip<K,V> tip, int size, K key, V value, Node<K,V> left, Node<K,V> right) {
            super(tip, size, key, value, left, right);
        }

        @Override
        @SuppressWarnings("unchecked")
        public PMap<K,V> clear() {
            return (PMap<K,V>)tip;
        }

        @Override
        public PSet<K> keySet() {
            return new KeySetView<>(this);
        }

        final SetTip<K> getSetTip() {
            return ((MapTip<K,V>)tip).toKeySet();
        }

        final PSet<K> toKeySet() {
            return toKeySet(this, getSetTip());
        }

        static <K,V> SetNode<K> toKeySet(Node<K,V> t, SetTip<K> st) {
            if (t.isEmpty()) {
                return st;
            } else {
                Bin<K,V> b = (Bin<K,V>)t;
                return st.cons(b.size, b.key, Unit.U, toKeySet(b.left, st), toKeySet(b.right, st));
            }
        }

        @Override
        public Iterator<Map.Entry<K,V>> iterator() {
            return new MapIterator<>(this);
        }

        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (!(obj instanceof PMap))
                return false;
            @SuppressWarnings("unchecked")
            PMap<K,V> other = (PMap<K,V>)obj;
            return this.size() == other.size() && containsAll(other);
        }

        public int hashCode() {
            return System.identityHashCode(this); // FIXME
        }

        public String toString() {
            return foldLeftKV(new StringJoiner(", ", "TreeMap[", "]"),
                              (sj, k, v) -> sj.add(k + "=>" + v))
                  .toString();
        }
    }

    @SuppressWarnings("override")
    static class KeySetView<K,V> implements PSet<K> {
        private final MapBin<K,V> m;
        KeySetView(MapBin<K,V> m) {
            this.m = m;
        }

        private PSet<K> copy() {
            return m.toKeySet();
        }

        public int size()                   { return m.size(); }
        public boolean isEmpty()            { return m.isEmpty(); }
        public boolean contains(Object e)   { return m.containsKey(e); }
        public PSet<K> add(K k)             { return copy().add(k); }
        public PSet<K> remove(Object e)     { return copy().remove(e); }
        public PSet<K> clear()              { return m.getSetTip(); }

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
            return Bin.foldRight(m, (e, r) -> f.apply(e.getKey(), r), sr);
        }
        public <R> R foldRight_(R z, BiFunction<? super K, R, R> f) {
            return Bin.foldRight_(m, z, (e, r) -> f.apply(e.getKey(), r));
        }
        public <R> R foldLeft(R z, BiFunction<R, ? super K, R> f) {
            return Bin.foldLeft(m, z, (r, e) -> f.apply(r, e.getKey()));
        }

        public Iterator<K> iterator() {
            return new TreeIterator<>(m);
        }
        public Spliterator<K> spliterator() {
            return new TreeSpliterator<>(m);
        }
        public void forEach(Consumer<? super K> action) {
            Bin.foldLeft(m, Unit.U, (z, e) -> { action.accept(e.getKey()); return z; });
        }
    }

    // ------------------------------------------------------------------------

    @SuppressWarnings({"rawtypes", "unchecked"})
    static final SetTip EMPTY_SET = new SetTip(Comparator.naturalOrder());

    // mixin interface to convert generic tree node to set node
    @SuppressWarnings("unchecked")
    interface SetNode<E> extends Node<E,Unit>, TreePSet<E> {
        @Override
        default boolean contains(Object e) {
            return __lookup((E)e) != null;
        }

        @Override
        default Maybe<E> find(Predicate<? super E> p) {
            return __find((k, v) -> p.test(k)).map(Map.Entry::getKey);
        }

        @Override
        default PSet<E> add(E e) {
            return (PSet<E>)__put(e, Unit.U);
        }

        @Override
        default PSet<E> remove(Object e) {
            return (PSet<E>)__remove((E)e);
        }

        @Override
        default boolean containsAll(PSet<? extends E> that) {
            if (size() < that.size()) {
                return false;
            } else if (that instanceof Node) {
                return Bin.isSubsetOf((Node<E,Unit>)that, this, (x,y) -> true);
            } else {
                return that.allMatch(this::contains);
            }
        }

        @Override
        default PSet<E> filter(Predicate<? super E> p) {
            return (PSet<E>)__filter(b -> p.test(b.getKey()));
        }

        @Override
        default <R> R foldLeft(R z, BiFunction<R, ? super E, R> f) {
            return Bin.foldLeft(this, z, (r, b) -> f.apply(r, b.getKey()));
        }

        @Override
        default <R> R foldRight(BiFunction<? super E, Supplier<R>, R> f, Supplier<R> sr) {
            return Bin.foldRight(this, (b, r) -> f.apply(b.getKey(), r), sr);
        }

        @Override
        default <R> R foldRight_(R z, BiFunction<? super E, R, R> f) {
            return Bin.foldRight_(this, z, (b, r) -> f.apply(b.getKey(), r));
        }

        @Override
        default PSet<E> union(PSet<E> s) {
            return (PSet<E>)Bin.union(this, (Node<E,Unit>)s);
        }

        @Override
        default PSet<E> difference(PSet<E> s) {
            return (PSet<E>)Bin.difference(this, (Node<E,Unit>)s);
        }

        @Override
        default PSet<E> intersection(PSet<E> s) {
            return (PSet<E>)Bin.intersection(this, (Node<E,Unit>)s);
        }

        @Override
        default Maybe<E> lower(E e) {
            return Bin.lookupLT(this, e).map(Map.Entry::getKey);
        }

        @Override
        default Maybe<E> floor(E e) {
            return Bin.lookupLE(this, e).map(Map.Entry::getKey);
        }

        @Override
        default Maybe<E> ceiling(E e) {
            return Bin.lookupGE(this, e).map(Map.Entry::getKey);
        }

        @Override
        default Maybe<E> higher(E e) {
            return Bin.lookupGT(this, e).map(Map.Entry::getKey);
        }

        @Override
        default E first() {
            return Bin.findFirst(this).getKey();
        }

        @Override
        default E last() {
            return Bin.findLast(this).getKey();
        }
    }

    static class SetTip<E> extends Tip<E,Unit> implements SetNode<E> {
        SetTip(Comparator<? super E> cmp) {
            super(cmp);
        }

        @Override
        SetNode<E> cons(int sz, E k, Unit v, Node<E,Unit> l, Node<E,Unit> r) {
            return new SetBin<>(this, sz, k, v, l, r);
        }

        @Override
        public PSet<E> clear() {
            return this;
        }

        @Override
        public Iterator<E> iterator() {
            return Collections.emptyIterator();
        }

        @Override
        public Spliterator<E> spliterator() {
            return Spliterators.emptySpliterator();
        }

        public boolean equals(Object obj) {
            return (obj instanceof PSet) && ((PSet)obj).isEmpty();
        }

        public int hashCode() {
            return 0;
        }

        public String toString() {
            return "{}";
        }
    }

    static class SetBin<E> extends Bin<E,Unit> implements SetNode<E> {
        SetBin(SetTip<E> tip, int size, E key, Unit value, Node<E,Unit> left, Node<E,Unit> right) {
            super(tip, size, key, value, left, right);
        }

        @Override
        @SuppressWarnings("unchecked")
        public PSet<E> clear() {
            return (PSet<E>)tip;
        }

        @Override
        public Iterator<E> iterator() {
            return new TreeIterator<>(this);
        }

        @Override
        public Spliterator<E> spliterator() {
            return new TreeSpliterator<>(this);
        }

        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (!(obj instanceof PSet))
                return false;
            @SuppressWarnings("unchecked")
            PSet<E> other = (PSet<E>)obj;
            return this.size() == other.size() && containsAll(other);
        }

        public int hashCode() {
            return System.identityHashCode(this); // FIXME
        }

        public String toString() {
            return show(", ", "{", "}");
        }
    }
}
