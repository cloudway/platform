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
import java.util.Optional;
import java.util.StringJoiner;
import java.util.function.BiFunction;
import java.util.function.BiPredicate;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

import com.cloudway.platform.common.fp.function.TriFunction;
import static com.cloudway.platform.common.fp.control.Comprehension.*;
import static com.cloudway.platform.common.fp.control.Conditionals.*;
import static com.cloudway.platform.common.fp.data.Tuple.Triple_;

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

        Node<K,V> __put(K k, V v);
        Node<K,V> __remove(K k);
        Node<K,V> __putIfAbsent(K k, V v);
        Node<K,V> __computeIfAbsent(K k, Function<? super K, ? extends V> f);
        Node<K,V> __computeIfPresent(K k, BiFunction<? super K, ? super V, Optional<? extends V>> f);
        Node<K,V> __compute(K k, BiFunction<? super K, Optional<V>, Optional<? extends V>> f);
        Node<K,V> __merge(K k, V v, BiFunction<? super V, ? super V, ? extends V> f);

        <R> Node<K,R> __map(Function<Map.Entry<K,V>, ? extends R> f);
        Node<K,V> __filter(Predicate<Map.Entry<K,V>> p);

        boolean valid();
    }
    
    /**
     * An empty tree and responsible to compare elements and construct
     * tree node.
     */
    static abstract class Tip<K,V> implements Node<K,V> {
        final Comparator<? super K> cmp;

        protected Tip(Comparator<? super K> cmp) {
            this.cmp = cmp;
        }

        int compare(K k1, K k2) {
            return cmp.compare(k1, k2);
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
        public Node<K,V> __computeIfPresent(K k, BiFunction<? super K, ? super V, Optional<? extends V>> f) {
            return this;
        }

        @Override
        public Node<K,V> __compute(K k, BiFunction<? super K, Optional<V>, Optional<? extends V>> f) {
            return f.apply(k, Optional.empty())
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
                int cmp = tip.compare(k, tb.key);
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
        public Node<K,V> __put(K k, V v) {
            int cmp = tip.compare(k, key);
            return cmp < 0 ? balanceL(left.__put(k, v), right) :
                   cmp > 0 ? balanceR(left, right.__put(k, v))
                           : modify(v);
        }

        @Override
        public Node<K,V> __remove(K k) {
            int cmp = tip.compare(k, key);
            return cmp < 0 ? balanceR(left.__remove(k), right) :
                   cmp > 0 ? balanceL(left, right.__remove(k))
                           : delete();
        }

        @Override
        public Node<K,V> __putIfAbsent(K k, V v) {
            int cmp = tip.compare(k, key);
            return cmp < 0 ? balanceL(left.__putIfAbsent(k, v), right) :
                   cmp > 0 ? balanceR(left, right.__putIfAbsent(k, v))
                           : this;
        }

        @Override
        public Node<K,V> __computeIfAbsent(K k, Function<? super K, ? extends V> f) {
            int cmp = tip.compare(k, key);
            return cmp < 0 ? balanceL(left.__computeIfAbsent(k, f), right) :
                   cmp > 0 ? balanceR(left, right.__computeIfAbsent(k, f))
                           : this;
        }

        @Override
        public Node<K,V> __computeIfPresent(K k, BiFunction<? super K, ? super V, Optional<? extends V>> f) {
            int cmp = tip.compare(k, key);
            return cmp < 0 ? balanceL(left.__computeIfPresent(k, f), right) :
                   cmp > 0 ? balanceR(left, right.__computeIfPresent(k, f))
                           : f.apply(key, value).map(this::modify).orElseGet(this::delete);
        }

        @Override
        public Node<K,V> __compute(K k, BiFunction<? super K, Optional<V>, Optional<? extends V>> f) {
            int cmp = tip.compare(k, key);
            return cmp < 0 ? balance(left.__compute(k, f), right) :
                   cmp > 0 ? balance(left, right.__compute(k, f))
                           : f.apply(key, Optional.of(value)).map(this::modify).orElseGet(this::delete);
        }

        @Override
        public Node<K,V> __merge(K k, V v, BiFunction<? super V, ? super V, ? extends V> f) {
            int cmp = tip.compare(k, key);
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

        static <K,V> Optional<Map.Entry<K,V>> lookupLT(Node<K,V> t, K k) {
            Bin<K,V> lt = null;
            while (!t.isEmpty()) {
                Bin<K,V> b = (Bin<K,V>)t;
                if (b.tip.compare(k, b.key) <= 0) {
                    t = b.left;
                } else {
                    lt = b;
                    t = b.right;
                }
            }
            return Optional.ofNullable(lt);
        }

        static <K,V> Optional<Map.Entry<K,V>> lookupGT(Node<K,V> t, K k) {
            Bin<K,V> gt = null;
            while (!t.isEmpty()) {
                Bin<K,V> b = (Bin<K,V>)t;
                if (b.tip.compare(k, b.key) >= 0) {
                    t = b.right;
                } else {
                    gt = b;
                    t = b.left;
                }
            }
            return Optional.ofNullable(gt);
        }

        static <K,V> Optional<Map.Entry<K,V>> lookupLE(Node<K,V> t, K k) {
            Bin<K,V> le = null;
            while (!t.isEmpty()) {
                Bin<K,V> b = (Bin<K,V>)t;
                int cmp = b.tip.compare(k, b.key);
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
            return Optional.ofNullable(le);
        }

        static <K,V> Optional<Map.Entry<K,V>> lookupGE(Node<K,V> t, K k) {
            Bin<K,V> ge = null;
            while (!t.isEmpty()) {
                Bin<K,V> b = (Bin<K,V>)t;
                int cmp = b.tip.compare(k, b.key);
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
            return Optional.ofNullable(ge);
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
            while (!t.isEmpty()) {
                Bin<K,V> b = (Bin<K,V>)t;
                z = f.apply(foldLeft(b.left, z, f), b);
                t = b.right;
            }
            return z;
        }

        static <K,V,R> R foldRight(Node<K,V> t, R z, BiFunction<Map.Entry<K,V>, Supplier<R>, R> f) {
            while (!t.isEmpty()) {
                Bin<K,V> b = (Bin<K,V>)t; R r = z;
                z = f.apply(b, () -> foldRight(b.right, r, f));
                t = b.left;
            }
            return z;
        }

        static <K,V,R> R foldRight_(Node<K,V> t, R z, BiFunction<Map.Entry<K,V>, R, R> f) {
            while (!t.isEmpty()) {
                Bin<K,V> b = (Bin<K,V>)t; R r = z;
                z = f.apply(b, foldRight_(b.right, r, f));
                t = b.left;
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
                    if (tip.compare(tb.key, lk) > 0)
                        break;
                    t = tb.right;
                }
            } else if (lk == null) {
                while (!t.isEmpty()) {
                    Bin<K,V> tb = (Bin<K,V>)t;
                    if (tip.compare(tb.key, hk) < 0)
                        break;
                    t = tb.left;
                }
            } else {
                while (!t.isEmpty()) {
                    Bin<K,V> tb = (Bin<K,V>)t;
                    if (tip.compare(tb.key, lk) <= 0) {
                        t = tb.right;
                    } else if (tip.compare(tb.key, hk) >= 0) {
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
                int cmp = tb.tip.compare(k, tb.key);
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
                int cmp = tb.tip.compare(tb.key, k);
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
                int cmp = b.tip.compare(k, b.key);
                return cmp < 0 ? inCaseOf(splitMember(b.left, k), Triple_((z, lt, gt) ->
                                    Tuple.of(z, lt, b.link(gt, b.right)))) :
                       cmp > 0 ? inCaseOf(splitMember(b.right, k), Triple_((z, lt, gt) ->
                                    Tuple.of(z, b.link(b.left, lt), gt)))
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
                return inCaseOf(splitMember(t2, t1b.key), Triple_((z, lt, gt) ->
                    z != null && p.test(t1b.value, z) &&
                    isSubsetOf(t1b.left, lt, p) &&
                    isSubsetOf(t1b.right, gt, p)));
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
                    && bounded(b.left, lo, k -> b.tip.compare(k, b.key) < 0)
                    && bounded(b.right, k -> b.tip.compare(k, b.key) > 0, hi);
            }
        }

        private static <K, V> boolean validsize(Node<K,V> t) {
            return realsize(t).filter(sz -> sz == t.size()).isPresent();
        }

        private static <K, V> Optional<Integer> realsize(Node<K,V> t) {
            if (t.isEmpty()) {
                return Optional.of(0);
            } else {
                Bin<K,V> b = (Bin<K,V>)t;
                return select.from(realsize(b.left), n ->
                              from(realsize(b.right), m ->
                              where(n + m + 1 == b.size,
                              yield(b.size))));
            }
        }
    }

    // ------------------------------------------------------------------------

    @SuppressWarnings({"rawtypes", "unchecked"})
    static final MapTip EMPTY_MAP = new MapTip(Comparator.naturalOrder());

    // mixin interface to convert generic tree node to map node
    @SuppressWarnings("unchecked")
    interface MapNode<K,V> extends Node<K,V>, TreeMap<K,V> {
        @Override
        default boolean containsKey(K k) {
            return __lookup(k) != null;
        }

        @Override
        default Optional<V> lookup(K k) {
            return Optional.ofNullable(__lookup(k));
        }

        @Override
        default V get(K k) {
            V v = __lookup(k);
            if (v == null)
                throw new NoSuchElementException();
            return v;
        }

        @Override
        default V getOrDefault(K k, V d) {
            V v = __lookup(k);
            return v != null ? v : d;
        }

        @Override
        default TreeMap<K,V> put(K k, V v) {
            Objects.requireNonNull(v);
            return (TreeMap<K,V>)__put(k, v);
        }

        @Override
        default TreeMap<K,V> remove(K k) {
            return (TreeMap<K,V>)__remove(k);
        }

        @Override
        default TreeMap<K,V> putIfAbsent(K k, V v) {
            Objects.requireNonNull(v);
            return (TreeMap<K,V>)__putIfAbsent(k, v);
        }

        @Override
        default TreeMap<K,V> computeIfAbsent(K k, Function<? super K, ? extends V> f) {
            return (TreeMap<K,V>)__computeIfAbsent(k, f);
        }

        @Override
        default TreeMap<K,V> computeIfPresent(K k, BiFunction<? super K, ? super V, Optional<? extends V>> f) {
            return (TreeMap<K,V>)__computeIfPresent(k, f);
        }

        @Override
        default TreeMap<K,V> compute(K k, BiFunction<? super K, Optional<V>, Optional<? extends V>> f) {
            return (TreeMap<K,V>)__compute(k, f);
        }

        @Override
        default TreeMap<K,V> merge(K k, V v, BiFunction<? super V, ? super V, ? extends V> f) {
            Objects.requireNonNull(v);
            return (TreeMap<K,V>)__merge(k, v, f);
        }

        @Override
        default boolean containsAll(TreeMap<K,V> m) {
            return this.size() >= m.size() && Bin.isSubsetOf((Node<K,V>)m, this, Objects::equals);
        }

        @Override
        default TreeMap<K,V> putAll(TreeMap<K,V> m) {
            return (TreeMap<K,V>)Bin.union((Node<K,V>)m, this);
        }

        @Override
        default <R> TreeMap<K,R> map(Function<? super V, ? extends R> f) {
            return (TreeMap<K,R>)__map(b -> f.apply(b.getValue()));
        }

        @Override
        default <R> TreeMap<K,R> mapKV(BiFunction<? super K, ? super V, ? extends R> f) {
            return (TreeMap<K,R>)__map(b -> f.apply(b.getKey(), b.getValue()));
        }

        @Override
        default TreeMap<K,V> filter(Predicate<? super V> p) {
            return (TreeMap<K,V>)__filter(b -> p.test(b.getValue()));
        }

        @Override
        default TreeMap<K,V> filterKV(BiPredicate<? super K, ? super V> p) {
            return (TreeMap<K,V>)__filter(b -> p.test(b.getKey(), b.getValue()));
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
        default <R> R foldRight(R z, BiFunction<? super V, Supplier<R>, R> f) {
            return Bin.foldRight(this, z, (b, r) -> f.apply(b.getValue(), r));
        }

        @Override
        default <R> R foldRightKV(R z, TriFunction<? super K, ? super V, Supplier<R>, R> f) {
            return Bin.foldRight(this, z, (b, r) -> f.apply(b.getKey(), b.getValue(), r));
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
        default Seq<Map.Entry<K,V>> entries() {
            return Bin.foldRight(this, Seq.nil(), Seq::cons);
        }

        @Override
        default Optional<Map.Entry<K, V>> lowerEntry(K k) {
            return Bin.lookupLT(this, k);
        }

        @Override
        default Optional<K> lowerKey(K k) {
            return Bin.lookupLT(this, k).map(Map.Entry::getKey);
        }

        @Override
        default Optional<Map.Entry<K,V>> floorEntry(K k) {
            return Bin.lookupLE(this, k);
        }

        @Override
        default Optional<K> floorKey(K k) {
            return Bin.lookupLE(this, k).map(Map.Entry::getKey);
        }

        @Override
        default Optional<Map.Entry<K,V>> ceilingEntry(K k) {
            return Bin.lookupGE(this, k);
        }

        @Override
        default Optional<K> ceilingKey(K k) {
            return Bin.lookupGE(this, k).map(Map.Entry::getKey);
        }

        @Override
        default Optional<Map.Entry<K,V>> higherEntry(K k) {
            return Bin.lookupGT(this, k);
        }

        @Override
        default Optional<K> higherKey(K k) {
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
        public TreeSet<K> keySet() {
            return toKeySet();
        }

        @SuppressWarnings("unchecked")
        final SetTip<K> toKeySet() {
            return this == EMPTY_MAP ? EMPTY_SET : new SetTip<>(cmp);
        }

        public boolean equals(Object obj) {
            return (obj instanceof TreeMap) && ((TreeMap)obj).isEmpty();
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
        public TreeSet<K> keySet() {
            return toKeySet(this, ((MapTip<K,V>)tip).toKeySet());
        }

        static <K,V> SetNode<K> toKeySet(Node<K,V> t, SetTip<K> st) {
            if (t.isEmpty()) {
                return st;
            } else {
                Bin<K,V> b = (Bin<K,V>)t;
                return st.cons(b.size, b.key, Unit.U, toKeySet(b.left, st), toKeySet(b.right, st));
            }
        }

        @SuppressWarnings({"unchecked", "rawtypes"})
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (!(obj instanceof TreeMap))
                return false;
            TreeMap other = (TreeMap)obj;
            return this.size() == other.size() && containsAll(other);
        }

        public int hashCode() {
            return System.identityHashCode(this); // FIXME
        }

        public String toString() {
            return foldLeftKV(new StringJoiner(", ", "[", "]"),
                        (sj, k, v) -> sj.add("(" + k + "," + v + ")")).toString();
        }
    }

    // ------------------------------------------------------------------------

    @SuppressWarnings({"rawtypes", "unchecked"})
    static final SetTip EMPTY_SET = new SetTip(Comparator.naturalOrder());

    // mixin interface to convert generic tree node to set node
    @SuppressWarnings("unchecked")
    interface SetNode<E> extends Node<E,Unit>, TreeSet<E> {
        @Override
        default boolean contains(E e) {
            return __lookup(e) != null;
        }

        @Override
        default TreeSet<E> add(E e) {
            return (TreeSet<E>)__put(e, Unit.U);
        }

        @Override
        default TreeSet<E> remove(E e) {
            return (TreeSet<E>)__remove(e);
        }

        @Override
        default boolean containsAll(TreeSet<E> s) {
            return this.size() >= s.size() && Bin.isSubsetOf((Node<E,Unit>)s, this, (x,y) -> true);
        }

        @Override
        default <R> TreeSet<R> map(Function<? super E, ? extends R> f) {
            return (TreeSet<R>)__map(b -> f.apply(b.getKey()));
        }

        @Override
        default TreeSet<E> filter(Predicate<? super E> p) {
            return (TreeSet<E>)__filter(b -> p.test(b.getKey()));
        }

        @Override
        default <R> R foldLeft(R z, BiFunction<R, ? super E, R> f) {
            return Bin.foldLeft(this, z, (r, b) -> f.apply(r, b.getKey()));
        }

        @Override
        default <R> R foldRight(R z, BiFunction<? super E, Supplier<R>, R> f) {
            return Bin.foldRight(this, z, (b, r) -> f.apply(b.getKey(), r));
        }

        @Override
        default <R> R foldRight_(R z, BiFunction<? super E, R, R> f) {
            return Bin.foldRight(this, z, (b, r) -> f.apply(b.getKey(), r.get()));
        }

        @Override
        default TreeSet<E> union(TreeSet<E> s) {
            return (TreeSet<E>)Bin.union(this, (Node<E,Unit>)s);
        }

        @Override
        default TreeSet<E> difference(TreeSet<E> s) {
            return (TreeSet<E>)Bin.difference(this, (Node<E,Unit>)s);
        }

        @Override
        default TreeSet<E> intersection(TreeSet<E> s) {
            return (TreeSet<E>)Bin.intersection(this, (Node<E,Unit>)s);
        }

        @Override
        default Optional<E> lower(E e) {
            return Bin.lookupLT(this, e).map(Map.Entry::getKey);
        }

        @Override
        default Optional<E> floor(E e) {
            return Bin.lookupLE(this, e).map(Map.Entry::getKey);
        }

        @Override
        default Optional<E> ceiling(E e) {
            return Bin.lookupGE(this, e).map(Map.Entry::getKey);
        }

        @Override
        default Optional<E> higher(E e) {
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

        public boolean equals(Object obj) {
            return (obj instanceof TreeSet) && ((TreeSet)obj).isEmpty();
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

        @SuppressWarnings({"unchecked", "rawtypes"})
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (!(obj instanceof TreeSet))
                return false;
            TreeSet other = (TreeSet)obj;
            return this.size() == other.size() && containsAll(other);
        }

        public int hashCode() {
            return System.identityHashCode(this); // FIXME
        }

        public String toString() {
            return foldLeft(new StringJoiner(",", "{", "}"),
                    (sj, e) -> sj.add(String.valueOf(e))).toString();
        }
    }
}
