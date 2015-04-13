/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util;

import java.util.Random;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.function.UnaryOperator;

import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;

import com.cloudway.platform.common.fp.data.FingerTree;
import com.cloudway.platform.common.fp.data.FingerTree.Maker;
import com.cloudway.platform.common.fp.data.Fn;
import com.cloudway.platform.common.fp.data.Foldable;
import com.cloudway.platform.common.fp.data.IntSeq;
import com.cloudway.platform.common.fp.data.Monoid;
import com.cloudway.platform.common.fp.data.Pair;
import com.cloudway.platform.common.fp.data.Seq;
import com.cloudway.platform.common.fp.data.Tuple;

public class FingerTreeTest {
    /**
     * An unoptimized sequence data structure used to test underlying finger tree.
     */
    static class Sequence<A> implements Foldable<A> {
        private static final Maker<Integer, ?> MAKER = FingerTree.maker(Monoid.intSum, Fn.pure(1));

        @SuppressWarnings("unchecked")
        private static <A> Maker<Integer, A> maker() {
            return (Maker<Integer, A>)MAKER;
        }

        private static Predicate<Integer> lookup(int ix) {
            return i -> i > ix;
        }

        private final FingerTree<Integer, A> tree;

        private Sequence(FingerTree<Integer, A> tree) {
            this.tree = tree;
        }

        @SuppressWarnings("unchecked")
        public static <A> Sequence<A> empty() {
            return new Sequence<>(Sequence.<A>maker().empty());
        }

        public boolean isEmpty() {
            return tree.isEmpty();
        }

        public int size() {
            return tree.measure();
        }

        public A head() {
            return tree.head();
        }

        public A last() {
            return tree.last();
        }

        public Sequence<A> tail() {
            return new Sequence<>(tree.tail());
        }

        public Sequence<A> init() {
            return new Sequence<>(tree.init());
        }

        public Sequence<A> cons(A a) {
            return new Sequence<>(tree.cons(a));
        }

        public Sequence<A> snoc(A a) {
            return new Sequence<>(tree.snoc(a));
        }

        public Sequence<A> append(Sequence<A> that) {
            return new Sequence<>(tree.append(that.tree));
        }

        public A at(int index) {
            return tree.lookup(lookup(index)).get();
        }

        public Sequence<A> update(int index, A value) {
            return modify(index, __ -> value);
        }

        public Sequence<A> modify(int index, UnaryOperator<A> f) {
            return new Sequence<>(tree.modify(lookup(index), f));
        }

        public Sequence<A> delete(int index) {
            return new Sequence<>(tree.split(lookup(index)).as((l, r) -> l.append(r.tail())));
        }

        public Sequence<A> insert(int index, A value) {
            return new Sequence<>(tree.split(lookup(index)).as((l, r) -> l.snoc(value).append(r)));
        }

        public Pair<Sequence<A>> splitAt(int index) {
            return tree.split(lookup(index)).map2(Sequence::new);
        }

        public Sequence<A> take(int n) {
            return new Sequence<>(tree.split(lookup(n)).first());
        }

        public Sequence<A> drop(int n) {
            return new Sequence<>(tree.split(lookup(n)).second());
        }

        public <B> Sequence<B> map(Function<? super A, ? extends B> f) {
            return new Sequence<>(tree.map(f, maker()));
        }

        public Sequence<A> filter(Predicate<? super A> p) {
            return new Sequence<>(foldLeft(Sequence.<A>maker().empty(), (xs, x) -> p.test(x) ? xs.snoc(x) : xs));
        }

        public Sequence<A> reverse() {
            return new Sequence<>(tree.reverse());
        }

        @Override
        public <R> R foldRight(BiFunction<? super A, Supplier<R>, R> f, Supplier<R> r) {
            return tree.foldRight(f, r);
        }

        @Override
        public <R> R foldRight_(R z, BiFunction<? super A, R, R> f) {
            return tree.foldRight_(z, f);
        }

        @Override
        public <R> R foldLeft(BiFunction<Supplier<R>, ? super A, R> f, Supplier<R> r) {
            return tree.foldLeft(f, r);
        }

        @Override
        public <R> R foldLeft(R z, BiFunction<R, ? super A, R> f) {
            return tree.foldLeft(z, f);
        }
    }

    static final int N = 100;
    private Sequence<Integer> seq;

    @Before
    public void init() {
        seq = Sequence.empty();
        for (int i = 0; i < N; i++) {
            seq = seq.snoc(i);
        }
    }

    @Test
    public void at() {
        for (int i = 0; i < N; i++) {
            assertEquals(i, (int)seq.at(i));
        }
    }

    @Test
    public void update() {
        Sequence<Integer> xs = seq;
        for (int i = 0; i < N; i++) {
            xs = xs.update(i, i * 10);
        }
        for (int i = 0; i < N; i++) {
            assertEquals("update " + i, i*10, (int)xs.at(i));
        }
    }

    @Test
    public void modify() {
        Sequence<Integer> xs = seq;
        for (int i = 0; i < N; i++) {
            xs = xs.modify(i, x -> x * 10);
        }
        for (int i = 0; i < N; i++) {
            assertEquals("modify " + i, i*10, (int)xs.at(i));
        }
    }

    @Test
    public void delete() {
        for (int i = 0; i < N; i++) {
            Sequence<Integer> xs = seq.delete(i);
            assertEquals("delete " + i, N - 1, xs.size());
            for (int j = 0; j < i; j++) {
                assertEquals("delete " + i, j, (int)xs.at(j));
            }
            for (int j = i+1; j < N; j++) {
                assertEquals("delete " + i, j, (int)xs.at(j-1));
            }
        }
    }

    @Test
    public void insert() {
        for (int i = 0; i <= N; i++) {
            Sequence<Integer> xs = seq.insert(i, 9999);
            assertEquals("insert " + i, N + 1, xs.size());
            for (int j = 0; j < i; j++) {
                assertEquals("insert " + i, j, (int)xs.at(j));
            }
            assertEquals(9999, (int)xs.at(i));
            for (int j = i; j < N; j++) {
                assertEquals("insert " + i, j, (int)xs.at(j + 1));
            }
        }
    }

    @Test
    public void splitAt() {
        for (int i = 0; i <= N; i++) {
            Pair<Sequence<Integer>> sp = seq.splitAt(i);
            assertEquals("splitAt " + i, i, sp.first().size());
            assertEquals("splitAt " + i, N - i, sp.second().size());
            for (int j = 0; j < i; j++) {
                assertEquals("splitAt " + i, j, (int)sp.first().at(j));
            }
            for (int j = i; j < N; j++) {
                assertEquals("splitAt " + i, j, (int)sp.second().at(j - i));
            }
        }
    }

    @Test
    public void take() {
        for (int i = 0; i <= N; i++) {
            Sequence<Integer> xs = seq.take(i);
            assertEquals("take " + i, i, xs.size());
            for (int j = 0; j < i; j++) {
                assertEquals("take " + i, j, (int)xs.at(j));
            }
        }
    }

    @Test
    public void drop() {
        for (int i = 0; i <= N; i++) {
            Sequence<Integer> xs = seq.drop(i);
            assertEquals("drop " + i, N - i, xs.size());
            for (int j = i; j < N; j++) {
                assertEquals("drop " + i, j, (int)xs.at(j - i));
            }
        }
    }

    @Test
    public void reverse() {
        Sequence<Integer> xs = seq.reverse();
        for (int i = 0; i < N; i++) {
            assertEquals(N - i - 1, (int)xs.at(i));
        }
    }

    @Test
    public void map() {
        Sequence<String> xs = seq.map(i -> "a" + i);
        assertEquals(N, xs.size());
        for (int i = 0; i < N; i++) {
            assertEquals("a" + i, xs.at(i));
        }
    }

    @Test
    public void filter() {
        Sequence<Integer> xs = seq.filter(i -> i % 2 == 0);
        assertEquals(N / 2, xs.size());
        for (int i = 0; i < N / 2; i++) {
            assertEquals(i * 2, (int)xs.at(i));
        }
    }

    @Test
    public void foldLeft() {
        Seq<Integer> xs = seq.foldLeft(Fn.flip(Seq::cons), Seq::nil);
        for (int i = N; --i >= 0; ) {
            assertEquals(i, (int)xs.head());
            xs = xs.tail();
        }
        assertTrue(xs.isEmpty());
    }

    @Test
    public void foldLeft_() {
        int sum = seq.foldLeft(0, Integer::sum);
        assertEquals(N * (N - 1) / 2, sum);
    }

    @Test
    public void foldRight() {
        Seq<Integer> xs = seq.foldRight(Seq.nil(), Seq::cons);
        for (int i = 0; i < N; i++) {
            assertEquals(i, (int)xs.head());
            xs = xs.tail();
        }
        assertTrue(xs.isEmpty());
    }

    @Test
    public void foldRight_() {
        int sum = seq.foldRight_(0, Integer::sum);
        assertEquals(N * (N - 1) / 2, sum);
    }

    /**
     * Min-priority queues implemented using the FingerTree type, following section
     * 4.6 of
     *
     * <pre>
     *  * Ralf Hinze and Ross Paterson,
     *    "Finger trees: a simple general-purpose data structure",
     *    Journal of Functional Programming 16:2 (2006) pp 197-217.
     *    http://staff.city.ac.uk/~ross/paper/FingerTree.html
     * </pre>
     */
    static class PQueue<K extends Comparable<K>, V> {
        private static class Entry<K extends Comparable<K>, V> {
            final K key;
            final V value;

            Entry(K k, V v) {
                key = k;
                value = v;
            }
        }

        private static class Prio<K extends Comparable<K>, V> {
            final K key;
            final V value;

            Prio(K k, V v) {
                key = k;
                value = v;
            }

            private static final Prio<?,?> EMPTY = new Prio<>(null, null);

            @SuppressWarnings("unchecked")
            private static <K extends Comparable<K>, V> Prio<K,V> empty() {
                return (Prio<K,V>)EMPTY;
            }

            boolean isEmpty() {
                return this == EMPTY;
            }

            private static <K extends Comparable<K>, V> Monoid<Prio<K,V>> monoid() {
                return Monoid.monoid_(empty(), (a1, a2) ->
                    a1.isEmpty() ? a2 :
                    a2.isEmpty() ? a1 :
                    a1.key.compareTo(a2.key) <= 0 ? a1 : a2
                );
            }

            static <K extends Comparable<K>, V> Maker<Prio<K,V>, Entry<K,V>> maker() {
                return FingerTree.maker(monoid(), e -> new Prio<>(e.key, e.value));
            }
        }

        private static final Maker<?,?> MAKER = Prio.maker();

        @SuppressWarnings("unchecked")
        private static <K extends Comparable<K>, V> Maker<Prio<K,V>, Entry<K,V>> maker() {
            return (Maker<Prio<K,V>, Entry<K,V>>)MAKER;
        }

        private final FingerTree<Prio<K,V>, Entry<K,V>> tree;

        private PQueue(FingerTree<Prio<K,V>, Entry<K,V>> tree) {
            this.tree = tree;
        }

        /**
         * Construct an empty priority queue.
         */
        public static <K extends Comparable<K>, V> PQueue<K,V> empty() {
            return new PQueue<>(PQueue.<K,V>maker().empty());
        }

        /**
         * Returns true if the priority queue is empty, false otherwise.
         */
        public boolean isEmpty() {
            return tree.isEmpty();
        }

        /**
         * Add a (priority, value) pair to the front of a priority queue.
         */
        public PQueue<K, V> insert(K key, V value) {
            return new PQueue<>(tree.cons(new Entry<>(key, value)));
        }

        /**
         * Add a (priority, value) pair to the back of a priority queue.
         */
        public PQueue<K, V> add(K key, V value) {
            return new PQueue<>(tree.snoc(new Entry<>(key, value)));
        }

        /**
         * Concatenate two priority queues.
         */
        public PQueue<K, V> union(PQueue<K, V> that) {
            return new PQueue<>(tree.append(that.tree));
        }

        /**
         * Returns a list of elements associated with the minimal priority
         * together with the rest of the priority queue.
         */
        public Seq<Tuple<K,V>> minView() {
            return minView(tree);
        }

        private static <K extends Comparable<K>, V> Seq<Tuple<K,V>> minView(FingerTree<Prio<K,V>, Entry<K,V>> t) {
            if (t.isEmpty()) {
                return Seq.nil();
            } else {
                Prio<K,V> p = t.measure();
                return t.split(below(p.key)).as((l, r) ->
                    Seq.cons(Tuple.of(p.key, p.value), () -> minView(l.append(r.tail()))));
            }
        }

        private static <K extends Comparable<K>, V> Predicate<Prio<K,V>> below(K k) {
            return p -> p.isEmpty() ? false : p.key.compareTo(k) <= 0;
        }
    }

    @Test
    public void priorityQueue() {
        Random rnd = new Random();
        PQueue<Integer, Integer> pq =
            IntSeq.generate(() -> rnd.nextInt(10))
                  .boxed().distinct().take(10)
                  .foldLeft(PQueue.empty(), (q, i) -> q.add(i, i));

        Seq<Integer> xs = pq.minView().map(Tuple::first);
        for (int i = 0; i < 10; i++) {
            assertEquals(i, (int)xs.head());
            xs = xs.tail();
        }
        assertTrue(xs.isEmpty());
    }

    static class Interval<V extends Comparable<V>> implements Comparable<Interval<V>> {
        final V low, high;

        private Interval(V low, V high) {
            this.low = low;
            this.high = high;
        }

        public static <V extends Comparable<V>> Interval<V> of(V low, V high) {
            return new Interval<>(low, high);
        }

        public V low() {
            return low;
        }

        public V high() {
            return high;
        }

        @Override
        public int compareTo(Interval<V> a) {
            int c;
            return (c = low.compareTo(a.low)) != 0 ? c : high.compareTo(a.high);
        }

        public boolean equals(Object obj) {
            if (obj == this)
                return true;
            if (!(obj instanceof Interval))
                return false;
            @SuppressWarnings("unchecked")
            Interval<V> a = (Interval<V>)obj;
            return low.equals(a.low) && high.equals(a.high);
        }

        public int hashCode() {
            return 31 * low.hashCode() + high.hashCode();
        }

        public String toString() {
            return "("+low+", "+high+")";
        }
    }

    static class IntervalMap<V extends Comparable<V>, A> {
        static class Node<V extends Comparable<V>, A> {
            final Interval<V> interval;
            final A value;

            Node(Interval<V> i, A a) {
                interval = i;
                value = a;
            }

            <R> R match(BiFunction<Interval<V>, ? super A, ? extends R> f) {
                return f.apply(interval, value);
            }
        }

        static class IntInterval<V extends Comparable<V>> {
            final Interval<V> interval;
            final V high;

            IntInterval(Interval<V> i, V h) {
                interval = i;
                high = h;
            }

            private static final IntInterval<?> EMPTY = new IntInterval<>(null, null);

            @SuppressWarnings("unchecked")
            public static <V extends Comparable<V>> IntInterval<V> empty() {
                return (IntInterval<V>)EMPTY;
            }

            public boolean isEmpty() {
                return this == EMPTY;
            }

            static <V extends Comparable<V>> Monoid<IntInterval<V>> monoid() {
                return Monoid.monoid_(empty(), (a1, a2) ->
                    a1.isEmpty() ? a2 :
                    a2.isEmpty() ? a1 :
                    new IntInterval<V>(a2.interval, max(a1.high, a2.high))
                );
            }

            static <V extends Comparable<V>, A> Maker<IntInterval<V>, Node<V,A>> maker() {
                return FingerTree.maker(monoid(), n -> new IntInterval<V>(n.interval, n.interval.high));
            }
        }

        static <T extends Comparable<T>> T max(T a, T b) {
            return a.compareTo(b) >= 0 ? a : b;
        }

        private static final Maker<?,?> MAKER = IntInterval.maker();

        @SuppressWarnings("unchecked")
        private static <V extends Comparable<V>, A> Maker<IntInterval<V>, Node<V,A>> maker() {
            return (Maker<IntInterval<V>, Node<V,A>>)MAKER;
        }

        private final FingerTree<IntInterval<V>, Node<V,A>> tree;

        private IntervalMap(FingerTree<IntInterval<V>, Node<V,A>> tree) {
            this.tree = tree;
        }

        public static <V extends Comparable<V>, A> IntervalMap<V,A> empty() {
            return new IntervalMap<>(IntervalMap.<V,A>maker().empty());
        }

        public IntervalMap<V,A> insert(Interval<V> i, A x) {
            if (i.low.compareTo(i.high) > 0) {
                return this;
            } else {
                Predicate<IntInterval<V>> larger = k -> k.interval.compareTo(i) >= 0;
                return tree.split(larger).as((l, r) ->
                    new IntervalMap<>(l.append(r.cons(new Node<>(i, x)))));
            }
        }

        public Seq<Tuple<Interval<V>, A>> search(V p) {
            return inRange(p, p);
        }

        private Seq<Tuple<Interval<V>, A>> inRange(V lo, V hi) {
            return matches(tree.takeUntil(greater(hi)), lo);
        }

        private static <V extends Comparable<V>, A> Seq<Tuple<Interval<V>, A>>
        matches(FingerTree<IntInterval<V>, Node<V,A>> t, V lo) {
            FingerTree<IntInterval<V>, Node<V,A>> xs = t.dropUntil(atleast(lo));
            if (xs.isEmpty()) {
                return Seq.nil();
            } else {
                return Seq.cons(xs.head().match(Tuple::of), () -> matches(xs.tail(), lo));
            }
        }

        private static <V extends Comparable<V>> Predicate<IntInterval<V>> atleast(V k) {
            return i -> k.compareTo(i.high) <= 0;
        }

        private static <V extends Comparable<V>> Predicate<IntInterval<V>> greater(V k) {
            return i -> i.interval.low.compareTo(k) > 0;
        }
    }

    @Test
    public void intervalMap() {
        IntervalMap<Integer, String> mathematicians = IntervalMap.<Integer, String>empty()
            .insert(Interval.of(1642, 1727), "Newton")
            .insert(Interval.of(1646, 1716), "Leibniz")
            .insert(Interval.of(1707, 1783), "Euler")
            .insert(Interval.of(1736, 1813), "Lagrange")
            .insert(Interval.of(1777, 1855), "Gauss")
            .insert(Interval.of(1811, 1831), "Galois");

        whoLivesInYear(mathematicians, 1700, "Newton,Leibniz");
        whoLivesInYear(mathematicians, 1710, "Newton,Leibniz,Euler");
        whoLivesInYear(mathematicians, 1720, "Newton,Euler");
        whoLivesInYear(mathematicians, 1800, "Lagrange,Gauss");
        whoLivesInYear(mathematicians, 1820, "Gauss,Galois");
    }

    static void whoLivesInYear(IntervalMap<Integer, String> xs, int year, String expected) {
        assertEquals(expected, xs.search(year).map(Tuple::second).show(",", "", ""));
    }
}
