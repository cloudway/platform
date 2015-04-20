/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.data;

import java.util.NoSuchElementException;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.function.UnaryOperator;

import com.cloudway.platform.common.fp.function.TriFunction;

final class FingerTreeImpl {
    private FingerTreeImpl() {}

    /**
     * Things that can be measured.
     */
    interface Measured<V> {
        V measure();
    }

    static abstract class FTMaker<V,A> implements FingerTree.Maker<V,A> {
        private final FTree<V,A> empty = new Empty<>(this);

        @Override
        public final FTree<V,A> empty() {
            return empty;
        }

        @Override
        public final FTree<V,A> singleton(A value) {
            return new Single<>(this, value);
        }

        // Measurement helper methods

        final V measure(A a, A b) {
            return sum(measure(a), measure(b));
        }

        final V measure(A a, A b, A c) {
            return sum(measure(a), measure(b), measure(c));
        }

        final V measure(A a, A b, A c, A d) {
            return sum(measure(a), measure(b), measure(c), measure(d));
        }

        final V sum(Measured<V> a, Measured<V> b) {
            return sum(a.measure(), b.measure());
        }

        final V sum(Measured<V> a, Measured<V> b, Measured<V> c) {
            return sum(a.measure(), b.measure(), c.measure());
        }

        final V sum(V a, V b, V c) {
            return sum(sum(a, b), c);
        }

        final V sum(V a, V b, V c, V d) {
            return sum(sum(sum(a, b), c), d);
        }

        final V sum(V a, V b, V c, V d, V e) {
            return sum(sum(sum(sum(a, b), c), d), e);
        }

        final V sum(V a, V b, V c, V d, V e, V f) {
            return sum(sum(sum(sum(sum(a, b), c), d), e), f);
        }

        // Factory methods

        final FTree<V,A> deep(V v, Digit<V,A> pr, FTree<V, Node<V,A>> mi, Digit<V,A> sf) {
            return new Deep<>(this, v, pr, mi, sf);
        }

        FTree<V,A> deep(Digit<V,A> pr, FTree<V, Node<V,A>> mi, Digit<V,A> sf) {
            return new Deep<>(this, sum(pr, mi, sf), pr, mi, sf);
        }

        final FTree<V,A> pullL(FTree<V, Node<V,A>> mi, Digit<V,A> sf) {
            return mi.isEmpty()
                 ? sf.toTree()
                 : deep(sum(mi, sf), mi.head().toDigit(), mi.tail(), sf);
        }

        final FTree<V,A> pullR(Digit<V,A> pr, FTree<V, Node<V,A>> mi) {
            return mi.isEmpty()
                 ? pr.toTree()
                 : deep(sum(pr, mi), pr, mi.init(), mi.last().toDigit());
        }

        final FTree<V,A> deepL(Digit<V,A> pr, FTree<V, Node<V,A>> mi, Digit<V,A> sf) {
            return pr == null ? pullL(mi, sf) : deep(pr, mi, sf);
        }

        final FTree<V,A> deepR(Digit<V,A> pr, FTree<V, Node<V,A>> mi, Digit<V,A> sf) {
            return sf == null ? pullR(pr, mi) : deep(pr, mi, sf);
        }

        final Digit<V,A> one(A a) {
            return new One<>(this, a);
        }

        final Digit<V,A> two(A a, A b) {
            return new Two<>(this, a, b);
        }

        final Digit<V,A> three(A a, A b, A c) {
            return new Three<>(this, a, b, c);
        }

        final Digit<V,A> four(A a, A b, A c, A d) {
            return new Four<>(this, a, b, c, d);
        }

        final Node<V,A> node2(A a, A b) {
            return new Node2<>(this, a, b);
        }

        final Node<V,A> node3(A a, A b, A c) {
            return new Node3<>(this, a, b, c);
        }

        private FTMaker<V, Node<V,A>> nm;

        final FTMaker<V, Node<V,A>> nodeMaker() {
            FTMaker<V, Node<V,A>> nm;
            if ((nm = this.nm) == null)
                nm = this.nm = new NodeMaker<>(this);
            return nm;
        }
    }

    static class MonoidMaker<V,A> extends FTMaker<V,A> {
        private final Monoid<V> monoid;
        private final Function<? super A, ? extends V> measure;

        MonoidMaker(Monoid<V> monoid, Function<? super A, ? extends V> measure) {
            this.monoid = monoid;
            this.measure = measure;
        }

        @Override
        public V zero() {
            return monoid.empty();
        }

        @Override
        public V sum(V v1, V v2) {
            return monoid.append(v1, v2);
        }

        @Override
        public V measure(A a) {
            return measure.apply(a);
        }
    }

    static class DelegateMaker<V,A> extends FTMaker<V,A> {
        private final FingerTree.Maker<V,A> delegate;

        DelegateMaker(FingerTree.Maker<V,A> delegate) {
            this.delegate = delegate;
        }

        @Override
        public V zero() {
            return delegate.zero();
        }

        @Override
        public V sum(V v1, V v2) {
            return delegate.sum(v1, v2);
        }

        @Override
        public V measure(A a) {
            return delegate.measure(a);
        }
    }

    static class NodeMaker<V,A> extends FTMaker<V, Node<V,A>> {
        private final FTMaker<V,A> delegate;

        NodeMaker(FTMaker<V,A> delegate) {
            this.delegate = delegate;
        }

        @Override
        public V zero() {
            return delegate.zero();
        }

        @Override
        public V sum(V v1, V v2) {
            return delegate.sum(v1, v2);
        }

        @Override
        public V measure(Node<V,A> node) {
            return node.measure();
        }
    }

    /**
     * An intermediate measurement lookup result.
     */
    static final class Place<V, A> {
        final V measure;
        final A result;

        Place(V v, A a) {
            measure = v;
            result = a;
        }
    }

    /**
     * A tree splitting result.
     */
    static final class Split<T, A> {
        final T left;
        final A value;
        final T right;

        Split(T left, A value, T right) {
            this.left = left;
            this.value = value;
            this.right = right;
        }

        <R> R as(TriFunction<T, A, T, R> f) {
            return f.apply(left, value, right);
        }
    }

    @FunctionalInterface
    interface QuadFunction<A, B, C, D, R> {
        R apply(A a, B b, C c, D d);
    }

    /**
     * A digit is a vector of 1-4 elements. Serves as a pointer to the
     * prefix or suffix of a finger tree.
     */
    static abstract class Digit<V,A> implements Foldable<A>, Measured<V> {
        final FTMaker<V,A> m;

        Digit(FTMaker<V,A> m) {
            this.m = m;
        }

        abstract int card();
        abstract A first();
        abstract A last();

        abstract Place<V,A> lookup(Predicate<? super V> p, V v);
        abstract Digit<V,A> modify(Predicate<? super V> p, V v, BiFunction<V,A,A> f);
        abstract Split<Digit<V,A>,A> split(Predicate<? super V> p, V v);
        abstract <B> Digit<V,B> map(Function<? super A, ? extends B> f, FTMaker<V,B> m);
        abstract Digit<V,A> reverse(Function<A,A> f);
        abstract FTree<V,A> toTree();

        <R> R as(Function<A,R> f) { throw new UnsupportedOperationException(); }
        <R> R as(BiFunction<A,A,R> f) { throw new UnsupportedOperationException(); }
        <R> R as(TriFunction<A,A,A,R> f) { throw new UnsupportedOperationException(); }
        <R> R as(QuadFunction<A,A,A,A,R> f) { throw new UnsupportedOperationException(); }
    }

    static final class One<V,A> extends Digit<V,A> {
        final A a;

        One(FTMaker<V,A> m, A a) {
            super(m);
            this.a = a;
        }

        @Override int card() { return 1; }
        @Override A first()  { return a; }
        @Override A last()   { return a; }

        @Override public V measure() {
            return m.measure(a);
        }

        @Override Place<V,A> lookup(Predicate<? super V> p, V v) {
            return new Place<>(v, a);
        }

        @Override Digit<V,A> modify(Predicate<? super V> p, V v, BiFunction<V,A,A> f) {
            return m.one(f.apply(v, a));
        }

        @Override Split<Digit<V,A>,A> split(Predicate<? super V> p, V v) {
            return new Split<>(null, a, null);
        }

        @Override <B> Digit<V,B> map(Function<? super A, ? extends B> f, FTMaker<V,B> m) {
            return m.one(f.apply(a));
        }

        @Override Digit<V,A> reverse(Function<A,A> f) {
            return m.one(f.apply(a));
        }

        @Override FTree<V,A> toTree() {
            return m.singleton(a);
        }

        @Override
        public <R> R foldRight(BiFunction<? super A, Supplier<R>, R> f, Supplier<R> r) {
            return f.apply(a, r);
        }

        @Override
        public <R> R foldRight_(R z, BiFunction<? super A, R, R> f) {
            return f.apply(a, z);
        }

        @Override
        public <R> R foldLeft(BiFunction<Supplier<R>, ? super A, R> f, Supplier<R> r) {
            return f.apply(r, a);
        }

        @Override
        public <R> R foldLeft(R z, BiFunction<R, ? super A, R> f) {
            return f.apply(z, a);
        }

        @Override <R> R as(Function<A,R> f) { return f.apply(a); }
    }

    static final class Two<V,A> extends Digit<V,A> {
        final A a, b;

        Two(FTMaker<V,A> m, A a, A b) {
            super(m);
            this.a = a;
            this.b = b;
        }

        @Override int card() { return 2; }
        @Override A first()  { return a; }
        @Override A last()   { return b; }

        @Override public V measure() {
            return m.measure(a, b);
        }

        @Override Place<V,A> lookup(Predicate<? super V> p, V v) {
            V va = m.sum(v, m.measure(a));
            return p.test(va) ? new Place<>(v, a)
                              : new Place<>(va, b);
        }

        @Override Digit<V,A> modify(Predicate<? super V> p, V v, BiFunction<V,A,A> f) {
            V va = m.sum(v, m.measure(a));
            return p.test(va) ? m.two(f.apply(v, a), b)
                              : m.two(a, f.apply(va, b));
        }

        @Override Split<Digit<V,A>,A> split(Predicate<? super V> p, V v) {
            V va = m.sum(v, m.measure(a));
            return p.test(va) ? new Split<>(null, a, m.one(b))
                              : new Split<>(m.one(a), b, null);
        }

        @Override <B> Digit<V,B> map(Function<? super A, ? extends B> f, FTMaker<V,B> m) {
            return m.two(f.apply(a), f.apply(b));
        }

        @Override Digit<V,A> reverse(Function<A,A> f) {
            return m.two(f.apply(b), f.apply(a));
        }

        @Override FTree<V,A> toTree() {
            return m.deep(m.one(a), m.nodeMaker().empty(), m.one(b));
        }

        @Override
        public <R> R foldRight(BiFunction<? super A, Supplier<R>, R> f, Supplier<R> r) {
            return f.apply(a, () -> f.apply(b, r));
        }

        @Override
        public <R> R foldRight_(R z, BiFunction<? super A, R, R> f) {
            return f.apply(a, f.apply(b, z));
        }

        @Override
        public <R> R foldLeft(BiFunction<Supplier<R>, ? super A, R> f, Supplier<R> r) {
            return f.apply(() -> f.apply(r, a), b);
        }

        @Override
        public <R> R foldLeft(R z, BiFunction<R, ? super A, R> f) {
            return f.apply(f.apply(z, a), b);
        }

        @Override <R> R as(BiFunction<A,A,R> f) { return f.apply(a, b); }
    }

    static final class Three<V,A> extends Digit<V,A> {
        private final A a, b, c;

        Three(FTMaker<V,A> m, A a, A b, A c) {
            super(m);
            this.a = a;
            this.b = b;
            this.c = c;
        }

        @Override int card() { return 3; }
        @Override A first()  { return a; }
        @Override A last()   { return c; }

        @Override public V measure() {
            return m.measure(a, b, c);
        }

        @Override Place<V,A> lookup(Predicate<? super V> p, V v) {
            V va = m.sum(v, m.measure(a));
            V vab = m.sum(va, m.measure(b));
            return p.test(va)  ? new Place<>(v, a) :
                   p.test(vab) ? new Place<>(va, b)
                               : new Place<>(vab, c);
        }

        @Override Digit<V,A> modify(Predicate<? super V> p, V v, BiFunction<V,A,A> f) {
            V va = m.sum(v, m.measure(a));
            V vab = m.sum(va, m.measure(b));
            return p.test(va)  ? m.three(f.apply(v, a), b, c) :
                   p.test(vab) ? m.three(a, f.apply(va, b), c)
                               : m.three(a, b, f.apply(vab, c));
        }

        @Override Split<Digit<V,A>,A> split(Predicate<? super V> p, V v) {
            V va = m.sum(v, m.measure(a));
            V vab = m.sum(va, m.measure(b));
            return p.test(va)  ? new Split<>(null, a, m.two(b, c)) :
                   p.test(vab) ? new Split<>(m.one(a), b, m.one(c))
                               : new Split<>(m.two(a, b), c, null);
        }

        @Override <B> Digit<V,B> map(Function<? super A, ? extends B> f, FTMaker<V,B> m) {
            return m.three(f.apply(a), f.apply(b), f.apply(c));
        }

        @Override Digit<V,A> reverse(Function<A,A> f) {
            return m.three(f.apply(c), f.apply(b), f.apply(a));
        }

        @Override FTree<V,A> toTree() {
            return m.deep(m.two(a, b), m.nodeMaker().empty(), m.one(c));
        }

        @Override
        public <R> R foldRight(BiFunction<? super A, Supplier<R>, R> f, Supplier<R> r) {
            return f.apply(a, () -> f.apply(b, () -> f.apply(c, r)));
        }

        @Override
        public <R> R foldRight_(R z, BiFunction<? super A, R, R> f) {
            return f.apply(a, f.apply(b, f.apply(c, z)));
        }

        @Override
        public <R> R foldLeft(BiFunction<Supplier<R>, ? super A, R> f, Supplier<R> r) {
            return f.apply(() -> f.apply(() -> f.apply(r, a), b), c);
        }

        @Override
        public <R> R foldLeft(R z, BiFunction<R, ? super A, R> f) {
            return f.apply(f.apply(f.apply(z, a), b), c);
        }

        @Override <R> R as(TriFunction<A,A,A,R> f) { return f.apply(a, b, c); }
    }

    static final class Four<V,A> extends Digit<V,A> {
        private final A a, b, c, d;

        Four(FTMaker<V,A> m, A a, A b, A c, A d) {
            super(m);
            this.a = a;
            this.b = b;
            this.c = c;
            this.d = d;
        }

        @Override int card() { return 4; }
        @Override A first()  { return a; }
        @Override A last()   { return d; }

        @Override public V measure() {
            return m.measure(a, b, c, d);
        }

        @Override Place<V,A> lookup(Predicate<? super V> p, V v) {
            V va = m.sum(v, m.measure(a));
            V vab = m.sum(va, m.measure(b));
            V vabc = m.sum(vab, m.measure(c));
            return p.test(va)   ? new Place<>(v, a) :
                   p.test(vab)  ? new Place<>(va, b) :
                   p.test(vabc) ? new Place<>(vab, c)
                                : new Place<>(vabc, d);
        }

        @Override Digit<V,A> modify(Predicate<? super V> p, V v, BiFunction<V,A,A> f) {
            V va = m.sum(v, m.measure(a));
            V vab = m.sum(va, m.measure(b));
            V vabc = m.sum(vab, m.measure(c));
            return p.test(va)   ? m.four(f.apply(v, a), b, c, d) :
                   p.test(vab)  ? m.four(a, f.apply(va, b), c, d) :
                   p.test(vabc) ? m.four(a, b, f.apply(vab, c), d)
                                : m.four(a, b, c, f.apply(vabc, d));
        }

        @Override Split<Digit<V,A>,A> split(Predicate<? super V> p, V v) {
            V va = m.sum(v, m.measure(a));
            V vab = m.sum(va, m.measure(b));
            V vabc = m.sum(vab, m.measure(c));
            return p.test(va)   ? new Split<>(null, a, m.three(b, c, d)) :
                   p.test(vab)  ? new Split<>(m.one(a), b, m.two(c, d)) :
                   p.test(vabc) ? new Split<>(m.two(a, b), c, m.one(d))
                                : new Split<>(m.three(a, b, c), d, null);
        }

        @Override <B> Digit<V,B> map(Function<? super A, ? extends B> f, FTMaker<V,B> m) {
            return m.four(f.apply(a), f.apply(b), f.apply(c), f.apply(d));
        }

        @Override Digit<V,A> reverse(Function<A,A> f) {
            return m.four(f.apply(d), f.apply(c), f.apply(b), f.apply(a));
        }

        @Override FTree<V,A> toTree() {
            return m.deep(m.two(a, b), m.nodeMaker().empty(), m.two(c, d));
        }

        @Override
        public <R> R foldRight(BiFunction<? super A, Supplier<R>, R> f, Supplier<R> r) {
            return f.apply(a, () -> f.apply(b, () -> f.apply(c, () -> f.apply(d, r))));
        }

        @Override
        public <R> R foldRight_(R z, BiFunction<? super A, R, R> f) {
            return f.apply(a, f.apply(b, f.apply(c, f.apply(d, z))));
        }

        @Override
        public <R> R foldLeft(BiFunction<Supplier<R>, ? super A, R> f, Supplier<R> r) {
            return f.apply(() -> f.apply(() -> f.apply(() -> f.apply(r, a), b), c), d);
        }

        @Override
        public <R> R foldLeft(R z, BiFunction<R, ? super A, R> f) {
            return f.apply(f.apply(f.apply(f.apply(z, a), b), c), d);
        }

        @Override <R> R as(QuadFunction<A,A,A,A,R> f) { return f.apply(a, b, c, d); }
    }

    // ------------------------------------------------------------------------

    static abstract class Node<V,A> implements Foldable<A>, Measured<V> {
        final FTMaker<V,A> m;
        final V v;

        Node(FTMaker<V,A> m, V v) {
            this.m = m;
            this.v = v;
        }

        @Override public V measure() { return v; }

        abstract Place<V,A> lookup(Predicate<? super V> p, V v);
        abstract Node<V,A> modify(Predicate<? super V> p, V v, BiFunction<V,A,A> f);
        abstract Split<Digit<V,A>,A> split(Predicate<? super V> p, V v);
        abstract <B> Node<V,B> map(Function<? super A, ? extends B> f, FTMaker<V,B> m);
        abstract Node<V,A> reverse(Function<A,A> f);
        abstract Digit<V,A> toDigit();
    }

    static final class Node2<V,A> extends Node<V,A> {
        final A a, b;

        Node2(FTMaker<V,A> m, A a, A b) {
            super(m, m.measure(a, b));
            this.a = a;
            this.b = b;
        }

        @Override
        Place<V,A> lookup(Predicate<? super V> p, V v) {
            V va = m.sum(v, m.measure(a));
            return p.test(va) ? new Place<>(v, a)
                              : new Place<>(va, b);
        }

        @Override
        Node<V,A> modify(Predicate<? super V> p, V v, BiFunction<V,A,A> f) {
            V va = m.sum(v, m.measure(a));
            return p.test(va) ? m.node2(f.apply(v, a), b)
                              : m.node2(a, f.apply(va, b));
        }

        @Override
        Split<Digit<V,A>,A> split(Predicate<? super V> p, V v) {
            V va = m.sum(v, m.measure(a));
            return p.test(va) ? new Split<>(null, a, m.one(b))
                              : new Split<>(m.one(a), b, null);
        }

        @Override
        <B> Node<V,B> map(Function<? super A, ? extends B> f, FTMaker<V,B> m) {
            return m.node2(f.apply(a), f.apply(b));
        }

        @Override
        Node<V,A> reverse(Function<A,A> f) {
            return m.node2(f.apply(b), f.apply(a));
        }

        @Override
        Digit<V,A> toDigit() {
            return m.two(a, b);
        }

        @Override
        public <R> R foldRight(BiFunction<? super A, Supplier<R>, R> f, Supplier<R> r) {
            return f.apply(a, () -> f.apply(b, r));
        }

        @Override
        public <R> R foldRight_(R z, BiFunction<? super A, R, R> f) {
            return f.apply(a, f.apply(b, z));
        }

        @Override
        public <R> R foldLeft(BiFunction<Supplier<R>, ? super A, R> f, Supplier<R> r) {
            return f.apply(() -> f.apply(r, a), b);
        }

        @Override
        public <R> R foldLeft(R z, BiFunction<R, ? super A, R> f) {
            return f.apply(f.apply(z, a), b);
        }
    }

    static final class Node3<V,A> extends Node<V,A> {
        final A a, b, c;

        Node3(FTMaker<V,A> m, A a, A b, A c) {
            super(m, m.measure(a, b, c));
            this.a = a;
            this.b = b;
            this.c = c;
        }

        @Override
        Place<V,A> lookup(Predicate<? super V> p, V v) {
            V va = m.sum(v, m.measure(a));
            V vab = m.sum(va, m.measure(b));
            return p.test(va)  ? new Place<>(v, a) :
                   p.test(vab) ? new Place<>(va, b)
                               : new Place<>(vab, c);
        }

        @Override
        Node<V,A> modify(Predicate<? super V> p, V v, BiFunction<V,A,A> f) {
            V va = m.sum(v, m.measure(a));
            V vab = m.sum(va, m.measure(b));
            return p.test(va)  ? m.node3(f.apply(v, a), b, c) :
                   p.test(vab) ? m.node3(a, f.apply(va, b), c)
                               : m.node3(a, b, f.apply(vab, c));
        }

        @Override
        Split<Digit<V,A>,A> split(Predicate<? super V> p, V v) {
            V va = m.sum(v, m.measure(a));
            V vab = m.sum(va, m.measure(b));
            return p.test(va)  ? new Split<>(null, a, m.two(b, c)) :
                   p.test(vab) ? new Split<>(m.one(a), b, m.one(c))
                               : new Split<>(m.two(a, b), c, null);
        }

        @Override
        <B> Node<V,B> map(Function<? super A, ? extends B> f, FTMaker<V,B> m) {
            return m.node3(f.apply(a), f.apply(b), f.apply(c));
        }

        @Override
        Node<V,A> reverse(Function<A,A> f) {
            return m.node3(f.apply(c), f.apply(b), f.apply(a));
        }

        @Override
        Digit<V,A> toDigit() {
            return m.three(a, b, c);
        }

        @Override
        public <R> R foldRight(BiFunction<? super A, Supplier<R>, R> f, Supplier<R> r) {
            return f.apply(a, () -> f.apply(b, () -> f.apply(c, r)));
        }

        @Override
        public <R> R foldRight_(R z, BiFunction<? super A, R, R> f) {
            return f.apply(a, f.apply(b, f.apply(c, z)));
        }

        @Override
        public <R> R foldLeft(BiFunction<Supplier<R>, ? super A, R> f, Supplier<R> r) {
            return f.apply(() -> f.apply(() -> f.apply(r, a), b), c);
        }

        @Override
        public <R> R foldLeft(R z, BiFunction<R, ? super A, R> f) {
            return f.apply(f.apply(f.apply(z, a), b), c);
        }
    }

    // ------------------------------------------------------------------------

    static abstract class FTree<V,A> implements FingerTree<V,A>, Measured<V> {
        final FTMaker<V,A> m;

        FTree(FTMaker<V,A> m) {
            this.m = m;
        }

        @Override
        public boolean isEmpty() {
            return false;
        }

        public boolean isSingle() {
            return false;
        }

        @Override public abstract FTree<V,A> tail();
        @Override public abstract FTree<V,A> init();
        @Override public abstract FTree<V,A> cons(A a);
        @Override public abstract FTree<V,A> snoc(A a);

        @Override
        public Maybe<A> lookup(Predicate<? super V> p) {
            if (isEmpty() || !p.test(measure())) {
                return Maybe.empty();
            } else {
                return Maybe.of(lookupTree(p, m.zero()).result);
            }
        }

        @Override
        public FingerTree<V,A> modify(Predicate<? super V> p, UnaryOperator<A> f) {
            if (isEmpty() || !p.test(measure())) {
                return this;
            } else {
                return modifyTree(p, m.zero(), (v, x) -> f.apply(x));
            }
        }

        @Override
        public Pair<FingerTree<V,A>> split(Predicate<? super V> p) {
            if (isEmpty()) {
                return Tuple.pair(this, this);
            } else if (p.test(measure())) {
                return splitTree(p, m.zero()).as((l, x, r) -> Tuple.pair(l, r.cons(x)));
            } else {
                return Tuple.pair(this, m.empty());
            }
        }

        @Override
        public <B> FingerTree<V,B> map(Function<? super A, ? extends B> f, Maker<V,B> m) {
            return mapTree(f, (FTMaker<V,B>)m);
        }

        @Override
        public FTree<V,A> reverse() {
            return reverseTree(Fn.id());
        }

        abstract Place<V,A> lookupTree(Predicate<? super V> p, V v);
        abstract FTree<V,A> modifyTree(Predicate<? super V> p, V v, BiFunction<V,A,A> f);
        abstract Split<FTree<V,A>,A> splitTree(Predicate<? super V> p, V v);
        abstract <B> FTree<V,B> mapTree(Function<? super A, ? extends B> f, FTMaker<V,B> mb);
        abstract FTree<V,A> reverseTree(Function<A,A> f);

        public String toString() {
            return show(", ", "FingerTree[", "]");
        }
    }

    static class Empty<V,A> extends FTree<V,A> {
        Empty(FTMaker<V,A> m) {
            super(m);
        }

        @Override
        public boolean isEmpty() {
            return true;
        }

        @Override
        public V measure() {
            return m.zero();
        }

        @Override
        public A head() {
            throw new NoSuchElementException();
        }

        @Override
        public A last() {
            throw new NoSuchElementException();
        }

        @Override
        public FTree<V,A> tail() {
            throw new NoSuchElementException();
        }

        @Override
        public FTree<V,A> init() {
            throw new NoSuchElementException();
        }

        @Override
        public FTree<V,A> cons(A a) {
            return m.singleton(a);
        }

        @Override
        public FTree<V,A> snoc(A a) {
            return m.singleton(a);
        }

        @Override
        public FingerTree<V,A> append(FingerTree<V,A> that) {
            return that;
        }

        @Override
        Place<V,A> lookupTree(Predicate<? super V> p, V v) {
            throw new NoSuchElementException();
        }

        @Override
        FTree<V,A> modifyTree(Predicate<? super V> p, V v, BiFunction<V,A,A> f) {
            throw new NoSuchElementException();
        }

        @Override
        Split<FTree<V,A>,A> splitTree(Predicate<? super V> p, V v) {
            throw new NoSuchElementException();
        }

        @Override
        <B> FTree<V,B> mapTree(Function<? super A, ? extends B> f, FTMaker<V,B> m) {
            return m.empty();
        }

        @Override
        FTree<V,A> reverseTree(Function<A,A> f) {
            return this;
        }

        @Override
        public <R> R foldRight(BiFunction<? super A, Supplier<R>, R> f, Supplier<R> r) {
            return r.get();
        }

        @Override
        public <R> R foldRight_(R z, BiFunction<? super A, R, R> f) {
            return z;
        }

        @Override
        public <R> R foldLeft(BiFunction<Supplier<R>, ? super A, R> f, Supplier<R> r) {
            return r.get();
        }

        @Override
        public <R> R foldLeft(R z, BiFunction<R, ? super A, R> f) {
            return z;
        }
    }

    static class Single<V,A> extends FTree<V,A> {
        final A a;

        Single(FTMaker<V,A> m, A a) {
            super(m);
            this.a = a;
        }

        @Override
        public boolean isSingle() {
            return true;
        }

        @Override
        public V measure() {
            return m.measure(a);
        }

        @Override
        public A head() {
            return a;
        }

        @Override
        public A last() {
            return a;
        }

        @Override
        public FTree<V,A> tail() {
            return m.empty();
        }

        @Override
        public FTree<V,A> init() {
            return m.empty();
        }

        @Override
        public FTree<V,A> cons(A b) {
            return m.deep(m.one(b), m.nodeMaker().empty(), m.one(a));
        }

        @Override
        public FTree<V,A> snoc(A b) {
            return m.deep(m.one(a), m.nodeMaker().empty(), m.one(b));
        }

        @Override
        public FingerTree<V,A> append(FingerTree<V,A> that) {
            return that.cons(a);
        }

        @Override
        Place<V,A> lookupTree(Predicate<? super V> p, V v) {
            return new Place<>(v, a);
        }

        @Override
        FTree<V,A> modifyTree(Predicate<? super V> p, V v, BiFunction<V,A,A> f) {
            return m.singleton(f.apply(v, a));
        }

        @Override
        Split<FTree<V,A>,A> splitTree(Predicate<? super V> p, V v) {
            return new Split<>(m.empty(), a, m.empty());
        }

        @Override
        <B> FTree<V,B> mapTree(Function<? super A, ? extends B> f, FTMaker<V,B> m) {
            return m.singleton(f.apply(a));
        }

        @Override
        FTree<V,A> reverseTree(Function<A,A> f) {
            return m.singleton(f.apply(a));
        }

        @Override
        public <R> R foldRight(BiFunction<? super A, Supplier<R>, R> f, Supplier<R> r) {
            return f.apply(a, r);
        }

        @Override
        public <R> R foldRight_(R z, BiFunction<? super A, R, R> f) {
            return f.apply(a, z);
        }

        @Override
        public <R> R foldLeft(BiFunction<Supplier<R>, ? super A, R> f, Supplier<R> r) {
            return f.apply(r, a);
        }

        @Override
        public <R> R foldLeft(R z, BiFunction<R, ? super A, R> f) {
            return f.apply(z, a);
        }
    }

    static class Deep<V,A> extends FTree<V,A> {
        final V v;
        final Digit<V,A> pr;
        final FTree<V, Node<V,A>> mi;
        final Digit<V,A> sf;

        Deep(FTMaker<V,A> m, V v, Digit<V,A> pr, FTree<V, Node<V,A>> mi, Digit<V,A> sf) {
            super(m);
            this.v = v;
            this.pr = pr;
            this.mi = mi;
            this.sf = sf;
        }

        @Override
        public V measure() {
            return v;
        }

        @Override
        public A head() {
            return pr.first();
        }

        @Override
        public A last() {
            return sf.last();
        }

        @Override
        public FTree<V,A> tail() {
            switch (pr.card()) {
              case 1: return pr.as((a) -> m.pullL(mi, sf));
              case 2: return pr.as((a, b) -> m.deep(m.one(b), mi, sf));
              case 3: return pr.as((a, b, c) -> m.deep(m.two(b, c), mi, sf));
              case 4: return pr.as((a, b, c, d) -> m.deep(m.three(b, c, d), mi, sf));
              default: throw new Error();
            }
        }

        @Override
        public FTree<V,A> init() {
            switch (sf.card()) {
              case 1: return sf.as((a) -> m.pullR(pr, mi));
              case 2: return sf.as((a, b) -> m.deep(pr, mi, m.one(a)));
              case 3: return sf.as((a, b, c) -> m.deep(pr, mi, m.two(a, b)));
              case 4: return sf.as((a, b, c, d) -> m.deep(pr, mi, m.three(a, b, c)));
              default: throw new Error();
            }
        }

        @Override
        public FTree<V,A> cons(A a) {
            V v = m.sum(m.measure(a), this.v);
            switch (pr.card()) {
              case 1: return pr.as((b) -> m.deep(v, m.two(a, b), mi, sf));
              case 2: return pr.as((b, c) -> m.deep(v, m.three(a, b, c), mi, sf));
              case 3: return pr.as((b, c, d) -> m.deep(v, m.four(a, b, c, d), mi, sf));
              case 4: return pr.as((b, c, d, e) -> m.deep(v, m.two(a, b), mi.cons(m.node3(c, d, e)), sf));
              default: throw new Error();
            }
        }

        @Override
        public FTree<V,A> snoc(A z) {
            V v = m.sum(this.v, m.measure(z));
            switch (sf.card()) {
              case 1: return sf.as((a) -> m.deep(v, pr, mi, m.two(a, z)));
              case 2: return sf.as((a, b) -> m.deep(v, pr, mi, m.three(a, b, z)));
              case 3: return sf.as((a, b, c) -> m.deep(v, pr, mi, m.four(a, b, c, z)));
              case 4: return sf.as((a, b, c, d) -> m.deep(v, pr, mi.snoc(m.node3(a, b, c)), m.two(d, z)));
              default: throw new Error();
            }
        }

        @Override
        Place<V,A> lookupTree(Predicate<? super V> p, V v) {
            V vpr = m.sum(v, pr.measure());
            V vm = m.sum(vpr, mi.measure());
            if (p.test(vpr)) {
                return pr.lookup(p, v);
            } else if (p.test(vm)) {
                Place<V,Node<V,A>> np = mi.lookupTree(p, vpr);
                return np.result.lookup(p, np.measure);
            } else {
                return sf.lookup(p, vm);
            }
        }

        @Override
        FTree<V,A> modifyTree(Predicate<? super V> p, V v, BiFunction<V,A,A> f) {
            V vpr = m.sum(v, pr.measure());
            V vm = m.sum(vpr, mi.measure());
            if (p.test(vpr)) {
                return m.deep(pr.modify(p, v, f), mi, sf);
            } else if (p.test(vm)) {
                return m.deep(pr, mi.modifyTree(p, vpr, (w, n) -> n.modify(p, w, f)), sf);
            } else {
                return m.deep(pr, mi, sf.modify(p, vm, f));
            }
        }

        @Override
        Split<FTree<V,A>,A> splitTree(Predicate<? super V> p, V v) {
            V vpr = m.sum(v, pr.measure());
            V vm = m.sum(vpr, mi.measure());
            if (p.test(vpr)) {
                return pr.split(p, v).as((l, x, r) ->
                    new Split<>(l != null ? l.toTree() : m.empty(), x, m.deepL(r, mi, sf)));
            } else if (p.test(vm)) {
                return mi.splitTree(p, vpr).as((ml, mx, mr) ->
                    mx.split(p, m.sum(vpr, ml.measure())).as((l, x, r) ->
                      new Split<>(m.deepR(pr, ml, l), x, m.deepL(r, mr, sf))));
            } else {
                return sf.split(p, vm).as((l, x, r) ->
                    new Split<>(m.deepR(pr, mi, l), x, r != null ? r.toTree() : m.empty()));
            }
        }

        @Override
        <B> FTree<V,B> mapTree(Function<? super A, ? extends B> f, FTMaker<V,B> m) {
            return m.deep(pr.map(f, m), mi.mapTree(n -> n.map(f, m), m.nodeMaker()), sf.map(f, m));
        }

        @Override
        FTree<V,A> reverseTree(Function<A, A> f) {
            return m.deep(sf.reverse(f), mi.reverseTree(n -> n.reverse(f)), pr.reverse(f));
        }

        @Override
        public FingerTree<V,A> append(FingerTree<V,A> that) {
            if (that.isEmpty()) {
                return this;
            } else if (that instanceof Single) {
                return snoc(that.head());
            } else {
                Deep<V,A> deep = (Deep<V,A>)that;
                V v = m.sum(this.v, deep.v);
                return m.deep(v, pr, addDigits0(m, mi, sf, deep.pr, deep.mi), deep.sf);
            }
        }

        static <V, A> FTree<V, Node<V,A>>
        addDigits0(FTMaker<V,A>        m,
                   FTree<V, Node<V,A>> m1,
                   Digit<V,A>          s1,
                   Digit<V,A>          p2,
                   FTree<V, Node<V,A>> m2) {
          switch ((s1.card() << 4) | p2.card()) {
            case 0x11: return s1.as(a -> p2.as(b ->
                       append1(m, m1, m.node2(a, b), m2)));
            case 0x12: return s1.as(a -> p2.as((b, c) ->
                       append1(m, m1, m.node3(a, b, c), m2)));
            case 0x13: return s1.as(a -> p2.as((b, c, d) ->
                       append2(m, m1, m.node2(a, b), m.node2(c, d), m2)));
            case 0x14: return s1.as(a -> p2.as((b, c, d, e) ->
                       append2(m, m1, m.node3(a, b, c), m.node2(d, e), m2)));

            case 0x21: return s1.as((a, b) -> p2.as(c ->
                       append1(m, m1, m.node3(a, b, c), m2)));
            case 0x22: return s1.as((a, b) -> p2.as((c, d) ->
                       append2(m, m1, m.node2(a, b), m.node2(c, d), m2)));
            case 0x23: return s1.as((a, b) -> p2.as((c, d, e) ->
                       append2(m, m1, m.node3(a, b, c), m.node2(d, e), m2)));
            case 0x24: return s1.as((a, b) -> p2.as((c, d, e, f) ->
                       append2(m, m1, m.node3(a, b, c), m.node3(d, e, f), m2)));

            case 0x31: return s1.as((a, b, c) -> p2.as(d ->
                       append2(m, m1, m.node2(a, b), m.node2(c, d), m2)));
            case 0x32: return s1.as((a, b, c) -> p2.as((d, e) ->
                       append2(m, m1, m.node3(a, b, c), m.node2(d, e), m2)));
            case 0x33: return s1.as((a, b, c) -> p2.as((d, e, f) ->
                       append2(m, m1, m.node3(a, b, c), m.node3(d, e, f), m2)));
            case 0x34: return s1.as((a, b, c) -> p2.as((d, e, f, g) ->
                       append3(m, m1, m.node3(a, b, c), m.node2(d, e), m.node2(f, g), m2)));

            case 0x41: return s1.as((a, b, c, d) -> p2.as(e ->
                       append2(m, m1, m.node3(a, b, c), m.node2(d, e), m2)));
            case 0x42: return s1.as((a, b, c, d) -> p2.as((e, f) ->
                       append2(m, m1, m.node3(a, b, c), m.node3(d, e, f), m2)));
            case 0x43: return s1.as((a, b, c, d) -> p2.as((e, f, g) ->
                       append3(m, m1, m.node3(a, b, c), m.node2(d, e), m.node2(f, g), m2)));
            case 0x44: return s1.as((a, b, c, d) -> p2.as((e, f, g, h) ->
                       append3(m, m1, m.node3(a, b, c), m.node3(d, e, f), m.node2(g, h), m2)));

            default: throw new Error();
          }
        }

        static <V,A> FTree<V, Node<V,A>>
        append1(FTMaker<V,A>        m,
                FTree<V, Node<V,A>> xs,
                Node<V,A>           a,
                FTree<V, Node<V,A>> ys) {
            if (xs.isEmpty()) {
                return ys.cons(a);
            } else if (ys.isEmpty()) {
                return xs.snoc(a);
            } else if (xs.isSingle()) {
                return ys.cons(a).cons(xs.head());
            } else if (ys.isSingle()) {
                return xs.snoc(a).snoc(ys.head());
            } else {
                Deep<V, Node<V,A>> d1 = (Deep<V,Node<V,A>>)xs;
                Deep<V, Node<V,A>> d2 = (Deep<V,Node<V,A>>)ys;
                FTMaker<V, Node<V,A>> nm = m.nodeMaker();
                V v = m.sum(d1.v, a.v, d2.v);
                return nm.deep(v, d1.pr, addDigits1(nm, d1.mi, d1.sf, a, d2.pr, d2.mi), d2.sf);
            }
        }

        static <V, A> FTree<V, Node<V, Node<V, A>>>
        addDigits1(FTMaker<V, Node<V,A>>        m,
                   FTree<V, Node<V, Node<V,A>>> m1,
                   Digit<V, Node<V,A>>          s1,
                   Node<V,A>                    x,
                   Digit<V, Node<V,A>>          p2,
                   FTree<V, Node<V, Node<V,A>>> m2) {
          switch ((s1.card() << 4) | p2.card()) {
            case 0x11: return s1.as(a -> p2.as(b ->
                       append1(m, m1, m.node3(a, x, b), m2)));
            case 0x12: return s1.as(a -> p2.as((b, c) ->
                       append2(m, m1, m.node2(a, x), m.node2(b, c), m2)));
            case 0x13: return s1.as(a -> p2.as((b, c, d) ->
                       append2(m, m1, m.node3(a, x, b), m.node2(c, d), m2)));
            case 0x14: return s1.as(a -> p2.as((b, c, d, e) ->
                       append2(m, m1, m.node3(a, x, b), m.node3(c, d, e), m2)));

            case 0x21: return s1.as((a, b) -> p2.as(c ->
                       append2(m, m1, m.node2(a, b), m.node2(x, c), m2)));
            case 0x22: return s1.as((a, b) -> p2.as((c, d) ->
                       append2(m, m1, m.node3(a, b, x), m.node2(c, d), m2)));
            case 0x23: return s1.as((a, b) -> p2.as((c, d, e) ->
                       append2(m, m1, m.node3(a, b, x), m.node3(c, d, e), m2)));
            case 0x24: return s1.as((a, b) -> p2.as((c, d, e, f) ->
                       append3(m, m1, m.node3(a, b, x), m.node2(c, d), m.node2(e, f), m2)));

            case 0x31: return s1.as((a, b, c) -> p2.as(d ->
                       append2(m, m1, m.node3(a, b, c), m.node2(x, d), m2)));
            case 0x32: return s1.as((a, b, c) -> p2.as((d, e) ->
                       append2(m, m1, m.node3(a, b, c), m.node3(x, d, e), m2)));
            case 0x33: return s1.as((a, b, c) -> p2.as((d, e, f) ->
                       append3(m, m1, m.node3(a, b, c), m.node2(x, d), m.node2(e, f), m2)));
            case 0x34: return s1.as((a, b, c) -> p2.as((d, e, f, g) ->
                       append3(m, m1, m.node3(a, b, c), m.node3(x, d, e), m.node2(f, g), m2)));

            case 0x41: return s1.as((a, b, c, d) -> p2.as(e ->
                       append2(m, m1, m.node3(a, b, c), m.node3(d, x, e), m2)));
            case 0x42: return s1.as((a, b, c, d) -> p2.as((e, f) ->
                       append3(m, m1, m.node3(a, b, c), m.node2(d, x),m.node2(e, f), m2)));
            case 0x43: return s1.as((a, b, c, d) -> p2.as((e, f, g) ->
                       append3(m, m1, m.node3(a, b, c), m.node3(d, x, e), m.node2(f, g), m2)));
            case 0x44: return s1.as((a, b, c, d) -> p2.as((e, f, g, h) ->
                       append3(m, m1, m.node3(a, b, c), m.node3(d, x, e), m.node3(f, g, h), m2)));

            default: throw new Error();
          }
        }

        static <V,A> FTree<V, Node<V,A>>
        append2(FTMaker<V,A>        m,
                FTree<V, Node<V,A>> xs,
                Node<V,A>           a,
                Node<V,A>           b,
                FTree<V, Node<V,A>> ys) {
            if (xs.isEmpty()) {
                return ys.cons(b).cons(a);
            } else if (ys.isEmpty()) {
                return xs.snoc(a).snoc(b);
            } else if (xs.isSingle()) {
                return ys.cons(b).cons(a).cons(xs.head());
            } else if (ys.isSingle()) {
                return xs.snoc(a).snoc(b).snoc(ys.head());
            } else {
                Deep<V, Node<V,A>> d1 = (Deep<V, Node<V,A>>)xs;
                Deep<V, Node<V,A>> d2 = (Deep<V, Node<V,A>>)ys;
                FTMaker<V, Node<V,A>> nm = m.nodeMaker();
                V v = m.sum(d1.v, a.v, b.v, d2.v);
                return nm.deep(v, d1.pr, addDigits2(nm, d1.mi, d1.sf, a, b, d2.pr, d2.mi), d2.sf);
            }
        }

        static <V,A> FTree<V, Node<V, Node<V, A>>>
        addDigits2(FTMaker<V, Node<V,A>>        m,
                   FTree<V, Node<V, Node<V,A>>> m1,
                   Digit<V,Node<V,A>>           s1,
                   Node<V,A>                    x,
                   Node<V,A>                    y,
                   Digit<V, Node<V,A>>          p2,
                   FTree<V, Node<V, Node<V,A>>> m2) {
          switch ((s1.card() << 4) | p2.card()) {
            case 0x11: return s1.as(a -> p2.as(b ->
                       append2(m, m1, m.node2(a, x), m.node2(y, b), m2)));
            case 0x12: return s1.as(a -> p2.as((b, c) ->
                       append2(m, m1, m.node3(a, x, y), m.node2(b, c), m2)));
            case 0x13: return s1.as(a -> p2.as((b, c, d) ->
                       append2(m, m1, m.node3(a, x, y), m.node3(b, c, d), m2)));
            case 0x14: return s1.as(a -> p2.as((b, c, d, e) ->
                       append3(m, m1, m.node3(a, x, y), m.node2(b, c), m.node2(d, e), m2)));

            case 0x21: return s1.as((a, b) -> p2.as(c ->
                       append2(m, m1, m.node3(a, b, x), m.node2(y, c), m2)));
            case 0x22: return s1.as((a, b) -> p2.as((c, d) ->
                       append2(m, m1, m.node3(a, b, x), m.node3(y, c, d), m2)));
            case 0x23: return s1.as((a, b) -> p2.as((c, d, e) ->
                       append3(m, m1, m.node3(a, b, x), m.node2(y, c), m.node2(d, e), m2)));
            case 0x24: return s1.as((a, b) -> p2.as((c, d, e, f) ->
                       append3(m, m1, m.node3(a, b, x), m.node3(y, c, d), m.node2(e, f), m2)));

            case 0x31: return s1.as((a, b, c) -> p2.as(d ->
                       append2(m, m1, m.node3(a, b, c), m.node3(x, y, d), m2)));
            case 0x32: return s1.as((a, b, c) -> p2.as((d, e) ->
                       append3(m, m1, m.node3(a, b, c), m.node2(x, y), m.node2(d, e), m2)));
            case 0x33: return s1.as((a, b, c) -> p2.as((d, e, f) ->
                       append3(m, m1, m.node3(a, b, c), m.node3(x, y, d), m.node2(e, f), m2)));
            case 0x34: return s1.as((a, b, c) -> p2.as((d, e, f, g) ->
                       append3(m, m1, m.node3(a, b, c), m.node3(x, y, d), m.node3(e, f, g), m2)));

            case 0x41: return s1.as((a, b, c, d) -> p2.as(e ->
                       append3(m, m1, m.node3(a, b, c), m.node2(d, x), m.node2(y, e), m2)));
            case 0x42: return s1.as((a, b, c, d) -> p2.as((e, f) ->
                       append3(m, m1, m.node3(a, b, c), m.node3(d, x, y), m.node2(e, f), m2)));
            case 0x43: return s1.as((a, b, c, d) -> p2.as((e, f, g) ->
                       append3(m, m1, m.node3(a, b, c), m.node3(d, x, y), m.node3(e, f, g), m2)));
            case 0x44: return s1.as((a, b, c, d) -> p2.as((e, f, g, h) ->
                       append4(m, m1, m.node3(a, b, c), m.node3(d, x, y), m.node2(e, f), m.node2(g, h), m2)));

            default: throw new Error();
          }
        }

        static <V, A> FTree<V, Node<V, A>>
        append3(FTMaker<V,A>        m,
                FTree<V, Node<V,A>> xs,
                Node<V,A>           a,
                Node<V,A>           b,
                Node<V,A>           c,
                FTree<V, Node<V,A>> ys) {
            if (xs.isEmpty()) {
                return ys.cons(c).cons(b).cons(a);
            } else if (ys.isEmpty()) {
                return xs.snoc(a).snoc(b).snoc(c);
            } else if (xs.isSingle()) {
                return ys.cons(c).cons(b).cons(a).cons(xs.head());
            } else if (ys.isSingle()) {
                return xs.snoc(a).snoc(b).snoc(c).snoc(ys.head());
            } else {
                Deep<V, Node<V,A>> d1 = (Deep<V, Node<V,A>>)xs;
                Deep<V, Node<V,A>> d2 = (Deep<V, Node<V,A>>)ys;
                FTMaker<V, Node<V,A>> nm = m.nodeMaker();
                V v = m.sum(d1.v, a.v, b.v, c.v, d2.v);
                return nm.deep(v, d1.pr, addDigits3(nm, d1.mi, d1.sf, a, b, c, d2.pr, d2.mi), d2.sf);
            }
        }

        static <V, A> FTree<V, Node<V, Node<V, A>>>
        addDigits3(FTMaker<V, Node<V,A>>        m,
                   FTree<V, Node<V, Node<V,A>>> m1,
                   Digit<V, Node<V,A>>          s1,
                   Node<V,A>                    x,
                   Node<V,A>                    y,
                   Node<V,A>                    z,
                   Digit<V, Node<V,A>>          p2,
                   FTree<V, Node<V, Node<V,A>>> m2) {
          switch ((s1.card() << 4) | p2.card()) {
            case 0x11: return s1.as(a -> p2.as(b ->
                       append2(m, m1, m.node3(a, x, y), m.node2(z, b), m2)));
            case 0x12: return s1.as(a -> p2.as((b, c) ->
                       append2(m, m1, m.node3(a, x, y), m.node3(z, b, c), m2)));
            case 0x13: return s1.as(a -> p2.as((b, c, d) ->
                       append3(m, m1, m.node3(a, x, y), m.node2(z, b), m.node2(c, d), m2)));
            case 0x14: return s1.as(a -> p2.as((b, c, d, e) ->
                       append3(m, m1, m.node3(a, x, y), m.node3(z, b, c), m.node2(d, e), m2)));

            case 0x21: return s1.as((a, b) -> p2.as(c ->
                       append2(m, m1, m.node3(a, b, x), m.node3(y, z, c), m2)));
            case 0x22: return s1.as((a, b) -> p2.as((c, d) ->
                       append3(m, m1, m.node3(a, b, x), m.node2(y, z), m.node2(c, d), m2)));
            case 0x23: return s1.as((a, b) -> p2.as((c, d, e) ->
                       append3(m, m1, m.node3(a, b, x), m.node3(y, z, c), m.node2(d, e), m2)));
            case 0x24: return s1.as((a, b) -> p2.as((c, d, e, f) ->
                       append3(m, m1, m.node3(a, b, x), m.node3(y, z, c), m.node3(d, e, f), m2)));

            case 0x31: return s1.as((a, b, c) -> p2.as(d ->
                       append3(m, m1, m.node3(a, b, c), m.node2(x, y), m.node2(z, d), m2)));
            case 0x32: return s1.as((a, b, c) -> p2.as((d, e) ->
                       append3(m, m1, m.node3(a, b, c), m.node3(x, y, z), m.node2(d, e), m2)));
            case 0x33: return s1.as((a, b, c) -> p2.as((d, e, f) ->
                       append3(m, m1, m.node3(a, b, c), m.node3(x, y, z), m.node3(d, e, f), m2)));
            case 0x34: return s1.as((a, b, c) -> p2.as((d, e, f, g) ->
                       append4(m, m1, m.node3(a, b, c), m.node3(x, y, z), m.node2(d, e), m.node2(f, g), m2)));

            case 0x41: return s1.as((a, b, c, d) -> p2.as(e ->
                       append3(m, m1, m.node3(a, b, c), m.node3(d, x, y), m.node2(z, e), m2)));
            case 0x42: return s1.as((a, b, c, d) -> p2.as((e, f) ->
                       append3(m, m1, m.node3(a, b, c), m.node3(d, x, y), m.node3(z, e, f), m2)));
            case 0x43: return s1.as((a, b, c, d) -> p2.as((e, f, g) ->
                       append4(m, m1, m.node3(a, b, c), m.node3(d, x, y), m.node2(z, e), m.node2(f, g), m2)));
            case 0x44: return s1.as((a, b, c, d) -> p2.as((e, f, g, h) ->
                       append4(m, m1, m.node3(a, b, c), m.node3(d, x, y), m.node3(z, e, f), m.node2(g, h), m2)));

            default: throw new Error();
          }
        }

        static <V, A> FTree<V, Node<V, A>>
        append4(FTMaker<V,A>        m,
                FTree<V, Node<V,A>> xs,
                Node<V,A>           a,
                Node<V,A>           b,
                Node<V,A>           c,
                Node<V,A>           d,
                FTree<V, Node<V,A>> ys) {
            if (xs.isEmpty()) {
                return ys.cons(d).cons(c).cons(b).cons(a);
            } else if (ys.isEmpty()) {
                return xs.snoc(a).snoc(b).snoc(c).snoc(d);
            } else if (xs.isSingle()) {
                return ys.cons(d).cons(c).cons(b).cons(a).cons(xs.head());
            } else if (ys.isSingle()) {
                return xs.snoc(a).snoc(b).snoc(c).snoc(d).snoc(ys.head());
            } else {
                Deep<V, Node<V,A>> d1 = (Deep<V, Node<V,A>>)xs;
                Deep<V, Node<V,A>> d2 = (Deep<V, Node<V,A>>)ys;
                FTMaker<V, Node<V,A>> nm = m.nodeMaker();
                V v = m.sum(d1.v, a.v, b.v, c.v, d.v, d2.v);
                return nm.deep(v, d1.pr, addDigits4(nm, d1.mi, d1.sf, a, b, c, d, d2.pr, d2.mi), d2.sf);
            }
        }

        static <V, A> FTree<V, Node<V, Node<V, A>>>
        addDigits4(FTMaker<V, Node<V,A>>        m,
                   FTree<V, Node<V, Node<V,A>>> m1,
                   Digit<V, Node<V,A>>          s1,
                   Node<V,A>                    x,
                   Node<V,A>                    y,
                   Node<V,A>                    z,
                   Node<V,A>                    w,
                   Digit<V, Node<V,A>>          p2,
                   FTree<V, Node<V, Node<V,A>>> m2) {
          switch ((s1.card() << 4) | p2.card()) {
            case 0x11: return s1.as(a -> p2.as(b ->
                       append2(m, m1, m.node3(a, x, y), m.node3(z, w, b), m2)));
            case 0x12: return s1.as(a -> p2.as((b, c) ->
                       append3(m, m1, m.node3(a, x, y), m.node2(z, w), m.node2(b, c), m2)));
            case 0x13: return s1.as(a -> p2.as((b, c, d) ->
                       append3(m, m1, m.node3(a, x, y), m.node3(z, w, b), m.node2(c, d), m2)));
            case 0x14: return s1.as(a -> p2.as((b, c, d, e) ->
                       append3(m, m1, m.node3(a, x, y), m.node3(z, w, b), m.node3(c, d, e), m2)));

            case 0x21: return s1.as((a, b) -> p2.as(c ->
                       append3(m, m1, m.node3(a, b, x), m.node2(y, z), m.node2(w, c), m2)));
            case 0x22: return s1.as((a, b) -> p2.as((c, d) ->
                       append3(m, m1, m.node3(a, b, x), m.node3(y, z, w), m.node2(c, d), m2)));
            case 0x23: return s1.as((a, b) -> p2.as((c, d, e) ->
                       append3(m, m1, m.node3(a, b, x), m.node3(y, z, w), m.node3(c, d, e), m2)));
            case 0x24: return s1.as((a, b) -> p2.as((c, d, e, f) ->
                       append4(m, m1, m.node3(a, b, x), m.node3(y, z, w), m.node2(c, d), m.node2(e, f), m2)));

            case 0x31: return s1.as((a, b, c) -> p2.as(d ->
                       append3(m, m1, m.node3(a, b, c), m.node3(x, y, z), m.node2(w, d), m2)));
            case 0x32: return s1.as((a, b, c) -> p2.as((d, e) ->
                       append3(m, m1, m.node3(a, b, c), m.node3(x, y, z), m.node3(w, d, e), m2)));
            case 0x33: return s1.as((a, b, c) -> p2.as((d, e, f) ->
                       append4(m, m1, m.node3(a, b, c), m.node3(x, y, z), m.node2(w, d), m.node2(e, f), m2)));
            case 0x34: return s1.as((a, b, c) -> p2.as((d, e, f, g) ->
                       append4(m, m1, m.node3(a, b, c), m.node3(x, y, z), m.node3(w, d, e), m.node2(f, g), m2)));

            case 0x41: return s1.as((a, b, c, d) -> p2.as(e ->
                       append3(m, m1, m.node3(a, b, c), m.node3(d, x, y), m.node3(z, w, e), m2)));
            case 0x42: return s1.as((a, b, c, d) -> p2.as((e, f) ->
                       append4(m, m1, m.node3(a, b, c), m.node3(d, x, y), m.node2(z, w), m.node2(e, f), m2)));
            case 0x43: return s1.as((a, b, c, d) -> p2.as((e, f, g) ->
                       append4(m, m1, m.node3(a, b, c), m.node3(d, x, y), m.node3(z, w, e), m.node2(f, g), m2)));
            case 0x44: return s1.as((a, b, c, d) -> p2.as((e, f, g, h) ->
                       append4(m, m1, m.node3(a, b, c), m.node3(d, x, y), m.node3(z, w, e), m.node3(f, g, h), m2)));

            default: throw new Error();
          }
        }

        @Override
        public <R> R foldRight(BiFunction<? super A, Supplier<R>, R> f, Supplier<R> sr) {
            return pr.foldRight(f, () -> mi.foldRight((x, r) -> x.foldRight(f, r), () -> sf.foldRight(f, sr)));
        }

        @Override
        public <R> R foldRight_(R z, BiFunction<? super A, R, R> f) {
            return pr.foldRight_(mi.foldRight_(sf.foldRight_(z, f), (x, r) -> x.foldRight_(r, f)), f);
        }

        @Override
        public <R> R foldLeft(BiFunction<Supplier<R>, ? super A, R> f, Supplier<R> sr) {
            return sf.foldLeft(f, () -> mi.foldLeft((r, x) -> x.foldLeft(f, r), () -> pr.foldLeft(f, sr)));
        }

        @Override
        public <R> R foldLeft(R z, BiFunction<R, ? super A, R> f) {
            return sf.foldLeft(mi.foldLeft(pr.foldLeft(z, f), (r, x) -> x.foldLeft(r, f)), f);
        }
    }
}
