/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.data;

import java.util.NoSuchElementException;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.IntFunction;
import java.util.function.Supplier;
import java.util.function.UnaryOperator;

import com.cloudway.fp.function.TriFunction;
import com.cloudway.fp.$;
import com.cloudway.fp.control.Applicative;

// @formatter:off

final class VectorImpl {
    private VectorImpl() {}

    // Internal Interfaces

    /**
     * Things that can be sized.
     */
    interface Sized {
        int size();
    }

    /**
     * An intermediate lookup result.
     */
    static final class Place<A> {
        final int index;
        final A result;

        Place(int i, A a) {
            index = i;
            result = a;
        }
    }

    /**
     * A tree split result.
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
    interface IndexFunction<A> {
        A apply(int i, A a);
    }

    @FunctionalInterface
    interface QuadFunction<A, B, C, D, R> {
        R apply(A a, B b, C c, D d);
    }

    // Utility Methods

    private static final FTree<?> EMPTY = new Empty<>();

    @SuppressWarnings("unchecked")
    static <A> FTree<A> emptyTree() {
        return (FTree<A>)EMPTY;
    }

    static <A> FTree<A> singleTree(A a) {
        return new Single<>(a);
    }

    static <A> FTree<A> deep(int size, Digit<A> pr, FTree<Node<A>> mi, Digit<A> sf) {
        return new Deep<>(size, pr, mi, sf);
    }

    static <A> FTree<A> pullL(int size, FTree<Node<A>> mi, Digit<A> sf) {
        return mi.isEmpty()
             ? sf.toTree()
             : deep(size, mi.head().toDigit(), mi.tail(), sf);
    }

    static <A> FTree<A> pullR(int size, Digit<A> pr, FTree<Node<A>> mi) {
        return mi.isEmpty()
             ? pr.toTree()
             : deep(size, pr, mi.init(), mi.last().toDigit());
    }

    static <A> FTree<A> deepL(Digit<A> pr, FTree<Node<A>> mi, Digit<A> sf) {
        if (pr == null) {
            return pullL(sizeof(mi, sf), mi, sf);
        } else {
            return deep(sizeof(pr, mi, sf), pr, mi, sf);
        }
    }

    static <A> FTree<A> deepR(Digit<A> pr, FTree<Node<A>> mi, Digit<A> sf) {
        if (sf == null) {
            return pullR(sizeof(pr, mi), pr, mi);
        } else {
            return deep(sizeof(pr, mi, sf), pr, mi, sf);
        }
    }

    static <A> Digit<A> one(A a) {
        return new One<>(a);
    }

    static <A> Digit<A> two(A a, A b) {
        return new Two<>(a, b);
    }

    static <A> Digit<A> three(A a, A b, A c) {
        return new Three<>(a, b, c);
    }

    static <A> Digit<A> four(A a, A b, A c, A d) {
        return new Four<>(a, b, c, d);
    }

    static <A> Node<A> node2(int s, A a, A b) {
        return new Node2<>(s, a, b);
    }

    static <A> Node<A> node2(A a, A b) {
        return new Node2<>(sizeof(a, b), a, b);
    }

    static <A> Node<A> node3(int s, A a, A b, A c) {
        return new Node3<>(s, a, b, c);
    }

    static <A> Node<A> node3(A a, A b, A c) {
        return new Node3<>(sizeof(a, b, c), a, b, c);
    }

    static <A> int sizeof(A a) {
        return (a instanceof Sized) ? ((Sized)a).size() : 1;
    }

    static <A> int sizeof(A a, A b) {
        return (a instanceof Sized) ? sizeof((Sized)a, (Sized)b) : 2;
    }

    static <A> int sizeof(A a, A b, A c) {
        return (a instanceof Sized) ? sizeof((Sized)a, (Sized)b, (Sized)c) : 3;
    }

    static <A> int sizeof(A a, A b, A c, A d) {
        return (a instanceof Sized) ? sizeof((Sized)a, (Sized)b, (Sized)c, (Sized)d) : 4;
    }

    static int sizeof(Sized a, Sized b) {
        return a.size() + b.size();
    }

    static int sizeof(Sized a, Sized b, Sized c) {
        return a.size() + b.size() + c.size();
    }

    static int sizeof(Sized a, Sized b, Sized c, Sized d) {
        return a.size() + b.size() + c.size() + d.size();
    }

    static <A> FTree<A> iterate(int len, IntFunction<A> f) {
        if (len < 0) {
            throw new IllegalArgumentException("iterate called with negative length");
        } else if (len == 0) {
            return emptyTree();
        } else {
            return create(f, 1, 0, len);
        }
    }

    private static <A> FTree<A> create(IntFunction<A> f, int s, int i, int trees) {
        switch (trees) {
          case 1: return singleTree(f.apply(i));
          case 2: return deep(2*s, c1(f, i),    emptyTree(), c1(f, i+s));
          case 3: return deep(3*s, c2(f, i, s), emptyTree(), c1(f, i+2*s));
          case 4: return deep(4*s, c2(f, i, s), emptyTree(), c2(f, i+2*s, s));
          case 5: return deep(5*s, c3(f, i, s), emptyTree(), c2(f, i+3*s, s));
          case 6: return deep(6*s, c3(f, i, s), emptyTree(), c3(f, i+3*s, s));

        default:
          int q = trees / 3, r = trees % 3;
          IntFunction<Node<A>> mf = j -> node3(3*s, f.apply(j), f.apply(j+s), f.apply(j+2*s));
          switch (r) {
            case 1:  return deep(trees*s, c2(f, i, s), create(mf, 3*s, i+2*s, q-1), c2(f, i+(2+3*(q-1))*s, s));
            case 2:  return deep(trees*s, c3(f, i, s), create(mf, 3*s, i+3*s, q-1), c2(f, i+(3+3*(q-1))*s, s));
            default: return deep(trees*s, c3(f, i, s), create(mf, 3*s, i+3*s, q-2), c3(f, i+(3+3*(q-2))*s, s));
          }
        }
    }

    private static <A> Digit<A> c1(IntFunction<A> f, int i)
        { return one(f.apply(i)); }
    private static <A> Digit<A> c2(IntFunction<A> f, int i, int s)
        { return two(f.apply(i), f.apply(i+s)); }
    private static <A> Digit<A> c3(IntFunction<A> f, int i, int s)
        { return three(f.apply(i), f.apply(i+s), f.apply(i+s+s)); }

    // ------------------------------------------------------------------------

    /**
     * A digit is a vector of 1-4 elements. Serves as a pointer to the
     * prefix or suffix of a finger tree.
     */
    static abstract class Digit<A> implements Foldable<A>, Sized, Forcible<Digit<A>> {
        abstract int card();
        abstract A first();
        abstract A last();

        abstract Place<A> lookup(int i);
        abstract Digit<A> modify(int i, IndexFunction<A> f);
        abstract Split<Digit<A>,A> split(int i);
        abstract <B> Digit<B> map(Function<? super A, ? extends B> f);
        abstract <F,B> $<F,Digit<B>> traverse(Applicative<F> m, Function<? super A, ? extends $<F,B>> f);
        abstract Digit<A> reverse(Function<A,A> f);
        abstract <B,S> Digit<B> splitMap(S s, BiFunction<Integer,S,Pair<S>> splt, BiFunction<S,A,B> f);
        abstract FTree<A> toTree();

        <R> R as(Function<A,R> f) { throw new UnsupportedOperationException(); }
        <R> R as(BiFunction<A,A,R> f) { throw new UnsupportedOperationException(); }
        <R> R as(TriFunction<A,A,A,R> f) { throw new UnsupportedOperationException(); }
        <R> R as(QuadFunction<A,A,A,A,R> f) { throw new UnsupportedOperationException(); }

        public String toString() {
            return show(", ", "Digit[", "]");
        }
    }

    static final class One<A> extends Digit<A> {
        private final A a;

        One(A a) {
            this.a = a;
        }

        @Override int card() { return 1; }
        @Override A first()  { return a; }
        @Override A last()   { return a; }

        @Override public int size() {
            return sizeof(a);
        }

        @Override Place<A> lookup(int i) {
            return new Place<>(i, a);
        }

        @Override Digit<A> modify(int i, IndexFunction<A> f) {
            return one(f.apply(i, a));
        }

        @Override Split<Digit<A>,A> split(int i) {
            return new Split<>(null, a, null);
        }

        @Override <B> Digit<B> map(Function<? super A, ? extends B> f) {
            return one(f.apply(a));
        }

        @Override <F,B> $<F, Digit<B>>
        traverse(Applicative<F> m, Function<? super A, ? extends $<F,B>> f) {
            return m.map(f.apply(a), VectorImpl::one);
        }

        @Override Digit<A> reverse(Function<A,A> f) {
            return one(f.apply(a));
        }

        @Override <B,S> Digit<B>
        splitMap(S s, BiFunction<Integer, S, Pair<S>> splt, BiFunction<S, A, B> f) {
            return one(f.apply(s, a));
        }

        @Override FTree<A> toTree() {
            return singleTree(a);
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

        @Override
        public Digit<A> force() {
            Forcible.force(a);
            return this;
        }

        @Override <R> R as(Function<A,R> f) { return f.apply(a); }
    }

    static final class Two<A> extends Digit<A> {
        final A a, b;

        Two(A a, A b) {
            this.a = a;
            this.b = b;
        }

        @Override int card() { return 2; }
        @Override A first()  { return a; }
        @Override A last()   { return b; }

        @Override public int size() {
            return sizeof(a, b);
        }

        @Override Place<A> lookup(int i) {
            int sa = sizeof(a);
            return i < sa ? new Place<>(i, a)
                          : new Place<>(i - sa, b);
        }

        @Override Digit<A> modify(int i, IndexFunction<A> f) {
            int sa = sizeof(a);
            return i < sa ? two(f.apply(i, a), b)
                          : two(a, f.apply(i - sa, b));
        }

        @Override Split<Digit<A>,A> split(int i) {
            int sa = sizeof(a);
            return i < sa ? new Split<>(null, a, one(b))
                          : new Split<>(one(a), b, null);
        }

        @Override <B> Digit<B> map(Function<? super A, ? extends B> f) {
            return two(f.apply(a), f.apply(b));
        }

        @Override <F,B> $<F, Digit<B>>
        traverse(Applicative<F> m, Function<? super A, ? extends $<F,B>> f) {
            return m.ap2(VectorImpl::two, f.apply(a), f.apply(b));
        }

        @Override Digit<A> reverse(Function<A,A> f) {
            return two(f.apply(b), f.apply(a));
        }

        @Override <B,S> Digit<B>
        splitMap(S s, BiFunction<Integer, S, Pair<S>> splt, BiFunction<S, A, B> f) {
            return splt.apply(sizeof(a), s).as((first, second) ->
                        two(f.apply(first, a), f.apply(second, b)));
        }

        @Override FTree<A> toTree() {
            return deep(size(), one(a), emptyTree(), one(b));
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

        @Override
        public Digit<A> force() {
            Forcible.force(a);
            Forcible.force(b);
            return this;
        }

        @Override <R> R as(BiFunction<A,A,R> f) { return f.apply(a, b); }
    }

    static final class Three<A> extends Digit<A> {
        private final A a, b, c;

        Three(A a, A b, A c) {
            this.a = a;
            this.b = b;
            this.c = c;
        }

        @Override int card() { return 3; }
        @Override A first()  { return a; }
        @Override A last()   { return c; }

        @Override public int size() {
            return sizeof(a, b, c);
        }

        @Override Place<A> lookup(int i) {
            int sa = sizeof(a);
            int sab = sa + sizeof(b);
            return i < sa  ? new Place<>(i, a) :
                   i < sab ? new Place<>(i - sa, b)
                           : new Place<>(i - sab, c);
        }

        @Override Digit<A> modify(int i, IndexFunction<A> f) {
            int sa = sizeof(a);
            int sab = sa + sizeof(b);
            return i < sa  ? three(f.apply(i, a), b, c) :
                   i < sab ? three(a, f.apply(i - sa, b), c)
                           : three(a, b, f.apply(i - sab, c));
        }

        @Override Split<Digit<A>,A> split(int i) {
            int sa = sizeof(a);
            int sab = sa + sizeof(b);
            return i < sa  ? new Split<>(null, a, two(b, c)) :
                   i < sab ? new Split<>(one(a), b, one(c))
                           : new Split<>(two(a, b), c, null);
        }

        @Override <B> Digit<B> map(Function<? super A, ? extends B> f) {
            return three(f.apply(a), f.apply(b), f.apply(c));
        }

        @Override <F,B> $<F, Digit<B>>
        traverse(Applicative<F> m, Function<? super A, ? extends $<F,B>> f) {
            return m.ap3(VectorImpl::three, f.apply(a), f.apply(b), f.apply(c));
        }

        @Override Digit<A> reverse(Function<A,A> f) {
            return three(f.apply(c), f.apply(b), f.apply(a));
        }

        @Override <B,S> Digit<B>
        splitMap(S s, BiFunction<Integer, S, Pair<S>> splt, BiFunction<S, A, B> f) {
            return splt.apply(sizeof(a), s).as((first, r) ->
                   splt.apply(sizeof(b), r).as((second, third) ->
                        three(f.apply(first, a), f.apply(second, b), f.apply(third, c))));
        }

        @Override FTree<A> toTree() {
            return deep(size(), two(a, b), emptyTree(), one(c));
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

        @Override
        public Digit<A> force() {
            Forcible.force(a);
            Forcible.force(b);
            Forcible.force(c);
            return this;
        }

        @Override <R> R as(TriFunction<A,A,A,R> f) { return f.apply(a, b, c); }
    }

    static final class Four<A> extends Digit<A> {
        private final A a, b, c, d;

        Four(A a, A b, A c, A d) {
            this.a = a;
            this.b = b;
            this.c = c;
            this.d = d;
        }

        @Override int card() { return 4; }
        @Override A first()  { return a; }
        @Override A last()   { return d; }

        @Override public int size() {
            return sizeof(a, b, c, d);
        }

        @Override Place<A> lookup(int i) {
            int sa = sizeof(a);
            int sab = sa + sizeof(b);
            int sabc = sab + sizeof(c);
            return i < sa   ? new Place<>(i, a) :
                   i < sab  ? new Place<>(i - sa, b) :
                   i < sabc ? new Place<>(i - sab, c)
                            : new Place<>(i - sabc, d);
        }

        @Override Digit<A> modify(int i, IndexFunction<A> f) {
            int sa = sizeof(a);
            int sab = sa + sizeof(b);
            int sabc = sab + sizeof(c);
            return i < sa   ? four(f.apply(i, a), b, c, d) :
                   i < sab  ? four(a, f.apply(i - sa, b), c, d) :
                   i < sabc ? four(a, b, f.apply(i - sab, c), d)
                            : four(a, b, c, f.apply(i - sabc, d));
        }

        @Override Split<Digit<A>,A> split(int i) {
            int sa = sizeof(a);
            int sab = sa + sizeof(b);
            int sabc = sab + sizeof(c);
            return i < sa   ? new Split<>(null, a, three(b, c, d)) :
                   i < sab  ? new Split<>(one(a), b, two(c, d)) :
                   i < sabc ? new Split<>(two(a, b), c, one(d))
                            : new Split<>(three(a, b, c), d, null);
        }

        @Override <B> Digit<B> map(Function<? super A, ? extends B> f) {
            return four(f.apply(a), f.apply(b), f.apply(c), f.apply(d));
        }

        @Override <F,B> $<F, Digit<B>>
        traverse(Applicative<F> m, Function<? super A, ? extends $<F,B>> f) {
            Function<B, Function<B, Function<B, Function<B, Digit<B>>>>> four_f =
                a -> b -> c -> d -> four(a, b, c, d);
            return m.ap(m.ap(m.ap(m.map(f.apply(a), four_f), f.apply(b)), f.apply(c)), f.apply(d));
        }

        @Override Digit<A> reverse(Function<A,A> f) {
            return four(f.apply(d), f.apply(c), f.apply(b), f.apply(a));
        }

        @Override <B,S> Digit<B>
        splitMap(S s, BiFunction<Integer, S, Pair<S>> splt, BiFunction<S, A, B> f) {
            return splt.apply(sizeof(a), s).as((first, r) ->
                   splt.apply(sizeof(b) + sizeof(c), r).as((middle, fourth) ->
                   splt.apply(sizeof(b), middle).as((second, third) ->
                        four(f.apply(first, a), f.apply(second, b), f.apply(third, c), f.apply(fourth, d)))));
        }

        @Override FTree<A> toTree() {
            return deep(size(), two(a, b), emptyTree(), two(c, d));
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

        @Override
        public Digit<A> force() {
            Forcible.force(a);
            Forcible.force(b);
            Forcible.force(c);
            Forcible.force(d);
            return this;
        }

        @Override <R> R as(QuadFunction<A,A,A,A,R> f) { return f.apply(a, b, c, d); }
    }

    // ------------------------------------------------------------------------

    static abstract class Node<A> implements Foldable<A>, Sized, Forcible<Node<A>> {
        abstract Place<A> lookup(int i);
        abstract Node<A> modify(int i, IndexFunction<A> f);
        abstract Split<Digit<A>,A> split(int i);
        abstract <B> Node<B> map(Function<? super A, ? extends B> f);
        abstract <F,B> $<F,Node<B>> traverse(Applicative<F> m, Function<? super A, ? extends $<F,B>> f);
        abstract Node<A> reverse(Function<A,A> f);
        abstract <B,S> Node<B> splitMap(S s, BiFunction<Integer,S,Pair<S>> splt, BiFunction<S,A,B> f);
        abstract Digit<A> toDigit();

        public String toString() {
            return show(", ", "Node[", "]");
        }
    }

    static final class Node2<A> extends Node<A> {
        final int size;
        final A a, b;

        Node2(int size, A a, A b) {
            this.size = size;
            this.a = a;
            this.b = b;
        }

        @Override public int size() {
            return size;
        }

        @Override Place<A> lookup(int i) {
            int sa = sizeof(a);
            return i < sa ? new Place<>(i, a)
                          : new Place<>(i - sa, b);
        }

        @Override Node<A> modify(int i, IndexFunction<A> f) {
            int sa = sizeof(a);
            return i < sa ? node2(size, f.apply(i, a), b)
                          : node2(size, a, f.apply(i - sa, b));
        }

        @Override Split<Digit<A>,A> split(int i) {
            int sa = sizeof(a);
            return i < sa ? new Split<>(null, a, one(b))
                          : new Split<>(one(a), b, null);
        }

        @Override <B> Node<B> map(Function<? super A, ? extends B> f) {
            return node2(size, f.apply(a), f.apply(b));
        }

        @Override <F,B> $<F, Node<B>>
        traverse(Applicative<F> m, Function<? super A, ? extends $<F,B>> f) {
            return m.ap2((x, y) -> node2(size, x, y), f.apply(a), f.apply(b));
        }

        @Override Node<A> reverse(Function<A,A> f) {
            return node2(size, f.apply(b), f.apply(a));
        }

        @Override <B,S> Node<B>
        splitMap(S s, BiFunction<Integer, S, Pair<S>> splt, BiFunction<S, A, B> f) {
            return splt.apply(sizeof(a), s).as((first, second) ->
                        node2(size, f.apply(first, a), f.apply(second, b)));
        }

        @Override Digit<A> toDigit() {
            return two(a, b);
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

        @Override
        public Node<A> force() {
            Forcible.force(a);
            Forcible.force(b);
            return this;
        }
    }

    static final class Node3<A> extends Node<A> {
        final int size;
        final A a, b, c;

        Node3(int size, A a, A b, A c) {
            this.size = size;
            this.a = a;
            this.b = b;
            this.c = c;
        }

        @Override public int size() {
            return size;
        }

        @Override Place<A> lookup(int i) {
            int sa = sizeof(a);
            int sab = sa + sizeof(b);
            return i < sa  ? new Place<>(i, a) :
                   i < sab ? new Place<>(i - sa, b)
                           : new Place<>(i - sab, c);
        }

        @Override Node<A> modify(int i, IndexFunction<A> f) {
            int sa = sizeof(a);
            int sab = sa + sizeof(b);
            return i < sa  ? node3(size, f.apply(i, a), b, c) :
                   i < sab ? node3(size, a, f.apply(i - sa, b), c)
                           : node3(size, a, b, f.apply(i - sab, c));
        }

        @Override Split<Digit<A>,A> split(int i) {
            int sa = sizeof(a);
            int sab = sa + sizeof(b);
            return i < sa  ? new Split<>(null, a, two(b, c)) :
                   i < sab ? new Split<>(one(a), b, one(c))
                           : new Split<>(two(a, b), c, null);
        }

        @Override <B> Node<B> map(Function<? super A, ? extends B> f) {
            return node3(size, f.apply(a), f.apply(b), f.apply(c));
        }

        @Override <F,B> $<F,Node<B>>
        traverse(Applicative<F> m, Function<? super A, ? extends $<F,B>> f) {
            return m.ap3((x, y, z) -> node3(size, x, y, z), f.apply(a), f.apply(b), f.apply(c));
        }

        @Override Node<A> reverse(Function<A,A> f) {
            return node3(size, f.apply(c), f.apply(b), f.apply(a));
        }

        @Override <B,S> Node<B>
        splitMap(S s, BiFunction<Integer, S, Pair<S>> splt, BiFunction<S, A, B> f) {
            return splt.apply(sizeof(a), s).as((first, r) ->
                   splt.apply(sizeof(b), r).as((second, third) ->
                        node3(size, f.apply(first, a), f.apply(second, b), f.apply(third, c))));
        }

        @Override Digit<A> toDigit() {
            return three(a, b, c);
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

        @Override
        public Node<A> force() {
            Forcible.force(a);
            Forcible.force(b);
            Forcible.force(c);
            return this;
        }
    }

    // ------------------------------------------------------------------------

    static abstract class FTree<A> implements Vector<A>, Sized {
        @Override
        public boolean isEmpty() {
            return false;
        }

        boolean isSingle() {
            return false;
        }

        @Override public abstract FTree<A> tail();
        @Override public abstract FTree<A> init();
        @Override public abstract FTree<A> cons(A a);
        @Override public abstract FTree<A> snoc(A a);

        @Override
        public A at(int i) {
            rangeCheck(i, size());
            return lookupTree(i).result;
        }

        @Override
        public Vector<A> update(int i, A a) {
            rangeCheck(i, size());
            return modifyTree(i, (_1,_2) -> a);
        }

        @Override
        public Vector<A> modify(int i, UnaryOperator<A> f) {
            rangeCheck(i, size());
            return modifyTree(i, (_1,x) -> f.apply(x));
        }

        @Override
        public Vector<A> delete(int i) {
            int size = this.size();
            rangeCheckPastLast(i, size);
            return i == 0      ? tail() :
                   i == size   ? this :
                   i == size-1 ? init()
                               : splitTree(i).as((l, x, r) -> l.append(r));
        }

        @Override
        public Vector<A> insert(int i, A a) {
            int size = this.size();
            rangeCheckPastLast(i, size);
            return i == 0    ? cons(a) :
                   i == size ? snoc(a)
                             : splitTree(i).as((l, x, r) -> l.snoc(a).snoc(x).append(r));
        }

        @Override
        public Pair<Vector<A>> splitAt(int i) {
            return i <= 0      ? Tuple.pair(emptyTree(), this) :
                   i >= size() ? Tuple.pair(this, emptyTree())
                               : splitTree(i).as((l, x, r) -> Tuple.pair(l, r.cons(x)));
        }

        @Override
        public <B> Vector<B> map(Function<? super A, ? extends B> f) {
            return mapTree(f);
        }

        @Override
        public Vector<A> reverse() {
            return reverseTree(Fn.id());
        }

        @Override
        @SuppressWarnings({"rawtypes", "unchecked"})
        public <B,C> Vector<C> zipWith(Vector<B> that, BiFunction<? super A, ? super B, ? extends C> f) {
            int minLen = Math.min(this.size(), that.size());
            Vector<A> s1 = this.take(minLen);
            Vector<B> s2 = that.take(minLen);
            return ((FTree<A>)s1).splitMap(s2, (i, s) -> s.splitAt(i), (s, a) -> f.apply(a, s.head()));
        }

        abstract Place<A> lookupTree(int i);
        abstract FTree<A> modifyTree(int i, IndexFunction<A> f);
        abstract Split<FTree<A>,A> splitTree(int i);
        abstract <B> FTree<B> mapTree(Function<? super A, ? extends B> f);
        abstract FTree<A> reverseTree(Function<A,A> f);
        abstract <B,S> FTree<B> splitMap(S s, BiFunction<Integer,S,Pair<S>> splt, BiFunction<S,A,B> f);

        private static void rangeCheck(int index, int size) {
            if (index < 0 || index >= size) {
                throw new IndexOutOfBoundsException(outOfBoundsMsg(index, size));
            }
        }

        private static void rangeCheckPastLast(int index, int size) {
            if (index < 0 || index > size) {
                throw new IndexOutOfBoundsException(outOfBoundsMsg(index, size));
            }
        }

        private static String outOfBoundsMsg(int index, int size) {
            return "Index: "+index+", Size: "+size;
        }
    }

    static class Empty<A> extends FTree<A> {
        @Override
        public boolean isEmpty() {
            return true;
        }

        @Override
        public int size() {
            return 0;
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
        public FTree<A> tail() {
            throw new NoSuchElementException();
        }

        @Override
        public FTree<A> init() {
            throw new NoSuchElementException();
        }

        @Override
        public FTree<A> cons(A a) {
            return singleTree(a);
        }

        @Override
        public FTree<A> snoc(A a) {
            return singleTree(a);
        }

        @Override
        public Vector<A> append(Vector<A> that) {
            return that;
        }

        @Override
        Place<A> lookupTree(int i) {
            throw new NoSuchElementException();
        }

        @Override
        FTree<A> modifyTree(int i, IndexFunction<A> f) {
            throw new NoSuchElementException();
        }

        @Override
        Split<FTree<A>,A> splitTree(int i) {
            throw new NoSuchElementException();
        }

        @Override
        <B> FTree<B> mapTree(Function<? super A, ? extends B> f) {
            return emptyTree();
        }

        @Override
        public <F,B> $<F, Vector<B>>
        traverse(Applicative<F> m, Function<? super A, ? extends $<F,B>> f) {
            return m.pure(emptyTree());
        }

        @Override
        FTree<A> reverseTree(Function<A,A> f) {
            return this;
        }

        @Override <B,S> FTree<B>
        splitMap(S s, BiFunction<Integer, S, Pair<S>> splt, BiFunction<S, A, B> f) {
            return emptyTree();
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

        @Override
        public Vector<A> force() {
            return this;
        }

        @Override
        public String toString() {
            return "Vector[]";
        }
    }

    static class Single<A> extends FTree<A> {
        final A a;

        Single(A a) {
            this.a = a;
        }

        @Override
        boolean isSingle() {
            return true;
        }

        @Override
        public int size() {
            return sizeof(a);
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
        public FTree<A> tail() {
            return emptyTree();
        }

        @Override
        public FTree<A> init() {
            return emptyTree();
        }

        @Override
        public FTree<A> cons(A b) {
            return deep(sizeof(a, b), one(b), emptyTree(), one(a));
        }

        @Override
        public FTree<A> snoc(A b) {
            return deep(sizeof(a, b), one(a), emptyTree(), one(b));
        }

        @Override
        public Vector<A> append(Vector<A> that) {
            return that.cons(a);
        }

        @Override
        Place<A> lookupTree(int i) {
            return new Place<>(i, a);
        }

        @Override
        FTree<A> modifyTree(int i, IndexFunction<A> f) {
            return singleTree(f.apply(i, a));
        }

        @Override
        Split<FTree<A>,A> splitTree(int i) {
            return new Split<>(emptyTree(), a, emptyTree());
        }

        @Override
        <B> FTree<B> mapTree(Function<? super A, ? extends B> f) {
            return singleTree(f.apply(a));
        }

        @Override
        public <F,B> $<F, Vector<B>>
        traverse(Applicative<F> m, Function<? super A, ? extends $<F,B>> f) {
            return m.map(f.apply(a), VectorImpl::singleTree);
        }

        @Override
        FTree<A> reverseTree(Function<A,A> f) {
            return singleTree(f.apply(a));
        }

        @Override <B,S> FTree<B>
        splitMap(S s, BiFunction<Integer, S, Pair<S>> splt, BiFunction<S, A, B> f) {
            return singleTree(f.apply(s, a));
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

        @Override
        public Vector<A> force() {
            Forcible.force(a);
            return this;
        }

        @Override
        public String toString() {
            return "Vector[" + a + "]";
        }
    }

    static class Deep<A> extends FTree<A> {
        final int size;
        final Digit<A> pr;
        final FTree<Node<A>> mi;
        final Digit<A> sf;

        Deep(int size, Digit<A> pr, FTree<Node<A>> mi, Digit<A> sf) {
            this.size = size;
            this.pr = pr;
            this.mi = mi;
            this.sf = sf;
        }

        @Override
        public int size() {
            return size;
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
        public FTree<A> tail() {
            int s = size - sizeof(pr.first());
            switch (pr.card()) {
              case 1: return pr.as((a) -> pullL(s, mi, sf));
              case 2: return pr.as((a, b) -> deep(s, one(b), mi, sf));
              case 3: return pr.as((a, b, c) -> deep(s, two(b, c), mi, sf));
              case 4: return pr.as((a, b, c, d) -> deep(s, three(b, c, d), mi, sf));
              default: throw new Error();
            }
        }

        @Override
        public FTree<A> init() {
            int s = size - sizeof(sf.last());
            switch (sf.card()) {
              case 1: return sf.as((a) -> pullR(s, pr, mi));
              case 2: return sf.as((a, b) -> deep(s, pr, mi, one(a)));
              case 3: return sf.as((a, b, c) -> deep(s, pr, mi, two(a, b)));
              case 4: return sf.as((a, b, c, d) -> deep(s, pr, mi, three(a, b, c)));
              default: throw new Error();
            }
        }

        @Override
        public FTree<A> cons(A a) {
            int s = this.size + sizeof(a);
            switch (pr.card()) {
              case 1: return pr.as((b) -> deep(s, two(a, b), mi, sf));
              case 2: return pr.as((b, c) -> deep(s, three(a, b, c), mi, sf));
              case 3: return pr.as((b, c, d) -> deep(s, four(a, b, c, d), mi, sf));
              case 4: return pr.as((b, c, d, e) -> deep(s, two(a, b), mi.cons(node3(c, d, e)), sf));
              default: throw new Error();
            }
        }

        @Override
        public FTree<A> snoc(A z) {
            int s = this.size + sizeof(z);
            switch (sf.card()) {
              case 1: return sf.as((a) -> deep(s, pr, mi, two(a, z)));
              case 2: return sf.as((a, b) -> deep(s, pr, mi, three(a, b, z)));
              case 3: return sf.as((a, b, c) -> deep(s, pr, mi, four(a, b, c, z)));
              case 4: return sf.as((a, b, c, d) -> deep(s, pr, mi.snoc(node3(a, b, c)), two(d, z)));
              default: throw new Error();
            }
        }

        @Override
        Place<A> lookupTree(int i) {
            int spr = pr.size();
            int spm = spr + mi.size();
            if (i < spr) {
                return pr.lookup(i);
            } else if (i < spm) {
                Place<Node<A>> np = mi.lookupTree(i - spr);
                return np.result.lookup(np.index);
            } else {
                return sf.lookup(i - spm);
            }
        }

        @Override
        FTree<A> modifyTree(int i, IndexFunction<A> f) {
            int spr = pr.size();
            int spm = spr + mi.size();
            if (i < spr) {
                return deep(size, pr.modify(i, f), mi, sf);
            } else if (i < spm) {
                return deep(size, pr, mi.modifyTree(i - spr, (j, n) -> n.modify(j, f)), sf);
            } else {
                return deep(size, pr, mi, sf.modify(i - spm, f));
            }
        }

        @Override
        Split<FTree<A>,A> splitTree(int i) {
            int spr = pr.size();
            int spm = spr + mi.size();
            if (i < spr) {
                return pr.split(i).as((l, x, r) ->
                    new Split<>(l != null ? l.toTree() : emptyTree(), x, deepL(r, mi, sf)));
            } else if (i < spm) {
                return mi.splitTree(i - spr).as((ml, mx, mr) ->
                    mx.split(i - spr - ml.size()).as((l, x, r) ->
                        new Split<>(deepR(pr, ml, l), x, deepL(r, mr, sf))));
            } else {
                return sf.split(i - spm).as((l, x, r) ->
                    new Split<>(deepR(pr, mi, l), x, r != null ? r.toTree() : emptyTree()));
            }
        }

        @Override
        <B> FTree<B> mapTree(Function<? super A, ? extends B> f) {
            return deep(size, pr.map(f), mi.mapTree(n -> n.map(f)), sf.map(f));
        }

        @Override
        public <F,B> $<F, Vector<B>>
        traverse(Applicative<F> m, Function<? super A, ? extends $<F,B>> f) {
            TriFunction<Digit<B>, Vector<Node<B>>, Digit<B>, Vector<B>> deep_f =
                (px, mx, sx) -> deep(size, px, (FTree<Node<B>>)mx, sx);
            return m.ap3(deep_f, pr.traverse(m, f),
                                 mi.traverse(m, n -> n.traverse(m, f)),
                                 sf.traverse(m, f));
        }

        @Override
        FTree<A> reverseTree(Function<A, A> f) {
            return deep(size, sf.reverse(f), mi.reverseTree(n -> n.reverse(f)), pr.reverse(f));
        }

        @Override <B,S> FTree<B>
        splitMap(S s, BiFunction<Integer,S,Pair<S>> splt, BiFunction<S,A,B> f) {
            return splt.apply(pr.size(), s).as((prs, r) ->
                   splt.apply(size - pr.size() - sf.size(), r).as((mis, sfs) ->
                        deep(size,
                             pr.splitMap(prs, splt, f),
                             mi.splitMap(mis, splt, (s1, n) -> n.splitMap(s1, splt, f)),
                             sf.splitMap(sfs, splt, f))));
        }

        @Override
        public Vector<A> append(Vector<A> that) {
            if (that.isEmpty()) {
                return this;
            } else if (that instanceof Single) {
                return snoc(that.head());
            } else {
                Deep<A> deep = (Deep<A>)that;
                int s = this.size() + deep.size();
                return deep(s, pr, addDigits0(mi, sf, deep.pr, deep.mi), deep.sf);
            }
        }

        static <A> FTree<Node<A>>
        addDigits0(FTree<Node<A>> m1,
                   Digit<A>       s1,
                   Digit<A>       p2,
                   FTree<Node<A>> m2) {
          switch ((s1.card() << 4) | p2.card()) {
            case 0x11: return s1.as(a -> p2.as(b ->
                       append1(m1, node2(a, b), m2)));
            case 0x12: return s1.as(a -> p2.as((b, c) ->
                       append1(m1, node3(a, b, c), m2)));
            case 0x13: return s1.as(a -> p2.as((b, c, d) ->
                       append2(m1, node2(a, b), node2(c, d), m2)));
            case 0x14: return s1.as(a -> p2.as((b, c, d, e) ->
                       append2(m1, node3(a, b, c), node2(d, e), m2)));

            case 0x21: return s1.as((a, b) -> p2.as(c ->
                       append1(m1, node3(a, b, c), m2)));
            case 0x22: return s1.as((a, b) -> p2.as((c, d) ->
                       append2(m1, node2(a, b), node2(c, d), m2)));
            case 0x23: return s1.as((a, b) -> p2.as((c, d, e) ->
                       append2(m1, node3(a, b, c), node2(d, e), m2)));
            case 0x24: return s1.as((a, b) -> p2.as((c, d, e, f) ->
                       append2(m1, node3(a, b, c), node3(d, e, f), m2)));

            case 0x31: return s1.as((a, b, c) -> p2.as(d ->
                       append2(m1, node2(a, b), node2(c, d), m2)));
            case 0x32: return s1.as((a, b, c) -> p2.as((d, e) ->
                       append2(m1, node3(a, b, c), node2(d, e), m2)));
            case 0x33: return s1.as((a, b, c) -> p2.as((d, e, f) ->
                       append2(m1, node3(a, b, c), node3(d, e, f), m2)));
            case 0x34: return s1.as((a, b, c) -> p2.as((d, e, f, g) ->
                       append3(m1, node3(a, b, c), node2(d, e), node2(f, g), m2)));

            case 0x41: return s1.as((a, b, c, d) -> p2.as(e ->
                       append2(m1, node3(a, b, c), node2(d, e), m2)));
            case 0x42: return s1.as((a, b, c, d) -> p2.as((e, f) ->
                       append2(m1, node3(a, b, c), node3(d, e, f), m2)));
            case 0x43: return s1.as((a, b, c, d) -> p2.as((e, f, g) ->
                       append3(m1, node3(a, b, c), node2(d, e), node2(f, g), m2)));
            case 0x44: return s1.as((a, b, c, d) -> p2.as((e, f, g, h) ->
                       append3(m1, node3(a, b, c), node3(d, e, f), node2(g, h), m2)));

            default: throw new Error();
          }
        }

        static <A> FTree<Node<A>>
        append1(FTree<Node<A>> xs, Node<A> a, FTree<Node<A>> ys) {
            if (xs.isEmpty()) {
                return ys.cons(a);
            } else if (ys.isEmpty()) {
                return xs.snoc(a);
            } else if (xs.isSingle()) {
                return ys.cons(a).cons(xs.head());
            } else if (ys.isSingle()) {
                return xs.snoc(a).snoc(ys.head());
            } else {
                Deep<Node<A>> d1 = (Deep<Node<A>>)xs;
                Deep<Node<A>> d2 = (Deep<Node<A>>)ys;
                int s = d1.size() + a.size() + d2.size();
                return deep(s, d1.pr, addDigits1(d1.mi, d1.sf, a, d2.pr, d2.mi), d2.sf);
            }
        }

        static <A> FTree<Node<Node<A>>>
        addDigits1(FTree<Node<Node<A>>> m1,
                   Digit<Node<A>>       s1,
                   Node<A>              x,
                   Digit<Node<A>>       p2,
                   FTree<Node<Node<A>>> m2) {
          switch ((s1.card() << 4) | p2.card()) {
            case 0x11: return s1.as(a -> p2.as(b ->
                       append1(m1, node3(a, x, b), m2)));
            case 0x12: return s1.as(a -> p2.as((b, c) ->
                       append2(m1, node2(a, x), node2(b, c), m2)));
            case 0x13: return s1.as(a -> p2.as((b, c, d) ->
                       append2(m1, node3(a, x, b), node2(c, d), m2)));
            case 0x14: return s1.as(a -> p2.as((b, c, d, e) ->
                       append2(m1, node3(a, x, b), node3(c, d, e), m2)));

            case 0x21: return s1.as((a, b) -> p2.as(c ->
                       append2(m1, node2(a, b), node2(x, c), m2)));
            case 0x22: return s1.as((a, b) -> p2.as((c, d) ->
                       append2(m1, node3(a, b, x), node2(c, d), m2)));
            case 0x23: return s1.as((a, b) -> p2.as((c, d, e) ->
                       append2(m1, node3(a, b, x), node3(c, d, e), m2)));
            case 0x24: return s1.as((a, b) -> p2.as((c, d, e, f) ->
                       append3(m1, node3(a, b, x), node2(c, d), node2(e, f), m2)));

            case 0x31: return s1.as((a, b, c) -> p2.as(d ->
                       append2(m1, node3(a, b, c), node2(x, d), m2)));
            case 0x32: return s1.as((a, b, c) -> p2.as((d, e) ->
                       append2(m1, node3(a, b, c), node3(x, d, e), m2)));
            case 0x33: return s1.as((a, b, c) -> p2.as((d, e, f) ->
                       append3(m1, node3(a, b, c), node2(x, d), node2(e, f), m2)));
            case 0x34: return s1.as((a, b, c) -> p2.as((d, e, f, g) ->
                       append3(m1, node3(a, b, c), node3(x, d, e), node2(f, g), m2)));

            case 0x41: return s1.as((a, b, c, d) -> p2.as(e ->
                       append2(m1, node3(a, b, c), node3(d, x, e), m2)));
            case 0x42: return s1.as((a, b, c, d) -> p2.as((e, f) ->
                       append3(m1, node3(a, b, c), node2(d, x), node2(e, f), m2)));
            case 0x43: return s1.as((a, b, c, d) -> p2.as((e, f, g) ->
                       append3(m1, node3(a, b, c), node3(d, x, e), node2(f, g), m2)));
            case 0x44: return s1.as((a, b, c, d) -> p2.as((e, f, g, h) ->
                       append3(m1, node3(a, b, c), node3(d, x, e), node3(f, g, h), m2)));

            default: throw new Error();
          }
        }

        static <A> FTree<Node<A>>
        append2(FTree<Node<A>> xs, Node<A> a, Node<A> b, FTree<Node<A>> ys) {
            if (xs.isEmpty()) {
                return ys.cons(b).cons(a);
            } else if (ys.isEmpty()) {
                return xs.snoc(a).snoc(b);
            } else if (xs.isSingle()) {
                return ys.cons(b).cons(a).cons(xs.head());
            } else if (ys.isSingle()) {
                return xs.snoc(a).snoc(b).snoc(ys.head());
            } else {
                Deep<Node<A>> d1 = (Deep<Node<A>>)xs;
                Deep<Node<A>> d2 = (Deep<Node<A>>)ys;
                int s = d1.size() + a.size() + b.size() + d2.size();
                return deep(s, d1.pr, addDigits2(d1.mi, d1.sf, a, b, d2.pr, d2.mi), d2.sf);
            }
        }

        static <A> FTree<Node<Node<A>>>
        addDigits2(FTree<Node<Node<A>>> m1,
                   Digit<Node<A>>       s1,
                   Node<A>              x,
                   Node<A>              y,
                   Digit<Node<A>>       p2,
                   FTree<Node<Node<A>>> m2) {
          switch ((s1.card() << 4) | p2.card()) {
            case 0x11: return s1.as(a -> p2.as(b ->
                       append2(m1, node2(a, x), node2(y, b), m2)));
            case 0x12: return s1.as(a -> p2.as((b, c) ->
                       append2(m1, node3(a, x, y), node2(b, c), m2)));
            case 0x13: return s1.as(a -> p2.as((b, c, d) ->
                       append2(m1, node3(a, x, y), node3(b, c, d), m2)));
            case 0x14: return s1.as(a -> p2.as((b, c, d, e) ->
                       append3(m1, node3(a, x, y), node2(b, c), node2(d, e), m2)));

            case 0x21: return s1.as((a, b) -> p2.as(c ->
                       append2(m1, node3(a, b, x), node2(y, c), m2)));
            case 0x22: return s1.as((a, b) -> p2.as((c, d) ->
                       append2(m1, node3(a, b, x), node3(y, c, d), m2)));
            case 0x23: return s1.as((a, b) -> p2.as((c, d, e) ->
                       append3(m1, node3(a, b, x), node2(y, c), node2(d, e), m2)));
            case 0x24: return s1.as((a, b) -> p2.as((c, d, e, f) ->
                       append3(m1, node3(a, b, x), node3(y, c, d), node2(e, f), m2)));

            case 0x31: return s1.as((a, b, c) -> p2.as(d ->
                       append2(m1, node3(a, b, c), node3(x, y, d), m2)));
            case 0x32: return s1.as((a, b, c) -> p2.as((d, e) ->
                       append3(m1, node3(a, b, c), node2(x, y), node2(d, e), m2)));
            case 0x33: return s1.as((a, b, c) -> p2.as((d, e, f) ->
                       append3(m1, node3(a, b, c), node3(x, y, d), node2(e, f), m2)));
            case 0x34: return s1.as((a, b, c) -> p2.as((d, e, f, g) ->
                       append3(m1, node3(a, b, c), node3(x, y, d), node3(e, f, g), m2)));

            case 0x41: return s1.as((a, b, c, d) -> p2.as(e ->
                       append3(m1, node3(a, b, c), node2(d, x), node2(y, e), m2)));
            case 0x42: return s1.as((a, b, c, d) -> p2.as((e, f) ->
                       append3(m1, node3(a, b, c), node3(d, x, y), node2(e, f), m2)));
            case 0x43: return s1.as((a, b, c, d) -> p2.as((e, f, g) ->
                       append3(m1, node3(a, b, c), node3(d, x, y), node3(e, f, g), m2)));
            case 0x44: return s1.as((a, b, c, d) -> p2.as((e, f, g, h) ->
                       append4(m1, node3(a, b, c), node3(d, x, y), node2(e, f), node2(g, h), m2)));

            default: throw new Error();
          }
        }

        static <A> FTree<Node<A>>
        append3(FTree<Node<A>> xs, Node<A> a, Node<A> b, Node<A> c, FTree<Node<A>> ys) {
            if (xs.isEmpty()) {
                return ys.cons(c).cons(b).cons(a);
            } else if (ys.isEmpty()) {
                return xs.snoc(a).snoc(b).snoc(c);
            } else if (xs.isSingle()) {
                return ys.cons(c).cons(b).cons(a).cons(xs.head());
            } else if (ys.isSingle()) {
                return xs.snoc(a).snoc(b).snoc(c).snoc(ys.head());
            } else {
                Deep<Node<A>> d1 = (Deep<Node<A>>)xs;
                Deep<Node<A>> d2 = (Deep<Node<A>>)ys;
                int s = d1.size() + a.size() + b.size() + c.size() + d2.size();
                return deep(s, d1.pr, addDigits3(d1.mi, d1.sf, a, b, c, d2.pr, d2.mi), d2.sf);
            }
        }

        static <A> FTree<Node<Node<A>>>
        addDigits3(FTree<Node<Node<A>>> m1,
                   Digit<Node<A>>       s1,
                   Node<A>              x,
                   Node<A>              y,
                   Node<A>              z,
                   Digit<Node<A>>       p2,
                   FTree<Node<Node<A>>> m2) {
          switch ((s1.card() << 4) | p2.card()) {
            case 0x11: return s1.as(a -> p2.as(b ->
                       append2(m1, node3(a, x, y), node2(z, b), m2)));
            case 0x12: return s1.as(a -> p2.as((b, c) ->
                       append2(m1, node3(a, x, y), node3(z, b, c), m2)));
            case 0x13: return s1.as(a -> p2.as((b, c, d) ->
                       append3(m1, node3(a, x, y), node2(z, b), node2(c, d), m2)));
            case 0x14: return s1.as(a -> p2.as((b, c, d, e) ->
                       append3(m1, node3(a, x, y), node3(z, b, c), node2(d, e), m2)));

            case 0x21: return s1.as((a, b) -> p2.as(c ->
                       append2(m1, node3(a, b, x), node3(y, z, c), m2)));
            case 0x22: return s1.as((a, b) -> p2.as((c, d) ->
                       append3(m1, node3(a, b, x), node2(y, z), node2(c, d), m2)));
            case 0x23: return s1.as((a, b) -> p2.as((c, d, e) ->
                       append3(m1, node3(a, b, x), node3(y, z, c), node2(d, e), m2)));
            case 0x24: return s1.as((a, b) -> p2.as((c, d, e, f) ->
                       append3(m1, node3(a, b, x), node3(y, z, c), node3(d, e, f), m2)));

            case 0x31: return s1.as((a, b, c) -> p2.as(d ->
                       append3(m1, node3(a, b, c), node2(x, y), node2(z, d), m2)));
            case 0x32: return s1.as((a, b, c) -> p2.as((d, e) ->
                       append3(m1, node3(a, b, c), node3(x, y, z), node2(d, e), m2)));
            case 0x33: return s1.as((a, b, c) -> p2.as((d, e, f) ->
                       append3(m1, node3(a, b, c), node3(x, y, z), node3(d, e, f), m2)));
            case 0x34: return s1.as((a, b, c) -> p2.as((d, e, f, g) ->
                       append4(m1, node3(a, b, c), node3(x, y, z), node2(d, e), node2(f, g), m2)));

            case 0x41: return s1.as((a, b, c, d) -> p2.as(e ->
                       append3(m1, node3(a, b, c), node3(d, x, y), node2(z, e), m2)));
            case 0x42: return s1.as((a, b, c, d) -> p2.as((e, f) ->
                       append3(m1, node3(a, b, c), node3(d, x, y), node3(z, e, f), m2)));
            case 0x43: return s1.as((a, b, c, d) -> p2.as((e, f, g) ->
                       append4(m1, node3(a, b, c), node3(d, x, y), node2(z, e), node2(f, g), m2)));
            case 0x44: return s1.as((a, b, c, d) -> p2.as((e, f, g, h) ->
                       append4(m1, node3(a, b, c), node3(d, x, y), node3(z, e, f), node2(g, h), m2)));

            default: throw new Error();
          }
        }

        static <A> FTree<Node<A>>
        append4(FTree<Node<A>> xs, Node<A> a, Node<A> b, Node<A> c, Node<A> d, FTree<Node<A>> ys) {
            if (xs.isEmpty()) {
                return ys.cons(d).cons(c).cons(b).cons(a);
            } else if (ys.isEmpty()) {
                return xs.snoc(a).snoc(b).snoc(c).snoc(d);
            } else if (xs.isSingle()) {
                return ys.cons(d).cons(c).cons(b).cons(a).cons(xs.head());
            } else if (ys.isSingle()) {
                return xs.snoc(a).snoc(b).snoc(c).snoc(d).snoc(ys.head());
            } else {
                Deep<Node<A>> d1 = (Deep<Node<A>>)xs;
                Deep<Node<A>> d2 = (Deep<Node<A>>)ys;
                int s = d1.size() + a.size() + b.size() + c.size() + d.size() + d2.size();
                return deep(s, d1.pr, addDigits4(d1.mi, d1.sf, a, b, c, d, d2.pr, d2.mi), d2.sf);
            }
        }

        static <A> FTree<Node<Node<A>>>
        addDigits4(FTree<Node<Node<A>>> m1,
                   Digit<Node<A>>       s1,
                   Node<A>              x,
                   Node<A>              y,
                   Node<A>              z,
                   Node<A>              w,
                   Digit<Node<A>>       p2,
                   FTree<Node<Node<A>>> m2) {
          switch ((s1.card() << 4) | p2.card()) {
            case 0x11: return s1.as(a -> p2.as(b ->
                       append2(m1, node3(a, x, y), node3(z, w, b), m2)));
            case 0x12: return s1.as(a -> p2.as((b, c) ->
                       append3(m1, node3(a, x, y), node2(z, w), node2(b, c), m2)));
            case 0x13: return s1.as(a -> p2.as((b, c, d) ->
                       append3(m1, node3(a, x, y), node3(z, w, b), node2(c, d), m2)));
            case 0x14: return s1.as(a -> p2.as((b, c, d, e) ->
                       append3(m1, node3(a, x, y), node3(z, w, b), node3(c, d, e), m2)));

            case 0x21: return s1.as((a, b) -> p2.as(c ->
                       append3(m1, node3(a, b, x), node2(y, z), node2(w, c), m2)));
            case 0x22: return s1.as((a, b) -> p2.as((c, d) ->
                       append3(m1, node3(a, b, x), node3(y, z, w), node2(c, d), m2)));
            case 0x23: return s1.as((a, b) -> p2.as((c, d, e) ->
                       append3(m1, node3(a, b, x), node3(y, z, w), node3(c, d, e), m2)));
            case 0x24: return s1.as((a, b) -> p2.as((c, d, e, f) ->
                       append4(m1, node3(a, b, x), node3(y, z, w), node2(c, d), node2(e, f), m2)));

            case 0x31: return s1.as((a, b, c) -> p2.as(d ->
                       append3(m1, node3(a, b, c), node3(x, y, z), node2(w, d), m2)));
            case 0x32: return s1.as((a, b, c) -> p2.as((d, e) ->
                       append3(m1, node3(a, b, c), node3(x, y, z), node3(w, d, e), m2)));
            case 0x33: return s1.as((a, b, c) -> p2.as((d, e, f) ->
                       append4(m1, node3(a, b, c), node3(x, y, z), node2(w, d), node2(e, f), m2)));
            case 0x34: return s1.as((a, b, c) -> p2.as((d, e, f, g) ->
                       append4(m1, node3(a, b, c), node3(x, y, z), node3(w, d, e), node2(f, g), m2)));

            case 0x41: return s1.as((a, b, c, d) -> p2.as(e ->
                       append3(m1, node3(a, b, c), node3(d, x, y), node3(z, w, e), m2)));
            case 0x42: return s1.as((a, b, c, d) -> p2.as((e, f) ->
                       append4(m1, node3(a, b, c), node3(d, x, y), node2(z, w), node2(e, f), m2)));
            case 0x43: return s1.as((a, b, c, d) -> p2.as((e, f, g) ->
                       append4(m1, node3(a, b, c), node3(d, x, y), node3(z, w, e), node2(f, g), m2)));
            case 0x44: return s1.as((a, b, c, d) -> p2.as((e, f, g, h) ->
                       append4(m1, node3(a, b, c), node3(d, x, y), node3(z, w, e), node3(f, g, h), m2)));

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

        @Override
        public Vector<A> force() {
            pr.force();
            mi.force();
            sf.force();
            return this;
        }

        public String toString() {
            return show(", ", "Vector[", "]");
        }
    }
}
