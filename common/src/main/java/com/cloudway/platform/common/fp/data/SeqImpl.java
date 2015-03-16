/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.data;

import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.StringJoiner;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;
import static java.util.Objects.requireNonNull;

final class SeqImpl {
    private SeqImpl() {}

    private interface Delayed {
        boolean computed();
    }

    @SuppressWarnings("rawtypes")
    private static final Seq NIL = new Seq() {
        @Override
        public boolean isEmpty() {
            return true;
        }

        @Override
        public Object head() {
            throw new NoSuchElementException();
        }

        @Override
        public Seq tail() {
            throw new NoSuchElementException();
        }

        @Override
        public Seq reverse() {
            return this;
        }

        @Override
        public String toString() {
            return "[]";
        }
    };

    private static class Single<T> implements Seq<T> {
        private final T value;

        Single(T value) {
            this.value = value;
        }

        @Override
        public boolean isEmpty() {
            return false;
        }

        @Override
        public T head() {
            return value;
        }

        @Override
        @SuppressWarnings("unchecked")
        public Seq<T> tail() {
            return NIL;
        }

        @Override
        public Seq<T> reverse() {
            return this;
        }

        public String toString() {
            return "[" + value + "]";
        }
    }

    private static class Cons<T> implements Seq<T> {
        private final T head;
        private final Seq<T> tail;

        Cons(T head, Seq<T> tail) {
            this.head = head;
            this.tail = tail;
        }

        @Override
        public boolean isEmpty() {
            return false;
        }

        @Override
        public T head() {
            return head;
        }

        @Override
        public Seq<T> tail() {
            return tail;
        }

        @Override
        public String toString() {
            return SeqImpl.toString(this);
        }
    }

    private static class LazySeq<T> implements Seq<T>, Delayed {
        private final T head;
        private volatile Supplier<Seq<T>> generator;
        private volatile Seq<T> tail;

        LazySeq(T head, Supplier<Seq<T>> generator) {
            this.head = head;
            this.generator = generator;
        }

        @Override
        public boolean isEmpty() {
            return false;
        }

        @Override
        public T head() {
            return head;
        }

        @Override
        public Seq<T> tail() {
            if (tail == null)
                expand();
            return tail;
        }

        private synchronized void expand() {
            if (tail == null) {
                tail = requireNonNull(generator.get());
                generator = null; // no longer used again
            }
        }

        @Override
        public boolean computed() {
            return generator == null;
        }

        @Override
        public String toString() {
            if (generator != null) {
                return "[" + head + ", ?]";
            } else {
                return SeqImpl.toString(this);
            }
        }
    }

    private static class DelaySeq<T> implements Seq<T>, Delayed {
        private final Seq<T> delayed;

        DelaySeq(Seq<T> delayed) {
            this.delayed = delayed;
        }

        Seq<T> force() {
            return delayed.tail();
        }

        @Override
        public boolean computed() {
            return ((Delayed)delayed).computed();
        }

        @Override
        public boolean isEmpty() {
            return force().isEmpty();
        }

        @Override
        public T head() {
            return force().head();
        }

        @Override
        public Seq<T> tail() {
            return force().tail();
        }

        @Override
        public String toString() {
            return SeqImpl.toString(this);
        }
    }

    static class Repeater<T> implements Seq<T> {
        private final T value;

        Repeater(T value) {
            this.value = value;
        }

        @Override
        public boolean isEmpty() {
            return false;
        }

        @Override
        public T head() {
            return value;
        }

        @Override
        public Seq<T> tail() {
            return this;
        }

        @Override
        public Seq<T> reverse() {
            return this;
        }

        @Override
        public String toString() {
            return "[" + value + ", ...]";
        }
    }

    @SuppressWarnings("unchecked")
    static <T> Seq<T> nil() {
        return (Seq<T>)NIL;
    }

    static <T> Seq<T> single(T value) {
        return new Single<>(value);
    }

    static <T> Seq<T> cons(T head, Seq<T> tail) {
        return new Cons<>(head, force(requireNonNull(tail)));
    }

    static <T> Seq<T> cons(T head, Supplier<Seq<T>> generator) {
        return new LazySeq<>(head, requireNonNull(generator));
    }

    static <T> Seq<T> delay(Seq<T> seq) {
        if ((seq instanceof Delayed) && !((Delayed)seq).computed()) {
            return new DelaySeq<>(seq);
        } else {
            return seq.tail();
        }
    }

    static <T> Seq<T> force(Seq<T> seq) {
        return (seq instanceof DelaySeq) ? ((DelaySeq<T>)seq).force() : seq;
    }

    @SuppressWarnings("unchecked")
    static <T> Seq<T> concat(Seq<? extends T> a, Seq<? extends T> b) {
        if (a.isEmpty())
            return (Seq<T>)b;
        if (b.isEmpty())
            return (Seq<T>)a;
        return cons(a.head(), () -> concat(a.tail(), b));
    }

    static <T> Seq<T> concat(Seq<? extends T> a, Supplier<? extends Seq<T>> b) {
        if (a.isEmpty())
            return b.get();
        return cons(a.head(), () -> concat(a.tail(), b));
    }

    static <T, U, R> Seq<R>
    zip(Seq<? extends T> a, Seq<? extends U> b, BiFunction<? super T, ? super U, ? extends R> zipper) {
        if (a.isEmpty() || b.isEmpty()) {
            return nil();
        } else {
            return cons(zipper.apply(a.head(), b.head()), () -> zip(a.tail(), b.tail(), zipper));
        }
    }

    static <T> Seq<T> sort(Seq<T> seq, Comparator<? super T> cmp) {
        return mergeAll(sequences(seq, cmp), cmp);
    }

    private static <T> Seq<Seq<T>> sequences(Seq<T> xs, Comparator<? super T> cmp) {
        if (!xs.isEmpty() && !xs.tail().isEmpty()) {
            T a = xs.head(); xs = xs.tail();
            T b = xs.head(); xs = xs.tail();
            return cmp.compare(a, b) > 0
                ? descending(b, cons(a, nil()), xs, cmp)
                : ascending(b, fcons(a, Function.identity()), xs, cmp);
        } else {
            return Seq.of(xs);
        }
    }

    private static <T> Seq<Seq<T>> descending(T a, Seq<T> as, Seq<T> bs, Comparator<? super T> cmp) {
        while (!bs.isEmpty() && cmp.compare(a, bs.head()) > 0) {
            as = cons(a, as); a = bs.head(); bs = bs.tail();
        }
        Seq<T> bs_ = bs;
        return cons(cons(a, as), () -> sequences(bs_, cmp));
    }

    private static <T> Seq<Seq<T>> ascending(T a, Function<Seq<T>, Seq<T>> as, Seq<T> bs, Comparator<? super T> cmp) {
        while (!bs.isEmpty() && cmp.compare(a, bs.head()) <= 0) {
            as = fcons(a, as); a = bs.head(); bs = bs.tail();
        }
        Seq<T> bs_ = bs;
        return cons(as.apply(Seq.of(a)), () -> sequences(bs_, cmp));
    }

    private static <T> Function<Seq<T>, Seq<T>> fcons(T a, Function<Seq<T>, Seq<T>> as) {
        return ys -> as.apply(cons(a, ys));
    }

    private static <T> Seq<T> mergeAll(Seq<Seq<T>> xs, Comparator<? super T> cmp) {
        while (!xs.tail().isEmpty())
            xs = mergePairs(xs, cmp);
        return xs.head();
    }

    private static <T> Seq<Seq<T>> mergePairs(Seq<Seq<T>> xs, Comparator<? super T> cmp) {
        if (!xs.isEmpty() && !xs.tail().isEmpty()) {
            return cons(merge(xs.head(), xs.tail().head(), cmp), () -> mergePairs(xs.tail().tail(), cmp));
        } else {
            return xs;
        }
    }

    private static <T> Seq<T> merge(Seq<T> as, Seq<T> bs, Comparator<? super T> cmp) {
        if (as.isEmpty())
            return bs;
        if (bs.isEmpty())
            return as;
        return cmp.compare(as.head(), bs.head()) > 0
            ? cons(bs.head(), () -> merge(as, bs.tail(), cmp))
            : cons(as.head(), () -> merge(as.tail(), bs, cmp));
    }

    static <T> Seq<T> distinct(Seq<T> xs) {
        return distinct(xs, new HashMap<>());
    }

    private static <T> Seq<T> distinct(Seq<T> xs, Map<T, Unit> ls) {
        while (!xs.isEmpty()) {
            T x = xs.head();
            Seq<T> t = xs;
            if (ls.putIfAbsent(x, Unit.U) == null) {
                return cons(x, () -> distinct(t.tail(), ls));
            } else {
                xs = xs.tail();
            }
        }
        return xs;
    }

    static <T> String toString(Seq<T> xs) {
        StringJoiner sj = new StringJoiner(", ", "[", "]");
        while (true) {
            if ((xs instanceof Delayed) && !((Delayed)xs).computed()) {
                sj.add("?");
                break;
            } else if (xs.isEmpty()) {
                break;
            } else if (xs instanceof Repeater) {
                sj.add(String.valueOf(xs.head()));
                sj.add("...");
                break;
            } else {
                sj.add(String.valueOf(xs.head()));
                xs = xs.tail();
            }
        }
        return sj.toString();
    }
}
