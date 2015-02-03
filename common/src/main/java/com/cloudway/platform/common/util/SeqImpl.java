/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util;

import java.util.NoSuchElementException;
import java.util.function.BiFunction;
import java.util.function.Supplier;
import static java.util.Objects.requireNonNull;

final class SeqImpl {
    private SeqImpl() {}

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
        public String toString() {
            return "[]";
        }
    };

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
            return show(100);
        }
    }

    private static class LazySeq<T> implements Seq<T> {
        private final T head;
        private volatile Supplier<Seq<T>> generator;
        private volatile Seq<T> tail;

        public LazySeq(T head, Supplier<Seq<T>> generator) {
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
        public String toString() {
            return show(100);
        }
    }

    private static class DelaySeq<T> implements Seq<T> {
        private final Seq<T> delegate;

        public DelaySeq(Seq<T> delegate) {
            this.delegate = delegate;
        }

        private Seq<T> force() {
            return delegate.tail();
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
            return show(100);
        }
    }

    @SuppressWarnings("unchecked")
    static <T> Seq<T> nil() {
        return (Seq<T>)NIL;
    }

    static <T> Seq<T> cons(T head, Seq<T> tail) {
        return new Cons<>(head, requireNonNull(tail));
    }

    static <T> Seq<T> cons(T head, Supplier<Seq<T>> generator) {
        return new LazySeq<>(head, requireNonNull(generator));
    }

    static <T> Seq<T> delay(Seq<T> seq) {
        return new DelaySeq<>(requireNonNull(seq));
    }

    @SuppressWarnings("unchecked")
    static <T> Seq<T> concat(Seq<? extends T> a, Seq<? extends T> b) {
        if (a.isEmpty())
            return (Seq<T>)b;
        if (b.isEmpty())
            return (Seq<T>)a;
        return cons(a.head(), () -> concat(a.tail(), b));
    }

    @SuppressWarnings("unchecked")
    static <T> Seq<T> concat(Seq<? extends T> a, Supplier<? extends Seq<? extends T>> b) {
        if (a.isEmpty())
            return (Seq<T>)b.get();
        return cons(a.head(), () -> concat(a.tail(), b));
    }

    static <T> Seq<T> cycle(Seq<T>[] h, Seq<T> n) {
        Seq<T> h0 = cons(n.head(), () -> {
            Seq<T> t = n.tail();
            return t.isEmpty() ? h[0] : cycle(h, t);
        });
        if (h[0] == null) h[0] = h0;
        return h0;
    }

    static <T, U, R> Seq<R>
    zip(Seq<? extends T> a, Seq<? extends U> b, BiFunction<? super T, ? super U, ? extends R> zipper) {
        if (a.isEmpty() || b.isEmpty()) {
            return nil();
        } else {
            return cons(zipper.apply(a.head(), b.head()), () -> zip(a.tail(), b.tail(), zipper));
        }
    }
}
