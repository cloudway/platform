/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util;

import java.util.NoSuchElementException;
import java.util.function.BiFunction;
import java.util.function.IntBinaryOperator;
import java.util.function.IntConsumer;
import java.util.function.Supplier;
import static java.util.Objects.requireNonNull;

final class IntSeqImpl {
    private IntSeqImpl() {}

    private static final IntSeq NIL = new IntSeq() {
        @Override
        public boolean isEmpty() {
            return true;
        }

        @Override
        public int head() {
            throw new NoSuchElementException();
        }

        @Override
        public IntSeq tail() {
            throw new NoSuchElementException();
        }

        @Override
        public String toString() {
            return "[]";
        }
    };

    private static class Cons implements IntSeq {
        private final int head;
        private final IntSeq tail;

        Cons(int head, IntSeq tail) {
            this.head = head;
            this.tail = tail;
        }

        @Override
        public boolean isEmpty() {
            return false;
        }

        @Override
        public int head() {
            return head;
        }

        @Override
        public IntSeq tail() {
            return tail;
        }

        @Override
        public String toString() {
            return show(100);
        }
    }

    private static class LazyIntSeq implements IntSeq {
        private final int head;
        private volatile Supplier<IntSeq> generator;
        private volatile IntSeq tail;

        public LazyIntSeq(int head, Supplier<IntSeq> generator) {
            this.head = head;
            this.generator = generator;
        }

        @Override
        public boolean isEmpty() {
            return false;
        }

        @Override
        public int head() {
            return head;
        }

        @Override
        public IntSeq tail() {
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

    private static class DelayIntSeq implements IntSeq {
        private final IntSeq delegate;

        public DelayIntSeq(IntSeq delegate) {
            this.delegate = delegate;
        }

        private IntSeq force() {
            return delegate.tail();
        }

        @Override
        public boolean isEmpty() {
            return force().isEmpty();
        }

        @Override
        public int head() {
            return force().head();
        }

        @Override
        public IntSeq tail() {
            return force().tail();
        }

        @Override
        public String toString() {
            return show(100);
        }
    }

    private static abstract class Range implements IntSeq {
        protected final int start, end, step;
        private volatile IntSeq tail;

        protected Range(int start, int end, int step) {
            this.start = start;
            this.end = end;
            this.step = step;
        }

        @Override
        public boolean isEmpty() {
            return false;
        }

        @Override
        public int head() {
            return start;
        }

        @Override
        public IntSeq tail() {
            if (tail == null)
                expand();
            return tail;
        }

        private synchronized void expand() {
            if (tail == null) {
                tail = makeRange(start + step, end, step);
            }
        }

        @Override
        public long count() {
            return (end - start + step) / step;
        }

        @Override
        public String toString() {
            String res = "[" + start + ".." + end + "]";
            if (step != 1)
                res += "(" + step + ")";
            return res;
        }
    }

    private static class StepUp extends Range {
        public StepUp(int start, int end, int step) {
            super(start, end, step);
        }

        @Override
        public IntSeq reverse() {
            return new StepDown(end, start, -step);
        }

        @Override
        public void forEach(IntConsumer action) {
            for (int i = start; i <= end; i += step) {
                action.accept(i);
            }
        }

        @Override
        public int foldLeft(int identity, IntBinaryOperator op) {
            int result = identity;
            for (int i = start; i <= end; i += step) {
                result = op.applyAsInt(result, i);
            }
            return result;
        }

        @Override
        public IntSeq take(int n) {
            return makeRange(start, Math.min(start + n * step, end), step);
        }

        @Override
        public IntSeq drop(int n) {
            return makeRange(start + n * step, end, step);
        }
    }

    private static class StepDown extends Range {
        public StepDown(int start, int end, int step) {
            super(start, end, step);
        }

        @Override
        public IntSeq reverse() {
            return new StepUp(end, start, -step);
        }

        @Override
        public void forEach(IntConsumer action) {
            for (int i = start; i >= end; i += step) {
                action.accept(i);
            }
        }

        @Override
        public int foldLeft(int identity, IntBinaryOperator op) {
            int result = identity;
            for (int i = start; i >= end; i += step) {
                result = op.applyAsInt(result, i);
            }
            return result;
        }

        @Override
        public IntSeq take(int n) {
            return makeRange(start, Math.max(start + n * step, end), step);
        }

        @Override
        public IntSeq drop(int n) {
            return makeRange(start + n * step, end, step);
        }
    }

    @SuppressWarnings("unchecked")
    static IntSeq nil() {
        return NIL;
    }

    static IntSeq cons(int head, IntSeq tail) {
        return new Cons(head, requireNonNull(tail));
    }

    static IntSeq cons(int head, Supplier<IntSeq> generator) {
        return new LazyIntSeq(head, requireNonNull(generator));
    }

    static IntSeq delay(IntSeq seq) {
        return new DelayIntSeq(requireNonNull(seq));
    }

    static IntSeq makeRange(int start, int end, int step) {
        if (step > 0) {
            if (start > end) {
                return nil();
            } else if (start + step > end) {
                return IntSeq.of(start);
            } else {
                return new StepUp(start, end, step);
            }
        } else if (step < 0) {
            if (start < end) {
                return nil();
            } else if (start + step < end) {
                return IntSeq.of(start);
            } else {
                return new StepDown(start, end, step);
            }
        } else {
            throw new IllegalArgumentException("0 range step is invalid");
        }
    }

    static IntSeq concat(IntSeq a, IntSeq b) {
        if (a.isEmpty())
            return b;
        if (b.isEmpty())
            return a;
        return cons(a.head(), () -> concat(a.tail(), b));
    }

    static IntSeq concat(IntSeq a, Supplier<IntSeq> b) {
        if (a.isEmpty())
            return b.get();
        return cons(a.head(), () -> concat(a.tail(), b));
    }

    static IntSeq cycle(Holder<IntSeq> h, IntSeq n) {
        return cons(n.head(), () -> {
            IntSeq t = n.tail();
            return t.isEmpty() ? h.get() : cycle(h, t);
        });
    }

    static IntSeq zip(IntSeq a, IntSeq b, IntBinaryOperator zipper) {
        if (a.isEmpty() || b.isEmpty()) {
            return nil();
        } else {
            return cons(zipper.applyAsInt(a.head(), b.head()), () -> zip(a.tail(), b.tail(), zipper));
        }
    }

    static <R> Seq<R> zipToObj(IntSeq a, IntSeq b, BiFunction<Integer, Integer, ? extends R> zipper) {
        if (a.isEmpty() || b.isEmpty()) {
            return Seq.nil();
        } else {
            return Seq.cons(zipper.apply(a.head(), b.head()), () -> zipToObj(a.tail(), b.tail(), zipper));
        }
    }
}
