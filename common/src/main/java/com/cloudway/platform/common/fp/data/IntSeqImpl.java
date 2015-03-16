/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.data;

import java.util.NoSuchElementException;
import java.util.StringJoiner;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.IntBinaryOperator;
import java.util.function.IntConsumer;
import java.util.function.Supplier;
import static java.util.Objects.requireNonNull;

final class IntSeqImpl {
    private IntSeqImpl() {}

    private interface Delayed {
        boolean computed();
    }

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
        public IntSeq reverse() {
            return this;
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
            return IntSeqImpl.toString(this);
        }
    }

    private static class LazyIntSeq implements IntSeq, Delayed {
        private final int head;
        private volatile Supplier<IntSeq> generator;
        private volatile IntSeq tail;

        LazyIntSeq(int head, Supplier<IntSeq> generator) {
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
        public boolean computed() {
            return generator == null;
        }

        @Override
        public String toString() {
            if (generator != null) {
                return "[" + head + ", ?]";
            } else {
                return IntSeqImpl.toString(this);
            }
        }
    }

    private static class DelayIntSeq implements IntSeq, Delayed {
        private final IntSeq delayed;

        DelayIntSeq(IntSeq delayed) {
            this.delayed = delayed;
        }

        IntSeq force() {
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
        public int head() {
            return force().head();
        }

        @Override
        public IntSeq tail() {
            return force().tail();
        }

        @Override
        public String toString() {
            return IntSeqImpl.toString(this);
        }
    }

    static class Repeater implements IntSeq {
        private final int value;

        Repeater(int value) {
            this.value = value;
        }

        @Override
        public boolean isEmpty() {
            return false;
        }

        @Override
        public int head() {
            return value;
        }

        @Override
        public IntSeq tail() {
            return this;
        }

        @Override
        public IntSeq reverse() {
            return this;
        }

        @Override
        public String toString() {
            return "[" + value + ", ...]";
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
            return "[" + showRange() + "]";
        }

        String showRange() {
            String res = start + ".." + end;
            if (step != -1)
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
            if (n <= 0) {
                return IntSeq.nil();
            } else {
                return makeRange(start, Math.min(start + (n - 1) * step, end), step);
            }
        }

        @Override
        public IntSeq drop(int n) {
            if (n <= 0) {
                return this;
            } else {
                return makeRange(start + n * step, end, step);
            }
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
            if (n <= 0) {
                return IntSeq.nil();
            } else {
                return makeRange(start, Math.max(start + (n-1) * step, end), step);
            }
        }

        @Override
        public IntSeq drop(int n) {
            if (n <= 0) {
                return this;
            } else {
                return makeRange(start + n * step, end, step);
            }
        }
    }

    @SuppressWarnings("unchecked")
    static IntSeq nil() {
        return NIL;
    }

    static IntSeq cons(int head, IntSeq tail) {
        return new Cons(head, force(requireNonNull(tail)));
    }

    static IntSeq cons(int head, Supplier<IntSeq> generator) {
        return new LazyIntSeq(head, requireNonNull(generator));
    }

    static IntSeq delay(IntSeq seq) {
        if ((seq instanceof Delayed) && !((Delayed)seq).computed()) {
            return new DelayIntSeq(seq);
        } else {
            return seq.tail();
        }
    }

    static IntSeq force(IntSeq seq) {
        return (seq instanceof DelayIntSeq) ? ((DelayIntSeq)seq).force() : seq;
    }

    static IntSeq makeRange(int start, int end, int step) {
        if (step > 0) {
            if (start > end) {
                return nil();
            } else if (start + step > end) {
                return IntSeq.of(start);
            } else {
                end = start + (end - start) / step * step;
                return new StepUp(start, end, step);
            }
        } else if (step < 0) {
            if (start < end) {
                return nil();
            } else if (start + step < end) {
                return IntSeq.of(start);
            } else {
                end = start - (start - end) / step * step;
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

    static IntSeq sort(IntSeq seq) {
        return mergeAll(sequences(seq));
    }

    private static Seq<IntSeq> sequences(IntSeq xs) {
        if (!xs.isEmpty() && !xs.tail().isEmpty()) {
            int a = xs.head(); xs = xs.tail();
            int b = xs.head(); xs = xs.tail();
            return a > b ? descending(b, cons(a, nil()), xs)
                         : ascending(b, fcons(a, Function.identity()), xs);
        } else {
            return Seq.of(xs);
        }
    }

    private static Seq<IntSeq> descending(int a, IntSeq as, IntSeq bs) {
        while (!bs.isEmpty() && a > bs.head()) {
            as = cons(a, as); a = bs.head(); bs = bs.tail();
        }
        IntSeq bs_ = bs;
        return Seq.cons(cons(a, as), () -> sequences(bs_));
    }

    private static Seq<IntSeq> ascending(int a, Function<IntSeq, IntSeq> as, IntSeq bs) {
        while (!bs.isEmpty() && a <= bs.head()) {
            as = fcons(a, as); a = bs.head(); bs = bs.tail();
        }
        IntSeq bs_ = bs;
        return Seq.cons(as.apply(IntSeq.of(a)), () -> sequences(bs_));
    }

    private static Function<IntSeq, IntSeq> fcons(int a, Function<IntSeq, IntSeq> as) {
        return ys -> as.apply(cons(a, ys));
    }

    private static IntSeq mergeAll(Seq<IntSeq> xs) {
        while (!xs.tail().isEmpty())
            xs = mergePairs(xs);
        return xs.head();
    }

    private static Seq<IntSeq> mergePairs(Seq<IntSeq> xs) {
        if (!xs.isEmpty() && !xs.tail().isEmpty()) {
            return Seq.cons(merge(xs.head(), xs.tail().head()), () -> mergePairs(xs.tail().tail()));
        } else {
            return xs;
        }
    }

    private static IntSeq merge(IntSeq as, IntSeq bs) {
        if (as.isEmpty())
            return bs;
        if (bs.isEmpty())
            return as;
        return as.head() > bs.head()
               ? cons(bs.head(), () -> merge(as, bs.tail()))
               : cons(as.head(), () -> merge(as.tail(), bs));
    }

    static String toString(IntSeq xs) {
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
            } else if (xs instanceof Range) {
                sj.add(((Range)xs).showRange());
                break;
            } else {
                sj.add(String.valueOf(xs.head()));
                xs = xs.tail();
            }
        }
        return sj.toString();
    }
}
