/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp;

import java.util.Arrays;
import java.util.NoSuchElementException;
import java.util.OptionalInt;
import java.util.Random;
import java.util.function.Consumer;
import java.util.function.IntBinaryOperator;

import org.junit.Test;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import com.cloudway.fp.data.IntSeq;
import static com.cloudway.fp.control.Conditionals.with;
import static com.cloudway.fp.data.IntSeq.IntCons;

public class IntSequenceTest {
    @Test
    public void nilTest() {
        assertTrue(IntSeq.nil().isEmpty());
        noSuchElement(IntSeq::head);
        noSuchElement(IntSeq::tail);
        assertFalse(IntSeq.nil().peek().isPresent());
    }

    static void noSuchElement(Consumer<IntSeq> consumer) {
        try {
            consumer.accept(IntSeq.nil());
            fail("NoSuchElementException was not thrown");
        } catch (NoSuchElementException ex) {
            // ok
        }
    }

    @Test
    public void consTest() {
        IntSeq seq = IntSeq.cons(42, IntSeq.nil());
        assertFalse(seq.isEmpty());
        assertTrue(seq.tail().isEmpty());
        assertEquals(42, seq.head());
        assertEquals(42, seq.peek().getAsInt());

        assertFiniteSeqEquals(IntSeq.cons(1, IntSeq.cons(2, IntSeq.cons(3, IntSeq.nil()))), 1, 2, 3);
        assertFiniteSeqEquals(IntSeq.cons(1, () -> IntSeq.cons(2, () -> IntSeq.cons(3, IntSeq.nil()))), 1, 2, 3);
    }

    static class ForceLazySeqException extends RuntimeException {
        private static final long serialVersionUID = -1632483700428668563L;
    }

    @Test
    public void lazySeqWillNotForceIfTailNotUsed() {
        IntSeq seq = IntSeq.cons(42, () -> { throw new ForceLazySeqException(); });
        assertFalse(seq.isEmpty());
        assertEquals(42, seq.head());
        assertEquals(42, seq.peek().getAsInt());
        assertEquals(21, seq.map(x -> x / 2).head());
        assertFiniteSeqEquals(seq.take(1), 42);
    }

    @Test(expected = ForceLazySeqException.class)
    public void lazySeqWillForceWhenTailUsed() {
        IntSeq seq = IntSeq.cons(42, () -> { throw new ForceLazySeqException(); });
        seq.tail().isEmpty();
    }

    @Test
    public void patternMatchingOnLazySeqWillNotForceIfTailNotUsed() {
        IntSeq seq = IntSeq.cons(42, () -> { throw new ForceLazySeqException(); });
        with(seq).when(IntCons((x, xs) -> {
            assertEquals(42, (int)x);
            return null;
        }));
    }

    @Test(expected = ForceLazySeqException.class)
    public void patternMatchingOnLazySeqWillForceWhenTailUsed() {
        IntSeq seq = IntSeq.cons(42, () -> { throw new ForceLazySeqException(); });
        with(seq).when(IntCons((x, xs) -> {
            xs.isEmpty();
            return null;
        }));
    }

    @Test
    public void iterateTest() {
        IntSeq seq = IntSeq.iterate(1, x -> x + 1);
        assertSeqEquals(seq, 1, 2, 3, 4, 5);
        assertSeqEquals(seq.map(x -> x * 10), 10, 20, 30, 40, 50);
        assertSeqEquals(seq.filter(x -> x % 2 == 0), 2, 4, 6, 8, 10);
    }

    @Test
    public void repeatTest() {
        IntSeq seq = IntSeq.repeat(5);
        assertSeqEquals(seq, 5, 5, 5, 5, 5);
        assertSeqEquals(seq.reverse(), 5, 5, 5, 5, 5);
        assertSeqEquals(seq.map(x -> x * 2), 10, 10, 10, 10, 10);
    }

    @Test
    public void replicateTest() {
        assertFiniteSeqEquals(IntSeq.replicate(-1, 5));
        assertFiniteSeqEquals(IntSeq.replicate(0, 5));
        assertFiniteSeqEquals(IntSeq.replicate(1, 5), 5);
        assertFiniteSeqEquals(IntSeq.replicate(2, 5), 5, 5);
        assertFiniteSeqEquals(IntSeq.replicate(3, 5), 5, 5, 5);
    }

    @Test
    public void cycleTest() {
        assertTrue(IntSeq.nil().cycle().isEmpty());

        IntSeq seq = IntSeq.of(1, 2, 3).cycle();
        assertSeqEquals(seq, 1, 2, 3, 1, 2, 3, 1);
        assertSeqEquals(seq.map(x -> x * 2), 2, 4, 6, 2, 4, 6, 2);
        assertSeqEquals(seq.filter(x -> x % 2 != 0), 1, 3, 1, 3, 1, 3);
    }

    @Test
    public void reverseTest() {
        assertTrue(IntSeq.nil().reverse().isEmpty());
        assertFiniteSeqEquals(IntSeq.of(1).reverse(), 1);
        assertFiniteSeqEquals(IntSeq.of(1, 2).reverse(), 2, 1);
        assertFiniteSeqEquals(IntSeq.of(1, 2, 3).reverse(), 3, 2, 1);
    }

    @Test
    public void appendTest() {
        assertTrue(IntSeq.nil().append(IntSeq.nil()).isEmpty());
        assertFiniteSeqEquals(IntSeq.nil().append(IntSeq.of(1, 2, 3)), 1, 2, 3);
        assertFiniteSeqEquals(IntSeq.of(1, 2, 3).append(IntSeq.nil()), 1, 2, 3);
        assertFiniteSeqEquals(IntSeq.of(1, 2, 3).append(IntSeq.of(4, 5, 6)), 1, 2, 3, 4, 5, 6);
    }

    @Test
    public void countTest() {
        IntSeq xs = IntSeq.nil();
        for (int i = 0; i < 10; i++) {
            assertEquals(i, xs.count());
            xs = IntSeq.cons(i, xs);
        }
    }

    @Test
    public void findTest() {
        assertFalse(IntSeq.nil().find(x -> true).isPresent());
        assertFalse(IntSeq.nil().find(x -> false).isPresent());
        assertFalse(IntSeq.nil().find(x -> { throw new IllegalStateException(); }).isPresent());

        IntSeq seq = IntSeq.of(1, 2, 3, 4, 5, 6);
        assertEquals(3, seq.find(x -> x % 3 == 0).getAsInt());
        assertFalse(seq.find(x -> x > 10).isPresent());
    }

    @Test
    public void foldTest() {
        assertEquals(6,  IntSeq.of(1, 2, 3).foldLeft(0, (acc, x) -> acc + x));
        assertEquals(6,  IntSeq.of(1, 2, 3).foldRight(0, (x, acc) -> x + acc));
        assertEquals(-6, IntSeq.of(1, 2, 3).foldLeft(0, (acc, x) -> acc - x));
        assertEquals(2,  IntSeq.of(1, 2, 3).foldRight(0, (x, acc) -> x - acc));

        assertEquals(0,  IntSeq.nil().foldLeft(0, (acc, x) -> acc + x));
        assertEquals(0,  IntSeq.nil().foldRight(0, (x, acc) -> x + acc));

        assertEquals(1,  IntSeq.of(1).foldLeft(0, (acc, x) -> acc + x));
        assertEquals(1,  IntSeq.of(1).foldRight(0, (x, acc) -> x + acc));
        assertEquals(-1, IntSeq.of(1).foldLeft(0, (acc, x) -> acc - x));
        assertEquals(1,  IntSeq.of(1).foldRight(0, (x, acc) -> x - acc));

        assertEquals(OptionalInt.of(6),  IntSeq.of(1, 2, 3).foldLeft((acc, x) -> acc + x));
        assertEquals(OptionalInt.of(6),  IntSeq.of(1, 2, 3).foldRight((x, acc) -> x + acc));
        assertEquals(OptionalInt.of(-4), IntSeq.of(1, 2, 3).foldLeft((acc, x) -> acc - x));
        assertEquals(OptionalInt.of(2),  IntSeq.of(1, 2, 3).foldRight((x, acc) -> x - acc));

        assertEquals(OptionalInt.empty(), IntSeq.nil().foldLeft((acc, x) -> acc + x));
        assertEquals(OptionalInt.empty(), IntSeq.nil().foldRight((x, acc) -> x + acc));

        assertEquals(OptionalInt.of(1),  IntSeq.of(1).foldLeft((acc, x) -> acc + x));
        assertEquals(OptionalInt.of(1),  IntSeq.of(1).foldRight((x, acc) -> x + acc));
        assertEquals(OptionalInt.of(1),  IntSeq.of(1).foldLeft((acc, x) -> acc - x));
        assertEquals(OptionalInt.of(1),  IntSeq.of(1).foldRight((x, acc) -> x - acc));

        assertEquals(OptionalInt.of(3),  IntSeq.of(1, 2).foldLeft((acc, x) -> acc + x));
        assertEquals(OptionalInt.of(3),  IntSeq.of(1, 2).foldRight((x, acc) -> x + acc));
        assertEquals(OptionalInt.of(-1), IntSeq.of(1, 2).foldLeft((acc, x) -> acc - x));
        assertEquals(OptionalInt.of(-1), IntSeq.of(1, 2).foldRight((x, acc) -> x - acc));
    }

    @Test
    public void sortTest() {
        Random rnd = new Random();
        int[] data = new int[100];
        Arrays.setAll(data, i -> rnd.nextInt(200));
        int[] sorted = Arrays.copyOf(data, data.length);
        Arrays.sort(sorted);
        assertFiniteSeqEquals(IntSeq.of(data).sorted(), sorted);

        assertFiniteSeqEquals(IntSeq.nil().sorted());
        assertFiniteSeqEquals(IntSeq.of(42).sorted(), 42);
        assertFiniteSeqEquals(IntSeq.of(1,2).sorted(), 1, 2);
        assertFiniteSeqEquals(IntSeq.of(2,1).sorted(), 1, 2);
        assertFiniteSeqEquals(IntSeq.of(1,1).sorted(), 1, 1);
    }

    @Test
    public void takeTest() {
        assertTrue(IntSeq.nil().take(0).isEmpty());
        assertTrue(IntSeq.nil().take(-1).isEmpty());
        assertTrue(IntSeq.nil().take(1).isEmpty());

        IntSeq xs = IntSeq.of(1, 2, 3);
        assertTrue(xs.take(0).isEmpty());
        assertTrue(xs.take(-1).isEmpty());
        assertFiniteSeqEquals(xs.take(1), 1);
        assertFiniteSeqEquals(xs.take(2), 1, 2);
        assertFiniteSeqEquals(xs.take(3), 1, 2, 3);
        assertFiniteSeqEquals(xs.take(4), 1, 2, 3);
    }

    @Test(timeout = 1000)
    public void takeInfiniteSeqTest() {
        IntSeq odd = IntSeq.of(1, 2, 3).append(IntSeq.repeat(4)).filter(x -> x % 2 != 0).take(2);
        assertFiniteSeqEquals(odd, 1, 3);

        IntSeq even = IntSeq.of(1, 2, 3).append(IntSeq.repeat(4)).filter(x -> x % 2 == 0).take(2);
        assertFiniteSeqEquals(even, 2, 4);
    }

    @Test
    public void dropTest() {
        assertTrue(IntSeq.nil().drop(0).isEmpty());
        assertTrue(IntSeq.nil().drop(-1).isEmpty());
        assertTrue(IntSeq.nil().drop(1).isEmpty());

        IntSeq xs = IntSeq.of(1, 2, 3);
        assertFiniteSeqEquals(xs.drop(0), 1, 2, 3);
        assertFiniteSeqEquals(xs.drop(-1), 1, 2, 3);
        assertFiniteSeqEquals(xs.drop(1), 2, 3);
        assertFiniteSeqEquals(xs.drop(2), 3);
        assertFiniteSeqEquals(xs.drop(3));
        assertFiniteSeqEquals(xs.drop(4));
    }

    @Test
    public void takeWhileTest() {
        assertTrue(IntSeq.nil().takeWhile(x -> true).isEmpty());
        assertTrue(IntSeq.nil().takeWhile(x -> false).isEmpty());
        assertTrue(IntSeq.nil().takeWhile(x -> { throw new IllegalStateException(); }).isEmpty());

        IntSeq xs = IntSeq.of(1, 2, 3, 4, 5);
        assertFiniteSeqEquals(xs.takeWhile(x -> x < 3), 1, 2);
        assertFiniteSeqEquals(xs.takeWhile(x -> x > 3));
    }

    @Test
    public void takeUntilTest() {
        assertTrue(IntSeq.nil().takeUntil(x -> true).isEmpty());
        assertTrue(IntSeq.nil().takeUntil(x -> false).isEmpty());
        assertTrue(IntSeq.nil().takeUntil(x -> { throw new IllegalStateException(); }).isEmpty());

        IntSeq xs = IntSeq.of(1, 2, 3, 4, 5);
        assertFiniteSeqEquals(xs.takeUntil(x -> x < 3));
        assertFiniteSeqEquals(xs.takeUntil(x -> x > 3), 1, 2, 3);
    }

    @Test
    public void dropWhileTest() {
        assertTrue(IntSeq.nil().dropWhile(x -> true).isEmpty());
        assertTrue(IntSeq.nil().dropWhile(x -> false).isEmpty());
        assertTrue(IntSeq.nil().dropWhile(x -> { throw new IllegalStateException(); }).isEmpty());

        IntSeq xs = IntSeq.of(1, 2, 3, 4, 5);
        assertFiniteSeqEquals(xs.dropWhile(x -> x < 3), 3, 4, 5);
        assertFiniteSeqEquals(xs.dropWhile(x -> x > 3), 1, 2, 3, 4, 5);
    }

    @Test
    public void dropUntilTest() {
        assertTrue(IntSeq.nil().dropUntil(x -> true).isEmpty());
        assertTrue(IntSeq.nil().dropUntil(x -> false).isEmpty());
        assertTrue(IntSeq.nil().dropUntil(x -> { throw new IllegalStateException(); }).isEmpty());

        IntSeq xs = IntSeq.of(1, 2, 3, 4, 5);
        assertFiniteSeqEquals(xs.dropUntil(x -> x < 3), 1, 2, 3, 4, 5);
        assertFiniteSeqEquals(xs.dropUntil(x -> x > 3), 4, 5);
    }

    @Test
    public void zipWithTest() {
        assertTrue(IntSeq.nil().zip(IntSeq.nil(), Integer::sum).isEmpty());
        assertTrue(IntSeq.nil().zip(IntSeq.of(1, 2, 3), Integer::sum).isEmpty());
        assertTrue(IntSeq.of(1,2, 3).zip(IntSeq.nil(), Integer::sum).isEmpty());

        assertFiniteSeqEquals(IntSeq.of(1, 2, 3).zip(IntSeq.of(4, 5, 6), Integer::sum), 5, 7, 9);
        assertFiniteSeqEquals(IntSeq.of(1, 2).zip(IntSeq.of(4, 5, 6), Integer::sum), 5, 7);
        assertFiniteSeqEquals(IntSeq.of(1, 2, 3).zip(IntSeq.of(4, 5), Integer::sum), 5, 7);
    }

    @Test
    public void anyMatchTest() {
        assertFalse(IntSeq.nil().anyMatch(i -> true));
        assertFalse(IntSeq.nil().anyMatch(i -> false));
        assertFalse(IntSeq.nil().anyMatch(i -> { throw new IllegalStateException(); }));

        IntSeq xs = IntSeq.of(1, 2);
        assertTrue(xs.anyMatch(i -> i > 0));
        assertFalse(xs.anyMatch(i -> i < 0));
        assertFalse(xs.anyMatch(i -> i == 0));
        assertTrue(xs.anyMatch(i -> i == 1));
        assertTrue(xs.anyMatch(i -> i == 2));
    }

    @Test
    public void allMatchTest() {
        assertTrue(IntSeq.nil().allMatch(i -> true));
        assertTrue(IntSeq.nil().allMatch(i -> false));
        assertTrue(IntSeq.nil().allMatch(i -> { throw new IllegalStateException(); }));

        IntSeq xs = IntSeq.of(1, 2);
        assertTrue(xs.allMatch(i -> i > 0));
        assertFalse(xs.allMatch(i -> i < 0));
        assertFalse(xs.allMatch(i -> i == 0));
        assertFalse(xs.allMatch(i -> i == 1));
        assertFalse(xs.allMatch(i -> i == 2));
    }

    @Test
    public void noneMatchTest() {
        assertTrue(IntSeq.nil().noneMatch(i -> true));
        assertTrue(IntSeq.nil().noneMatch(i -> false));
        assertTrue(IntSeq.nil().noneMatch(i -> { throw new IllegalStateException(); }));

        IntSeq xs = IntSeq.of(1, 2);
        assertFalse(xs.noneMatch(i -> i > 0));
        assertTrue(xs.noneMatch(i -> i < 0));
        assertTrue(xs.noneMatch(i -> i == 0));
        assertFalse(xs.noneMatch(i -> i == 1));
        assertFalse(xs.noneMatch(i -> i == 2));
    }

    @Test
    public void rangeTest() {
        assertRangeEquals(IntSeq.range(0, 0));
        assertRangeEquals(IntSeq.range(0, 0, 1));
        assertRangeEquals(IntSeq.range(0, 0, 2));
        assertRangeEquals(IntSeq.range(0, 0, -1));
        assertRangeEquals(IntSeq.range(0, 0, -2));

        assertRangeEquals(IntSeq.range(5, 0));
        assertRangeEquals(IntSeq.range(5, 0, 1));
        assertRangeEquals(IntSeq.range(5, 0, 2));
        assertRangeEquals(IntSeq.range(0, 5, -1));
        assertRangeEquals(IntSeq.range(0, 5, -2));

        assertRangeEquals(IntSeq.range(0, 1), 0);
        assertRangeEquals(IntSeq.range(0, 2), 0, 1);
        assertRangeEquals(IntSeq.range(0, 3), 0, 1, 2);
        assertRangeEquals(IntSeq.range(0, 1, 1), 0);
        assertRangeEquals(IntSeq.range(0, 2, 1), 0, 1);
        assertRangeEquals(IntSeq.range(0, 3, 1), 0, 1, 2);

        assertRangeEquals(IntSeq.range(0, 1, 2), 0);
        assertRangeEquals(IntSeq.range(0, 2, 2), 0);
        assertRangeEquals(IntSeq.range(0, 3, 2), 0, 2);
        assertRangeEquals(IntSeq.range(0, 4, 2), 0, 2);
        assertRangeEquals(IntSeq.range(0, 5, 2), 0, 2, 4);

        assertRangeEquals(IntSeq.range(1, 0, -1), 1);
        assertRangeEquals(IntSeq.range(2, 0, -1), 2, 1);
        assertRangeEquals(IntSeq.range(3, 0, -1), 3, 2, 1);

        assertRangeEquals(IntSeq.range(1, 0, -2), 1);
        assertRangeEquals(IntSeq.range(2, 0, -2), 2);
        assertRangeEquals(IntSeq.range(3, 0, -2), 3, 1);
        assertRangeEquals(IntSeq.range(4, 0, -2), 4, 2);
        assertRangeEquals(IntSeq.range(5, 0, -2), 5, 3, 1);
    }

    @Test
    public void rangeClosedTest() {
        assertRangeEquals(IntSeq.rangeClosed(0, 0), 0);
        assertRangeEquals(IntSeq.rangeClosed(0, 0, 1), 0);
        assertRangeEquals(IntSeq.rangeClosed(0, 0, 2), 0);
        assertRangeEquals(IntSeq.rangeClosed(0, 0, -1), 0);
        assertRangeEquals(IntSeq.rangeClosed(0, 0, -2), 0);

        assertRangeEquals(IntSeq.rangeClosed(5, 0));
        assertRangeEquals(IntSeq.rangeClosed(5, 0, 1));
        assertRangeEquals(IntSeq.rangeClosed(5, 0, 2));
        assertRangeEquals(IntSeq.rangeClosed(0, 5, -1));
        assertRangeEquals(IntSeq.rangeClosed(0, 5, -2));

        assertRangeEquals(IntSeq.rangeClosed(0, 1), 0, 1);
        assertRangeEquals(IntSeq.rangeClosed(0, 2), 0, 1, 2);
        assertRangeEquals(IntSeq.rangeClosed(0, 3), 0, 1, 2, 3);
        assertRangeEquals(IntSeq.rangeClosed(0, 1, 1), 0, 1);
        assertRangeEquals(IntSeq.rangeClosed(0, 2, 1), 0, 1, 2);
        assertRangeEquals(IntSeq.rangeClosed(0, 3, 1), 0, 1, 2, 3);

        assertRangeEquals(IntSeq.rangeClosed(0, 1, 2), 0);
        assertRangeEquals(IntSeq.rangeClosed(0, 2, 2), 0, 2);
        assertRangeEquals(IntSeq.rangeClosed(0, 3, 2), 0, 2);
        assertRangeEquals(IntSeq.rangeClosed(0, 4, 2), 0, 2, 4);
        assertRangeEquals(IntSeq.rangeClosed(0, 5, 2), 0, 2, 4);

        assertRangeEquals(IntSeq.rangeClosed(1, 0, -1), 1, 0);
        assertRangeEquals(IntSeq.rangeClosed(2, 0, -1), 2, 1, 0);
        assertRangeEquals(IntSeq.rangeClosed(3, 0, -1), 3, 2, 1, 0);

        assertRangeEquals(IntSeq.rangeClosed(1, 0, -2), 1);
        assertRangeEquals(IntSeq.rangeClosed(2, 0, -2), 2, 0);
        assertRangeEquals(IntSeq.rangeClosed(3, 0, -2), 3, 1);
        assertRangeEquals(IntSeq.rangeClosed(4, 0, -2), 4, 2, 0);
        assertRangeEquals(IntSeq.rangeClosed(5, 0, -2), 5, 3, 1);
    }

    @Test(expected = IllegalArgumentException.class)
    public void zeroStepRangeIsInvalid() {
        IntSeq.range(0, 10, 0);
    }

    @Test
    public void takeRangeTest() {
        IntSeq xs = IntSeq.rangeClosed(1, 3);
        assertRangeEquals(xs.take(-1));
        assertRangeEquals(xs.take(0));
        assertRangeEquals(xs.take(1), 1);
        assertRangeEquals(xs.take(2), 1, 2);
        assertRangeEquals(xs.take(3), 1, 2, 3);
        assertRangeEquals(xs.take(4), 1, 2, 3);

        xs = IntSeq.rangeClosed(1, 5, 2);
        assertRangeEquals(xs.take(-1));
        assertRangeEquals(xs.take(0));
        assertRangeEquals(xs.take(1), 1);
        assertRangeEquals(xs.take(2), 1, 3);
        assertRangeEquals(xs.take(3), 1, 3, 5);
        assertRangeEquals(xs.take(4), 1, 3, 5);

        xs = IntSeq.rangeClosed(3, 1, -1);
        assertRangeEquals(xs.take(-1));
        assertRangeEquals(xs.take(0));
        assertRangeEquals(xs.take(1), 3);
        assertRangeEquals(xs.take(2), 3, 2);
        assertRangeEquals(xs.take(3), 3, 2, 1);
        assertRangeEquals(xs.take(4), 3, 2, 1);

        xs = IntSeq.rangeClosed(5, 1, -2);
        assertRangeEquals(xs.take(-1));
        assertRangeEquals(xs.take(0));
        assertRangeEquals(xs.take(1), 5);
        assertRangeEquals(xs.take(2), 5, 3);
        assertRangeEquals(xs.take(3), 5, 3, 1);
        assertRangeEquals(xs.take(4), 5, 3, 1);
    }

    @Test
    public void dropRangeTest() {
        IntSeq xs = IntSeq.rangeClosed(1, 3);
        assertRangeEquals(xs.drop(-1), 1, 2, 3);
        assertRangeEquals(xs.drop(0), 1, 2, 3);
        assertRangeEquals(xs.drop(1), 2, 3);
        assertRangeEquals(xs.drop(2), 3);
        assertRangeEquals(xs.drop(3));
        assertRangeEquals(xs.drop(4));

        xs = IntSeq.rangeClosed(1, 5, 2);
        assertRangeEquals(xs.drop(-1), 1, 3, 5);
        assertRangeEquals(xs.drop(0), 1, 3, 5);
        assertRangeEquals(xs.drop(1), 3, 5);
        assertRangeEquals(xs.drop(2), 5);
        assertRangeEquals(xs.drop(3));
        assertRangeEquals(xs.drop(4));

        xs = IntSeq.rangeClosed(3, 1, -1);
        assertRangeEquals(xs.drop(-1), 3, 2, 1);
        assertRangeEquals(xs.drop(0), 3, 2, 1);
        assertRangeEquals(xs.drop(1), 2, 1);
        assertRangeEquals(xs.drop(2), 1);
        assertRangeEquals(xs.drop(3));
        assertRangeEquals(xs.drop(4));

        xs = IntSeq.rangeClosed(5, 1, -2);
        assertRangeEquals(xs.drop(-1), 5, 3, 1);
        assertRangeEquals(xs.drop(0), 5, 3, 1);
        assertRangeEquals(xs.drop(1), 3, 1);
        assertRangeEquals(xs.drop(2), 1);
        assertRangeEquals(xs.drop(3));
        assertRangeEquals(xs.drop(4));
    }

    private static void assertSeqEquals(IntSeq xs, int... expected) {
        for (int x : expected) {
            assertEquals(x, xs.head());
            xs = xs.tail();
        }
    }

    private static void assertFiniteSeqEquals(IntSeq xs, int... expected) {
        for (int x : expected) {
            assertEquals(x, xs.head());
            xs = xs.tail();
        }
        assertTrue(xs.isEmpty());
    }

    private static void assertRangeEquals(IntSeq xs, int... expected) {
        int reversed[] = reverse(expected);
        IntSeq rev = xs.reverse();

        assertFiniteSeqEquals(xs, expected);
        assertEquals(expected.length, xs.count());
        assertFiniteSeqEquals(rev, reversed);
        assertEquals(reversed.length, rev.count());

        IntBinaryOperator sub = (x, y) -> x - y;
        assertEquals(foldl(0, expected, sub), xs.foldLeft(0, sub));
        assertEquals(foldr(0, expected, sub), xs.foldRight(0, sub));
    }

    static int[] reverse(int[] a) {
        int[] b = new int[a.length];
        for (int i = 0; i < a.length; i++) {
            b[a.length - 1 - i] = a[i];
        }
        return b;
    }

    static int foldl(int acc, int[] a, IntBinaryOperator op) {
        for (int i = 0; i < a.length; i++) {
            acc = op.applyAsInt(acc, a[i]);
        }
        return acc;
    }

    static int foldr(int acc, int[] a, IntBinaryOperator op) {
        for (int i = a.length; --i >= 0; ) {
            acc = op.applyAsInt(a[i], acc);
        }
        return acc;
    }
}
