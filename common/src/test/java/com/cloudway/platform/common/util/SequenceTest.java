/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Random;
import java.util.Set;
import java.util.StringJoiner;
import java.util.function.Consumer;

import org.junit.Test;
import static java.util.stream.Collectors.*;
import static org.junit.Assert.*;

import com.cloudway.platform.common.fp.data.Maybe;
import com.cloudway.platform.common.fp.data.Seq;
import com.cloudway.platform.common.fp.data.Tuple;
import static com.cloudway.platform.common.fp.control.Conditionals.*;
import static com.cloudway.platform.common.fp.data.Seq.Cons;

public class SequenceTest
{
    @Test
    public void nilTest() {
        assertTrue(Seq.nil().isEmpty());
        noSuchElement(Seq::head);
        noSuchElement(Seq::tail);
        assertFalse(Seq.nil().peek().isPresent());
    }

    static <T> void noSuchElement(Consumer<Seq<T>> consumer) {
        try {
            consumer.accept(Seq.nil());
            fail("NoSuchElementException was not thrown");
        } catch (NoSuchElementException ex) {
            // ok
        }
    }

    @Test
    public void consTest() {
        Seq<String> seq = Seq.cons("a", Seq.nil());
        assertFalse(seq.isEmpty());
        assertTrue(seq.tail().isEmpty());
        assertEquals("a", seq.head());
        assertEquals("a", seq.peek().get());

        assertFiniteSeqEquals(Seq.cons(1, Seq.cons(2, Seq.cons(3, Seq.nil()))), 1, 2, 3);
        assertFiniteSeqEquals(Seq.cons(1, () -> Seq.cons(2, () -> Seq.cons(3, Seq.nil()))), 1, 2, 3);
    }

    static class ForceLazySeqException extends RuntimeException {
        private static final long serialVersionUID = -1632483700428668563L;
    }

    @Test
    public void lazySeqWillNotForceIfTailNotUsed() {
        Seq<Integer> seq = Seq.cons(42, () -> { throw new ForceLazySeqException(); });
        assertFalse(seq.isEmpty());
        assertEquals(42, (int)seq.head());
        assertEquals(42, (int)seq.peek().get());
        assertEquals(21, (int)seq.map(x -> x / 2).head());
        assertSeqEquals(seq.take(1), 42);
    }

    @Test(expected = ForceLazySeqException.class)
    public void lazySeqWillForceWhenTailUsed() {
        Seq<Integer> seq = Seq.cons(42, () -> { throw new ForceLazySeqException(); });
        seq.tail().isEmpty();
    }

    @Test
    public void patternMatchingOnLazySeqWillNotForceIfTailNotUsed() {
        Seq<Integer> seq = Seq.cons(42, () -> { throw new ForceLazySeqException(); });
        with(seq).when(Cons((x, xs) -> {
            assertEquals(42, (int)x);
            return null;
        }));
    }

    @Test(expected = ForceLazySeqException.class)
    public void patternMatchingOnLazySeqWillForceWhenTailUsed() {
        Seq<Integer> seq = Seq.cons(42, () -> { throw new ForceLazySeqException(); });
        with(seq).when(Cons((x, xs) -> {
            xs.isEmpty();
            return null;
        }));
    }

    @Test
    public void iterateTest() {
        Seq<Integer> seq = Seq.iterate(1, x -> x + 1);
        assertSeqEquals(seq, 1, 2, 3, 4, 5);
        assertSeqEquals(seq.map(x -> x * 10), 10, 20, 30, 40, 50);
        assertSeqEquals(seq.filter(x -> x % 2 == 0), 2, 4, 6, 8, 10);
    }

    @Test
    public void repeatTest() {
        Seq<Integer> seq = Seq.repeat(5);
        assertSeqEquals(seq, 5, 5, 5, 5, 5);
        assertSeqEquals(seq.reverse(), 5, 5, 5, 5, 5);
        assertSeqEquals(seq.map(x -> x * 2), 10, 10, 10, 10, 10);
    }

    @Test
    public void replicateTest() {
        assertFiniteSeqEquals(Seq.replicate(-1, 5));
        assertFiniteSeqEquals(Seq.replicate(0, 5));
        assertFiniteSeqEquals(Seq.replicate(1, 5), 5);
        assertFiniteSeqEquals(Seq.replicate(2, 5), 5, 5);
        assertFiniteSeqEquals(Seq.replicate(3, 5), 5, 5, 5);
    }

    @Test
    public void cycleTest() {
        assertTrue(Seq.nil().cycle().isEmpty());

        Seq<Integer> seq = Seq.of(1, 2, 3).cycle();
        assertSeqEquals(seq, 1, 2, 3, 1, 2, 3, 1);
        assertSeqEquals(seq.map(x -> x * 2), 2, 4, 6, 2, 4, 6, 2);
        assertSeqEquals(seq.filter(x -> x % 2 != 0), 1, 3, 1, 3, 1, 3);
    }

    @Test
    public void reverseTest() {
        assertTrue(Seq.nil().reverse().isEmpty());
        assertFiniteSeqEquals(Seq.of(1).reverse(), 1);
        assertFiniteSeqEquals(Seq.of(1, 2).reverse(), 2, 1);
        assertFiniteSeqEquals(Seq.of(1, 2, 3).reverse(), 3, 2, 1);
    }

    @Test
    public void appendTest() {
        assertTrue(Seq.nil().append(Seq.nil()).isEmpty());
        assertFiniteSeqEquals(Seq.nil().append(Seq.of(1, 2, 3)), 1, 2, 3);
        assertFiniteSeqEquals(Seq.of(1, 2, 3).append(Seq.nil()), 1, 2, 3);
        assertFiniteSeqEquals(Seq.of(1, 2, 3).append(Seq.of(4, 5, 6)), 1, 2, 3, 4, 5, 6);
    }

    @Test
    public void iteratorTest() {
        assertFalse(Seq.nil().iterator().hasNext());

        Iterator<Integer> it = Seq.of(1, 2, 3).iterator();
        assertTrue(it.hasNext());
        assertEquals(1, (int)it.next());
        assertTrue(it.hasNext());
        assertEquals(2, (int)it.next());
        assertTrue(it.hasNext());
        assertEquals(3, (int)it.next());
        assertFalse(it.hasNext());
    }

    @Test
    public void countTest() {
        Seq<Integer> xs = Seq.nil();
        for (int i = 0; i < 10; i++) {
            assertEquals(i, xs.count());
            xs = Seq.cons(i, xs);
        }
    }

    @Test
    public void findTest() {
        assertFalse(Seq.nil().find(x -> true).isPresent());
        assertFalse(Seq.nil().find(x -> false).isPresent());
        assertFalse(Seq.nil().find(x -> { throw new IllegalStateException(); }).isPresent());

        Seq<Integer> seq = Seq.of(1, 2, 3, 4, 5, 6);
        assertEquals(3, (int)seq.find(x -> x % 3 == 0).get());
        assertFalse(seq.find(x -> x > 10).isPresent());
    }

    @Test
    public void foldTest() {
        assertEquals(6,  (int)Seq.of(1, 2, 3).foldLeft(0, (acc, x) -> acc + x));
        assertEquals(6,  (int)Seq.of(1, 2, 3).foldRight(0, (x, acc) -> x + acc.get()));
        assertEquals(6,  (int)Seq.of(1, 2, 3).foldRight_(0, (x, acc) -> x + acc));
        assertEquals(-6, (int)Seq.of(1, 2, 3).foldLeft(0, (acc, x) -> acc - x));
        assertEquals(2,  (int)Seq.of(1, 2, 3).foldRight(0, (x, acc) -> x - acc.get()));
        assertEquals(2,  (int)Seq.of(1, 2, 3).foldRight_(0, (x, acc) -> x - acc));

        assertEquals(0, (int)Seq.<Integer>nil().foldLeft(0, (acc, x) -> acc + x));
        assertEquals(0, (int)Seq.<Integer>nil().foldRight(0, (x, acc) -> x + acc.get()));
        assertEquals(0, (int)Seq.<Integer>nil().foldRight_(0, (x, acc) -> x + acc));

        assertEquals(1,  (int)Seq.of(1).foldLeft(0, (acc, x) -> acc + x));
        assertEquals(1,  (int)Seq.of(1).foldRight(0, (x, acc) -> x + acc.get()));
        assertEquals(1,  (int)Seq.of(1).foldRight_(0, (x, acc) -> x + acc));
        assertEquals(-1, (int)Seq.of(1).foldLeft(0, (acc, x) -> acc - x));
        assertEquals(1,  (int)Seq.of(1).foldRight(0, (x, acc) -> x - acc.get()));
        assertEquals(1,  (int)Seq.of(1).foldRight_(0, (x, acc) -> x - acc));

        assertEquals(Maybe.of(6),  Seq.of(1, 2, 3).foldLeft((acc, x) -> acc + x));
        assertEquals(Maybe.of(6),  Seq.of(1, 2, 3).foldRight((x, acc) -> x + acc));
        assertEquals(Maybe.of(-4), Seq.of(1, 2, 3).foldLeft((acc, x) -> acc - x));
        assertEquals(Maybe.of(2),  Seq.of(1, 2, 3).foldRight((x, acc) -> x - acc));

        assertEquals(Maybe.<Integer>empty(), Seq.<Integer>nil().foldLeft((acc, x) -> acc + x));
        assertEquals(Maybe.<Integer>empty(), Seq.<Integer>nil().foldRight((x, acc) -> x + acc));

        assertEquals(Maybe.of(1),  Seq.of(1).foldLeft((acc, x) -> acc + x));
        assertEquals(Maybe.of(1),  Seq.of(1).foldRight((x, acc) -> x + acc));
        assertEquals(Maybe.of(1),  Seq.of(1).foldLeft((acc, x) -> acc - x));
        assertEquals(Maybe.of(1),  Seq.of(1).foldRight((x, acc) -> x - acc));

        assertEquals(Maybe.of(3),  Seq.of(1, 2).foldLeft((acc, x) -> acc + x));
        assertEquals(Maybe.of(3),  Seq.of(1, 2).foldRight((x, acc) -> x + acc));
        assertEquals(Maybe.of(-1), Seq.of(1, 2).foldLeft((acc, x) -> acc - x));
        assertEquals(Maybe.of(-1), Seq.of(1, 2).foldRight((x, acc) -> x - acc));

        assertEquals("abc", Seq.of("a", "b", "c").foldLeft("", String::concat));
        assertEquals("abc", Seq.of("a", "b", "c").foldLeft(new StringBuilder(), StringBuilder::append).toString());
        assertEquals("a,b,c", Seq.of("a", "b", "c").foldLeft(new StringJoiner(","), StringJoiner::add).toString());
    }

    @Test(timeout = 1000)
    public void infiniteFoldRightTest() {
        Seq<Integer> xs = Seq.iterate(1, x -> x + 1).foldRight(Seq.nil(), (x, acc) -> Seq.cons(x * x, acc));
        assertSeqEquals(xs, 1, 4, 9, 16, 25, 36);
    }

    @Test
    public void scanLeftTest() {
        Seq<Integer> xs = Seq.of(3, 5, 2, 1).scanLeft(0, (acc, x) -> acc + x);
        assertFiniteSeqEquals(xs, 0, 3, 8, 10, 11);
    }

    @Test
    public void scanRightTest() {
        Seq<Integer> xs = Seq.of(3, 5, 2, 1).scanRight(0, (x, acc) -> x + acc.get());
        assertFiniteSeqEquals(xs, 11, 8, 3, 1, 0);
    }

    @Test
    public void sortTest() {
        Random rnd = new Random();
        Integer[] data = new Integer[100];
        Arrays.setAll(data, i -> rnd.nextInt(200));
        Integer[] sorted = Arrays.copyOf(data, data.length);
        Arrays.sort(sorted);
        assertFiniteSeqEquals(Seq.of(data).sorted(), sorted);

        assertFiniteSeqEquals(Seq.<Integer>nil().sorted());
        assertFiniteSeqEquals(Seq.of(42).sorted(), 42);
        assertFiniteSeqEquals(Seq.of(1,2).sorted(), 1, 2);
        assertFiniteSeqEquals(Seq.of(2, 1).sorted(), 1, 2);
        assertFiniteSeqEquals(Seq.of(1, 1).sorted(), 1, 1);
    }

    @Test
    public void collectTest() {
        Integer[] data = {3, 1, 4, 1, 6};
        Seq<Integer> xs = Seq.of(data);

        assertEquals(Arrays.asList(data), xs.toList());
        assertEquals(setOf(data), xs.collect(toSet()));
        assertEquals("3,1,4,1,6", xs.map(String::valueOf).collect(joining(",")));
        assertEquals(data.length, (long)xs.collect(counting()));
        assertEquals(15, (int)xs.collect(summingInt(i -> i)));

        Map<Boolean, Set<Integer>> partition = xs.collect(partitioningBy(i -> i % 2 == 0, toSet()));
        assertEquals(setOf(4, 6), partition.get(true));
        assertEquals(setOf(1, 3), partition.get(false));
    }

    @SafeVarargs
    static <T> Set<T> setOf(T... elements) {
        return new HashSet<>(Arrays.asList(elements));
    }

    @Test
    public void takeTest() {
        assertTrue(Seq.nil().take(0).isEmpty());
        assertTrue(Seq.nil().take(-1).isEmpty());
        assertTrue(Seq.nil().take(1).isEmpty());

        Seq<Integer> xs = Seq.of(1, 2, 3);
        assertTrue(xs.take(0).isEmpty());
        assertTrue(xs.take(-1).isEmpty());
        assertFiniteSeqEquals(xs.take(1), 1);
        assertFiniteSeqEquals(xs.take(2), 1, 2);
        assertFiniteSeqEquals(xs.take(3), 1, 2, 3);
        assertFiniteSeqEquals(xs.take(4), 1, 2, 3);
    }

    @Test(timeout = 1000)
    public void takeInfiniteSeqTest() {
        Seq<Integer> odd = Seq.of(1, 2, 3).append(Seq.repeat(4)).filter(x -> x % 2 != 0).take(2);
        assertFiniteSeqEquals(odd, 1, 3);

        Seq<Integer> even = Seq.of(1, 2, 3).append(Seq.repeat(4)).filter(x -> x % 2 == 0).take(2);
        assertFiniteSeqEquals(even, 2, 4);
    }

    @Test
    public void dropTest() {
        assertTrue(Seq.nil().drop(0).isEmpty());
        assertTrue(Seq.nil().drop(-1).isEmpty());
        assertTrue(Seq.nil().drop(1).isEmpty());

        Seq<Integer> xs = Seq.of(1, 2, 3);
        assertFiniteSeqEquals(xs.drop(0), 1, 2, 3);
        assertFiniteSeqEquals(xs.drop(-1), 1, 2, 3);
        assertFiniteSeqEquals(xs.drop(1), 2, 3);
        assertFiniteSeqEquals(xs.drop(2), 3);
        assertFiniteSeqEquals(xs.drop(3));
        assertFiniteSeqEquals(xs.drop(4));
    }

    @Test
    public void takeWhileTest() {
        assertTrue(Seq.nil().takeWhile(x -> true).isEmpty());
        assertTrue(Seq.nil().takeWhile(x -> false).isEmpty());
        assertTrue(Seq.nil().takeWhile(x -> { throw new IllegalStateException(); }).isEmpty());

        Seq<Integer> xs = Seq.of(1, 2, 3, 4, 5);
        assertFiniteSeqEquals(xs.takeWhile(x -> x < 3), 1, 2);
        assertFiniteSeqEquals(xs.takeWhile(x -> x > 3));
    }

    @Test
    public void takeUntilTest() {
        assertTrue(Seq.nil().takeUntil(x -> true).isEmpty());
        assertTrue(Seq.nil().takeUntil(x -> false).isEmpty());
        assertTrue(Seq.nil().takeUntil(x -> { throw new IllegalStateException(); }).isEmpty());

        Seq<Integer> xs = Seq.of(1, 2, 3, 4, 5);
        assertFiniteSeqEquals(xs.takeUntil(x -> x < 3));
        assertFiniteSeqEquals(xs.takeUntil(x -> x > 3), 1, 2, 3);
    }

    @Test
    public void dropWhileTest() {
        assertTrue(Seq.nil().dropWhile(x -> true).isEmpty());
        assertTrue(Seq.nil().dropWhile(x -> false).isEmpty());
        assertTrue(Seq.nil().dropWhile(x -> { throw new IllegalStateException(); }).isEmpty());

        Seq<Integer> xs = Seq.of(1, 2, 3, 4, 5);
        assertFiniteSeqEquals(xs.dropWhile(x -> x < 3), 3, 4, 5);
        assertFiniteSeqEquals(xs.dropWhile(x -> x > 3), 1, 2, 3, 4, 5);
    }

    @Test
    public void dropUntilTest() {
        assertTrue(Seq.nil().dropUntil(x -> true).isEmpty());
        assertTrue(Seq.nil().dropUntil(x -> false).isEmpty());
        assertTrue(Seq.nil().dropUntil(x -> { throw new IllegalStateException(); }).isEmpty());

        Seq<Integer> xs = Seq.of(1, 2, 3, 4, 5);
        assertFiniteSeqEquals(xs.dropUntil(x -> x < 3), 1, 2, 3, 4, 5);
        assertFiniteSeqEquals(xs.dropUntil(x -> x > 3), 4, 5);
    }

    @Test
    public void transposeTest() {
        Seq<Seq<Integer>> seq = Seq.transpose(Seq.of(Seq.of(1, 2, 3), Seq.of(4, 5, 6), Seq.of(7, 8, 9)));
        assertFiniteSeqEquals(seq.head(), 1, 4, 7); seq = seq.tail();
        assertFiniteSeqEquals(seq.head(), 2, 5, 8); seq = seq.tail();
        assertFiniteSeqEquals(seq.head(), 3, 6, 9); seq = seq.tail();
        assertTrue(seq.isEmpty());

        Seq<Seq<Character>> cs = Seq.transpose(Seq.of(Seq.wrap("hey"), Seq.wrap("there"), Seq.wrap("guys")));
        assertEquals("htg", joinSeq(cs.head())); cs = cs.tail();
        assertEquals("ehu", joinSeq(cs.head())); cs = cs.tail();
        assertEquals("yey", joinSeq(cs.head())); cs = cs.tail();
        assertEquals("rs",  joinSeq(cs.head())); cs = cs.tail();
        assertEquals("e",   joinSeq(cs.head())); cs = cs.tail();
        assertTrue(cs.isEmpty());

        Seq<Seq<Integer>> coefs = Seq.of(Seq.of(0, 3, 5, 9), Seq.of(10, 0, 0, 9), Seq.of(8, 5, 1, -1));
        Seq<Integer> sum = Seq.transpose(coefs).map(xs -> xs.foldLeft(0, Integer::sum));
        assertFiniteSeqEquals(sum, 18, 8, 6, 17);
    }

    @Test
    public void zipTest() {
        assertTrue(Seq.nil().zip(Seq.nil()).isEmpty());
        assertTrue(Seq.nil().zip(Seq.of(1, 2, 3)).isEmpty());
        assertTrue(Seq.of(1, 2, 3).zip(Seq.nil()).isEmpty());

        assertFiniteSeqEquals(Seq.of(1, 2, 3).zip(Seq.of(4, 5, 6)),
            Tuple.of(1, 4), Tuple.of(2, 5), Tuple.of(3, 6));

        assertFiniteSeqEquals(Seq.of(1, 2).zip(Seq.of(4, 5, 6)),
            Tuple.of(1, 4), Tuple.of(2, 5));

        assertFiniteSeqEquals(Seq.of(1, 2, 3).zip(Seq.of(4, 5)),
            Tuple.of(1, 4), Tuple.of(2, 5));
    }

    @Test
    public void zipWithTest() {
        assertTrue(Seq.<Integer>nil().zip(Seq.nil(), Integer::sum).isEmpty());
        assertTrue(Seq.<Integer>nil().zip(Seq.of(1, 2, 3), Integer::sum).isEmpty());
        assertTrue(Seq.of(1, 2, 3).zip(Seq.nil(), Integer::sum).isEmpty());

        assertFiniteSeqEquals(Seq.of(1, 2, 3).zip(Seq.of(4, 5, 6), Integer::sum), 5, 7, 9);
        assertFiniteSeqEquals(Seq.of(1, 2).zip(Seq.of(4, 5, 6), Integer::sum), 5, 7);
        assertFiniteSeqEquals(Seq.of(1, 2, 3).zip(Seq.of(4, 5), Integer::sum), 5, 7);
    }

    @Test
    public void unzipTest() {
        Seq<Tuple<Integer, String>> zipped = Seq.of(1, 2, 3).zip(Seq.of("a", "b", "c"));
        Tuple<Seq<Integer>, Seq<String>> unzipped = Seq.unzip(zipped);
        assertFiniteSeqEquals(unzipped.first(), 1, 2, 3);
        assertFiniteSeqEquals(unzipped.second(), "a", "b", "c");
    }

    @Test
    public void anyMatchTest() {
        assertFalse(Seq.nil().anyMatch(i -> true));
        assertFalse(Seq.nil().anyMatch(i -> false));
        assertFalse(Seq.nil().anyMatch(i -> { throw new IllegalStateException(); }));

        Seq<Integer> xs = Seq.of(1, 2);
        assertTrue(xs.anyMatch(i -> i > 0));
        assertFalse(xs.anyMatch(i -> i < 0));
        assertFalse(xs.anyMatch(i -> i == 0));
        assertTrue(xs.anyMatch(i -> i == 1));
        assertTrue(xs.anyMatch(i -> i == 2));
    }

    @Test
    public void allMatchTest() {
        assertTrue(Seq.nil().allMatch(i -> true));
        assertTrue(Seq.nil().allMatch(i -> false));
        assertTrue(Seq.nil().allMatch(i -> { throw new IllegalStateException(); }));

        Seq<Integer> xs = Seq.of(1, 2);
        assertTrue(xs.allMatch(i -> i > 0));
        assertFalse(xs.allMatch(i -> i < 0));
        assertFalse(xs.allMatch(i -> i == 0));
        assertFalse(xs.allMatch(i -> i == 1));
        assertFalse(xs.allMatch(i -> i == 2));
    }

    @Test
    public void noneMatchTest() {
        assertTrue(Seq.nil().noneMatch(i -> true));
        assertTrue(Seq.nil().noneMatch(i -> false));
        assertTrue(Seq.nil().noneMatch(i -> { throw new IllegalStateException(); }));

        Seq<Integer> xs = Seq.of(1, 2);
        assertFalse(xs.noneMatch(i -> i > 0));
        assertTrue(xs.noneMatch(i -> i < 0));
        assertTrue(xs.noneMatch(i -> i == 0));
        assertFalse(xs.noneMatch(i -> i == 1));
        assertFalse(xs.noneMatch(i -> i == 2));
    }

    static String joinSeq(Seq<Character> cs) {
        return cs.foldLeft(new StringBuilder(), StringBuilder::append).toString();
    }

    @SafeVarargs
    private static <T> void assertSeqEquals(Seq<T> xs, T... expected) {
        for (T x : expected) {
            assertEquals(x, xs.head());
            xs = xs.tail();
        }
    }

    @SafeVarargs
    private static <T> void assertFiniteSeqEquals(Seq<T> xs, T... expected) {
        for (T x : expected) {
            assertEquals(x, xs.head());
            xs = xs.tail();
        }
        assertTrue(xs.isEmpty());
    }
}
