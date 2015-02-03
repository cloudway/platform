/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.Random;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.LongStream;
import java.util.stream.Stream;
import com.google.common.collect.ImmutableList;

import org.junit.Test;
import static org.junit.Assert.*;

import static com.cloudway.platform.common.util.Conditionals.*;
import static com.cloudway.platform.common.util.Tuple.Tuple;
import static com.cloudway.platform.common.util.ListComprehension.*;

// @formatter:off
public class FunctionalTest
{
    static abstract class Queues<S> {
        private final int n;

        protected Queues(int n) {
            this.n = n;
        }

        public S solve() {
            return scan(n, n);
        }

        protected abstract S scan(int n, int i);

        public static boolean safe(int q, IntSeq qs, int n) {
            return with(qs).<Boolean>get()
                .when(IntSeq.Nil(() -> true))
                .when(IntSeq.Seq((x, xs) -> x != q && x != q + n && x != q - n && safe(q, xs, n+1)))
                .get();
        }
    }

    static final class QueuesSeq extends Queues<Seq<IntSeq>> {
        public QueuesSeq(int n) {
            super(n);
        }

        @Override
        protected Seq<IntSeq> scan(int n, int i) {
            return i == 0
                ? Seq.of(IntSeq.nil())
                : From(scan(n, i-1), qs ->
                  From(IntSeq.rangeClosed(1, n),  q ->
                  Where(safe(q, qs, 1),
                  Yield(IntSeq.cons(q, qs))))).build();
        }
    }

    static final class QueuesStream extends Queues<Stream<IntSeq>> {
        public QueuesStream(int n) {
            super(n);
        }

        @Override
        protected Stream<IntSeq> scan(int n, int i) {
            return i == 0
                ? Stream.of(IntSeq.nil())
                : From(scan(n, i-1), qs ->
                  From(IntStream.rangeClosed(1, n),  q ->
                  Where(safe(q, qs, 1),
                  Yield(IntSeq.cons(q, qs))))).build();
        }
    }

    @Test
    public void solveQueuesPuzzle() {
        new QueuesSeq(8).solve().forEach(qs -> {
            assertTrue(Queues.safe(qs.head(), qs.tail(), 8));
        });

        new QueuesStream(8).solve().forEach(qs -> {
            assertTrue(Queues.safe(qs.head(), qs.tail(), 8));
        });
    }

    public static IntSeq qsort(IntSeq seq) {
        return with(seq).<IntSeq>get()
            .when(IntSeq.Nil(IntSeq::nil))
            .when(IntSeq.Seq((x, xs) -> IntSeq.concat(
                    qsort(xs.filter(y -> y < x)),
                    IntSeq.of(x),
                    qsort(xs.filter(y -> y >= x))
            )))
            .get();
    }

    @Test
    public void quickSortTest() {
        Random rnd = new Random();
        int[] data = new int[10];
        Arrays.setAll(data, i -> rnd.nextInt(10));

        IntSeq sorted = qsort(IntSeq.of(data));
        Arrays.sort(data);
        assertSeqEquals(data, sorted);
    }

    private static class Fibs {
        private final IntSeq values = IntSeq.cons(1, IntSeq.cons(1, more()));

        private Supplier<IntSeq> more() {
            return () -> values.zip(values.tail(), Integer::sum);
        }

        public IntSeq values() {
            return values;
        }

        public static IntSeq generate() {
            return f(1, 1);
        }

        private static IntSeq f(int a, int b) {
            return IntSeq.cons(a, () -> f(b, a+b));
        }
    }

    private static class Primes {
        private final IntSeq values = IntSeq.cons(2, more());

        private Supplier<IntSeq> more() {
            return () -> IntSeq.from(3).filter(n -> sieve(values, n));
        }

        private static boolean sieve(IntSeq xs, int n) {
            do {
                int x = xs.head();
                if (x*x > n)
                    return true;
                if (n%x == 0)
                    return false;
                xs = xs.tail();
            } while (true);
        }

        public IntSeq values() {
            return values;
        }
    }

    /**
     * The Pi algorithm based on Gosper's series from the paper
     * "Unbounded Spigot Algorithm for the Digits of Pi",
     * Copyright Mathematical Association of America 2005.
     * author Jeremy Gibbons <jeremy.gibbons@comlab.ox.ac.uk>.
     * http://web.comlab.ox.ac.uk/oucl/work/jeremy.gibbons/publications/spigot.pdf
     */
    @SuppressWarnings("JavacQuirks")
    private static final class PI {
        private static BigInteger _(long n) { return BigInteger.valueOf(n); }

        // The original haskell program from paper:
        //
        // pi = g(1,180,60,2) where
        //   g(q,r,t,i) = let (u,y) = (3*(3*i+1)*(3*i+2), div(q*(27*i-12)+5*r)(5*t))
        //                in y : g(10*q*i*(2*i-1), 10*u*(q*(5*i-2)+r-y*t), t*u, i+1)

        public static IntSeq generate() {
            return g(_(1), _(180), _(60), _(2));
        }

        private static IntSeq g(BigInteger q, BigInteger r, BigInteger t, BigInteger i) {
            BigInteger u = _(3).multiply(_(3).multiply(i).add(_(1))).multiply(_(3).multiply(i).add(_(2)));
            BigInteger y = (q.multiply(_(27).multiply(i).subtract(_(12))).add(_(5).multiply(r))).divide(_(5).multiply(t));
            return IntSeq.cons(y.intValue(), () ->
                g(_(10).multiply(q).multiply(i).multiply(_(2).multiply(i).subtract(_(1))),
                  _(10).multiply(u).multiply(q.multiply(_(5).multiply(i).subtract(_(2))).add(r).subtract(y.multiply(t))),
                  t.multiply(u),
                  i.add(_(1))));
        }
    }

    @Test
    public void fibonacciTest() {
        int[] expected = {1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144};
        assertSeqEquals(expected, new Fibs().values());
        assertSeqEquals(expected, Fibs.generate());
    }

    @Test
    public void primeTest() {
        int[] expected = {2, 3, 5, 7, 11, 13, 17, 19, 23, 29};
        assertSeqEquals(expected, new Primes().values());
    }

    @Test
    public void piTest() {
        int[] expected = {3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9};
        assertSeqEquals(expected, PI.generate());
    }

    private static void assertSeqEquals(int[] expected, IntSeq actual) {
        for (int i = 0; i < expected.length; i++) {
            assertEquals(expected[i], actual.head());
            actual = actual.tail();
        }
    }

    // ----------------------------------------------------------------------

    /**
     * <pre>
     * Formalization of two Puzzles Involving Knowledge
     * McCarthy, John (1987).
     * http://www-formal.stanford.edu/jmc/puzzles.html
     *
     * We pick two numbers a and b, so that a>=b and both numbers are within
     * the range [2,99]. We give Mr.P the product a*b and give Mr.S the sum
     * a+b.
     *
     * The following dialog takes place:
     *
     *  Mr.P: I don't know the numbers
     *  Mr.S: I knew you didn't know. I don't know either.
     *  Mr.P: Now I know the numbers
     *  Mr.S: Now I know them too
     *
     * Can we find the numbers a and b?
     * </pre>
     */
    @Test
    public void McCarthyPuzzle() {
        class Puzzle {
            private final int low, high;
            private final Seq<Pair<Integer>> pairs;

            Puzzle(int low, int high) {
                this.low = low;
                this.high = high;
                this.pairs = From(IntSeq.rangeClosed(low, high), a ->
                             From(IntSeq.rangeClosed(a,   high), b ->
                             Yield(Pair.make(a, b)))).build();
            }

            private Seq<Pair<Integer>> S(int s) {
                return pairs.filter(with(Tuple((a,b) -> a+b == s)));
            }

            private Seq<Pair<Integer>> P(int p) {
                return pairs.filter(with(Tuple((a,b) -> a*b == p)));
            }

            private <T> boolean unique(Seq<T> s) {
                return with(s).<Boolean>get().when(Seq.Seq((x, xs) -> xs.isEmpty())).orElse(false);
            }

            private boolean MrP_dont_know(int p) {
                return !unique(P(p));
            }

            private boolean MrS_knew_MrP_dont_know(int s) {
                return S(s).allMatch(with(Tuple((a,b) -> MrP_dont_know(a*b))));
            }

            private boolean MrP_now_know(int p) {
                return unique(P(p).filter(with(Tuple((a,b) -> MrS_knew_MrP_dont_know(a+b)))));
            }

            private boolean MrS_now_know(int s) {
                return unique(S(s).filter(with(Tuple((a,b) -> MrP_now_know(a*b)))));
            }

            private Seq<Pair<Integer>> solve() {
                return From(IntSeq.rangeClosed(low, high), x ->
                       From(IntSeq.rangeClosed(x,   high), y ->
                       Where(MrP_dont_know(x*y) &&
                             MrS_knew_MrP_dont_know(x+y) &&
                             MrP_now_know(x*y) &&
                             MrS_now_know(x+y),
                       Yield(Pair.make(x, y))))).build();
            }
        }

        Pair<Integer> result = new Puzzle(2, 99).solve().head();
        assertEquals(Pair.make(4, 13), result);
    }

    /**
     * Dinesman's multiple-dwelling problem:
     *
     * <p><b>The problem</b></p>
     * <p>Baker, Cooper, Fletcher, Miller, and Smith live on different floors of
     * an apartment house that contains only five floors.</p>
     *
     * <pre>
     * Baker does not live on the top floor.
     * Cooper does not live on the bottom floor.
     * Fletcher does not live on either the top or the bottom floor.
     * Miller lives on a higher floor than does Cooper.
     * Smith does not live on a floor adjacent to Fletcher's.
     * Fletcher does not live on a floor adjacent to Cooper's.
     * </pre>
     *
     * <p><i>Where does everyone live?</i></p>
     */
    @Test
    public void MultipleDwelling() {
        Seq<ImmutableList<Integer>> result =
            From(IntSeq.rangeClosed(1, 5), baker ->
            From(IntSeq.rangeClosed(1, 5), cooper ->
            From(IntSeq.rangeClosed(1, 5), fletcher ->
            From(IntSeq.rangeClosed(1, 5), miller ->
            From(IntSeq.rangeClosed(1, 5), smith ->
            Where(distinct(baker, cooper, fletcher, miller, smith) &&
                  baker != 5 && cooper != 1 &&
                  fletcher != 5 && fletcher != 1 &&
                  miller > cooper &&
                  Math.abs(smith - fletcher) != 1 &&
                  Math.abs(fletcher - cooper) != 1,
            Yield(ImmutableList.of(baker, cooper, fletcher, miller, smith))))))))
            .build();

        ImmutableList<Integer> expected = ImmutableList.of(3, 2, 4, 5, 1);
        assertEquals(expected, result.head());
    }

    private static boolean distinct(int... elements) {
        for (int i = 0; i < elements.length; i++) {
            for (int j = i+1; j < elements.length; j++) {
                if (elements[i] == elements[j])
                    return false;
            }
        }
        return true;
    }

    // ----------------------------------------------------------------------

    // Test case for LinuxContainerPlugin#combination.  That method is not used
    // in reality.

    interface Combination {
        int SET_SIZE = 16;
        int GROUP_SIZE = 5;
        int MAX_COMBINATIONS = (int)C(SET_SIZE, GROUP_SIZE);

        // see http://en.wikipedia.org/wiki/Combination
        static long C(int n, int k) {
            return LongStream.rangeClosed(n-k+1, n).reduce(1, (a,b) -> a*b) /
                   LongStream.rangeClosed(1, k).reduce(1, (a,b) -> a*b);
        }

        String compute(int n);
    }

    private enum Combinations implements Combination {
        /**
         * The combination algorithm implemented using lazy list.
         */
        SEQ {
            @Override
            public String compute(int n) {
                return combination(SET_SIZE, 0, GROUP_SIZE).drop(n).head().show(",c", "c", "");
            }

            private Seq<IntSeq> combination(int n, int k, int m) {
                return m == 0
                    ? Seq.of(IntSeq.nil())
                    : IntSeq.rangeClosed(k, n - m).flatMapToObj(i ->
                      combination(n, i + 1, m - 1).map(c ->
                      IntSeq.cons(i, c)));
            }
        },

        /**
         * The combination algorithm implemented using streams.
         * The performance is relatively very low.
         */
        STREAM {
            @Override
            public String compute(int n) {
                return combination(SET_SIZE, 0, GROUP_SIZE)
                    .skip(n)
                    .findFirst()            // an optional IntStream
                    .get()                  // an IntStream
                    .mapToObj(i -> "c" + i) // joining the IntStream
                    .collect(Collectors.joining(","));
            }

            private Stream<IntStream> combination(int n, int k, int m) {
                return m == 0
                    ? Stream.of(IntStream.empty())
                    : IntStream.rangeClosed(k, n - m).boxed().flatMap(i ->
                      combination(n, i + 1, m - 1).map(c ->
                      IntStream.concat(IntStream.of(i), c)));
            }
        }
    }

    @Test
    public void combinationTest() {
        Random rnd = new Random();
        IntStream.generate(() -> rnd.nextInt(Combination.MAX_COMBINATIONS)).limit(10).forEach(i -> {
            // test if two algorithms yield the same result
            String label_seq = Combinations.SEQ.compute(i);
            String label_stm = Combinations.STREAM.compute(i);
            assertEquals(label_seq, label_stm);
        });
    }
}
