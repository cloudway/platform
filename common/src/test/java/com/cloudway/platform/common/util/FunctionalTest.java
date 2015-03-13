/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.Optional;
import java.util.Random;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.LongStream;
import java.util.stream.Stream;
import com.google.common.collect.ImmutableList;

import org.junit.Test;
import static org.junit.Assert.*;

import com.cloudway.platform.common.fp.data.Fn;
import com.cloudway.platform.common.fp.control.Cont;
import com.cloudway.platform.common.fp.control.MonadState;
import com.cloudway.platform.common.fp.control.StateCont;
import com.cloudway.platform.common.fp.control.Trampoline;
import com.cloudway.platform.common.fp.data.IntSeq;
import com.cloudway.platform.common.fp.data.Tuple;
import com.cloudway.platform.common.fp.data.Pair;
import com.cloudway.platform.common.fp.data.TreeMap;
import com.cloudway.platform.common.fp.data.Seq;
import com.cloudway.platform.common.fp.data.Unit;

import static com.cloudway.platform.common.fp.control.Conditionals.*;
import static com.cloudway.platform.common.fp.control.Trampoline.immediate;
import static com.cloudway.platform.common.fp.control.Trampoline.suspend;
import static com.cloudway.platform.common.fp.control.Comprehension.*;
import static com.cloudway.platform.common.fp.data.IntSeq.IntCons;
import static com.cloudway.platform.common.fp.data.IntSeq.IntNil;
import static com.cloudway.platform.common.fp.data.Seq.Cons;
import static com.cloudway.platform.common.fp.data.Seq.Nil;
import static com.cloudway.platform.common.fp.data.Seq.Single;
import static com.cloudway.platform.common.fp.data.Tuple.Tuple_;

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
                .when(IntNil(() -> true))
                .when(IntCons((x, xs) -> x != q && x != q + n && x != q - n && safe(q, xs, n+1)))
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
                : select.from(scan(n, i-1), qs ->
                         from(IntSeq.rangeClosed(1, n),  q ->
                         where(safe(q, qs, 1),
                         yield(IntSeq.cons(q, qs)))));
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
                : select.from(scan(n, i-1), qs ->
                         from(IntStream.rangeClosed(1, n),  q ->
                         where(safe(q, qs, 1),
                         yield(IntSeq.cons(q, qs)))));
        }
    }

    @Test
    public void solveQueuesPuzzle() {
        new QueuesSeq(8).solve().forEach(qs ->
            assertTrue(Queues.safe(qs.head(), qs.tail(), 8)));

        new QueuesStream(8).solve().forEach(qs ->
            assertTrue(Queues.safe(qs.head(), qs.tail(), 8)));
    }

    public static IntSeq qsort(IntSeq seq) {
        return with(seq).<IntSeq>get()
            .when(IntNil(IntSeq::nil))
            .when(IntCons((x, xs) -> IntSeq.concat(
                    qsort(xs.filter(y -> y < x)),
                    IntSeq.of(x),
                    qsort(xs.filter(y -> y >= x))
            ))).get();
    }

    @Test
    public void quickSortTest() {
        Random rnd = new Random();
        int[] data = new int[10];
        Arrays.setAll(data, i -> rnd.nextInt(10));

        IntSeq sorted = qsort(IntSeq.of(data));
        Arrays.sort(data);
        assertSeqEquals(data, sorted );
    }

    private static <T extends Comparable<T>> T max(T a, T b) {
        return a.compareTo(b) > 0 ? a : b;
    }

    private static <T extends Comparable<T>> Optional<T> maximum(Seq<T> list) {
        return with(list).<Optional<T>>get()
            .when(Nil(Optional::empty))
            .when(Single(Optional::of))
            .when(Cons((x, y, ys) -> maximum(Seq.cons(max(x, y), ys))))
            .get();
    }

    @Test
    public void maximumTest() {
        assertEquals(Optional.<String>empty(), maximum(Seq.<String>nil()));
        assertEquals(Optional.of("apple"), maximum(Seq.of("apple")));
        assertEquals(Optional.of("mango"), maximum(Seq.of("apple", "mango")));
        assertEquals(Optional.of("mango"), maximum(Seq.of("mango", "apple")));

        String[] fruits = { "apple", "mango", "orange" };
        IntStream.range(0, fruits.length).forEach(i ->
        IntStream.range(0, fruits.length).filter(j -> j != i).forEach(j ->
        IntStream.range(0, fruits.length).filter(k -> k != i && k != j).forEach(k ->
            assertEquals(Optional.of("orange"), maximum(Seq.of(fruits[i], fruits[j], fruits[k])))
        )));
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
        private final IntSeq primes = IntSeq.cons(2, () -> IntSeq.from(3).filter(this::sieve));

        private boolean sieve(int n) {
            return primes.takeWhile(x -> x * x <= n).allMatch(x -> n % x != 0);
        }

        public IntSeq values() {
            return primes;
        }
    }

    /**
     * The Pi algorithm based on Gosper's series from the paper
     * "Unbounded Spigot Algorithm for the Digits of Pi",
     * Copyright Mathematical Association of America 2005.
     * author Jeremy Gibbons <jeremy.gibbons@comlab.ox.ac.uk>.
     * http://web.comlab.ox.ac.uk/oucl/work/jeremy.gibbons/publications/spigot.pdf
     */
    private static final class PI {
        private static BigInteger $(long n) { return BigInteger.valueOf(n); }

        // The original haskell program from paper:
        //
        // pi = g(1,180,60,2) where
        //   g(q,r,t,i) = let (u,y) = (3*(3*i+1)*(3*i+2), div(q*(27*i-12)+5*r)(5*t))
        //                in y : g(10*q*i*(2*i-1), 10*u*(q*(5*i-2)+r-y*t), t*u, i+1)

        public static IntSeq generate() {
            return g($(1), $(180), $(60), $(2));
        }

        private static IntSeq g(BigInteger q, BigInteger r, BigInteger t, BigInteger i) {
            BigInteger u = $(3).multiply($(3).multiply(i).add($(1))).multiply($(3).multiply(i).add($(2)));
            BigInteger y = (q.multiply($(27).multiply(i).subtract($(12))).add($(5).multiply(r))).divide($(5).multiply(t));
            return IntSeq.cons(y.intValue(), () ->
                g($(10).multiply(q).multiply(i).multiply($(2).multiply(i).subtract($(1))),
                  $(10).multiply(u).multiply(q.multiply($(5).multiply(i).subtract($(2))).add(r).subtract(y.multiply(t))),
                  t.multiply(u),
                  i.add($(1))));
        }
    }

    @Test
    public void fibonacciTest() {
        int[] expected = {1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144};
        assertSeqEquals(expected, new Fibs().values() );
        assertSeqEquals(expected, Fibs.generate() );
    }

    @Test
    public void primeTest() {
        int[] expected = {2, 3, 5, 7, 11, 13, 17, 19, 23, 29};
        assertSeqEquals(expected, new Primes().values() );
    }

    @Test
    public void piTest() {
        int[] expected = {3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9};
        assertSeqEquals(expected, PI.generate() );
    }

    private static void assertSeqEquals(int[] expected, IntSeq xs) {
        for (int x : expected) {
            assertEquals(x, xs.head());
            xs = xs.tail();
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
                this.pairs = select.
                             from(IntSeq.rangeClosed(low, high), a ->
                             from(IntSeq.rangeClosed(a,   high), b ->
                             yield(Tuple.pair(a, b))));
            }

            private Seq<Pair<Integer>> S(int s) {
                return pairs.filter(on(Tuple_((a,b) -> a+b == s)));
            }

            private Seq<Pair<Integer>> P(int p) {
                return pairs.filter(on(Tuple_((a,b) -> a*b == p)));
            }

            private <T> boolean unique(Seq<T> s) {
                return with(s).<Boolean>get().when(Single(x -> true)).orElse(false);
            }

            private boolean MrP_dont_know(int p) {
                return !unique(P(p));
            }

            private boolean MrS_knew_MrP_dont_know(int s) {
                return S(s).allMatch(on(Tuple_((a,b) -> MrP_dont_know(a*b))));
            }

            private boolean MrP_now_know(int p) {
                return unique(P(p).filter(on(Tuple_((a,b) -> MrS_knew_MrP_dont_know(a+b)))));
            }

            private boolean MrS_now_know(int s) {
                return unique(S(s).filter(on(Tuple_((a,b) -> MrP_now_know(a*b)))));
            }

            private Seq<Pair<Integer>> solve() {
                return select.
                       from(IntSeq.rangeClosed(low, high), x ->
                       from(IntSeq.rangeClosed(x,   high), y ->
                       where(MrP_dont_know(x*y) &&
                             MrS_knew_MrP_dont_know(x+y) &&
                             MrP_now_know(x*y) &&
                             MrS_now_know(x+y),
                       yield(Tuple.pair(x, y)))));
            }
        }

        Pair<Integer> result = new Puzzle(2, 99).solve().head();
        assertEquals(Tuple.pair(4, 13), result);
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
        Seq<ImmutableList<Integer>> result = select.
            from(IntSeq.rangeClosed(1, 5), baker ->
            from(IntSeq.rangeClosed(1, 5), cooper ->
            from(IntSeq.rangeClosed(1, 5), fletcher ->
            from(IntSeq.rangeClosed(1, 5), miller ->
            from(IntSeq.rangeClosed(1, 5), smith ->
            where(distinct(baker, cooper, fletcher, miller, smith) &&
                  baker != 5 && cooper != 1 &&
                  fletcher != 5 && fletcher != 1 &&
                  miller > cooper &&
                  Math.abs(smith - fletcher) != 1 &&
                  Math.abs(fletcher - cooper) != 1,
            yield(ImmutableList.of(baker, cooper, fletcher, miller, smith))))))));

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

    // ----------------------------------------------------------------------

    static final class TrampolineTest {
        private TrampolineTest() {}

        static Trampoline<BigInteger> factorial(int n) {
            if (n <= 1) {
                return immediate(BigInteger.ONE);
            } else {
                return suspend(() -> factorial(n - 1), x ->
                       immediate(x.multiply(BigInteger.valueOf(n))));
            }
        }

        static Trampoline<BigInteger> fibonacci(int n) {
            if (n <= 2) {
                return immediate(BigInteger.ONE);
            } else {
                return suspend(() -> fibonacci(n - 1), a ->
                       suspend(() -> fibonacci(n - 2), b ->
                       immediate(a.add(b))));
            }
        }

        static Trampoline<BigInteger> fibzipper(int n) {
            if (n <= 2) {
                return immediate(BigInteger.ONE);
            } else {
                return Trampoline.zip(
                       suspend(() -> fibzipper(n - 1)),
                       suspend(() -> fibzipper(n - 2)),
                       BigInteger::add);
            }
        }

        static Trampoline<BigInteger> fibtailcall(int n) {
            return fibtailcall(n, BigInteger.ONE, BigInteger.ONE);
        }

        private static Trampoline<BigInteger> fibtailcall(int n, BigInteger a, BigInteger b) {
            return n <= 2
                   ? immediate(b)
                   : suspend(() -> fibtailcall(n - 1, b, a.add(b)));
        }

        // This is not a trampoline test case, just demonstrate to use memoization
        // to improve performance
        static BigInteger fibmemo(int n) {
            TreeMap<Integer, BigInteger> memo = TreeMap.empty();
            memo = memo.put(1, BigInteger.ONE).put(2, BigInteger.ONE);
            return fibmemo_(n).eval(memo);
        }

        private static MonadState<BigInteger, TreeMap<Integer, BigInteger>> fibmemo_(int n) {
            return do_(MonadState.get(), memo ->
                       memo.containsKey(n)
                       ? MonadState.pure(memo.get(n))
                       : do_(fibmemo_(n - 1), (BigInteger a) ->
                         do_(fibmemo_(n - 2), (BigInteger b) ->
                         let(a.add(b), c ->
                         do_(updateMemo(n, c),
                         do_(MonadState.pure(c)))))));
        }

        private static MonadState<Unit, TreeMap<Integer, BigInteger>> updateMemo(int n, BigInteger c) {
            return MonadState.modify(memo -> {
                memo = memo.put(n, c);
                assertTrue(memo.valid());
                return memo;
            });
        }
    }

    @Test(timeout = 10000)
    public void trampolineTest() {
        assertEquals(BigInteger.valueOf(3628800), TrampolineTest.factorial(10).run());
        assertEquals(BigInteger.valueOf(144), TrampolineTest.fibonacci(12).run());
        assertEquals(BigInteger.valueOf(144), TrampolineTest.fibzipper(12).run());
        assertEquals(BigInteger.valueOf(144), TrampolineTest.fibtailcall(12).run());
        assertEquals(BigInteger.valueOf(144), TrampolineTest.fibmemo(12));

        // tail call has constant stack usage and should not overflow
        TrampolineTest.fibtailcall(10000).run();

        // memoization should improve performance dramatically
        TrampolineTest.fibmemo(100);
    }

    // ----------------------------------------------------------------------

    static final class CallCC {
        private CallCC() {}

        static Cont<String> ask(String name) {
            return Cont.callCC(exit ->
                do_(exit.escapeIf(name.isEmpty(), "You forget to tell me your name!"),
                do_(exit.escapeIf(name.equals("martin"), "Welcome to earth, uncle martin"),
                do_(Cont.yield("Welcome, " + name + "!")))));
        }

        static Cont<Double> harmsum(IntSeq nums) {
            return Cont.callCC(exit ->
                Cont.foldM(0.0, nums.boxed(), (r, x) -> do_(
                    exit.escapeIf(x == 0, 0.0),
                    Cont.yield(r + 1.0 / x))
                ));
        }
    }

    @Test
    public void callCCTest() {
        CallCC.ask("bob").exec(msg -> assertEquals("Welcome, bob!", msg));
        CallCC.ask("").exec(msg -> assertEquals("You forget to tell me your name!", msg));

        assertEquals("Welcome, bob!", CallCC.ask("bob").eval());
        assertEquals("Welcome to earth, uncle martin", CallCC.ask("martin").eval());
        assertEquals("You forget to tell me your name!", CallCC.ask("").eval());

        double expected = IntSeq.of(1, 2, 3, 4).boxed().foldLeft(0.0, (Double r, Integer x) -> r + 1.0 / x);
        double actual = CallCC.harmsum(IntSeq.of(1, 2, 3, 4)).eval();
        assertTrue(Double.compare(expected, actual) == 0);
        assertTrue(Double.compare(0.0, CallCC.harmsum(IntSeq.of(1, 2, 0, 4)).eval()) == 0);
    }

    // This implementation is ugly because it uses mutable state
    static final class UglyCPS {
        private Seq<Function<Integer, Unit>> queue = Seq.nil();
        private Seq<String> log = Seq.nil();

        public void go() {
            Cont.reset(
                do_(log("Welcome!"),
                do_(ask("Please give me a number"), first ->
                do_(ask("Please enter another number"), second ->
                do_(log("The sum of your numbers is: " + (first + second))))))
            ).eval();
        }

        private Cont<Integer> ask(String prompt) {
            return Cont.shift((Function<Integer, Unit> k) -> {
                queue = queue.append(k);
                return log(prompt);
            });
        }

        public void resume(int n) {
            Function<Integer, Unit> k = queue.head();
            queue = queue.tail();
            log = Seq.nil();
            k.apply(n);
        }

        private Cont<Unit> log(String message) {
            return Cont.action(() -> log = log.append(message));
        }

        public Seq<String> getLog() {
            return log;
        }
    }

    @Test
    public void resetShiftTest() {
        UglyCPS cps = new UglyCPS();

        cps.go();
        assertSeqEquals(cps.getLog(), "Welcome!", "Please give me a number");

        cps.resume(3);
        assertSeqEquals(cps.getLog(), "Please enter another number");

        cps.resume(5);
        assertSeqEquals(cps.getLog(), "The sum of your numbers is: 8");
    }

    // This implementation is good because state transformation is transparency
    static class GoodCPS {
        private final Seq<Function<Integer, MonadState<Unit, GoodCPS>>> queue;
        private final Seq<String> log;

        public GoodCPS() {
            this(Seq.nil(), Seq.nil());
        }

        private GoodCPS(Seq<Function<Integer, MonadState<Unit, GoodCPS>>> queue, Seq<String> log) {
            this.queue = queue;
            this.log = log;
        }

        public static GoodCPS go() {
            return StateCont.reset(
                do_(log("Welcome!"),
                do_(ask("Please give me a number"), first ->
                do_(ask("Please enter another number"), second ->
                do_(log("The sum of your numbers is: " + (first + second))))))
            ).eval().exec(new GoodCPS());
        }

        private static StateCont<Integer, GoodCPS> ask(String prompt) {
            return StateCont.<Integer, Unit, GoodCPS>shift(k ->
                do_(StateCont.modify(state -> state.save(k)),
                do_(log(prompt))));
        }

        private static StateCont<Unit, GoodCPS> log(String message) {
            return StateCont.modify(state -> state.addLog(message));
        }

        public GoodCPS resume(int n) {
            return queue.head().apply(n).exec(new GoodCPS(queue.tail(), Seq.nil()));
        }

        public Seq<String> getLog() {
            return log;
        }

        private GoodCPS save(Function<Integer, MonadState<Unit, GoodCPS>> f) {
            return new GoodCPS(queue.append(f), log);
        }

        private GoodCPS addLog(String message) {
            return new GoodCPS(queue, log.append(message));
        }
    }

    @Test
    public void resetShiftStateTest() {
        GoodCPS cps = GoodCPS.go();
        assertSeqEquals(cps.getLog(), "Welcome!", "Please give me a number");

        cps = cps.resume(3);
        assertSeqEquals(cps.getLog(), "Please enter another number");

        cps = cps.resume(5);
        assertSeqEquals(cps.getLog(), "The sum of your numbers is: 8");
    }

    // ----------------------------------------------------------------------

    @Test
    public void functionBindTest() {
        Function<Integer, Integer> f = x -> x * 5;
        Function<Integer, Integer> g = x -> x + 10;
        Function<Integer, Integer> h = Fn.bind(f, x -> Fn.bind(g, y -> Fn.pure(x + y))); // x -> (x*5)+(x+10)
        assertEquals(10, (int)h.apply(0));
        assertEquals(16, (int)h.apply(1));
        assertEquals(22, (int)h.apply(2));
    }

    @Test
    public void functionMapTest() {
        Function<Integer, Integer> f = x -> x * 5;
        Function<Integer, Integer> g = x -> x + 10;
        Function<Integer, Integer> h = Fn.map(f, g); // x->(x*5)+10
        assertEquals(10, (int)h.apply(0));
        assertEquals(15, (int)h.apply(1));
        assertEquals(20, (int)h.apply(2));
    }

    @Test
    public void functionZipTest() {
        Function<Integer, Integer> f = x -> x * 5;
        Function<Integer, Integer> g = x -> x + 10;
        Function<Integer, Integer> h = Fn.zip(f, g, Integer::sum); // x->(x*5)+(x+10)
        assertEquals(10, (int)h.apply(0));
        assertEquals(16, (int)h.apply(1));
        assertEquals(22, (int)h.apply(2));
    }

    @Test
    public void functionFlatMTest() {
        Seq<Function<Integer, Integer>> fs = Seq.of(x -> x * 5, x -> x + 10);
        Function<Integer, Seq<Integer>> h = Fn.flatM(fs);
        assertSeqEquals(h.apply(0), 0, 10);
        assertSeqEquals(h.apply(1), 5, 11);
        assertSeqEquals(h.apply(2), 10, 12);
    }

    private static <T> void assertSeqEquals(Seq<T> xs, Object... expected) {
        for (Object x : expected) {
            assertEquals(x, xs.head());
            xs = xs.tail();
        }
        assertTrue(xs.isEmpty());
    }
}
