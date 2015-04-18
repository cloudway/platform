/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util;

import java.util.Iterator;
import java.util.function.Function;

import org.junit.Test;
import static org.junit.Assert.*;

import com.cloudway.platform.common.fp.control.Cont;
import com.cloudway.platform.common.fp.control.Generator;
import com.cloudway.platform.common.fp.control.StateCont;
import com.cloudway.platform.common.fp.data.Seq;
import com.cloudway.platform.common.fp.data.Tuple;
import com.cloudway.platform.common.fp.data.Unit;
import com.cloudway.platform.common.fp.typeclass.$;

import static com.cloudway.platform.common.fp.control.Conditionals.as;
import static com.cloudway.platform.common.fp.control.StateCont.callCC;
import static com.cloudway.platform.common.fp.control.Syntax.do_;
import static com.cloudway.platform.common.fp.control.Syntax.forever;
import static com.cloudway.platform.common.fp.control.Syntax.when;
import static com.cloudway.platform.common.fp.data.Tuple.Tuple_;

// @formatter:off
public class GeneratorTest {
    static final class YieldTest {
        private YieldTest() {}

        static Generator<Integer> fibonacci() {
            return Cont.generator(fibonacci_(1, 1));
        }

        private static Cont<Integer> fibonacci_(int a, int b) {
            return Cont.yield(a).then(() -> fibonacci_(b, a+b));
        }

        static Generator<Integer> factorial(int max) {
            return Cont.generator(factorial_(1, 1, max));
        }

        private static Cont<Integer> factorial_(int i, int n, int max) {
            return i <= max ? Cont.yield(i*n).then(() -> factorial_(i+1, i*n, max))
                            : Cont.finish();
        }
    }

    static final class StateYieldTest {
        private StateYieldTest() {}

        static Generator<Integer> fibonacci() {
            return StateCont.generator(Tuple.of(1, 1),
              forever(
                do_(StateCont.get(), as(Tuple_((Integer a, Integer b) ->
                do_(StateCont.yield(a),
                do_(StateCont.put(Tuple.of(b, a + b))
              )))))));
        }

        private static Generator<Integer> factorial(int max) {
            return StateCont.generator(Tuple.of(1, 1),
              callCC(exit -> forever(
                do_(StateCont.get(), as(Tuple_((Integer i, Integer n) ->
                    i <= max ? do_(StateCont.yield(i*n),
                               do_(StateCont.put(Tuple.of(i+1, i*n))))
                             : exit.escape(0))
              )))));
        }
    }

    @Test
    public void yieldTest() {
        testFibonacciGenerator(YieldTest.fibonacci());
        testFactorialGenerator(YieldTest.factorial(10));
    }

    @Test
    public void stateYieldTest() {
        testFibonacciGenerator(StateYieldTest.fibonacci());
        testFactorialGenerator(StateYieldTest.factorial(10));
    }

    private static void testFibonacciGenerator(Generator<Integer> fib) {
        Integer[] expected = {1, 1, 2, 3, 5, 8, 13, 21, 34, 55};

        Iterator<Integer> it = fib.iterator();
        for (Integer i : expected) {
            assertTrue(it.hasNext());
            assertEquals(i, it.next());
        }
        assertTrue(it.hasNext());

        assertSeqEquals(fib.asList().take(expected.length), (Object[])expected);
    }

    private static void testFactorialGenerator(Generator<Integer> fac) {
        Integer[] expected = {1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800};

        Iterator<Integer> it = fac.iterator();
        for (Integer i : expected) {
            assertTrue(it.hasNext());
            assertEquals(i, it.next());
        }
        assertFalse(it.hasNext());

        assertSeqEquals(fac.asList(), (Object[])expected);
    }

    private static <T> void assertSeqEquals(Seq<T> xs, Object... expected) {
        for (Object x : expected) {
            assertEquals(x, xs.head());
            xs = xs.tail();
        }
        assertTrue(xs.isEmpty());
    }

    /**
     * Co-routines test case. Ported from a Python co-routine tutorial:
     *   http://eli.thegreenplace.net/2009/08/29/co-routines-as-an-alternative-to-state-machines
     *
     * The problem - serial framing:
     *   We have an endless incoming stream of bytes, from which we need to deduce
     *   structured data frames. That is, we have to find where a frame starts,
     *   where it ends and what is the data it carries. For this purpose we use
     *   a special header value, footer value and an escape byte (DLE).
     */
    static final class CoroutineTest {
        private CoroutineTest() {}

        private static final char HEADER = 0x61;
        private static final char FOOTER = 0x62;
        private static final char DLE    = 0xab;

        private static final Character DUMMY = 0;

        private static Generator<Character> unwrap(Generator.Channel<Seq<Character>> target) {
            return StateCont.generator(Seq.nil(),
              forever( // outer loop looking for a frame header
                readChar(b1 ->
                when(b1 == HEADER,
                  do_(StateCont.put(Seq.nil()),
                  callCC(inner -> forever( // capture the full frame
                    readChar(b2 ->
                    b2 == FOOTER ? sendFrame(target).then(inner.escape(Unit.U)) :
                    b2 == DLE    ? readChar(b3 -> putChar(b3))
                                 : putChar(b2)
              ))))))));
        }

        // utility methods to help type inference

        private static StateCont<Unit, Seq<Character>>
        readChar(Function<Character, $<StateCont.µ<Seq<Character>>, Unit>> f) {
            return StateCont.<Character, Seq<Character>>yield(DUMMY).bind(f);
        }

        private static StateCont<Unit, Seq<Character>>
        putChar(Character c) {
            return StateCont.modify(frame -> frame.append(c));
        }

        private static StateCont<Unit, Seq<Character>>
        sendFrame(Generator.Channel<Seq<Character>> target) {
            return StateCont.<Seq<Character>>get().bind(frame -> StateCont.sendTo(target, frame));
        }

        private static Generator<Seq<Character>> receiver(StringBuilder buffer) {
            return Cont.generator(
              forever(
                do_(Cont.yield((Seq.<Character>nil())), frame ->
                do_(Cont.action(() -> buffer.append(hex(frame))
            )))));
        }

        private static String hex(Seq<Character> cs) {
            return cs.map(Integer::toHexString)
                     .map(c -> c.length() == 1 ? '0' + c : c)
                     .show("", "[", "]");
        }

        public static void testAll() {
            final char[] msg_bytes = {
                0x70, 0x24,
                0x61, 0x99, 0xaf, 0xd1, 0x62,
                0x56, 0x62,
                0x61, 0xab, 0xab, 0x14, 0x62,
                0x07
            };

            StringBuilder buffer = new StringBuilder();
            Generator.Channel<Seq<Character>> receiver = receiver(buffer).start();
            Generator.Channel<Character> unwrapper = unwrap(receiver).start();

            for (char c : msg_bytes) {
                unwrapper.send(c);
            }

            assertEquals("[99afd1][ab14]", buffer.toString());
        }
    }

    @Test
    public void coroutineTest() {
        CoroutineTest.testAll();
    }
}
