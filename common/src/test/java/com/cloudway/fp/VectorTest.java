/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp;

import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;

import com.cloudway.fp.data.Fn;
import com.cloudway.fp.data.Pair;
import com.cloudway.fp.data.Seq;
import com.cloudway.fp.data.Tuple;
import com.cloudway.fp.data.Vector;

public class VectorTest {
    private static final int N = 100;
    private Vector<Integer> vec;

    @Before
    public void init() {
        vec = Vector.iterate(N, i -> i);
    }

    @Test
    public void at() {
        for (int i = 0; i < N; i++) {
            assertEquals(i, (int)vec.at(i));
        }
    }

    @Test
    public void update() {
        Vector<Integer> xs = vec;
        for (int i = 0; i < N; i++) {
            xs = xs.update(i, i * 10);
        }
        for (int i = 0; i < N; i++) {
            assertEquals("update " + i, i*10, (int)xs.at(i));
        }
    }

    @Test
    public void modify() {
        Vector<Integer> xs = vec;
        for (int i = 0; i < N; i++) {
            xs = xs.modify(i, x -> x * 10);
        }
        for (int i = 0; i < N; i++) {
            assertEquals("modify " + i, i*10, (int)xs.at(i));
        }
    }

    @Test
    public void delete() {
        for (int i = 0; i < N; i++) {
            Vector<Integer> xs = vec.delete(i);
            assertEquals("delete " + i, N - 1, xs.size());
            for (int j = 0; j < i; j++) {
                assertEquals("delete " + i, j, (int)xs.at(j));
            }
            for (int j = i+1; j < N; j++) {
                assertEquals("delete " + i, j, (int)xs.at(j - 1));
            }
        }
    }

    @Test
    public void insert() {
        for (int i = 0; i <= N; i++) {
            Vector<Integer> xs = vec.insert(i, 9999);
            assertEquals("insert " + i, N + 1, xs.size());
            for (int j = 0; j < i; j++) {
                assertEquals("insert " + i, j, (int)xs.at(j));
            }
            assertEquals(9999, (int)xs.at(i));
            for (int j = i; j < N; j++) {
                assertEquals("insert " + i, j, (int)xs.at(j + 1));
            }
        }
    }

    @Test
    public void splitAt() {
        for (int i = 0; i <= N; i++) {
            Pair<Vector<Integer>> sp = vec.splitAt(i);
            assertEquals("splitAt " + i, i, sp.first().size());
            assertEquals("splitAt " + i, N - i, sp.second().size());
            for (int j = 0; j < i; j++) {
                assertEquals("splitAt " + i, j, (int)sp.first().at(j));
            }
            for (int j = i; j < N; j++) {
                assertEquals("splitAt " + i, j, (int)sp.second().at(j - i));
            }
        }
    }

    @Test
    public void take() {
        for (int i = 0; i <= N; i++) {
            Vector<Integer> xs = vec.take(i);
            assertEquals("take " + i, i, xs.size());
            for (int j = 0; j < i; j++) {
                assertEquals("take " + i, j, (int)xs.at(j));
            }
        }
    }

    @Test
    public void drop() {
        for (int i = 0; i <= N; i++) {
            Vector<Integer> xs = vec.drop(i);
            assertEquals("drop " + i, N - i, xs.size());
            for (int j = i; j < N; j++) {
                assertEquals("drop " + i, j, (int)xs.at(j - i));
            }
        }
    }

    @Test
    public void reverse() {
        Vector<Integer> xs = vec.reverse();
        for (int i = 0; i < N; i++) {
            assertEquals(N - i - 1, (int)xs.at(i));
        }
    }

    @Test
    public void map() {
        Vector<String> xs = vec.map(i -> "a" + i);
        assertEquals(N, xs.size());
        for (int i = 0; i < N; i++) {
            assertEquals("a" + i, xs.at(i));
        }
    }

    @Test
    public void filter() {
        Vector<Integer> xs = vec.filter(i -> i % 2 == 0);
        assertEquals(N / 2, xs.size());
        for (int i = 0; i < N / 2; i++) {
            assertEquals(i * 2, (int)xs.at(i));
        }
    }

    @Test
    public void foldLeft() {
        Seq<Integer> xs = vec.foldLeft(Fn.flip(Seq::cons), Seq::nil);
        for (int i = N; --i >= 0; ) {
            assertEquals(i, (int)xs.head());
            xs = xs.tail();
        }
        assertTrue(xs.isEmpty());
    }

    @Test
    public void foldLeft_() {
        int sum = vec.foldLeft(0, Integer::sum);
        assertEquals(N * (N - 1) / 2, sum);
    }

    @Test
    public void foldRight() {
        Seq<Integer> xs = vec.foldRight(Seq.nil(), Seq::cons);
        for (int i = 0; i < N; i++) {
            assertEquals(i, (int)xs.head());
            xs = xs.tail();
        }
        assertTrue(xs.isEmpty());
    }

    @Test
    public void foldRight_() {
        int sum = vec.foldRight_(0, Integer::sum);
        assertEquals(N * (N - 1) / 2, sum);
    }

    @Test
    public void foldLeftWithIndex() {
        Seq<Tuple<Integer,Integer>> xs = vec.foldLeftWithIndex((r, i, x) -> Seq.cons(Tuple.of(i, x), r), Seq::nil);
        for (int i = N; --i >= 0; ) {
            assertEquals(Tuple.of(i, i), xs.head());
            xs = xs.tail();
        }
        assertTrue(xs.isEmpty());
    }

    @Test
    public void foldLeftWithIndex_() {
        Seq<Tuple<Integer,Integer>> xs = vec.foldLeftWithIndex_(Seq.nil(), (r, i, x) -> Seq.cons(Tuple.of(i, x), r));
        for (int i = N; --i >= 0; ) {
            assertEquals(Tuple.of(i, i), xs.head());
            xs = xs.tail();
        }
        assertTrue(xs.isEmpty());
    }

    @Test
    public void foldRightWithIndex() {
        Seq<Tuple<Integer,Integer>> xs = vec.foldRightWithIndex((i, x, r) -> Seq.cons(Tuple.of(i, x), r), Seq::nil);
        for (int i = 0; i < N; i++) {
            assertEquals(Tuple.of(i, i), xs.head());
            xs = xs.tail();
        }
        assertTrue(xs.isEmpty());
    }

    @Test
    public void foldRightWithIndex_() {
        Seq<Tuple<Integer,Integer>> xs = vec.foldRightWithIndex_(Seq.nil(), (i, x, r) -> Seq.cons(Tuple.of(i, x), r));
        for (int i = 0; i < N; i++) {
            assertEquals(Tuple.of(i, i), xs.head());
            xs = xs.tail();
        }
        assertTrue(xs.isEmpty());
    }

    @Test
    public void indexOf() {
        for (int i = 0; i < N; i++) {
            assertEquals(i, vec.indexOf(i));
        }
        assertEquals(-1, vec.indexOf(-1));
        assertEquals(-1, vec.indexOf(N));
    }

    @Test
    public void lastIndexOf() {
        for (int i = 0; i < N; i++) {
            assertEquals(i, vec.lastIndexOf(i));
        }
        assertEquals(-1, vec.lastIndexOf(-1));
        assertEquals(-1, vec.lastIndexOf(N));
    }

    @Test
    public void contains() {
        for (int i = 0; i < N; i++) {
            assertTrue(vec.contains(i));
        }
        assertFalse(vec.contains(-1));
        assertFalse(vec.contains(N));
    }
}
