/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp;

import java.util.function.Function;
import java.util.stream.Stream;

import org.junit.Test;
import static java.util.stream.Collectors.joining;
import static org.junit.Assert.assertEquals;

import com.cloudway.fp.data.Maybe;
import com.cloudway.fp.data.Seq;
import com.cloudway.fp.data.Tuple;

import static com.cloudway.fp.control.Conditionals.*;
import static com.cloudway.fp.control.Syntax.*;
import static com.cloudway.fp.data.Maybe.Just;
import static com.cloudway.fp.data.Tuple.Tuple_;

// @formatter:off
public class ComprehensionTest
{
    @Test
    public void comprehensionFromMaybe() {
        Maybe<String> empty = Maybe.empty();
        assertEquals(Maybe.of("hello, world"), concatMaybes("hello", "world"));
        assertEquals(empty, concatMaybes("hello", null));
        assertEquals(empty, concatMaybes("hello", ""));
        assertEquals(empty, concatMaybes(null, "world"));
        assertEquals(empty, concatMaybes("", "world"));
        assertEquals(empty, concatMaybes(null, null));
        assertEquals(empty, concatMaybes("", ""));
        assertEquals(empty, concatMaybes(null, ""));
        assertEquals(empty, concatMaybes("", null));
    }

    static Maybe<String> concatMaybes(String first, String second) {
        return select.
               from(Maybe.ofNullable(first), (String a) ->
               from(Maybe.ofNullable(second), (String b) ->
               where(!a.isEmpty() && !b.isEmpty(),
               yield(a + ", " + b))));
    }

    @Test
    public void comprehensionFromFunction() {
        assertEquals(19, (int)addStuff(x -> x * 2, x -> x + 10).apply(3));
    }

    static Function<Integer, Integer> addStuff(Function<Integer,Integer> first, Function<Integer,Integer> second) {
        return select.from(first, a -> from(second, b -> yield(a + b)));
    }

    @Test
    public void comprehensionPatternMatchingMaybes() {
        @SuppressWarnings("unchecked") Maybe<String>[] source = new Maybe[] {
            Maybe.of("hello"), Maybe.empty(), Maybe.of("world"), Maybe.of("")
        };
        String result;

        result = select.from(Stream.of(source), as(Just(x ->
                        where(!x.isEmpty(),
                        yield(x)))))
                       .collect(joining(", "));
        assertEquals("hello, world", result);

        result = select.from(Seq.of(source), as(Just(x ->
                        where(!x.isEmpty(),
                        yield(x)))))
                       .show(", ", "", "");
        assertEquals("hello, world", result);

        result = Stream.of(source)
                       .filter(on(Just((String x) -> !x.isEmpty())))
                       .map(Maybe::get)
                       .collect(joining(", "));
        assertEquals("hello, world", result);
    }

    @Test
    public void comprehensionPatternMatchingTuples() {
        @SuppressWarnings("unchecked") Tuple<String, Integer>[] source = new Tuple[] {
            Tuple.of("a", 1), Tuple.of("b", 2), Tuple.of("c", 3)
        };
        String result;

        result = select.from(Stream.of(source), as(Tuple_((a, b) ->
                        yield(a + ":" + b))))
                       .collect(joining(","));
        assertEquals("a:1,b:2,c:3", result);

        result = select.from(Seq.of(source), as(Tuple_((a, b) ->
                        yield(a + ":" + b))))
                       .show(",", "", "");
        assertEquals("a:1,b:2,c:3", result);
    }

    @Test
    public void comprehensionPatternMatchingMaybeTuples() {
        @SuppressWarnings("unchecked") Maybe<Tuple<String,Integer>>[] source = new Maybe[] {
            Maybe.of(Tuple.of("a", 1)), Maybe.empty(), Maybe.of(Tuple.of("c", 3))
        };
        String result;

        result = select.from(Stream.of(source), as(in(Just(Tuple_((a,b) ->
                        yield(a + ":" + b))))))
                       .collect(joining(","));
        assertEquals("a:1,c:3", result);

        result = select.from(Seq.of(source), as(in(Just(Tuple_((a,b) ->
                        yield(a + ":" + b))))))
                       .show(",", "", "");
        assertEquals("a:1,c:3", result);
    }
}
