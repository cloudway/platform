/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util;

import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Stream;

import org.junit.Test;
import static java.util.stream.Collectors.joining;
import static org.junit.Assert.assertEquals;

import com.cloudway.platform.common.fp.data.Seq;
import com.cloudway.platform.common.fp.data.Tuple;

import static com.cloudway.platform.common.fp.control.Conditionals.*;
import static com.cloudway.platform.common.fp.control.Comprehension.*;
import static com.cloudway.platform.common.fp.data.Optionals.Just;
import static com.cloudway.platform.common.fp.data.Tuple.Tuple_;

// @formatter:off
public class ComprehensionTest
{
    @Test
    public void comprehensionFromOptional() {
        Optional<String> empty = Optional.empty();
        assertEquals(Optional.of("hello, world"), concatOptionals("hello", "world"));
        assertEquals(empty, concatOptionals("hello", null));
        assertEquals(empty, concatOptionals("hello", ""));
        assertEquals(empty, concatOptionals(null, "world"));
        assertEquals(empty, concatOptionals("", "world"));
        assertEquals(empty, concatOptionals(null, null));
        assertEquals(empty, concatOptionals("", ""));
        assertEquals(empty, concatOptionals(null, ""));
        assertEquals(empty, concatOptionals("", null));
    }

    static Optional<String> concatOptionals(String first, String second) {
        return select.
               from(Optional.ofNullable(first), (String a) ->
               from(Optional.ofNullable(second), (String b) ->
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
    public void comprehensionPatternMatchingOptionals() {
        @SuppressWarnings("unchecked") Optional<String>[] source = new Optional[] {
            Optional.of("hello"), Optional.empty(), Optional.of("world"), Optional.of("")
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
                       .map(Optional::get)
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
    public void comprehensionPatternMatchingOptionalTuples() {
        @SuppressWarnings("unchecked") Optional<Tuple<String,Integer>>[] source = new Optional[] {
            Optional.of(Tuple.of("a", 1)), Optional.empty(), Optional.of(Tuple.of("c", 3))
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
