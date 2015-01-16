/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util;

import java.util.Arrays;
import java.util.Random;
import java.util.stream.Stream;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMultiset;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.ImmutableSortedSet;
import com.google.common.collect.LinkedHashMultimap;
import com.google.common.collect.Multimap;
import static com.cloudway.platform.common.util.MoreCollectors.*;

import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;

public class MoreCollectorsTest
{
    private static final int DATA_SIZE = 10;
    private Integer[] sample;

    @Before
    public void prepareData() {
        Random r = new Random();
        sample = new Integer[DATA_SIZE];
        Arrays.setAll(sample, x -> Math.abs(r.nextInt()));
    }

    @Test
    public void collectToImmutableList() {
        ImmutableList<Integer> collected = Stream.of(sample).collect(toImmutableList());
        ImmutableList<Integer> constructed = ImmutableList.copyOf(sample);
        assertEquals(collected, constructed);
    }

    @Test
    public void collectToImmutableSet() {
        ImmutableSet<Integer> collected = Stream.of(sample).collect(toImmutableSet());
        ImmutableSet<Integer> constructed = ImmutableSet.copyOf(sample);
        assertEquals(collected, constructed);
    }

    @Test
    public void collectToImmutableSortedSet() {
        ImmutableSortedSet<Integer> collected = Stream.of(sample).collect(toImmutableCollection(ImmutableSortedSet::naturalOrder));
        ImmutableSortedSet<Integer> constructed = ImmutableSortedSet.copyOf(sample);
        assertEquals(collected, constructed);
    }

    @Test
    public void collectToImmutableMultiset() {
        ImmutableMultiset<Integer> collected = Stream.of(sample).collect(toImmutableCollection(ImmutableMultiset::builder));
        ImmutableMultiset<Integer> constructed = ImmutableMultiset.copyOf(sample);
        assertEquals(collected, constructed);
    }

    @Test
    public void collectToMultimap() {
        checkMultimap(Stream.of(sample).collect(toMultimap(i -> i % 3, i -> i)));
    }

    @Test
    public void collectToLinkedMultimap() {
        checkMultimap(Stream.of(sample).collect(toMultimap(i -> i % 3, i -> i, LinkedHashMultimap::create)));
    }

    @Test
    public void classifyToMultimap() {
        checkMultimap(Stream.of(sample).collect(toMultimap(i -> i % 3)));
    }

    @Test
    public void collectToImmutableMultimap() {
        checkMultimap(Stream.of(sample).collect(toImmutableMultimap(i -> i % 3, i -> i)));
    }

    @Test
    public void classifyToImmutableMultimap() {
        checkMultimap(Stream.of(sample).collect(toImmutableMultimap(i -> i % 3)));
    }

    private static void checkMultimap(Multimap<Integer, Integer> map) {
        map.entries().forEach(e -> assertEquals(e.getKey().intValue(), e.getValue().intValue() % 3));
        map.asMap().forEach((k, vs) -> vs.forEach(v -> assertEquals(k.intValue(), v.intValue() % 3)));
    }
}
