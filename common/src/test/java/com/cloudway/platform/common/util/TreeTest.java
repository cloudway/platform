/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.Random;
import java.util.stream.IntStream;

import org.junit.Before;
import org.junit.Test;
import static java.util.Comparator.*;
import static org.junit.Assert.*;

import com.cloudway.platform.common.fp.data.IntSeq;
import com.cloudway.platform.common.fp.data.Seq;
import com.cloudway.platform.common.fp.data.TreeMap;
import com.cloudway.platform.common.fp.data.TreeSet;

public class TreeTest {
    private TreeMap<String, Integer> tm;
    private Integer[] data;

    private static final String KEY = "42";

    @Before
    public void initialize() {
        data = shuffle(1000);
        tm = TreeMap.empty();
        for (Integer x : data) {
            put(x.toString(), x);
        }
    }


    private static Integer[] shuffle(int len) {
        Random rnd = new Random();
        return IntStream.generate(() -> rnd.nextInt(len))
                        .distinct().limit(len)
                        .boxed().toArray(Integer[]::new);
    }

    private <K, V> TreeMap<K,V> validate(TreeMap<K,V> tm) {
        if (!tm.valid()) {
            StringBuilder msg = new StringBuilder();
            msg.append("Internal tree structure corrupted\n\n");
            msg.append("Tree Structure: \n");
            msg.append(tm.showTree());
            msg.append("\nSample Data: \n");
            msg.append(Arrays.toString(data));
            fail(msg.toString());
        }
        return tm;
    }

    private void assertGetFail(String key) {
        try {
            tm.get(key);
            fail("Removed mapping still exist");
        } catch (NoSuchElementException ex) {
            // ok
        }
    }

    private void put(String key, Integer value) {
        validate(tm = tm.put(key, value));
    }

    private void remove(String key) {
        validate(tm = tm.remove(key));
    }

    @Test
    public void test_empty() {
        testEmptyMap(TreeMap.<String, Integer>empty());
        testEmptyMap(TreeMap.<String, Integer>empty(reverseOrder()));
    }

    private static void testEmptyMap(TreeMap<String, Integer> m) {
        assertTrue(m.isEmpty());
        assertEquals(0, m.size());
        assertFalse(m.containsKey(KEY));
    }

    @Test
    public void test_containsKey() {
        assertTrue(tm.containsKey(KEY));
        assertTrue(!tm.containsKey("XXX"));
    }

    @Test
    public void test_get() {
        assertSame(tm.get(data[42].toString()), data[42]);
        assertGetFail("XXX");
    }

    @Test
    public void test_get_same_value() {
        TreeMap<Integer, Object> tm = TreeMap.empty();
        Integer k1 = 1, k2 = 2;
        Object o = new Object();
        tm = tm.put(k1, o).put(k2, o);
        assertSame(o, tm.get(k1));
        assertSame(o, tm.get(k2));
    }

    @Test
    public void test_put() {
        String k1 = "42";   Integer o1 = 1984;
        put(k1, o1);
        assertEquals(data.length, tm.size());
        assertSame(o1, tm.get(k1));

        String k2 = "1984"; Integer o2 = 2046;
        put(k2, o2);
        assertEquals(data.length + 1, tm.size());
        assertEquals(o2, tm.get(k2));
    }

    @Test
    public void validate_remove() {
        for (int i = 0; i < data.length; i++) {
            remove(String.valueOf(i));
        }
        assertTrue(tm.isEmpty());
        assertEquals(0, tm.size());
    }

    @Test
    public void test_remove() {
        remove(KEY);
        assertEquals(data.length - 1, tm.size());
        assertTrue(!tm.containsKey(KEY));
        assertTrue(!tm.lookup(KEY).isPresent());
        assertGetFail(KEY);
    }

    @Test
    public void test_remove_not_exists() {
        TreeMap<String, Integer> orig = tm;
        remove("XXX");
        assertEquals(data.length, tm.size());
        assertEquals(orig, tm);
    }

    @Test
    public void test_putIfAbsent_positive() {
        Integer o = tm.get(KEY);
        tm = validate(tm.putIfAbsent(KEY, 1984));
        assertEquals(data.length, tm.size());
        assertSame(tm.get(KEY), o);
    }

    @Test
    public void test_putIfAbsent_negative() {
        String k = "1984";
        Integer o = 1984;

        assertTrue(!tm.containsKey(k));
        tm = validate(tm.putIfAbsent(k, o));
        assertEquals(data.length + 1, tm.size());
        assertSame(tm.get(k), o);
    }

    @Test
    public void test_containsAll_positive() {
        assertTrue(tm.containsAll(TreeMap.empty()));
        assertTrue(tm.containsAll(TreeMap.singleton("1", 1)));

        TreeMap<String, Integer> m2 = TreeMap.empty();
        for (int i = 1; i <= 10; i++) {
            m2 = m2.put(String.valueOf(i), i);
        }
        assertTrue(tm.containsAll(m2));

        m2 = TreeMap.empty();
        for (int i = 0; i < 10; i++) {
            int x = data.length - 1 - i;
            m2 = m2.put(String.valueOf(x), x);
        }
        assertTrue(tm.containsAll(m2));
    }

    @Test
    public void test_containsAll_negative() {
        assertFalse(TreeMap.<String, Integer>empty().containsAll(tm));
        assertFalse(tm.containsAll(TreeMap.singleton("-1", -1)));

        TreeMap<String, Integer> m2 = TreeMap.empty();
        for (int i = -1; i < 10; i++) {
            m2 = m2.put(String.valueOf(i), i);
        }
        assertFalse(tm.containsAll(m2));

        for (int i = 0; i < 10; i++) {
            int x = data.length - i;
            m2 = m2.put(String.valueOf(x), x);
        }
        assertFalse(tm.containsAll(m2));
    }

    @Test
    public void test_putAll_same_value() {
        assertSame(tm, tm.putAll(TreeMap.empty()));
        assertSame(tm, TreeMap.<String, Integer>empty().putAll(tm));

        TreeMap<String, Integer> m2 = TreeMap.empty();
        for (int i = 0; i < 10; i++) {
            m2 = m2.put(String.valueOf(i), i);
        }

        TreeMap<String, Integer> m12 = validate(tm.putAll(m2));
        TreeMap<String, Integer> m21 = validate(m2.putAll(tm));

        assertEquals(tm, m12);
        assertEquals(tm, m21);
        assertEquals(m12, m21);

        assertTrue(m12.containsAll(tm));
        assertTrue(m12.containsAll(m2));
        assertTrue(m21.containsAll(tm));
        assertTrue(m21.containsAll(m2));
    }

    @Test
    public void test_putAll_diff_value() {
        TreeMap<String, Integer> m2 = TreeMap.empty();
        for (int i = 0; i < 10; i++) {
            m2 = m2.put(String.valueOf(i), i * 2);
        }

        TreeMap<String, Integer> m12 = validate(tm.putAll(m2));
        TreeMap<String, Integer> m21 = validate(m2.putAll(tm));

        assertNotEquals(tm, m12);
        assertEquals(tm, m21);
        assertNotEquals(m12, m21);

        assertFalse(m12.containsAll(tm));
        assertTrue(m12.containsAll(m2));
        assertTrue(m21.containsAll(tm));
        assertFalse(m21.containsAll(m2));
    }

    @Test
    public void test_putAll_diff_key() {
        TreeMap<String, Integer> m2 = TreeMap.empty();
        for (int i = -5; i <= 5; i++) {
            m2 = m2.put(String.valueOf(i), i);
        }

        TreeMap<String, Integer> m12 = validate(tm.putAll(m2));
        TreeMap<String, Integer> m21 = validate(m2.putAll(tm));

        assertNotEquals(tm, m12);
        assertNotEquals(tm, m21);
        assertEquals(m12, m21);

        assertTrue(m12.containsAll(tm));
        assertTrue(m12.containsAll(m2));
        assertTrue(m21.containsAll(tm));
        assertTrue(m21.containsAll(m2));
    }

    @Test
    public void test_compute() {
        TreeMap<String, Integer> actual = TreeMap.empty();
        for (int i = -(data.length - 1); i < data.length; i++) {
            actual = validate(actual.compute(String.valueOf(i), (k, v) -> {
                // remove odd and double even
                if (v.isPresent()) {
                    return v.filter(x -> x % 2 == 0).map(x -> x * 2);
                } else {
                    int x = Integer.parseInt(k);
                    return x % 2 == 0 ? Optional.of(x * 2) : Optional.empty();
                }
            }));
        }

        TreeMap<String, Integer> expected = TreeMap.empty();
        for (int i = 0; i < data.length; i += 2) {
            expected = expected.put(String.valueOf(i), i * 2)
                               .put(String.valueOf(-i), -i * 2);
        }

        assertEquals(expected, actual);
    }

    @Test
    public void test_merge() {
        String message = "the quick brown fox jumps over the lazy dog";
        TreeMap<Character, Integer> actual = Seq.wrap(message).filter(c -> c != ' ')
            .foldLeft(TreeMap.empty(), (m, c) -> m.merge(c, 1, Integer::sum));
        validate(actual);

        HashMap<Character, Integer> expected = Seq.wrap(message).filter(c -> c != ' ')
            .foldLeft(new HashMap<>(), (m, c) -> { m.merge(c, 1, Integer::sum); return m; });

        assertEquals(expected.size(), actual.size());
        for (char c = 'a'; c <= 'z'; c++) {
            assertEquals(expected.get(c), actual.get(c));
        }
    }

    @Test
    public void test_map() {
        TreeMap<String, Integer> expected = TreeMap.empty();
        for (int i = 0; i < data.length; i++) {
            expected = expected.put(String.valueOf(i), i * 2);
        }

        assertEquals(expected, validate(tm.map((k, v) -> v * 2)));
    }

    @Test
    public void test_filter() {
        TreeMap<String, Integer> expected = TreeMap.empty();
        for (int i = 0; i < data.length; i += 2) {
            expected = expected.put(String.valueOf(i), i);
        }

        assertEquals(expected, validate(tm.filter((k, v) -> v % 2 == 0)));
    }

    @Test
    public void test_keySet() {
        TreeSet<String> ks = tm.keySet();
        assertEquals(data.length, ks.size());
        for (Integer x : data) {
            assertTrue(ks.contains(x.toString()));
        }
    }

    @Test
    public void test_keys() {
        Seq<String> ks = tm.keys();
        assertTrue(ks.allMatch(tm::containsKey));
        assertEquals(IntSeq.range(0, data.length).mapToObj(String::valueOf).sorted().toList(), ks.toList());
    }

    @Test
    public void test_values() {
        List<Integer> expected = IntSeq.range(0, data.length).boxed()
            .sorted(comparing(String::valueOf))
            .toList();
        assertEquals(expected, tm.values().toList());
    }

    @Test
    @SuppressWarnings("NumberEquality")
    public void test_entries() {
        tm.entries().forEach(t -> {
            assertSame(tm.get(t.first()), t.second());
        });
    }
}
