/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Random;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import org.junit.Before;
import org.junit.Test;
import static java.util.Comparator.*;
import static org.junit.Assert.*;

import com.cloudway.fp.data.Maybe;
import com.cloudway.fp.data.MutablePMap;
import com.cloudway.fp.data.IntSeq;
import com.cloudway.fp.data.PMap;
import com.cloudway.fp.data.PSet;
import com.cloudway.fp.data.Seq;
import com.cloudway.fp.data.TreePMap;
import com.cloudway.fp.data.TreePSet;

public class TreeTest {
    private PMap<String, Integer> tm;
    private Integer[] data;

    private static final String KEY = "42";

    @Before
    public void initialize() {
        data = shuffle(200);
        tm = TreePMap.empty();
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

    private <K, V> PMap<K,V> validate(PMap<K,V> pm) {
        TreePMap<K,V> tm = (TreePMap<K,V>)pm;
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
        testEmptyMap(TreePMap.<String, Integer>empty());
        testEmptyMap(TreePMap.<String, Integer>empty(reverseOrder()));
    }

    private static void testEmptyMap(PMap<String, Integer> m) {
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
        PMap<Integer, Object> tm = TreePMap.empty();
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
        PMap<String, Integer> orig = tm;
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
        assertTrue(tm.containsAll(TreePMap.empty()));
        assertTrue(tm.containsAll(TreePMap.singleton("1", 1)));

        PMap<String, Integer> m2 = TreePMap.empty();
        for (int i = 1; i <= 10; i++) {
            m2 = m2.put(String.valueOf(i), i);
        }
        assertTrue(tm.containsAll(m2));

        m2 = TreePMap.empty();
        for (int i = 0; i < 10; i++) {
            int x = data.length - 1 - i;
            m2 = m2.put(String.valueOf(x), x);
        }
        assertTrue(tm.containsAll(m2));
    }

    @Test
    public void test_containsAll_negative() {
        assertFalse(TreePMap.<String, Integer>empty().containsAll(tm));
        assertFalse(tm.containsAll(TreePMap.singleton("-1", -1)));

        PMap<String, Integer> m2 = TreePMap.empty();
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
        assertSame(tm, tm.putAll(TreePMap.empty()));
        assertSame(tm, TreePMap.<String, Integer>empty().putAll(tm));

        PMap<String, Integer> m2 = TreePMap.empty();
        for (int i = 0; i < 10; i++) {
            m2 = m2.put(String.valueOf(i), i);
        }

        PMap<String, Integer> m12 = validate(tm.putAll(m2));
        PMap<String, Integer> m21 = validate(m2.putAll(tm));

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
        PMap<String, Integer> m2 = TreePMap.empty();
        for (int i = 0; i < 10; i++) {
            m2 = m2.put(String.valueOf(i), i * 2);
        }

        PMap<String, Integer> m12 = validate(tm.putAll(m2));
        PMap<String, Integer> m21 = validate(m2.putAll(tm));

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
        PMap<String, Integer> m2 = TreePMap.empty();
        for (int i = -5; i <= 5; i++) {
            m2 = m2.put(String.valueOf(i), i);
        }

        PMap<String, Integer> m12 = validate(tm.putAll(m2));
        PMap<String, Integer> m21 = validate(m2.putAll(tm));

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
        PMap<String, Integer> actual = TreePMap.empty();
        for (int i = -(data.length - 1); i < data.length; i++) {
            actual = validate(actual.compute(String.valueOf(i), (k, v) -> {
                // remove odd and double even
                if (v.isPresent()) {
                    return v.filter(x -> x % 2 == 0).map(x -> x * 2);
                } else {
                    int x = Integer.parseInt(k);
                    return x % 2 == 0 ? Maybe.of(x * 2) : Maybe.empty();
                }
            }));
        }

        PMap<String, Integer> expected = TreePMap.empty();
        for (int i = 0; i < data.length; i += 2) {
            expected = expected.put(String.valueOf(i), i * 2)
                               .put(String.valueOf(-i), -i * 2);
        }

        assertEquals(expected, actual);
    }

    @Test
    public void test_merge() {
        String message = "the quick brown fox jumps over the lazy dog";
        PMap<Character, Integer> actual = Seq.wrap(message).filter(c -> c != ' ')
            .foldLeft(TreePMap.empty(), (m, c) -> m.merge(c, 1, Integer::sum));
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
        PMap<String, Integer> expected = TreePMap.empty();
        for (int i = 0; i < data.length; i++) {
            expected = expected.put(String.valueOf(i), i * 2);
        }

        assertEquals(expected, validate(tm.map(x -> x * 2)));
    }

    @Test
    public void test_filter() {
        PMap<String, Integer> expected = TreePMap.empty();
        for (int i = 0; i < data.length; i += 2) {
            expected = expected.put(String.valueOf(i), i);
        }

        assertEquals(expected, validate(tm.filter(x -> x % 2 == 0)));
    }

    @Test
    public void test_keySet() {
        Set<String> expected = Stream.of(data).map(String::valueOf).collect(Collectors.toSet());
        PSet<String> actual = tm.keySet();

        assertEquals(expected.size(), actual.size());
        for (String k : expected)
            assertTrue(actual.contains(k));
        for (String k : actual)
            assertTrue(expected.contains(k));
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
    public void test_entries() {
        tm.entries().forEach(t -> {
            assertSame(tm.get(t.getKey()), t.getValue());
        });
    }

    @Test
    public void test_iterator() {
        HashMap<String, Integer> expected = new HashMap<>();
        for (int i : data) {
            expected.put(String.valueOf(i), i);
        }

        HashMap<String, Integer> actual = new HashMap<>();
        for (Map.Entry<String,Integer> e : tm) {
            actual.put(e.getKey(), e.getValue());
        }

        assertEquals(expected, actual);
    }

    @Test
    public void test_navigation() {
        TreePMap<Integer, Integer> tm = (TreePMap<Integer,Integer>)
            IntSeq.rangeClosed(0, 100, 2).boxed()
                  .foldLeft(TreePMap.<Integer,Integer>empty(), (m, x) -> m.put(x, x));

        assertEntryEquals(0, 0, tm.firstEntry());
        assertEquals(Integer.valueOf(0), tm.firstKey());
        assertEntryEquals(100, 100, tm.lastEntry());
        assertEquals(Integer.valueOf(100), tm.lastKey());

        assertEntryEquals(40, 40, tm.lowerEntry(42));
        assertEquals(Maybe.of(40), tm.lowerKey(42));
        assertEntryEquals(40, 40, tm.lowerEntry(41));
        assertEquals(Maybe.of(40), tm.lowerKey(41));
        assertEntryEquals(42, 42, tm.floorEntry(42));
        assertEquals(Maybe.of(42), tm.floorKey(42));
        assertEntryEquals(40, 40, tm.floorEntry(41));
        assertEquals(Maybe.of(40), tm.floorKey(41));
        assertEntryEquals(42, 42, tm.ceilingEntry(42));
        assertEquals(Maybe.of(42), tm.ceilingKey(42));
        assertEntryEquals(42, 42, tm.ceilingEntry(41));
        assertEquals(Maybe.of(42), tm.ceilingKey(41));
        assertEntryEquals(44, 44, tm.higherEntry(42));
        assertEquals(Maybe.of(44), tm.higherKey(42));
        assertEntryEquals(42, 42, tm.higherEntry(41));
        assertEquals(Maybe.of(42), tm.higherKey(41));

        assertEquals(Maybe.<Map.Entry<Integer,Integer>>empty(), tm.lowerEntry(0));
        assertEquals(Maybe.<Integer>empty(), tm.lowerKey(0));
        assertEquals(Maybe.<Map.Entry<Integer,Integer>>empty(), tm.higherEntry(100));
        assertEquals(Maybe.<Integer>empty(), tm.higherKey(100));
    }

    static <K,V> void assertEntryEquals(K k, V v, Map.Entry<K,V> e) {
        assertEquals(k, e.getKey());
        assertEquals(v, e.getValue());
    }

    static <K,V> void assertEntryEquals(K k, V v, Maybe<Map.Entry<K,V>> e) {
        assertTrue(e.isPresent());
        assertEquals(k, e.get().getKey());
        assertEquals(v, e.get().getValue());
    }

    @Test
    public void test_MultableTreeMap() {
        List<Character> vowels = Arrays.asList('a', 'e', 'i', 'o', 'u');

        MutablePMap<Character, Integer> actual = new MutablePMap<Character, Integer>(TreePMap.empty());
        for (char c = 'a'; c <= 'z'; c++) {
            actual.put(c, (int)c);
        }
        actual.keySet().removeAll(vowels);

        HashMap<Character, Integer> expected = new HashMap<>();
        for (char c = 'a'; c <= 'z'; c++) {
            expected.put(c, (int)c);
        }
        expected.keySet().removeAll(vowels);

        assertEquals(expected.size(), actual.size());
        expected.forEach((k, v) -> {
            assertTrue(actual.containsKey(k));
            assertEquals(v, actual.get(k));
        });
    }

    @Test
    public void test_set() {
        Random rnd = new Random();
        PSet<Integer> s1 = TreePSet.fromList(Seq.generate(() -> rnd.nextInt(200)).take(100));
        PSet<Integer> s2 = TreePSet.fromList(Seq.generate(() -> rnd.nextInt(200)).take(100));

        PSet<Integer> u12 = s1.union(s2);
        PSet<Integer> u21 = s2.union(s1);
        PSet<Integer> i12 = s1.intersection(s2);
        PSet<Integer> i21 = s2.intersection(s1);
        PSet<Integer> d12 = s1.difference(s2);
        PSet<Integer> d21 = s2.difference(s1);

        assertTrue(u12.containsAll(s1));
        assertTrue(u12.containsAll(s2));
        assertTrue(u21.containsAll(s1));
        assertTrue(u21.containsAll(s2));
        assertTrue(u12.equals(u21));

        assertTrue(s1.containsAll(i12));
        assertTrue(s2.containsAll(i12));
        assertTrue(s1.containsAll(i21));
        assertTrue(s2.containsAll(i21));
        assertTrue(i12.equals(i21));

        assertTrue(s1.containsAll(d12));
        assertTrue(s2.containsAll(d21));
        assertFalse(d12.equals(d21));
    }
}
