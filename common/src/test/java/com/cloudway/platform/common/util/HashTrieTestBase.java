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
import java.util.Random;
import java.util.stream.IntStream;

import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;

import com.cloudway.platform.common.fp.data.HashPMap;
import com.cloudway.platform.common.fp.data.HashPSet;
import com.cloudway.platform.common.fp.data.MutablePMap;
import com.cloudway.platform.common.fp.data.PMap;
import com.cloudway.platform.common.fp.data.PSet;
import com.cloudway.platform.common.fp.data.Seq;

public abstract class HashTrieTestBase {
    interface Key {
        int hashCode();
        boolean equals(Object obj);
    }

    private PMap<Key, Integer> hm;
    private Integer[] data;
    private Key key;

    protected abstract Key newKey(int value);

    @Before
    public void initialize() {
        data = shuffle(200);
        hm = HashPMap.empty();
        key = newKey(42);
        for (Integer x : data) {
            put(newKey(x), x);
        }
    }

    static Integer[] shuffle(int len) {
        Random rnd = new Random();
        return IntStream.generate(() -> rnd.nextInt(len))
                        .distinct().limit(len)
                        .boxed().toArray(Integer[]::new);
    }

    private void assertGetFail(Object key) {
        try {
            hm.get(key);
            fail("Removed mapping still exist");
        } catch (NoSuchElementException ex) {
            // ok
        }
    }

    private void put(Key key, Integer value) {
        hm = hm.put(key, value);
    }

    private void remove(Object key) {
        hm = hm.remove(key);
    }

    @Test
    public void test_empty() {
        PMap<String,Integer> m = HashPMap.empty();
        assertTrue(m.isEmpty());
        assertEquals(0, m.size());
        assertFalse(m.containsKey(key));
    }

    @Test
    public void test_containsKey() {
        assertTrue(hm.containsKey(key));
        assertTrue(!hm.containsKey(newKey(-1)));
        assertTrue(!hm.containsKey("XXX"));
    }

    @Test
    public void test_get() {
        assertSame(data[42], hm.get(newKey(data[42])));
        assertGetFail(newKey(-1));
        assertGetFail("XXX");
    }

    @Test
    public void test_get_same_value() {
        PMap<Integer, Object> m = HashPMap.empty();
        Integer k1 = 1, k2 = 2;
        Object o = new Object();
        m = m.put(k1, o).put(k2, o);
        assertSame(o, m.get(k1));
        assertSame(o, m.get(k2));
    }

    @Test
    public void test_put() {
        Key k1 = newKey(42);
        Integer o1 = 1984;
        put(k1, o1);
        assertEquals(data.length, hm.size());
        assertSame(o1, hm.get(k1));

        Key k2 = newKey(1984);
        Integer o2 = 2046;
        put(k2, o2);
        assertEquals(data.length + 1, hm.size());
        assertEquals(o2, hm.get(k2));
    }

    @Test
    public void test_remove() {
        remove(key);
        assertEquals(data.length - 1, hm.size());
        assertTrue(!hm.containsKey(key));
        assertTrue(!hm.lookup(key).isPresent());
        assertGetFail(key);
    }

    @Test
    public void validate_remove() {
        for (int i = 0; i < data.length; i++) {
            remove(newKey(i));
        }
        assertTrue(hm.isEmpty());
        assertEquals(0, hm.size());
    }

    @Test
    public void test_remove_not_exists() {
        remove(newKey(-1));
        remove("XXX");
        assertEquals(data.length, hm.size());
    }

    @Test
    public void test_putIfAbsent_positive() {
        Integer o = hm.get(key);
        hm = hm.putIfAbsent(key, 1984);
        assertEquals(data.length, hm.size());
        assertSame(o, hm.get(key));
    }

    @Test
    public void test_putIfAbsent_negative() {
        Key k = newKey(1984);
        Integer o = 1984;

        assertTrue(!hm.containsKey(k));
        hm = hm.putIfAbsent(k, o);
        assertEquals(data.length + 1, hm.size());
        assertSame(o, hm.get(k));
    }

    @Test
    public void test_filter() {
        hm = hm.filter(HashTrieTestBase::isPrime);
        for (int i = 0; i < data.length; i++) {
            assertEquals(isPrime(i), hm.containsKey(newKey(i)));
        }
    }

    static boolean isPrime(int n) {
        int max = (int)Math.sqrt(n);
        for (int i = 2; i <= max; i++) {
            if (n % i == 0)
                return false;
        }
        return true;
    }

    @Test
    public void test_keySet() {
        PSet<Key> ks = hm.keySet();
        assertEquals(data.length, ks.size());
        for (int k : data) {
            assertTrue(ks.contains(newKey(k)));
        }
    }

    @Test
    public void test_keys() {
        Seq<Key> ks = hm.keys();
        assertTrue(ks.allMatch(hm::containsKey));
    }

    @Test
    public void test_values() {
        hm.values().forEach(v -> assertSame(hm.get(newKey(v)), v));
    }

    @Test
    public void test_entries() {
        hm.entries().forEach(t -> assertSame(hm.get(t.getKey()), t.getValue()));
    }

    @Test
    public void test_set() {
        Random rnd = new Random();
        PSet<Key> s1 = HashPSet.fromList(Seq.generate(() -> newKey(rnd.nextInt(200))).take(100));
        PSet<Key> s2 = HashPSet.fromList(Seq.generate(() -> newKey(rnd.nextInt(200))).take(100));

        PSet<Key> u12 = s1.union(s2);
        PSet<Key> u21 = s2.union(s1);
        PSet<Key> i12 = s1.intersection(s2);
        PSet<Key> i21 = s2.intersection(s1);
        PSet<Key> d12 = s1.difference(s2);
        PSet<Key> d21 = s2.difference(s1);

        assertTrue(u12.containsAll(s1));
        assertTrue(u12.containsAll(s2));
        assertTrue(u21.containsAll(s1));
        assertTrue(u21.containsAll(s2));

        assertFalse(s1.containsAll(u12));
        assertFalse(s2.containsAll(u12));
        assertFalse(s1.containsAll(u21));
        assertFalse(s2.containsAll(u21));

        assertTrue(u12.equals(u21));

        assertTrue(s1.containsAll(i12));
        assertTrue(s2.containsAll(i12));
        assertTrue(s1.containsAll(i21));
        assertTrue(s2.containsAll(i21));

        assertFalse(i12.containsAll(s1));
        assertFalse(i12.containsAll(s2));
        assertFalse(i21.containsAll(s1));
        assertFalse(i21.containsAll(s2));

        assertTrue(i12.equals(i21));

        assertTrue(s1.containsAll(d12));
        assertFalse(containsAny(s2, d12));
        assertTrue(s2.containsAll(d21));
        assertFalse(containsAny(s1, d21));

        assertFalse(d12.containsAll(s1));
        assertFalse(containsAny(d12, s2));
        assertFalse(d21.containsAll(s2));
        assertFalse(containsAny(d21, s1));

        assertFalse(d12.equals(d21));
    }

    static <T> boolean containsAny(PSet<T> s1, PSet<T> s2) {
        return s2.anyMatch(s1::contains);
    }

    @Test
    public void test_MutableHashTrieMap() {
        List<Character> vowels = Arrays.asList('a', 'e', 'i', 'o', 'u');

        MutablePMap<Character, Integer> actual = new MutablePMap<>(HashPMap.empty());
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
}
