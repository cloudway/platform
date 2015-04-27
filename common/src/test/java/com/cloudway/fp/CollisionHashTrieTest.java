/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp;

import org.junit.Test;
import static org.junit.Assert.*;

import com.cloudway.fp.data.HashPMap;
import com.cloudway.fp.data.PMap;

public class CollisionHashTrieTest extends HashTrieTestBase {
    static class CollisionKey implements Key {
        final int value;

        CollisionKey(int value) {
            this.value = value;
        }

        public int hashCode() {
            return value % 37;
        }

        public boolean equals(Object obj) {
            return (obj instanceof CollisionKey)
                && value == ((CollisionKey)obj).value;
        }

        public String toString() {
            return String.valueOf(value);
        }
    }

    @Override
    protected Key newKey(int value) {
        return new CollisionKey(value);
    }

    @Test
    public void collision_merge_1() {
        PMap<Key, Integer> m1 = HashPMap.singleton(newKey(1), 1).put(newKey(38), 2); // collision key
        PMap<Key, Integer> m2 = HashPMap.singleton(newKey(1), 3).put(newKey(38), 4);
        PMap<Key, Integer> m3;

        m3 = m1.putAll(m2);
        assertEquals(2, m3.size());
        assertEquals(3, (int)m3.get(newKey(1)));
        assertEquals(4, (int)m3.get(newKey(38)));

        m3 = m2.putAll(m1);
        assertEquals(2, m3.size());
        assertEquals(1, (int)m3.get(newKey(1)));
        assertEquals(2, (int)m3.get(newKey(38)));
    }

    @Test
    public void collision_merge_2() {
        PMap<Key, Integer> m1 = HashPMap.singleton(newKey(1), 1).put(newKey(38), 2); // collision key
        PMap<Key, Integer> m2 = HashPMap.singleton(newKey(1), 3).put(newKey(39), 4);
        PMap<Key, Integer> m3;

        m3 = m1.putAll(m2);
        assertEquals(3, m3.size());
        assertEquals(3, (int)m3.get(newKey(1)));
        assertEquals(2, (int)m3.get(newKey(38)));
        assertEquals(4, (int)m3.get(newKey(39)));

        m3 = m2.putAll(m1);
        assertEquals(3, m3.size());
        assertEquals(1, (int)m3.get(newKey(1)));
        assertEquals(2, (int)m3.get(newKey(38)));
        assertEquals(4, (int)m3.get(newKey(39)));
    }
}
