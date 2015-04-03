/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util;

public class HashTrieTest extends HashTrieTestBase {
    static class NormalKey implements Key {
        final int value;

        NormalKey(int value) {
            this.value = value;
        }

        public int hashCode() {
            return value;
        }

        public boolean equals(Object obj) {
            return (obj instanceof NormalKey)
                && value == ((NormalKey)obj).value;
        }

        public String toString() {
            return String.valueOf(value);
        }
    }

    @Override
    protected Key newKey(int value) {
        return new NormalKey(value);
    }
}
