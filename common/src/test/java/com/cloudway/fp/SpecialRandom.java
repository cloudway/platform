/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp;

import java.util.Random;

/**
 * Utility class for being able to seed a {@link Random} value with a passed
 * in seed from a benchmark parameter.
 */
public final class SpecialRandom extends Random {
    private static final long serialVersionUID = 1100619216472146788L;

    public static SpecialRandom valueOf(String s) {
        return s.isEmpty()
            ? new SpecialRandom()
            : new SpecialRandom(Long.parseLong(s));
    }

    private final boolean hasSeed;
    private final long seed;

    public SpecialRandom() {
        this.hasSeed = false;
        this.seed = 0;
    }

    public SpecialRandom(long seed) {
        super(seed);
        this.hasSeed = true;
        this.seed = seed;
    }

    @Override public String toString() {
        return hasSeed ? "(seed:" + seed + ")" : "(default seed)";
    }
}
