/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.data;

/**
 * The unit type which has only one value.
 */
public enum Unit {
    /**
     * The only value of the unit type.
     */
    U;

    /**
     * The monoid for the unit value.
     */
    public static final Monoid<Unit> monoid = Monoid.monoid(U, (u1, u2) -> U);

    /**
     * Returns the string representation of the unit value.
     *
     * @return the string representation of the unit value
     */
    public String toString() {
        return "()";
    }
}
