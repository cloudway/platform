/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.data;

/**
 * This interface provides an operation for fully evaluating data structures
 * (that is, evaluating to "Normal Form").  A typical use is to prevent
 * resource leaks.
 */
public interface Forcible<T> {
    /**
     * Fully evaluate the data structure.
     *
     * @return the data structure in "Normal Form"
     */
    T force();

    /**
     * Try to fully evaluate the given object if its implements {@code Forcible}
     * interface, otherwise, leave the object unchanged.
     *
     * @param x an object to be fully evaluated
     * @return the fully evaluated object
     */
    static <T> T force(T x) {
        if (x instanceof Forcible)
            ((Forcible)x).force();
        return x;
    }
}
