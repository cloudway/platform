/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.typeclass;

/**
 * An interface for "lifting" a type constructors to type parameter level in
 * order to allow the simulation of higher order type polymorphism. The "hidden"
 * class µ (which should be an inner static class of the type constructor)
 * identifiers the type class and allows to cast values back to their normal
 * version. For this, the type constructor should provide a static method called
 * narrow.
 *
 * @param <T> the type class of the data structure
 * @param <A> the element type of the data structure
 */
public interface $<T, A> {
    /**
     * Returns the type class of this data structure at runtime.
     */
    T getTypeClass();
}
