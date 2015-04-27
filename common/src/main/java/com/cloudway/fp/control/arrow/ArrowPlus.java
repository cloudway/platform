/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.control.arrow;

import com.cloudway.fp.π;

/**
 * A monoid on arrows.
 */
public interface ArrowPlus<A> extends Arrow<A> {
    /**
     * The identity of the monoid.
     */
    <B, C> π<A, B, C> zero();

    /**
     * An associative operation with identity zero.
     */
    <B, C> π<A, B, C> plus(π<A, B, C> f, π<A, B, C> g);
}
