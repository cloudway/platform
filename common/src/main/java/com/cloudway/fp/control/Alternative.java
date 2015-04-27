/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.control;

import com.cloudway.fp.$;
import com.cloudway.fp.data.Maybe;
import com.cloudway.fp.data.Unit;

/**
 * A monoid on applicative functors.
 */
public interface Alternative<F> extends Applicative<F> {
    /**
     * The identity of the monoid.
     *
     * @return the identity of the monoid.
     */
    <A> $<F, A> mzero();

    /**
     * An associative binary operation.
     *
     * @param a1 the first argument of the binary operation
     * @param a2 the second argument of the binary operation
     * @return the result of the binary operation
     */
    <A> $<F, A> mplus($<F, A> a1, $<F, A> a2);

    /**
     * Returns {@code pure()} if the given guard condition is true, otherwise
     * return {@code mzero}
     */
    default $<F, Unit> guard(boolean b) {
        return b ? pure(Unit.U) : mzero();
    }

    /**
     * One or none.
     */
    default <A> $<F, Maybe<A>> optional($<F, A> v) {
        return mplus(map(v, Maybe::of), pure(Maybe.empty()));
    }
}
