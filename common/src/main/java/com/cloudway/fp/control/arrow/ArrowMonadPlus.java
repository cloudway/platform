/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.control.arrow;

import com.cloudway.fp.$;
import com.cloudway.fp.control.MonadPlus;

/**
 * Any instance of {@link ArrowPlus} defines a {@link MonadPlus}.
 */
public class ArrowMonadPlus<A extends ArrowApply<A> & ArrowPlus<A>>
    extends ArrowMonad<A> implements MonadPlus<ArrowMonad<A>>
{
    ArrowMonadPlus(A arrow) {
        super(arrow);
    }

    @Override
    public <B> $<ArrowMonad<A>, B> mzero() {
        return monad(arrow.zero());
    }

    @Override
    public <B> $<ArrowMonad<A>, B> mplus($<ArrowMonad<A>, B> x, $<ArrowMonad<A>, B> y) {
        return monad(arrow.plus(unMonad(x), unMonad(y)));
    }
}
