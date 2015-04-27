/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.control.arrow;

import com.cloudway.fp.π;
import com.cloudway.fp.control.MonadPlus;
import static com.cloudway.fp.control.arrow.Kleisli.narrow;

/**
 * The Kleisli arrow plus typeclass definition.
 */
public class KleisliArrowPlus<M extends MonadPlus<M>>
    extends KleisliArrow<M> implements ArrowPlus<KleisliArrow<M>>
{
    protected KleisliArrowPlus(M nm) {
        super(nm);
    }

    @Override
    public <B, C> Kleisli<M, B, C> zero() {
        return kleisli(__ -> nm.mzero());
    }

    @Override
    public <B, C> Kleisli<M, B, C>
    plus(π<KleisliArrow<M>, B, C> f, π<KleisliArrow<M>, B, C> g) {
        return kleisli(x -> nm.mplus(narrow(f).apply(x), narrow(g).apply(x)));
    }
}
