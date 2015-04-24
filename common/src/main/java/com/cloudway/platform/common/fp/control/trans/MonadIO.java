/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.control.trans;

import com.cloudway.platform.common.fp.$;
import com.cloudway.platform.common.fp.data.Unit;
import com.cloudway.platform.common.fp.io.IO;
import com.cloudway.platform.common.fp.io.VoidIO;

/**
 * Monads in which IO computations may be embedded. Any monad built by applying
 * a sequence of monad transformers to the IO monad will be an instance of this
 * class.
 */
public interface MonadIO<M> {
    /**
     * Lift a computation from the IO monad.
     *
     * @param m an IO computation
     * @return the computation lifted into current monad
     */
    <A> $<M, A> liftIO($<IO.µ, A> m);

    /**
     * Convenient overloaded method.
     */
    default <A> $<M, A> liftIO(IO<A> m) {
        return liftIO(($<IO.µ, A>)m);
    }

    /**
     * Convenient overloaded method.
     */
    default <A> $<M, Unit> liftIO_(VoidIO m) {
        return liftIO(m);
    }
}
