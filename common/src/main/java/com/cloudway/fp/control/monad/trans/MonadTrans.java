/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.control.monad.trans;

import java.util.function.Function;
import com.cloudway.fp.$;
import com.cloudway.fp.control.Monad;
import com.cloudway.fp.io.IO;

/**
 * The class of monad transformers.  Instances should satisfy the following
 * laws, which state that 'lift' is a monad transformation:
 *
 * <pre>{@code
 * lift . return = return
 * lift (m >>= f) = lift m >= (lift . f)
 * }</pre>
 */
public interface MonadTrans<T, M extends Monad<M>> extends Monad<T>, MonadIO<T> {
    /**
     * Returns the inner monad typeclass.
     *
     * @return the inner monad typeclass
     */
    M inner();

    /**
     * Lift a computation from the argument monad to the constructed monad.
     *
     * @param m a computation to be lifted
     * @return a lifted computation
     */
    <A> $<T, A> lift($<M, A> m);

    /**
     * Apply the given function with inner monad and lift the result.
     *
     * @param f the function to be applied on the inner monad context
     * @return the computation returned from inner monad context
     */
    default <A> $<T, A> lift(Function<? super M, ? extends $<M, A>> f) {
        return lift(f.apply(inner()));
    }

    /**
     * Lift a computation from the IO monad.
     */
    @Override
    @SuppressWarnings("unchecked")
    default <A> $<T, A> liftIO($<IO.µ, A> m) {
        M nm = inner();
        if (nm instanceof MonadIO) {
            return lift(((MonadIO<M>)nm).liftIO(m));
        } else {
            throw new UnsupportedOperationException("liftIO: unsupported operation");
        }
    }
}
