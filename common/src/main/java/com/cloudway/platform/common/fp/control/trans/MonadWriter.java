/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.control.trans;

import java.util.function.Function;

import com.cloudway.platform.common.fp.$;
import com.cloudway.platform.common.fp.control.Monad;
import com.cloudway.platform.common.fp.data.Tuple;
import com.cloudway.platform.common.fp.data.Unit;

/**
 * The {@code MonadWriter} typeclass.
 *
 * <p>Minimal complete definition:
 * ({@link #writer} | {@link #tell}), {@link #listen}, {@link #pass}
 */
public interface MonadWriter<M, W> extends Monad<M> {
    /**
     * Embeds a simple writer action.
     */
    default <A> $<M, A> writer(Tuple<A, W> aw) {
        return seqR(tell(aw.second()), pure(aw.first()));
    }

    /**
     * An action that produces the output.
     */
    default $<M, Unit> tell(W w) {
        return writer(Tuple.of(Unit.U, w));
    }

    /**
     * An action that executes the given action and adds its output to the
     * value of the computation.
     */
    <A> $<M, Tuple<A, W>> listen($<M, A> m);

    /**
     * An action that executes the given action, which returns a value and
     * a function, and return the value, applying the function to the output.
     * @param m
     * @param <A>
     * @return
     */
    <A> $<M, A> pass($<M, Tuple<A, Function<W, W>>> m);

    /**
     * An action that executes the given action and adds the result of applying
     * the given function to the output to the value of the computation.
     */
    default <A, B> $<M, Tuple<A, B>> listens(Function<? super W, ? extends B> f, $<M, A> m) {
        return bind(listen(m), aw -> aw.as((a, w) -> pure(Tuple.of(a, f.apply(w)))));
    }

    /**
     * An action that executes the given action and applies the given function
     * to its output, leaving the return value unchanged.
     */
    default <A> $<M, A> censor(Function<W, W> f, $<M, A> m) {
        return pass(bind(m, a -> pure(Tuple.of(a, f))));
    }
}
