/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.control.monad.trans;

import java.util.function.Function;

import com.cloudway.fp.$;
import com.cloudway.fp.control.Monad;
import com.cloudway.fp.data.Tuple;
import com.cloudway.fp.data.Unit;

/**
 * The {@code MonadState} typeclass.
 *
 * <p>Minimal complete definition: ({@link #get}, {@link #put}) | {@link #state}
 */
public interface MonadState<M, S> extends Monad<M> {
    /**
     * Embed a simple state action into the monad.
     */
    default <A> $<M, A> state(Function<? super S, Tuple<A, S>> f) {
        return bind(get(), s -> f.apply(s).as((a, s1) -> seqR(put(s1), pure(a))));
    }

    /**
     * Returns the state from the internals of the monad.
     */
    default $<M, S> get() {
        return state(s -> Tuple.of(s, s));
    }

    /**
     * Replace the state inside the monad.
     */
    default $<M, Unit> put(S s) {
        return state(__ -> Tuple.of(Unit.U, s));
    }

    /**
     * Monadic state transformer. Maps an old state to a new state inside
     * a state monad, The old state is thrown away.
     */
    default $<M, Unit> modify(Function<S, S> f) {
        return state(s -> Tuple.of(Unit.U, f.apply(s)));
    }

    /**
     * Fetch the current value of the state and bind the value to the given
     * function.
     */
    default <A> $<M, A> get(Function<? super S, ? extends $<M, A>> f) {
        return bind(get(), f);
    }

    /**
     * Gets specific component of the state, using a projection function supplied.
     */
    default <A> $<M, A> gets(Function<? super S, ? extends A> f) {
        return map(get(), f);
    }
}
