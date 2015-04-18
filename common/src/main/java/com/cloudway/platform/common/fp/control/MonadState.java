/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.control;

import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;

import com.cloudway.platform.common.fp.data.Fn;
import com.cloudway.platform.common.fp.data.Foldable;
import com.cloudway.platform.common.fp.data.Seq;
import com.cloudway.platform.common.fp.data.Tuple;
import com.cloudway.platform.common.fp.data.Unit;
import com.cloudway.platform.common.fp.typeclass.Monad;
import com.cloudway.platform.common.fp.typeclass.$;

import static com.cloudway.platform.common.fp.control.Trampoline.immediate;
import static com.cloudway.platform.common.fp.control.Trampoline.suspend;

/**
 * The state monad, passing an updatable state through a computation.
 *
 * @param <A> the type of result of computation
 * @param <S> the type of state passing to the computation
 */
public final class MonadState<A, S> implements $<MonadState.µ<S>, A> {
    // the state transfer function
    private final Function<S, Trampoline<Tuple<A, S>>> sf;

    // construct a monad state
    private MonadState(Function<S, Trampoline<Tuple<A, S>>> f) {
        this.sf = f;
    }

    // helper method to construct a monad state from a transfer function
    static <A, S> MonadState<A, S> $(Function<S, Trampoline<Tuple<A, S>>> f) {
        return new MonadState<>(f);
    }

    // single step the state transfer
    Trampoline<Tuple<A, S>> go(S s) {
        return sf.apply(s);
    }

    /**
     * Create a state monad from the given state transfer function.
     *
     * @param f the state transfer function
     * @return the state monad
     */
    public static <A, S> MonadState<A, S> state(Function<? super S, Tuple<A, S>> f) {
        return $(s -> immediate(f.apply(s)));
    }

    /**
     * Constructs a pure computation that results in the given value.
     *
     * @param a the final result
     * @return the state monad that hold the final result
     */
    public static <A, S> MonadState<A, S> pure(A a) {
        return $(s -> immediate(Tuple.of(a, s)));
    }

    private static final MonadState<Unit,?> _unit = pure(Unit.U);

    /**
     * Returns a do nothing computation.
     */
    @SuppressWarnings("unchecked")
    public static <S> MonadState<Unit, S> unit() {
        return (MonadState<Unit,S>)_unit;
    }

    /**
     * Constructs a pure computation that results in the given lazy evaluation
     * thunk.
     *
     * @param a a thunk that eventually produce computation result
     * @return the state monad that hold the computation
     */
    public static <A, S> MonadState<A, S> lazy(Supplier<A> a) {
        Supplier<A> t = Fn.lazy(a);
        return $(s -> immediate(Tuple.of(t.get(), s)));
    }

    /**
     * Constructs a computation that performs the given action with no result.
     */
    public static <S> MonadState<Unit, S> action(Runnable a) {
        return $(s -> { a.run(); return immediate(Tuple.of(Unit.U, s)); });
    }

    /**
     * Evaluate a state computation with the given initial state and return
     * a tuple of final value and final state.
     *
     * @param s the initial state
     * @return the tuple of final value and final state
     */
    public Tuple<A, S> run(S s) {
        return go(s).run();
    }

    /**
     * Evaluate a state computation with the given initial state and return
     * the final value, discarding the final state.
     */
    public A eval(S s) {
        return run(s).first();
    }

    /**
     * Evaluate a state computation with the given initial state and return
     * the final state, discarding the final value.
     */
    public S exec(S s) {
        return run(s).second();
    }

    /**
     * Transfer a state computation by feeding the value to the given function
     * and wrapping the result to new state.
     */
    public <B> MonadState<B, S> map(Function<? super A, ? extends B> f) {
        return $(s -> suspend(() -> go(s).map(t -> t.mapFirst(f))));
    }

    /**
     * Transfer a state computation by feeding the value to the given function.
     */
    public <B> MonadState<B, S> bind(Function<? super A, ? extends $<µ<S>, B>> f) {
        return $(s -> suspend(() -> go(s).bind(t -> suspend(() -> narrow(f.apply(t.first())).go(t.second())))));
    }

    /**
     * Transfer a state computation by discarding the intermediate value.
     */
    public <B> MonadState<B, S> then(Supplier<? extends $<µ<S>, B>> next) {
        return $(s -> suspend(() -> go(s).bind(t -> suspend(() -> narrow(next.get()).go(t.second())))));
    }

    /**
     * Transfer a state computation by discarding the intermediate value.
     */
    public <B> MonadState<B, S> then($<µ<S>, B> next) {
        return $(s -> suspend(() -> go(s).bind(t -> suspend(() -> narrow(next).go(t.second())))));
    }

    /**
     * Map both the return value and final state of computation using the given
     * function.
     */
    public <B> MonadState<B, S> mapState(Function<Tuple<A, S>, Tuple<B, S>> f) {
        return $(s -> suspend(() -> go(s).map(f)));
    }

    /**
     * Map both the return value and final state of computation using the given
     * function.
     */
    public <B> MonadState<B, S> mapState(BiFunction<? super A, ? super S, Tuple<B, S>> f) {
        return $(s -> suspend(() -> go(s).map(t -> t.as(f))));
    }

    /**
     * Executes action on a state modified by applying function.
     */
    public MonadState<A, S> withState(Function<S, S> f) {
        return $(s -> suspend(() -> go(f.apply(s))));
    }

    /**
     * Fetch the current value of the state within the monad.
     */
    public static <S> MonadState<S,S> get() {
        return $(s -> immediate(Tuple.of(s, s)));
    }

    /**
     * Fetch the current value of the state and bind the value to the given function.
     */
    public static <B,S> MonadState<B,S> get(Function<? super S, ? extends $<µ<S>, B>> f) {
        return $(s -> suspend(() -> narrow(f.apply(s)).go(s))); // get().bind(f)
    }

    /**
     * Sets the state within the monad.
     */
    public static <S> MonadState<Unit, S> put(S s) {
        return $(__ -> immediate(Tuple.of(Unit.U, s)));
    }

    /**
     * Updates the state to the result of applying a function to the current state.
     */
    public static <S> MonadState<Unit, S> modify(Function<S, S> f) {
        return $(s -> immediate(Tuple.of(Unit.U, f.apply(s))));
    }

    /**
     * Get a specific component of the state, using a projection function supplied.
     */
    public static <A, S> MonadState<A, S> gets(Function<S, A> f) {
        return $(s -> immediate(Tuple.of(f.apply(s), s)));
    }

    // Monad

    public static final class µ<S> implements Monad<µ<S>> {
        @Override
        public <A> MonadState<A, S> pure(A a) {
            return MonadState.pure(a);
        }

        @Override
        public <A, B> MonadState<B, S> map($<µ<S>, A> a, Function<? super A, ? extends B> f) {
            return narrow(a).map(f);
        }

        @Override
        public <A, B> MonadState<B, S> bind($<µ<S>, A> a, Function<? super A, ? extends $<µ<S>, B>> k) {
            return narrow(a).bind(k);
        }

        @Override
        public <A, B> MonadState<B, S> seqR($<µ<S>, A> a, $<µ<S>, B> b) {
            return narrow(a).then(b);
        }

        @Override
        public <A, B> MonadState<B, S> seqR($<µ<S>, A> a, Supplier<? extends $<µ<S>, B>> b) {
            return narrow(a).then(b);
        }
    }

    public static <A, S> MonadState<A, S> narrow($<µ<S>, A> value) {
        return (MonadState<A, S>)value;
    }

    private static final µ<?> _TCLASS = new µ<>();

    @SuppressWarnings("unchecked")
    public static <S> µ<S> tclass() {
        return (µ<S>)_TCLASS;
    }

    @Override
    public µ<S> getTypeClass() {
        return tclass();
    }

    // Convenient static monad methods

    public static <A, S> MonadState<Seq<A>, S> flatM(Seq<? extends $<µ<S>, A>> ms) {
        return narrow(MonadState.<S>tclass().flatM(ms));
    }

    public static <A, S> MonadState<Unit, S> sequence(Foldable<? extends $<µ<S>, A>> ms) {
        return narrow(MonadState.<S>tclass().sequence(ms));
    }

    public static <A, B, S> MonadState<Seq<B>, S> mapM(Seq<A> xs, Function<? super A, ? extends $<µ<S>, B>> f) {
        return narrow(MonadState.<S>tclass().mapM(xs, f));
    }

    public static <A, B, S> MonadState<Unit, S> mapM_(Foldable<A> xs, Function<? super A, ? extends $<µ<S>, B>> f) {
        return narrow(MonadState.<S>tclass().mapM_(xs, f));
    }

    public static <A, S> MonadState<Seq<A>, S> filterM(Seq<A> xs, Function<? super A, ? extends $<µ<S>, Boolean>> p) {
        return narrow(MonadState.<S>tclass().filterM(xs, p));
    }

    public static <A, B, S> MonadState<B, S> foldM(B r0, Foldable<A> xs, BiFunction<B, ? super A, ? extends $<µ<S>, B>> f) {
        return narrow(MonadState.<S>tclass().foldM(r0, xs, f));
    }

    public static <A, S> MonadState<Seq<A>, S> replicateM(int n, $<µ<S>, A> a) {
        return narrow(MonadState.<S>tclass().replicateM(n, a));
    }

    public static <A, S> MonadState<Unit, S> replicateM_(int n, $<µ<S>, A> a) {
        return narrow(MonadState.<S>tclass().replicateM_(n, a));
    }
}
