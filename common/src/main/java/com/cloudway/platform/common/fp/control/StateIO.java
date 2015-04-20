/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.control;

import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;

import com.cloudway.platform.common.fp.data.Foldable;
import com.cloudway.platform.common.fp.data.Seq;
import com.cloudway.platform.common.fp.data.Traversable;
import com.cloudway.platform.common.fp.data.Tuple;
import com.cloudway.platform.common.fp.data.Unit;
import com.cloudway.platform.common.fp.io.IO;
import com.cloudway.platform.common.fp.io.VoidIO;
import com.cloudway.platform.common.fp.$;

import static com.cloudway.platform.common.fp.control.TrampolineIO.*;

/**
 * The stateful IO monad.
 *
 * @param <A> the type of result of computation
 * @param <S> the type of state passing to the computation
 */
public final class StateIO<A, S> implements $<StateIO.µ<S>, A> {
    // the state transfer function
    private final Function<S, TrampolineIO<Tuple<A, S>>> sf;

    // construct a stateful IO monad
    private StateIO(Function<S, TrampolineIO<Tuple<A, S>>> f) {
        this.sf = f;
    }

    // helper method to construct a monad from a transfer function
    static <A, S> StateIO<A, S> $(Function<S, TrampolineIO<Tuple<A, S>>> f) {
        return new StateIO<>(f);
    }

    // single step the state transfer
    private TrampolineIO<Tuple<A, S>> go(S s) {
        return sf.apply(s);
    }

    /**
     * Create a stateful IO monad from the given state transfer function.
     *
     * @param f the state transfer function
     * @return the stateful IO monad
     */
    public static <A, S> StateIO<A, S> state(Function<S, Tuple<A, S>> f) {
        return $(s -> immediate(f.apply(s)));
    }

    /**
     * Construct a pure computation that results in the given value.
     *
     * @param a the final result
     * @return the stateful IO monad that hold the final result
     */
    public static <A, S> StateIO<A, S> pure(A a) {
        return $(s -> immediate(Tuple.of(a, s)));
    }

    private static final StateIO<Unit,?> _unit = pure(Unit.U);

    /**
     * Returns a do nothing I/O action.
     */
    @SuppressWarnings("unchecked")
    public static <S> StateIO<Unit, S> unit() {
        return (StateIO<Unit,S>)_unit;
    }

    /**
     * Constructs a pure computation that results in the given lazy evaluation
     * thunk.
     *
     * @param a a thunk that eventually produce computation result
     * @return the state monad that hold the computation
     */
    public static <A, S> StateIO<A, S> lazy(IO<A> a) {
        return lift(a);
    }

    /**
     * Constructs a computation that performs the given action with no result.
     */
    public static <S> StateIO<Unit, S> action(VoidIO a) {
        return lift(a);
    }

    /**
     * Promote an IO action to a stateful IO action.
     *
     * @param m the IO action
     * @return the prompted stateful IO action
     */
    public static <A, S> StateIO<A, S> lift(IO<A> m) {
        return $(s -> TrampolineIO.lift(m).map(a -> Tuple.of(a, s)));
    }

    /**
     * Promote an IO action to a stateful IO action.
     *
     * @param m the IO action which has no return value
     * @return the prompted stateful IO action
     */
    public static <S> StateIO<Unit, S> lift_(VoidIO m) {
        return lift(m);
    }

    /**
     * Evaluate a state computation with the given initial state and return
     * a tuple of final value and final state.
     *
     * @param s the initial state
     * @return the tuple of final value and final state wrapped in an IO monad
     */
    public IO<Tuple<A, S>> run(S s) {
        return go(s).run();
    }

    /**
     * Evaluate a state computation with the given initial state and return
     * the final value, discarding the final state.
     *
     * @param s the initial state
     * @return the final computation value wrapped in an IO monad
     */
    public IO<A> eval(S s) {
        return run(s).map(Tuple::first);
    }

    /**
     * Evaluate a state computation with the given initial state and return
     * the final value, discarding the final state.
     *
     * @param s the initial state
     * @return the final state wrapped in an IO monad
     */
    public IO<S> exec(S s) {
        return run(s).map(Tuple::second);
    }

    /**
     * Transfer a state computation by feeding the value to the given function
     * and wrapping the result to a new state.
     */
    public <B> StateIO<B, S> map(Function<? super A, ? extends B> f) {
        return $(s -> suspend(() -> go(s).map(t -> t.mapFirst(f))));
    }

    /**
     * Transfer a state computation by feeding the value to the given function.
     */
    public <B> StateIO<B, S> bind(Function<? super A, ? extends $<µ<S>, B>> f) {
        return $(s -> suspend(() -> go(s).bind(t -> suspend(() -> narrow(f.apply(t.first())).go(t.second())))));
    }

    /**
     * Transfer a state computation by discarding the intermediate value.
     */
    public <B> StateIO<B, S> then(Supplier<? extends $<µ<S>, B>> next) {
        return $(s -> suspend(() -> go(s).bind(t -> suspend(() -> narrow(next.get()).go(t.second())))));
    }

    /**
     * Transfer a state computation by discarding the intermediate value.
     */
    public <B> StateIO<B, S> then($<µ<S>, B> next) {
        return $(s -> suspend(() -> go(s).bind(t -> suspend(() -> narrow(next).go(t.second())))));
    }

    /**
     * Map both the return value and final state of computation using the
     * given function.
     */
    public <B> StateIO<B, S> mapState(Function<IO<Tuple<A, S>>, IO<Tuple<B, S>>> f) {
        return $(s -> suspend(() -> TrampolineIO.lift(f.apply(run(s)))));
    }

    /**
     * Execute action on a state modified by applying function.
     */
    public StateIO<A, S> withState(Function<S, S> f) {
        return $(s -> suspend(() -> go(f.apply(s))));
    }

    /**
     * Fetch the current value of the state within the monad.
     */
    public static <S> StateIO<S, S> get() {
        return $(s -> immediate(Tuple.of(s, s)));
    }

    /**
     * Fetch the current value of the state and bind the value to the given function.
     */
    public static <B, S> StateIO<B,S> get(Function<? super S, ? extends $<µ<S>, B>> f) {
        return $(s -> suspend(() -> narrow(f.apply(s)).go(s))); // get().bind(f)
    }

    /**
     * Sets the state within the monad.
     */
    public static <S> StateIO<Unit, S> put(S s) {
        return $(__ -> immediate(Tuple.of(Unit.U, s)));
    }

    /**
     * Updates the state to the result of applying a function to the current state.
     */
    public static <S> StateIO<Unit, S> modify(Function<S, S> f) {
        return $(s -> immediate(Tuple.of(Unit.U, f.apply(s))));
    }

    /**
     * Get specific component of the state, using a projection function applied.
     */
    public static <A, S> StateIO<A, S> gets(Function<S, A> f) {
        return $(s -> immediate(Tuple.of(f.apply(s), s)));
    }

    // Monad

    public static final class µ<S> implements Monad<µ<S>> {
        @Override
        public <A> StateIO<A, S> pure(A a) {
            return StateIO.pure(a);
        }

        @Override
        public <A, B> StateIO<B, S> map($<µ<S>, A> a, Function<? super A, ? extends B> f) {
            return narrow(a).map(f);
        }

        @Override
        public <A, B> StateIO<B, S> bind($<µ<S>, A> a, Function<? super A, ? extends $<µ<S>, B>> k) {
            return narrow(a).bind(k);
        }

        @Override
        public <A, B> StateIO<B, S> seqR($<µ<S>, A> a, $<µ<S>, B> b) {
            return narrow(a).then(b);
        }

        @Override
        public <A, B> StateIO<B, S> seqR($<µ<S>, A> a, Supplier<? extends $<µ<S>, B>> b) {
            return narrow(a).then(b);
        }
    }

    public static <A, S> StateIO<A, S> narrow($<µ<S>, A> value) {
        return (StateIO<A, S>)value;
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

    public static <T, A, S> StateIO<? extends Traversable<T, A>, S>
    flatM(Traversable<T, ? extends $<µ<S>, A>> ms) {
        return narrow(StateIO.<S>tclass().flatM(ms));
    }

    @SuppressWarnings("unchecked")
    public static <A, S> StateIO<Seq<A>, S> flatM(Seq<? extends $<µ<S>, A>> ms) {
        return (StateIO<Seq<A>, S>)StateIO.<S>tclass().flatM(ms);
    }

    public static <T, A, B, S> StateIO<? extends Traversable<T, B>, S>
    mapM(Traversable<T, A> xs, Function<? super A, ? extends $<µ<S>, B>> f) {
        return narrow(StateIO.<S>tclass().mapM(xs, f));
    }

    @SuppressWarnings("unchecked")
    public static <A, B, S> StateIO<Seq<B>, S>
    mapM(Seq<A> xs, Function<? super A, ? extends $<µ<S>, B>> f) {
        return (StateIO<Seq<B>, S>)StateIO.<S>tclass().mapM(xs, f);
    }

    public static <A, S> StateIO<Unit, S> sequence(Foldable<? extends $<µ<S>, A>> ms) {
        return narrow(StateIO.<S>tclass().sequence(ms));
    }

    public static <A, B, S> StateIO<Unit, S>
    mapM_(Foldable<A> xs, Function<? super A, ? extends $<µ<S>, B>> f) {
        return narrow(StateIO.<S>tclass().mapM_(xs, f));
    }

    public static <A, S> StateIO<Seq<A>, S>
    filterM(Seq<A> xs, Function<? super A, ? extends $<µ<S>, Boolean>> p) {
        return narrow(StateIO.<S>tclass().filterM(xs, p));
    }

    public static <A, B, S> StateIO<B, S>
    foldM(B r0, Foldable<A> xs, BiFunction<B, ? super A, ? extends $<µ<S>, B>> f) {
        return narrow(StateIO.<S>tclass().foldM(r0, xs, f));
    }

    public static <A, S> StateIO<Seq<A>, S> replicateM(int n, $<µ<S>, A> a) {
        return narrow(StateIO.<S>tclass().replicateM(n, a));
    }

    public static <A, S> StateIO<Unit, S> replicateM_(int n, $<µ<S>, A> a) {
        return narrow(StateIO.<S>tclass().replicateM_(n, a));
    }
}
