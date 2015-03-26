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
import com.cloudway.platform.common.fp.data.Tuple;
import com.cloudway.platform.common.fp.data.Unit;
import com.cloudway.platform.common.fp.function.TriFunction;
import com.cloudway.platform.common.fp.io.IO;
import com.cloudway.platform.common.fp.io.VoidIO;

import static com.cloudway.platform.common.fp.control.TrampolineIO.*;

/**
 * The stateful IO monad.
 *
 * @param <A> the type of result of computation
 * @param <S> the type of state passing to the computation
 */
public final class StateIO<A, S> {
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
    public <B> StateIO<B, S> bind(Function<? super A, StateIO<B, S>> f) {
        return $(s -> suspend(() -> go(s).bind(t -> suspend(() -> f.apply(t.first()).go(t.second())))));
    }

    /**
     * Transfer a state computation by discarding the intermediate value.
     */
    public <B> StateIO<B, S> then(Supplier<StateIO<B, S>> next) {
        return $(s -> suspend(() -> go(s).bind(t -> suspend(() -> next.get().go(t.second())))));
    }

    /**
     * Transfer a state computation by discarding the intermediate value.
     */
    public <B> StateIO<B, S> then(StateIO<B, S> next) {
        return $(s -> suspend(() -> go(s).bind(t -> suspend(() -> next.go(t.second())))));
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
    public static <B, S> StateIO<B,S> get(Function<? super S, StateIO<B, S>> f) {
        return $(s -> suspend(() -> f.apply(s).go(s))); // get().bind(f)
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

    /**
     * Evaluate each action in the sequence from left to right, and collect
     * the result.
     */
    public static <A, S> StateIO<Seq<A>, S> flatM(Seq<StateIO<A, S>> ms) {
        return ms.foldRight_(pure(Seq.nil()), liftM2(Seq::cons));
    }

    /**
     * Evaluate each action in the sequence from left to right, and ignore
     * the result.
     */
    public static <A, S> StateIO<Unit, S> sequence(Foldable<StateIO<A, S>> ms) {
        return ms.foldRight(pure(Unit.U), StateIO::then);
    }

    /**
     * The {@code mapM} is analogous to {@link Seq#map(Function) map} except that
     * its result is encapsulated in an {@code StateIO}.
     */
    public static <A, B, S> StateIO<Seq<B>, S>
    mapM(Seq<A> xs, Function<? super A, StateIO<B,S>> f) {
        return flatM(xs.map(f));
    }

    /**
     * {@code mapM_} is equivalent to {@code sequence(xs.map(f))}.
     */
    public static <A, B, S> StateIO<Unit, S>
    mapM_(Foldable<A> xs, Function<? super A, StateIO<B,S>> f) {
        return xs.foldRight(unit(), (x, r) -> f.apply(x).then(r));
    }

    /**
     * Generalizes {@link Seq#zip(Seq,BiFunction)} to arbitrary monads.
     * Bind the given function to the given computations with a final join.
     */
    public static <A, B, C, S> StateIO<Seq<C>, S>
    zipM(Seq<A> xs, Seq<B> ys, BiFunction<? super A, ? super B, StateIO<C, S>> f) {
        return flatM(Seq.zip(xs, ys, f));
    }

    /**
     * The extension of {@link #zipM(Seq,Seq,BiFunction) zipM} which ignores the
     * final result.
     */
    public static <A, B, C, S> StateIO<Unit, S>
    zipM_(Seq<A> xs, Seq<B> ys, BiFunction<? super A, ? super B, StateIO<C, S>> f) {
        return sequence(Seq.zip(xs, ys, f));
    }

    /**
     * This generalizes the list-based filter function.
     */
    public static <A, S> StateIO<Seq<A>, S>
    filterM(Seq<A> xs, Function<? super A, StateIO<Boolean,S>> p) {
        return xs.isEmpty()
            ? pure(Seq.nil())
            : p.apply(xs.head()).bind(flg ->
              filterM(xs.tail(), p).bind(ys ->
              pure(flg ? Seq.cons(xs.head(), ys) : ys)));
    }

    /**
     * The {@code foldM} is analogous to {@link Seq#foldLeft(Object,BiFunction) foldLeft},
     * except that its result is encapsulated in a {@code StateIO}. Note that
     * {@code foldM} works from left-to-right over the lists arguments. If right-to-left
     * evaluation is required, the input list should be reversed.
     */
    public static <A, B, S> StateIO<B, S>
    foldM(B r0, Foldable<A> xs, BiFunction<B, ? super A, StateIO<B, S>> f) {
        return xs.foldLeft(pure(r0), (m, x) -> m.bind(r -> f.apply(r, x)));
    }

    /**
     * Perform the action n times, gathering the results.
     */
    public static <A, S> StateIO<Seq<A>, S> replicateM(int n, StateIO<A, S> a) {
        return flatM(Seq.replicate(n, a));
    }

    /**
     * Perform the action n times, discards the result.
     */
    public static <A, S> StateIO<Unit, S> replicateM_(int n, StateIO<A, S> a) {
        return sequence(Seq.replicate(n, a));
    }

    /**
     * Kleisli composition of monads.
     */
    public static <A, B, C, S> Function<A, StateIO<C, S>>
    kleisli(Function<A, StateIO<B, S>> f, Function<B, StateIO<C, S>> g) {
        return x -> f.apply(x).bind(g);
    }

    /**
     * Promote a function to a monad state function.
     */
    public static <A, B, S> Function<StateIO<A, S>, StateIO<B, S>>
    liftM(Function<? super A, ? extends B> f) {
        return m -> m.map(f);
    }

    /**
     * Promote a function to a monad state function.
     */
    public static <A, B, C, S> BiFunction<StateIO<A,S>, StateIO<B,S>, StateIO<C,S>>
    liftM2(BiFunction<? super A, ? super B, ? extends C> f) {
        return (m1, m2) -> m1.bind(x1 -> m2.map(x2 -> f.apply(x1, x2)));
    }

    /**
     * Promote a function to a monad state function.
     */
    public static <A, B, C, D, S> TriFunction<StateIO<A,S>, StateIO<B,S>, StateIO<C,S>, StateIO<D,S>>
    liftM3(TriFunction<? super A, ? super B, ? super C, ? extends D> f) {
        return (m1, m2, m3) -> m1.bind(x1 -> m2.bind(x2 -> m3.map(x3 -> f.apply(x1, x2, x3))));
    }
}
