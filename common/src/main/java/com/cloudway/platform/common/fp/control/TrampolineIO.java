/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.control;

import java.io.IOException;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;

import com.cloudway.platform.common.fp.data.Either;
import com.cloudway.platform.common.fp.data.Foldable;
import com.cloudway.platform.common.fp.data.Seq;
import com.cloudway.platform.common.fp.data.Unit;
import com.cloudway.platform.common.fp.function.TriFunction;
import com.cloudway.platform.common.fp.io.IO;
import com.cloudway.platform.common.fp.io.VoidIO;

import static com.cloudway.platform.common.fp.data.Either.*;

/**
 * A trampoline stacked on top of an I/O action.
 *
 * @param <A> the type of computation result
 *
 * @see Trampoline
 */
public abstract class TrampolineIO<A> {
    /**
     * A Normal Trampoline is either done or suspended, and is allowed to be a
     * subcomputation of a Codense.  This is the pointed functor part of the
     * Trampoline monad.
     */
     private static abstract class Normal<A> extends TrampolineIO<A> {
        protected abstract <B, R extends TrampolineIO<B>> TrampolineIO<B>
        foldNormal(Function<A, R> pure, Function<Supplier<TrampolineIO<A>>, R> k);

        @Override
        public <B> TrampolineIO<B> bind(Function<? super A, ? extends TrampolineIO<B>> f) {
            return codense(this, f);
        }
    }

    /**
     * A Codense Trampoline delimits a subcomputation and tracks its current
     * continuation. Subcomputations are only allowed to be Normal, so all of
     * the continuations accumulate on the right.
     */
    private static final class Codense<A> extends TrampolineIO<A> {
        // The Normal subcomputation
        private final Normal<Object> sub;

        // The current continuation
        private final Function<Object, TrampolineIO<A>> cont;

        Codense(Normal<Object> t, Function<Object, TrampolineIO<A>> k) {
            sub = t; cont = k;
        }

        @Override
        protected <R> R fold(Function<Normal<A>, R> n, Function<Codense<A>, R> gs) {
            return gs.apply(this);
        }

        /**
         * The monadic bind constructs a new Codense whose subcomputation is still
         * `sub`, and Kleisli-composes the continuations.
         */
        @Override
        public <B> TrampolineIO<B> bind(Function<? super A, ? extends TrampolineIO<B>> f) {
            return codense(sub, x -> suspend(() -> cont.apply(x).bind(f)));
        }

        /**
         * The resumption of a Codense is the resumption of its subcomputation.
         * If that computation is done, its result gets shifted into the
         * continuation.
         */
        @Override
        @SuppressWarnings("Convert2MethodRef")
        public IO<Either<Supplier<TrampolineIO<A>>, A>> resume() {
            return sub.resume().map(e -> left(e.either(p -> () -> {
                Function<Normal<Object>, TrampolineIO<A>> f =
                    n -> n.foldNormal(x -> cont.apply(x), t -> t.get().bind(cont));
                Function<Codense<Object>, TrampolineIO<A>> g =
                    c -> codense(c.sub, x -> c.cont.apply(x).bind(cont));
                return p.get().fold(f, g);
            }, x -> () -> cont.apply(x))));
        }
    }

    /**
     * A suspended computation that can be resumed.
     */
    private static final class Suspend<A> extends Normal<A> {
        private final Supplier<TrampolineIO<A>> suspension;

        Suspend(Supplier<TrampolineIO<A>> s) {
            suspension = s;
        }

        @Override
        protected <B, R extends TrampolineIO<B>> TrampolineIO<B>
        foldNormal(Function<A, R> pure, Function<Supplier<TrampolineIO<A>>, R> k) {
            return k.apply(suspension);
        }

        @Override
        protected <R> R fold(Function<Normal<A>, R> n, Function<Codense<A>, R> gs) {
            return n.apply(this);
        }

        @Override
        public IO<Either<Supplier<TrampolineIO<A>>, A>> resume() {
            return IO.pure(left(suspension));
        }
    }

    /**
     * A pure value at the leaf of a computation.
     */
    private static final class Pure<A> extends Normal<A> {
        private final A value;

        Pure(A a) {
            value = a;
        }

        @Override
        protected <B, R extends TrampolineIO<B>> TrampolineIO<B>
        foldNormal(Function<A, R> pure, Function<Supplier<TrampolineIO<A>>, R> k) {
            return pure.apply(value);
        }

        @Override
        protected <R> R fold(Function<Normal<A>, R> n, Function<Codense<A>, R> gs) {
            return n.apply(this);
        }

        @Override
        public IO<Either<Supplier<TrampolineIO<A>>, A>> resume() {
            return IO.pure(right(value));
        }
    }

    /**
     * A lifted IO action.
     */
    private static final class Lift<A> extends Normal<A> {
        private final IO<A> action;

        Lift(IO<A> a) {
            action = a;
        }

        @Override
        protected <B, R extends TrampolineIO<B>> TrampolineIO<B>
        foldNormal(Function<A, R> pure, Function<Supplier<TrampolineIO<A>>, R> k) {
            return lift(action.bind(a -> pure.apply(a).resume()).map(e -> e.right()));
        }

        @Override
        protected <R> R fold(Function<Normal<A>, R> n, Function<Codense<A>, R> gs) {
            return n.apply(this);
        }

        @Override
        public IO<Either<Supplier<TrampolineIO<A>>, A>> resume() {
            return action.map(Either::right);
        }
    }

    @SuppressWarnings("unchecked")
    private static <A, B> Codense<B> codense(Normal<A> a, Function<? super A, ? extends TrampolineIO<B>> k) {
        return new Codense<>((Normal<Object>)a, (Function<Object, TrampolineIO<B>>)k);
    }

    /**
     * Constructs a pure computation that results in the given value.
     *
     * @param a the value of the result
     * @return a trampoline that results in the given value
     */
    public static <A> TrampolineIO<A> pure(A a) {
        return new Pure<>(a);
    }

    private static final TrampolineIO<Unit> _unit = pure(Unit.U);

    /**
     * Returns a do nothing computation.
     */
    public static TrampolineIO<Unit> unit() {
        return _unit;
    }

    /**
     * Synonym for {@link #pure(Object) pure}.
     */
    public static <A> TrampolineIO<A> immediate(A a) {
        return pure(a);
    }

    /**
     * Constructs a pure computation that results in the given lazy evaluation
     * thunk.
     *
     * @param a a thunk that eventually produce computation result
     * @return the trampoline that holds the computation
     */
    public static <A> TrampolineIO<A> lazy(IO<A> a) {
        return lift(a);
    }

    /**
     * Constructs a computation that performs the given action with no result.
     */
    public static TrampolineIO<Unit> action(VoidIO a) {
        return lift(a);
    }

    /**
     * Promote an IO action to a trampoline action.
     *
     * @param m the IO action
     * @return the prompted trampoline action
     */
    public static <A> TrampolineIO<A> lift(IO<A> m) {
        return new Lift<>(m);
    }

    /**
     * Promote an IO action to a trampoline action.
     *
     * @param m the IO action
     * @return the promoted trampoline action
     */
    public static TrampolineIO<Unit> lift_(VoidIO m) {
        return lift(m);
    }

    /**
     * Suspends the given computation in a thunk.
     *
     * @param a a trampoline suspended in a thunk
     * @return a trampoline whose next step runs the given thunk
     */
    public static <A> TrampolineIO<A> suspend(Supplier<TrampolineIO<A>> a) {
        return new Suspend<>(a);
    }

    /**
     * A convenient method to chain trampolines together without a 'do' notation.
     * Same as {@code suspend(a).bind(f)}.
     *
     * @param a a trampoline suspended in a thunk
     * @param f a transfer function that yields a new trampoline
     * @return the chained trampoline
     */
    public static <A, B> TrampolineIO<B>
    suspend(Supplier<TrampolineIO<A>> a, Function<? super A, ? extends TrampolineIO<B>> f) {
        return suspend(a).bind(f);
    }

    /**
     * Returns a single step of this computation.
     *
     * @return the next step of this computation.
     */
    public abstract IO<Either<Supplier<TrampolineIO<A>>, A>> resume();

    /**
     * Runs this computation all the way to the end, in constant stack.
     *
     * @return the final result of this computation
     */
    @SuppressWarnings({"Convert2Lambda", "override"})
    public IO<A> run() {
        return new IO<A>() {
            public A runIO() throws IOException {
                TrampolineIO<A> current = TrampolineIO.this;
                while (true) {
                    Either<Supplier<TrampolineIO<A>>, A> x = current.resume().runIO();
                    if (x.isRight())
                        return x.right();
                    current = x.left().get();
                }
            }
        };
    }

    protected abstract <R> R fold(Function<Normal<A>, R> n, Function<Codense<A>, R> gs);

    /**
     * Maps the given function across the result of this trampoline
     *
     * @param f a function that gets applied to the result of this trampoline
     * @return  a new trampoline that runs trampoline, then applies the given function
     *          to the result.
     */
    public <B> TrampolineIO<B> map(Function<? super A, ? extends B> f) {
        return bind(a -> pure(f.apply(a)));
    }

    /**
     * Binds the given continuation to the result of this trampoline.
     *
     * @param f a function that constructs a trampoline from the result of this
     *          trampoline
     * @return  a new trampoline that runs this trampoline, then continues with
     *          the given function
     */
    public abstract <B> TrampolineIO<B> bind(Function<? super A, ? extends TrampolineIO<B>> f);

    /**
     * Transfer a trampoline by discarding the intermediate value.
     *
     * @param b the new trampoline transformation
     * @return a trampoline that transfers this trampoline to the given trampoline
     */
    public <B> TrampolineIO<B> then(TrampolineIO<B> b) {
        return bind(__ -> b);
    }

    /**
     * Transfer a trampoline by discarding the intermediate value.
     *
     * @param b the new trampoline transformation
     * @return a trampoline that transfers this trampoline to the given trampoline
     */
    public <B> TrampolineIO<B> then(Supplier<TrampolineIO<B>> b) {
        return bind(__ -> b.get());
    }

    /**
     * Evaluate each action in the sequence from left to right, and collect
     * the result.
     */
    public static <A> TrampolineIO<Seq<A>> flatM(Seq<TrampolineIO<A>> ms) {
        return ms.foldRight_(pure(Seq.nil()), liftM2(Seq::cons));
    }

    /**
     * Evaluate each action in the sequence from left to right, and ignore
     * the result.
     */
    public static <A> TrampolineIO<Unit> sequence(Foldable<TrampolineIO<A>> ms) {
        return ms.foldRight(pure(Unit.U), TrampolineIO::then);
    }

    /**
     * The {@code mapM} is analogous to {@link Seq#map(Function) map} except that
     * its result is encapsulated in a {@code Trampoline}.
     */
    public static <A, B> TrampolineIO<Seq<B>>
    mapM(Seq<A> xs, Function<? super A, ? extends TrampolineIO<B>> f) {
        return flatM(xs.map(f));
    }

    /**
     * {@code mapM_} is equivalent to {@code sequence(xs.map(f))}.
     */
    public static <A, B> TrampolineIO<Unit>
    mapM_(Foldable<A> xs, Function<? super A, ? extends TrampolineIO<B>> f) {
        return xs.foldRight(unit(), (x, r) -> f.apply(x).then(r));
    }

    /**
     * Generalizes {@link Seq#zip(Seq,BiFunction)} to arbitrary monads.
     * Bind the given function to the given computations with a final join.
     */
    public static <A, B, C> TrampolineIO<Seq<C>>
    zipM(Seq<A> xs, Seq<B> ys, BiFunction<? super A, ? super B, TrampolineIO<C>> f) {
        return flatM(Seq.zip(xs, ys, f));
    }

    /**
     * The extension of {@link #zipM(Seq,Seq,BiFunction) zipM} which ignores the
     * final result.
     */
    public static <A, B, C> TrampolineIO<Unit>
    zipM_(Seq<A> xs, Seq<B> ys, BiFunction<? super A, ? super B, TrampolineIO<C>> f) {
        return sequence(Seq.zip(xs, ys, f));
    }

    /**
     * This generalizes the list-based filter function.
     */
    public static <A> TrampolineIO<Seq<A>>
    filterM(Seq<A> xs, Function<? super A, ? extends TrampolineIO<Boolean>> p) {
        return xs.isEmpty()
            ? pure(Seq.nil())
            : p.apply(xs.head()).bind(flg ->
              filterM(xs.tail(), p).bind(ys ->
              pure(flg ? Seq.cons(xs.head(), ys) : ys)));
    }

    /**
     * The {@code foldM} is analogous to {@link Seq#foldLeft(Object,BiFunction) foldLeft},
     * except that its result is encapsulated in an {@code TrampolineIO}. Note that
     * {@code foldM} works from left-to-right over the lists arguments. If right-to-left
     * evaluation is required, the input list should be reversed.
     */
    public static <A, B> TrampolineIO<B>
    foldM(B r0, Foldable<A> xs, BiFunction<B, ? super A, TrampolineIO<B>> f) {
        return xs.foldLeft(pure(r0), (m, x) -> m.bind(r -> f.apply(r, x)));
    }

    /**
     * Perform the action n times, gathering the results.
     */
    public static <A> TrampolineIO<Seq<A>> replicateM(int n, TrampolineIO<A> a) {
        return flatM(Seq.replicate(n, a));
    }

    /**
     * Perform the action n times, discards the result.
     */
    public static <A> TrampolineIO<Unit> replicateM_(int n, TrampolineIO<A> a) {
        return sequence(Seq.replicate(n, a));
    }

    /**
     * Kleisli composition of monads.
     */
    public static <A, B, C> Function<A, TrampolineIO<C>>
    kleisli(Function<A, TrampolineIO<B>> f, Function<B, TrampolineIO<C>> g) {
        return x -> f.apply(x).bind(g);
    }

    /**
     * Promote a function to a trampoline function.
     */
    public static <A, B> Function<TrampolineIO<A>, TrampolineIO<B>>
    liftM(Function<? super A, ? extends B> f) {
        return m -> m.map(f);
    }

    /**
     * Promote a function to a trampoline function.
     */
    public static <A, B, C> BiFunction<TrampolineIO<A>, TrampolineIO<B>, TrampolineIO<C>>
    liftM2(BiFunction<? super A, ? super B, ? extends C> f) {
        return (m1, m2) -> m1.bind(x1 -> m2.map(x2 -> f.apply(x1, x2)));
    }

    /**
     * Promote a function to a trampoline function.
     */
    public static <A, B, C, D> TriFunction<TrampolineIO<A>, TrampolineIO<B>, TrampolineIO<C>, TrampolineIO<D>>
    liftM3(TriFunction<? super A, ? super B, ? super C, ? extends D> f) {
        return (m1, m2, m3) -> m1.bind(x1 -> m2.bind(x2 -> m3.map(x3 -> f.apply(x1, x2, x3))));
    }
}
