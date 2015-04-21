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
import com.cloudway.platform.common.fp.data.Traversable;
import com.cloudway.platform.common.fp.data.Unit;
import com.cloudway.platform.common.fp.io.IO;
import com.cloudway.platform.common.fp.io.VoidIO;
import com.cloudway.platform.common.fp.$;

import static com.cloudway.platform.common.fp.data.Either.*;

/**
 * A trampoline stacked on top of an I/O action.
 *
 * @param <A> the type of computation result
 *
 * @see Trampoline
 */
public abstract class TrampolineIO<A> implements $<TrampolineIO.µ, A> {
    /**
     * A Normal Trampoline is either done or suspended, and is allowed to be a
     * subcomputation of a Codense.  This is the pointed functor part of the
     * Trampoline monad.
     */
     private static abstract class Normal<A> extends TrampolineIO<A> {
        protected abstract <B, R extends TrampolineIO<B>> TrampolineIO<B>
        foldNormal(Function<A, R> pure, Function<Supplier<TrampolineIO<A>>, R> k);

        @Override
        public <B> TrampolineIO<B> bind(Function<? super A, ? extends $<µ, B>> f) {
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
        public <B> TrampolineIO<B> bind(Function<? super A, ? extends $<µ, B>> f) {
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
    private static <A, B> Codense<B> codense(Normal<A> a, Function<? super A, ? extends $<µ, B>> k) {
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
    @SuppressWarnings("unchecked")
    public static <A> TrampolineIO<A> suspend(Supplier<? extends $<µ, A>> a) {
        return new Suspend<>((Supplier<TrampolineIO<A>>)a);
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
    suspend(Supplier<TrampolineIO<A>> a, Function<? super A, ? extends $<µ, B>> f) {
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
    public abstract <B> TrampolineIO<B> bind(Function<? super A, ? extends $<µ, B>> f);

    /**
     * Transfer a trampoline by discarding the intermediate value.
     *
     * @param b the new trampoline transformation
     * @return a trampoline that transfers this trampoline to the given trampoline
     */
    public <B> TrampolineIO<B> then($<µ, B> b) {
        return bind(__ -> b);
    }

    /**
     * Transfer a trampoline by discarding the intermediate value.
     *
     * @param b the new trampoline transformation
     * @return a trampoline that transfers this trampoline to the given trampoline
     */
    public <B> TrampolineIO<B> then(Supplier<? extends $<µ, B>> b) {
        return bind(__ -> b.get());
    }

    // Monad

    public static final class µ implements Monad<µ> {
        @Override
        public <A> TrampolineIO<A> pure(A a) {
            return TrampolineIO.pure(a);
        }

        @Override
        public <A> TrampolineIO<A> lazy(Supplier<A> a) {
            return TrampolineIO.lazy(a::get);
        }

        @Override
        public TrampolineIO<Unit> action(Runnable a) {
            return TrampolineIO.action(a::run);
        }

        @Override
        public <A, B> TrampolineIO<B> map($<µ, A> a, Function<? super A, ? extends B> f) {
            return narrow(a).map(f);
        }

        @Override
        public <A, B> TrampolineIO<B> bind($<µ, A> a, Function<? super A, ? extends $<µ, B>> k) {
            return narrow(a).bind(k);
        }

        @Override
        public <A, B> TrampolineIO<B> seqR($<µ, A> a, $<µ, B> b) {
            return narrow(a).then(b);
        }

        @Override
        public <A, B> TrampolineIO<B> seqR($<µ, A> a, Supplier<? extends $<µ, B>> b) {
            return narrow(a).then(b);
        }
    }

    public static final µ tclass = new µ();

    @Override
    public µ getTypeClass() {
        return tclass;
    }

    public static <A> TrampolineIO<A> narrow($<µ, A> value) {
        return (TrampolineIO<A>)value;
    }

    public static <A> IO<A> run($<µ, A> m) {
        return narrow(m).run();
    }

    // Convenient static monad methods

    public static <T, A> TrampolineIO<? extends Traversable<T, A>>
    flatM(Traversable<T, ? extends $<µ, A>> ms) {
        return narrow(tclass.flatM(ms));
    }

    @SuppressWarnings("unchecked")
    public static <A> TrampolineIO<Seq<A>> flatM(Seq<? extends $<µ, A>> ms) {
        return (TrampolineIO<Seq<A>>)tclass.flatM(ms);
    }

    public static <T, A, B> TrampolineIO<? extends Traversable<T, B>>
    mapM(Traversable<T, A> xs, Function<? super A, ? extends $<µ, B>> f) {
        return narrow(tclass.mapM(xs, f));
    }

    @SuppressWarnings("unchecked")
    public static <A, B> TrampolineIO<Seq<B>>
    mapM(Seq<A> xs, Function<? super A, ? extends $<µ, B>> f) {
        return (TrampolineIO<Seq<B>>)tclass.mapM(xs, f);
    }

    public static <A> TrampolineIO<Unit> sequence(Foldable<? extends $<µ, A>> ms) {
        return narrow(tclass.sequence(ms));
    }

    public static <A, B> TrampolineIO<Unit>
    mapM_(Foldable<A> xs, Function<? super A, ? extends $<µ, B>> f) {
        return narrow(tclass.mapM_(xs, f));
    }

    public static <A> TrampolineIO<Seq<A>>
    filterM(Seq<A> xs, Function<? super A, ? extends $<µ, Boolean>> p) {
        return narrow(tclass.filterM(xs, p));
    }

    public static <A, B> TrampolineIO<B>
    foldM(B r0, Foldable<A> xs, BiFunction<B, ? super A, ? extends $<µ, B>> f) {
        return narrow(tclass.foldM(r0, xs, f));
    }

    public static <A> TrampolineIO<Seq<A>> replicateM(int n, $<µ, A> a) {
        return narrow(tclass.replicateM(n, a));
    }

    public static <A> TrampolineIO<Unit> replicateM_(int n, $<µ, A> a) {
        return narrow(tclass.replicateM_(n, a));
    }
}
