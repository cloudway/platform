/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.control;

import java.util.function.Function;
import java.util.function.BiFunction;
import java.util.function.Supplier;

import com.cloudway.fp.data.Fn;
import com.cloudway.fp.data.Either;
import com.cloudway.fp.data.Foldable;
import com.cloudway.fp.data.Seq;
import com.cloudway.fp.data.Traversable;
import com.cloudway.fp.data.Unit;
import com.cloudway.fp.$;

import static com.cloudway.fp.control.Conditionals.with;
import static com.cloudway.fp.data.Either.*;

/**
 * A Trampoline is a potentially branching computation that can be stepped
 * through and executed in constant stack. It represent suspendable coroutines
 * with subroutine calls, reified as a data structure.
 *
 * @param <A> the type of computation result
 */
public abstract class Trampoline<A> implements $<Trampoline.µ, A> {
    /**
     * A Normal Trampoline is either done or suspended, and is allowed to be a
     * subcomputation of a Codense.  This is the pointed functor part of the
     * Trampoline monad.
     */
     private static abstract class Normal<A> extends Trampoline<A> {
        protected abstract <R> R foldNormal(Function<A, R> pure, Function<Supplier<Trampoline<A>>, R> k);

        @Override
        public <B> Trampoline<B> bind(Function<? super A, ? extends $<µ, B>> f) {
            return codense(this, f);
        }
    }

    /**
     * A Codense Trampoline delimits a subcomputation and tracks its current
     * continuation. Subcomputations are only allowed to be Normal, so all of
     * the continuations accumulate on the right.
     */
    private static final class Codense<A> extends Trampoline<A> {
        // The Normal subcomputation
        private final Normal<Object> sub;

        // The current continuation
        private final Function<Object, Trampoline<A>> cont;

        private Codense(Normal<Object> t, Function<Object, Trampoline<A>> k) {
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
        public <B> Trampoline<B> bind(Function<? super A, ? extends $<µ, B>> f) {
            return codense(sub, x -> suspend(() -> cont.apply(x).bind(f)));
        }

        /**
         * The resumption of a Codense is the resumption of its subcomputation.
         * If that computation is done, its result gets shifted into the
         * continuation.
         */
        @Override
        @SuppressWarnings("Convert2MethodRef")
        public Either<Supplier<Trampoline<A>>, A> resume() {
            return left(sub.resume().either(p -> () -> {
                Function<Normal<Object>, Trampoline<A>> f =
                    n -> n.foldNormal(x -> cont.apply(x), t -> t.get().bind(cont));
                Function<Codense<Object>, Trampoline<A>> g =
                    c -> codense(c.sub, x -> c.cont.apply(x).bind(cont));
                return p.get().fold(f, g);
            }, x -> () -> cont.apply(x)));
        }
    }

    /**
     * A suspended computation that can be resumed.
     */
    private static final class Suspend<A> extends Normal<A> {
        private final Supplier<Trampoline<A>> suspension;

        private Suspend(Supplier<Trampoline<A>> s) {
            suspension = s;
        }

        @Override
        protected <R> R foldNormal(Function<A, R> pure, Function<Supplier<Trampoline<A>>, R> k) {
            return k.apply(suspension);
        }

        @Override
        protected <R> R fold(Function<Normal<A>, R> n, Function<Codense<A>, R> gs) {
            return n.apply(this);
        }

        @Override
        public Either<Supplier<Trampoline<A>>, A> resume() {
            return left(suspension);
        }
    }

    /**
     * A pure value at the leaf of a computation.
     */
    private static final class Pure<A> extends Normal<A> {
        private final A value;

        private Pure(A a) {
            value = a;
        }

        @Override
        protected <R> R foldNormal(Function<A, R> pure, Function<Supplier<Trampoline<A>>, R> k) {
            return pure.apply(value);
        }

        @Override
        protected <R> R fold(Function<Normal<A>, R> n, Function<Codense<A>, R> gs) {
            return n.apply(this);
        }

        @Override
        public Either<Supplier<Trampoline<A>>, A> resume() {
            return right(value);
        }
    }

    /**
     * A thunk of computation.
     */
    private static final class Thunk<A> extends Normal<A> {
        private final Supplier<A> thunk;

        private Thunk(Supplier<A> t) {
            thunk = t;
        }

        @Override
        protected <R> R foldNormal(Function<A, R> pure, Function<Supplier<Trampoline<A>>, R> k) {
            return pure.apply(thunk.get());
        }

        @Override
        protected <R> R fold(Function<Normal<A>, R> n, Function<Codense<A>, R> gs) {
            return n.apply(this);
        }

        @Override
        public Either<Supplier<Trampoline<A>>, A> resume() {
            return right(thunk.get());
        }
    }

    @SuppressWarnings("unchecked")
    private static <A, B> Codense<B> codense(Normal<A> a, Function<? super A, ? extends $<µ, B>> k) {
        return new Codense<>((Normal<Object>)a, (Function<Object, Trampoline<B>>)k);
    }

    /**
     * Constructs a pure computation that results in the given value.
     *
     * @param a the value of the result
     * @return a trampoline that results in the given value
     */
    public static <A> Trampoline<A> pure(A a) {
        return new Pure<>(a);
    }

    private static final Trampoline<Unit> _unit = pure(Unit.U);

    /**
     * Returns a do nothing computation.
     */
    public static Trampoline<Unit> unit() {
        return _unit;
    }

    /**
     * Synonym for {@link #pure(Object) pure}.
     */
    public static <A> Trampoline<A> immediate(A a) {
        return pure(a);
    }

    /**
     * Constructs a pure computation that results in the given lazy evaluation
     * thunk.
     *
     * @param a a thunk that eventually produce computation result
     * @return the trampoline that holds the computation
     */
    public static <A> Trampoline<A> lazy(Supplier<A> a) {
        return new Thunk<>(Fn.lazy(a));
    }

    /**
     * Constructs a computation that performs the given action with no result.
     */
    public static Trampoline<Unit> action(Runnable a) {
        return new Thunk<>(() -> { a.run(); return Unit.U; });
    }

    /**
     * Suspends the given computation in a thunk.
     *
     * @param a a trampoline suspended in a thunk
     * @return a trampoline whose next step runs the given thunk
     */
    @SuppressWarnings("unchecked")
    public static <A> Trampoline<A> suspend(Supplier<? extends $<µ, A>> a) {
        return new Suspend<>((Supplier<Trampoline<A>>)a);
    }

    /**
     * A convenient method to chain trampolines together without a 'do' notation.
     * Same as {@code suspend(a).bind(f)}.
     *
     * @param a a trampoline suspended in a thunk
     * @param f a transfer function that yields a new trampoline
     * @return the chained trampoline
     */
    public static <A, B> Trampoline<B>
    suspend(Supplier<Trampoline<A>> a, Function<? super A, ? extends $<µ, B>> f) {
        return suspend(a).bind(f);
    }

    /**
     * Returns a single step of this computation.
     *
     * @return the next step of this computation.
     */
    public abstract Either<Supplier<Trampoline<A>>, A> resume();

    /**
     * Runs this computation all the way to the end, in constant stack.
     *
     * @return the end result of this computation
     */
    public A run() {
        Trampoline<A> current = this;
        while (true) {
            Either<Supplier<Trampoline<A>>, A> x = current.resume();
            if (x.isRight())
                return x.right();
            current = x.left().get();
        }
    }

    protected abstract <R> R fold(Function<Normal<A>, R> n, Function<Codense<A>, R> gs);

    /**
     * Maps the given function across the result of this trampoline
     *
     * @param f a function that gets applied to the result of this trampoline
     * @return  a new trampoline that runs trampoline, then applies the given function
     *          to the result.
     */
    public <B> Trampoline<B> map(Function<? super A, ? extends B> f) {
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
    public abstract <B> Trampoline<B> bind(Function<? super A, ? extends $<µ, B>> f);

    /**
     * Transfer a trampoline by discarding the intermediate value.
     *
     * @param b the new trampoline transformation
     * @return a trampoline that transfers this trampoline to the given trampoline
     */
    public <B> Trampoline<B> then($<µ, B> b) {
        return bind(__ -> b);
    }

    /**
     * Transfer a trampoline by discarding the intermediate value.
     *
     * @param b the new trampoline transformation
     * @return a trampoline that transfers this trampoline to the given trampoline
     */
    public <B> Trampoline<B> then(Supplier<? extends $<µ, B>> b) {
        return bind(__ -> suspend(b));
    }

    /**
     * Combines two trampolines so they run cooperatively. The results are
     * combined with the given function.
     *
     * @param ta the first trampoline to combine with
     * @param tb the second trampoline to combine with
     * @param f a function to combine the results of the two trampolines
     * @return a new trampoline that runs two trampolines simultaneously
     */
    public static <A, B, C> Trampoline<C>
    zip(Trampoline<A> ta, Trampoline<B> tb, BiFunction<? super A, ? super B, ? extends C> f) {
        return with(ta.resume(), tb.resume()).<Trampoline<C>>get()
            .when(Right(a -> Right(b -> pure(f.apply(a, b)))))
            .when(Right(a -> Left (b -> suspend(() -> zip(pure(a), b.get(), f)))))
            .when(Left (a -> Right(b -> suspend(() -> zip(a.get(), pure(b), f)))))
            .when(Left (a -> Left (b -> suspend(() -> zip(a.get(), b.get(), f)))))
            .get();
    }

    // Monad

    public static final class µ implements Monad<µ> {
        @Override
        public <A> Trampoline<A> pure(A a) {
            return Trampoline.pure(a);
        }

        @Override
        public <A> Trampoline<A> lazy(Supplier<A> a) {
            return Trampoline.lazy(a);
        }

        @Override
        public Trampoline<Unit> action(Runnable a) {
            return Trampoline.action(a);
        }

        @Override
        public <A, B> Trampoline<B> map($<µ, A> a, Function<? super A, ? extends B> f) {
            return narrow(a).map(f);
        }

        @Override
        public <A, B> Trampoline<B> bind($<µ, A> a, Function<? super A, ? extends $<µ, B>> k) {
            return narrow(a).bind(k);
        }

        @Override
        public <A, B> Trampoline<B> seqR($<µ, A> a, $<µ, B> b) {
            return narrow(a).then(b);
        }

        @Override
        public <A, B> Trampoline<B> seqR($<µ, A> a, Supplier<? extends $<µ, B>> b) {
            return narrow(a).then(b);
        }
    }

    public static final µ tclass = new µ();

    @Override
    public µ getTypeClass() {
        return tclass;
    }

    public static <A> Trampoline<A> narrow($<µ, A> value) {
        return (Trampoline<A>)value;
    }

    public static <A> A run($<µ, A> m) {
        return narrow(m).run();
    }

    // Convenient static monad methods

    public static <T, A> Trampoline<? extends Traversable<T, A>>
    flatM(Traversable<T, ? extends $<µ, A>> ms) {
        return narrow(tclass.flatM(ms));
    }

    @SuppressWarnings("unchecked")
    public static <A> Trampoline<Seq<A>> flatM(Seq<? extends $<µ, A>> ms) {
        return (Trampoline<Seq<A>>)tclass.flatM(ms);
    }

    public static <T, A, B> Trampoline<? extends Traversable<T, B>>
    mapM(Traversable<T, A> xs, Function<? super A, ? extends $<µ, B>> f) {
        return narrow(tclass.mapM(xs, f));
    }

    @SuppressWarnings("unchecked")
    public static <A, B> Trampoline<Seq<B>>
    mapM(Seq<A> xs, Function<? super A, ? extends $<µ, B>> f) {
        return (Trampoline<Seq<B>>)tclass.mapM(xs, f);
    }

    public static <A> Trampoline<Unit> sequence(Foldable<? extends $<µ, A>> ms) {
        return narrow(tclass.sequence(ms));
    }

    public static <A, B> Trampoline<Unit>
    mapM_(Foldable<A> xs, Function<? super A, ? extends $<µ, B>> f) {
        return narrow(tclass.mapM_(xs, f));
    }

    public static <A> Trampoline<Seq<A>>
    filterM(Seq<A> xs, Function<? super A, ? extends $<µ, Boolean>> p) {
        return narrow(tclass.filterM(xs, p));
    }

    public static <A, B> Trampoline<B>
    foldM(B r0, Foldable<A> xs, BiFunction<B, ? super A, ? extends $<µ, B>> f) {
        return narrow(tclass.foldM(r0, xs, f));
    }

    public static <A> Trampoline<Seq<A>> replicateM(int n, $<µ, A> a) {
        return narrow(tclass.replicateM(n, a));
    }

    public static <A> Trampoline<Unit> replicateM_(int n, $<µ, A> a) {
        return narrow(tclass.replicateM_(n, a));
    }
}
