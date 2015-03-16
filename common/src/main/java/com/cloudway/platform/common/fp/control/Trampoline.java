/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.control;

import java.util.function.Function;
import java.util.function.BiFunction;
import java.util.function.Supplier;

import com.cloudway.platform.common.fp.data.Fn;
import com.cloudway.platform.common.fp.data.Either;
import com.cloudway.platform.common.fp.data.Seq;
import com.cloudway.platform.common.fp.data.Unit;
import com.cloudway.platform.common.fp.function.TriFunction;
import static com.cloudway.platform.common.fp.control.Conditionals.with;
import static com.cloudway.platform.common.fp.data.Either.*;

/**
 * A Trampoline is a potentially branching computation that can be stepped
 * through and executed in constant stack. It represent suspendable coroutines
 * with subroutine calls, reified as a data structure.
 *
 * @param <A> the type of computation result
 */
public abstract class Trampoline<A> {
    /**
     * A Normal Trampoline is either done or suspended, and is allowed to be a
     * subcomputation of a Codense.  This is the pointed functor part of the
     * Trampoline monad.
     */
     private static abstract class Normal<A> extends Trampoline<A> {
        protected abstract <R> R foldNormal(Function<A, R> pure, Function<Supplier<Trampoline<A>>, R> k);

        @Override
        public <B> Trampoline<B> bind(Function<? super A, ? extends Trampoline<B>> f) {
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
        public <B> Trampoline<B> bind(Function<? super A, ? extends Trampoline<B>> f) {
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
    private static <A, B> Codense<B> codense(Normal<A> a, Function<? super A, ? extends Trampoline<B>> k) {
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
    public static <A> Trampoline<A> suspend(Supplier<Trampoline<A>> a) {
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
    public static <A, B> Trampoline<B>
    suspend(Supplier<Trampoline<A>> a, Function<? super A, ? extends Trampoline<B>> f) {
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
    public abstract <B> Trampoline<B> bind(Function<? super A, ? extends Trampoline<B>> f);

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

    /**
     * Evaluate each action in the sequence from left to right, and collect
     * the result.
     */
    public static <A> Trampoline<Seq<A>> flatM(Seq<Trampoline<A>> ms) {
        return ms.foldRight_(pure(Seq.nil()), liftM2(Seq::cons));
    }

    /**
     * The {@code mapM} is analogous to {@link Seq#map(Function) map} except that
     * its result is encapsulated in a {@code Trampoline}.
     */
    public static <A, B> Trampoline<Seq<B>> mapM(Seq<A> xs, Function<? super A, ? extends Trampoline<B>> f) {
        return flatM(xs.map(f));
    }

    /**
     * This generalizes the list-based filter function.
     */
    public static <A> Trampoline<Seq<A>> filterM(Seq<A> xs, Function<? super A, ? extends Trampoline<Boolean>> p) {
        return xs.isEmpty()
            ? pure(Seq.nil())
            : p.apply(xs.head()).bind(flg ->
              filterM(xs.tail(), p).bind(ys ->
              pure(flg ? Seq.cons(xs.head(), ys) : ys)));
    }

    /**
     * The {@code foldM} is analogous to {@link Seq#foldLeft(Object,BiFunction) foldLeft},
     * except that its result is encapsulated in an {@code Trampoline}. Note that
     * {@code foldM} works from left-to-right over the lists arguments. If right-to-left
     * evaluation is required, the input list should be reversed.
     */
    public static <A, B> Trampoline<B> foldM(B r0, Seq<A> xs, BiFunction<B, ? super A, Trampoline<B>> f) {
        return xs.foldLeft(pure(r0), (m, x) -> m.bind(r -> f.apply(r, x)));
    }

    /**
     * Kleisli composition of monads.
     */
    public static <A, B, C> Function<A, Trampoline<C>>
    compose(Function<A, Trampoline<B>> f, Function<B, Trampoline<C>> g) {
        return x -> f.apply(x).bind(g);
    }

    /**
     * Promote a function to a trampoline function.
     */
    public static <A, B> Function<Trampoline<A>, Trampoline<B>>
    liftM(Function<? super A, ? extends B> f) {
        return m -> m.map(f);
    }

    /**
     * Promote a function to a trampoline function.
     */
    public static <A, B, C> BiFunction<Trampoline<A>, Trampoline<B>, Trampoline<C>>
    liftM2(BiFunction<? super A, ? super B, ? extends C> f) {
        return (m1, m2) -> m1.bind(x1 -> m2.map(x2 -> f.apply(x1, x2)));
    }

    /**
     * Promote a function to a trampoline function.
     */
    public static <A, B, C, D> TriFunction<Trampoline<A>, Trampoline<B>, Trampoline<C>, Trampoline<D>>
    liftM3(TriFunction<? super A, ? super B, ? super C, ? extends D> f) {
        return (m1, m2, m3) -> m1.bind(x1 -> m2.bind(x2 -> m3.map(x3 -> f.apply(x1, x2, x3))));
    }
}
