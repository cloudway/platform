/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.control;

import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

import com.cloudway.platform.common.fp.data.Fn;
import com.cloudway.platform.common.fp.data.Seq;
import com.cloudway.platform.common.fp.data.Unit;
import com.cloudway.platform.common.fp.function.TriFunction;

/**
 * <p>Continuation monad.</p>
 *
 * <p>Continuation&lt;A, R&gt; is a CPS computation that produces an intermediate
 * result of type {@code A} within a CPS computation whose final type is {@code R}</p>
 *
 * <p>The {@code yield} method simply creates a continuation which passes the
 * value on.</p>
 *
 * <p>The {@code bind} method adds the bound function into the continuation chain.</p>
 *
 * @param <A> the type of intermediate result of computation
 */
public final class Cont<A> {
    // the CPS transfer type
    @FunctionalInterface
    private interface K<A, R> {
        R apply(Function<A, R> f);
    }

    // the CPS transfer function
    private final K<A, ?> kf;

    // construct a continuation monad
    private <R> Cont(K<A, R> f) {
        this.kf = f;
    }

    // helper method to construct continuation from a transfer function
    private static <A, R> Cont<A> $(K<A, R> f) {
        return new Cont<>(f);
    }

    /**
     * Run the continuation to get final result.
     *
     * @param k the function that accept computation value and return final result
     * @return the final result of computation
     */
    @SuppressWarnings("unchecked")
    public <R> R run(Function<? super A, ? extends R> k) {
        return ((K<A,R>)kf).apply((Function<A,R>)k);
    }

    /**
     * The result of running a CPS computation with 'return' as the final
     * continuation.
     *
     * <p><pre>
     *     yield(x).eval() == x
     * </pre></p>
     */
    public A eval() {
        return run(Fn.id());
    }

    /**
     * Execute the continuation, discard the final result.
     *
     * @param k the function that accept computation value and return nothing
     */
    public void exec(Consumer<? super A> k) {
        kf.apply(x -> {
            k.accept(x);
            return null;
        });
    }

    /**
     * Yield a pure computation that results in the given value.
     *
     * @param a the pure value of the computation result
     * @return the continuation that yield the pure value
     */
    public static <A> Cont<A> pure(A a) {
        return $(f -> f.apply(a));
    }

    /**
     * Synonym of {@link #pure(Object)}.
     */
    public static <A> Cont<A> yield(A a) {
        return pure(a);
    }

    /**
     * Yield a thunk that has a lazily evaluated computation.
     *
     * @param a a thunk that eventually produce computation result
     * @return the continuation that yield the computation.
     */
    public static <A> Cont<A> lazy(Supplier<A> a) {
        Supplier<A> t = Fn.lazy(a);
        return $(f -> f.apply(t.get()));
    }

    /**
     * Yield an action computation that has no result.
     */
    public static Cont<Unit> action(Runnable a) {
        return $(f -> {
            a.run();
            return f.apply(Unit.U);
        });
    }

    /**
     * Transfer a continuation by feeding the intermediate value to the given
     * function and yield the result of function application.
     *
     * @param f the function that accept a intermediate value and compute
     *          a new value
     * @return the continuation that yield the result of function application
     */
    public <B> Cont<B> map(Function<? super A, ? extends B> f) {
        return $(c -> run(c.compose(f)));
    }

    /**
     * Transfer a continuation by feeding the intermediate value to the given
     * function.
     *
     * @param k the function that accept a intermediate value and transfer
     *          to a new continuation
     * @return the new continuation applying the transfer function
     */
    public <B> Cont<B> bind(Function<? super A, Cont<B>> k) {
        return $(c -> run(a -> k.apply(a).run(c)));
    }

    /**
     * Transfer a continuation by discarding the intermediate value.
     *
     * @param b the new continuation transformation
     * @return a continuation that transfers this continuation to the given continuation
     */
    public <B> Cont<B> andThen(Cont<B> b) {
        return $(c -> run(a -> b.run(c)));
    }

    /**
     * Transfer a continuation by discarding the intermediate value.
     *
     * @param b the new continuation transformation
     * @return a continuation that transfers this continuation to the given continuation
     */
    public <B> Cont<B> andThen(Supplier<Cont<B>> b) {
        return $(c -> run(a -> b.get().run(c)));
    }

    /**
     * Apply a function to transform the result of a computation.
     *
     * @param f the function that transform the result of computation
     * @return a continuation applying the transfer function
     */
    public <R> Cont<A> mapCont(Function<R, R> f) {
        return $((Function<A, R> c) -> f.apply(run(c)));
    }

    /**
     * Apply a function to transform the continuation passed to a CPS
     * computation.
     *
     * @param f the transfer function
     * @return a continuation applying the transfer function
     */
    public <B, R> Cont<B> withCont(Function<Function<B, R>, Function<A, R>> f) {
        return $((Function<B, R> c) -> run(f.apply(c)));
    }

    /**
     * The exit function type.
     */
    public interface Exit<A> {
        /**
         * Escape from CPS computation with a value.
         *
         * @param value the escaped value
         */
        <B> Cont<B> escape(A value);
    }

    /**
     * {@code callCC (call-with-current-continuation)} calls its argument function,
     * passing it the current continuation.  It provides an escape continuation
     * mechanism for use with continuation monads.  Escape continuations one allow
     * to abort the current computation and return a value immediately. They achieve
     * a similar effect to 'try-catch-throw' control flow.  The advantage of this
     * function over calling 'return' is that it makes the continuation explicit,
     * allowing more flexibility and better control.
     *
     * <p>The standard idiom used with {@code callCC} is to provide a lambda-expression
     * to name the continuation. Then calling the named continuation anywhere within
     * its scope will escape from the computation, even if it is many layers deep
     * within nested computations</p>
     *
     * @param f the function that passing the current continuation
     * @return a continuation that may or may not escaped from current continuation
     */
    public static <A> Cont<A> callCC(Function<Exit<A>, Cont<A>> f) {
        // Note: the compacted code is as:
        //     $(c -> f.apply(a -> $(__ -> c.apply(a))).run(c))
        // but generic method can not be represented as a lambda expression
        return $(c -> {
            Exit<A> exit = new Exit<A>() {
                @Override public <B> Cont<B> escape(A a) {
                    return $(__ -> c.apply(a));
                }
            };
            return f.apply(exit).run(c);
        });
    }

    /**
     * Delimits the continuation of any {@link #shift(Function) shift} inside current
     * continuation.
     */
    public static <A> Cont<A> reset(Cont<A> m) {
        return $(k -> k.apply(m.eval()));
    }

    /**
     * Captures the continuation up to the nearest enclosing {@link #reset(Cont) reset}
     * and passes it to the given function.
     */
    public static <A, R> Cont<A> shift(Function<Function<A, R>, Cont<R>> f) {
        return $((Function<A, R> k) -> f.apply(k).eval());
    }

    /**
     * Evaluate each action in the sequence from left to right, and collect
     * the result.
     */
    public static <A> Cont<Seq<A>> flatM(Seq<Cont<A>> ms) {
        return ms.foldRightStrict(pure(Seq.nil()), liftM2(Seq::cons));
    }

    /**
     * Evaluate each action in the sequence from left to right, and ignore
     * the result.
     */
    public static <A> Cont<Unit> sequence(Seq<Cont<A>> ms) {
        return ms.foldRight(pure(Unit.U), Cont::andThen);
    }

    /**
     * The {@code mapM} is analogous to {@link Seq#map(Function) map} except that
     * its result is encapsulated in a {@code Continuation}.
     */
    public static <A, B> Cont<Seq<B>>
    mapM(Seq<A> xs, Function<? super A, Cont<B>> f) {
        return flatM(xs.map(f));
    }

    /**
     * {@code mapM_} is equivalent to {@code sequence(xs.map(f))}.
     */
    public static <A, B> Cont<Unit>
    mapM_(Seq<A> xs, Function<? super A, Cont<B>> f) {
        return sequence(xs.map(f));
    }

    /**
     * This generalizes the list-based filter function.
     */
    public static <A> Cont<Seq<A>>
    filterM(Seq<A> xs, Function<? super A, Cont<Boolean>> p) {
        return xs.isEmpty()
               ? pure(Seq.nil())
               : p.apply(xs.head()).bind(flg ->
                 filterM(xs.tail(), p).bind(ys ->
                 pure(flg ? Seq.cons(xs.head(), ys) : ys)));
    }

    /**
     * The {@code foldM} is analogous to {@link Seq#foldLeft(Object,BiFunction) foldLeft},
     * except that its result is encapsulated in a {@code Continuation}. Note that
     * {@code foldM} works from left-to-right over the lists arguments. If right-to-left
     * evaluation is required, the input list should be reversed.
     */
    public static <A, B> Cont<B>
    foldM(B r0, Seq<A> xs, BiFunction<B, ? super A, Cont<B>> f) {
        return xs.foldLeft(pure(r0), (m, x) -> m.bind(r -> f.apply(r, x)));
    }

    /**
     * Kleisli composition of monads.
     */
    public static <A, B, C> Function<A, Cont<C>>
    compose(Function<A, Cont<B>> f, Function<B, Cont<C>> g) {
        return x -> f.apply(x).bind(g);
    }

    /**
     * Promote a function to a CPS computation function.
     */
    public static <A, B> Function<Cont<A>, Cont<B>>
    liftM(Function<? super A, ? extends B> f) {
        return m -> m.map(f);
    }

    /**
     * Promote a function to a CPS computation function.
     */
    public static <A, B, C> BiFunction<Cont<A>, Cont<B>, Cont<C>>
    liftM2(BiFunction<? super A, ? super B, ? extends C> f) {
        return (m1, m2) -> m1.bind(x1 -> m2.map(x2 -> f.apply(x1, x2)));
    }

    /**
     * Promote a function to a CPS computation function.
     */
    public static <A, B, C, D> TriFunction<Cont<A>, Cont<B>, Cont<C>, Cont<D>>
    liftM3(TriFunction<? super A, ? super B, ? super C, ? extends D> f) {
        return (m1, m2, m3) -> m1.bind(x1 -> m2.bind(x2 -> m3.map(x3 -> f.apply(x1, x2, x3))));
    }

    /**
     * Bind the given function to the given CPS computations with a final join.
     */
    public static <A, B, C> Cont<C>
    zip(Cont<A> ma, Cont<B> mb, BiFunction<? super A, ? super B, ? extends C> f) {
        return liftM2(f).apply(ma, mb);
    }

    /**
     * Bind the given function to the given CPS computations with a final join.
     */
    public static <A, B, C, D> Cont<D>
    zip3(Cont<A> ma, Cont<B> mb, Cont<C> mc,
         TriFunction<? super A, ? super B, ? super C, ? extends D> f) {
        return liftM3(f).apply(ma, mb, mc);
    }
}
