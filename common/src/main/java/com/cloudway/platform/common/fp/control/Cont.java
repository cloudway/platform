/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.control;

import java.util.Iterator;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

import com.cloudway.platform.common.fp.data.Fn;
import com.cloudway.platform.common.fp.data.Foldable;
import com.cloudway.platform.common.fp.data.Maybe;
import com.cloudway.platform.common.fp.data.Seq;
import com.cloudway.platform.common.fp.data.Unit;
import com.cloudway.platform.common.fp.typeclass.Monad;
import com.cloudway.platform.common.fp.typeclass.$;

/**
 * <p>Continuation monad.</p>
 *
 * <p>Continuation&lt;A, R&gt; is a CPS computation that produces an intermediate
 * result of type {@code A} within a CPS computation whose final type is {@code R}</p>
 *
 * <p>The {@code pure} method simply creates a continuation which passes the
 * value on.</p>
 *
 * <p>The {@code bind} method adds the bound function into the continuation chain.</p>
 *
 * @param <A> the type of intermediate result of computation
 */
public final class Cont<A> implements $<Cont.µ, A> {
    // the CPS transfer type: (a -> r) -> r
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
     * <pre>{@code
     *     yield(x).eval() == x
     * }</pre>
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

    private static final Cont<Unit> _unit = pure(Unit.U);

    /**
     * Returns a do nothing computation.
     */
    public static Cont<Unit> unit() {
        return _unit;
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
    public <B> Cont<B> bind(Function<? super A, ? extends $<µ, B>> k) {
        return $(c -> run(a -> narrow(k.apply(a)).run(c)));
    }

    /**
     * Transfer a continuation by discarding the intermediate value.
     *
     * @param b the new continuation transformation
     * @return a continuation that transfers this continuation to the given continuation
     */
    public <B> Cont<B> then($<µ, B> b) {
        return $(c -> run(a -> narrow(b).run(c)));
    }

    /**
     * Transfer a continuation by discarding the intermediate value.
     *
     * @param b the new continuation transformation
     * @return a continuation that transfers this continuation to the given continuation
     */
    public <B> Cont<B> then(Supplier<? extends $<µ, B>> b) {
        return $(c -> run(a -> narrow(b.get()).run(c)));
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
    public static <A> Cont<A> callCC(Function<Exit<A>, ? extends $<µ, A>> f) {
        // Note: the compacted code is as:
        //     $(c -> f.apply(a -> $(__ -> c.apply(a))).run(c))
        // but generic method can not be represented as a lambda expression
        return $(c -> {
            Exit<A> exit = new Exit<A>() {
                @Override public <B> Cont<B> escape(A a) {
                    return $(__ -> c.apply(a));
                }
            };
            return narrow(f.apply(exit)).run(c);
        });
    }

    /**
     * Delimits the continuation of any {@link #shift(Function) shift} inside current
     * continuation.
     */
    public static <A> Cont<A> reset($<µ, A> m) {
        return $(k -> k.apply(narrow(m).eval()));
    }

    /**
     * Captures the continuation up to the nearest enclosing {@link #reset}
     * and passes it to the given function.
     */
    public static <A, R> Cont<A> shift(Function<Function<A, R>, ? extends $<µ, R>> f) {
        return $((Function<A, R> k) -> narrow(f.apply(k)).eval());
    }

    // Monad

    public static final class µ implements Monad<µ> {
        @Override
        public <A> Cont<A> pure(A a) {
            return Cont.pure(a);
        }

        @Override
        public <A, B> Cont<B> map($<µ, A> a, Function<? super A, ? extends B> f) {
            return narrow(a).map(f);
        }

        @Override
        public <A, B> Cont<B> bind($<µ, A> a, Function<? super A, ? extends $<µ, B>> k) {
            return narrow(a).bind(k);
        }

        @Override
        public <A, B> Cont<B> seqR($<µ, A> a, $<µ, B> b) {
            return narrow(a).then(b);
        }

        @Override
        public <A, B> Cont<B> seqR($<µ, A> a, Supplier<? extends $<µ, B>> b) {
            return narrow(a).then(b);
        }
    }

    public static <A> Cont<A> narrow($<µ, A> value) {
        return (Cont<A>)value;
    }

    public static final µ tclass = new µ();

    @Override
    public µ getTypeClass() {
        return tclass;
    }

    // Convenient static monad methods

    public static <A> Cont<Seq<A>> flatM(Seq<? extends $<µ, A>> ms) {
        return narrow(tclass.flatM(ms));
    }

    public static <A> Cont<Unit> sequence(Foldable<? extends $<µ, A>> ms) {
        return narrow(tclass.sequence(ms));
    }

    public static <A, B> Cont<Seq<B>> mapM(Seq<A> xs, Function<? super A, ? extends $<µ, B>> f) {
        return narrow(tclass.mapM(xs, f));
    }

    public static <A, B> Cont<Unit> mapM_(Foldable<A> xs, Function<? super A, ? extends $<µ, B>> f) {
        return narrow(tclass.mapM_(xs, f));
    }

    public static <A> Cont<Seq<A>> filterM(Seq<A> xs, Function<? super A, ? extends $<µ, Boolean>> p) {
        return narrow(tclass.filterM(xs, p));
    }

    public static <A, B> Cont<B> foldM(B r0, Foldable<A> xs, BiFunction<B, ? super A, ? extends $<µ, B>> f) {
        return narrow(tclass.foldM(r0, xs, f));
    }

    public static <A> Cont<Seq<A>> replicateM(int n, $<µ, A> a) {
        return narrow(tclass.replicateM(n, a));
    }

    public static <A> Cont<Unit> replicateM_(int n, $<µ, A> a) {
        return narrow(tclass.replicateM_(n, a));
    }

    // Generator

    private static class Yield<A> {
        final A result;
        final Function<A, Maybe<Yield<A>>> cont;

        Yield(A e, Function<A, Maybe<Yield<A>>> k) {
            result = e;
            cont = k;
        }

        Maybe<Yield<A>> next() {
            return cont.apply(result);
        }

        Maybe<Yield<A>> send(A value) {
            return cont.apply(value);
        }
    }

    Maybe<Yield<A>> runYield() {
        return reset(map(__ -> Maybe.<Yield<A>>empty())).eval();
    }

    public static <A> Cont<A> yield(A a) {
        return Cont.<A, Maybe<Yield<A>>>shift(k -> pure(Maybe.of(new Yield<A>(a, k))));
    }

    public static <A> Cont<A> yield(Supplier<A> a) {
        return Cont.<A, Maybe<Yield<A>>>shift(k -> pure(Maybe.of(new Yield<A>(a.get(), k))));
    }

    public static <A> Cont<A> yieldFrom(Generator<A> g) {
        if (g instanceof Gen) {
            return ((Gen<A>)g).cont;
        } else {
            return g.foldRight(finish(), (x, r) -> yield(x).then(r));
        }
    }

    public static <A> Cont<A> yieldList(Seq<A> xs) {
        return xs.map(Cont::yield).foldRight(finish(), Cont::then);
    }

    public static <A> Cont<Unit> sendTo(Generator.Channel<A> target, A value) {
        return action(() -> target.send(value));
    }

    private static final Cont<?> _finish = pure(null);

    @SuppressWarnings("unchecked")
    public static <A> Cont<A> finish() {
        return (Cont<A>)_finish; // no NPE since null will be discarded
    }

    public static <A> Generator<A> generator($<µ, A> k) {
        return new Gen<A>(narrow(k));
    }

    private static class Gen<A> implements Generator<A> {
        private final Cont<A> cont;

        Gen(Cont<A> k) {
            cont = k;
        }

        @Override
        public Iterator<A> iterator() {
            return start();
        }

        @Override
        public Channel<A> start() {
            return new Channel<A>() {
                private Maybe<Yield<A>> ch = cont.runYield();

                @Override
                public boolean hasNext() {
                    return ch.isPresent();
                }

                @Override
                public A next() {
                    Yield<A> y = ch.get();
                    ch = y.next();
                    return y.result;
                }

                @Override
                public A send(A value) {
                    ch = ch.get().send(value);
                    return ch.get().result;
                }
            };
        }

        @Override
        public Generator<A> filter(Predicate<? super A> p) {
            return generator(foldRight(finish(), (x, r) ->
                p.test(x) ? yield(x).then(r) : r.get()
            ));
        }

        @Override
        public <B> Generator<B> map(Function<? super A, ? extends B> f) {
            return generator(foldRight(finish(), (x, r) -> yield(f.apply(x)).then(r)));
        }

        @Override
        public <B> Generator<B> bind(Function<? super A, ? extends $<µ, B>> f) {
            return generator(foldRight(finish(), (x, r) ->
                yieldFrom(Generator.narrow(f.apply(x))).then(r)));
        }

        @Override
        public <R> R foldRight(BiFunction<? super A, Supplier<R>, R> f, Supplier<R> r) {
            return foldr(cont.runYield(), f, r);
        }

        private static <A, R>
        R foldr(Maybe<Yield<A>> yield, BiFunction<? super A, Supplier<R>, R> f, Supplier<R> r) {
            if (yield.isPresent()) {
                Yield<A> y = yield.get();
                return f.apply(y.result, () -> foldr(y.next(), f, r));
            } else {
                return r.get();
            }
        }

        @Override
        public <B, C> Generator<C> zip(Generator<B> b, BiFunction<? super A, ? super B, ? extends C> f) {
            return generator(zip_(cont.runYield(), yieldFrom(b).runYield(), f));
        }

        private static <A, B, C> Cont<C> zip_(Maybe<Yield<A>> ya, Maybe<Yield<B>> yb,
                    BiFunction<? super A, ? super B, ? extends C> f) {
            if (ya.isPresent() && yb.isPresent()) {
                Yield<A> a = ya.get(); Yield<B> b = yb.get();
                return yield(f.apply(a.result, b.result)).then(() -> zip_(a.next(), b.next(), f));
            } else {
                return finish();
            }
        }
    }
}
