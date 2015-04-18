/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.control;

import java.util.Iterator;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

import com.cloudway.platform.common.fp.data.Fn;
import com.cloudway.platform.common.fp.data.Foldable;
import com.cloudway.platform.common.fp.data.Seq;
import com.cloudway.platform.common.fp.data.Unit;
import com.cloudway.platform.common.fp.typeclass.Monad;
import com.cloudway.platform.common.fp.typeclass.$;

/**
 * A stateful CPS computation.
 *
 * @param <A> the type of intermediate result of computation
 * @param <S> the type of state used during computation
 *
 * @see Cont
 */
public final class StateCont<A, S> implements $<StateCont.µ<S>, A> {
    // the CPS transfer type
    @FunctionalInterface
    private interface K<A, S, R> {
        $<MonadState.µ<S>, R> apply(Function<? super A, ? extends $<MonadState.µ<S>, R>> f);
    }

    // the CPS transfer function
    private final K<A, S, ?> kf;

    // construct a continuation monad
    private <R> StateCont(K<A, S, R> f) {
        this.kf = f;
    }

    // helper method to construct continuation from a transfer function
    private static <A, S, R> StateCont<A, S> $(K<A, S, R> f) {
        return new StateCont<>(f);
    }

    /**
     * Run the CPS computation to get final result.
     *
     * @param k the function that accept computation value and return final result
     * @return the final result of computation
     */
    @SuppressWarnings("unchecked")
    public <R> MonadState<R, S> run(Function<? super A, ? extends $<MonadState.µ<S>, R>> k) {
        return MonadState.narrow(((K<A,S,R>)kf).apply(k));
    }

    /**
     * The result of running a CPS computation with 'return' as the final
     * continuation.
     */
    public MonadState<A, S> eval() {
        return run(MonadState::<A,S>pure);
    }

    /**
     * Yield a pure computation that results in the given value.
     *
     * @param a the pure value of the computation result
     * @return the continuation that yield the pure value
     */
    public static <A, S> StateCont<A, S> pure(A a) {
        return $(f -> f.apply(a));
    }

    private static final StateCont<Unit,?> _unit = pure(Unit.U);

    /**
     * Returns a do nothing computation.
     */
    @SuppressWarnings("unchecked")
    public static <S> StateCont<Unit, S> unit() {
        return (StateCont<Unit,S>)_unit;
    }

    /**
     * Yield a thunk that has a lazily evaluated computation.
     *
     * @param a a thunk that eventually produce computation result
     * @return the continuation that yield the computation.
     */
    public static <A, S> StateCont<A, S> lazy(Supplier<A> a) {
        Supplier<A> t = Fn.lazy(a);
        return $(f -> f.apply(t.get()));
    }

    /**
     * Yield an action computation that has no result.
     */
    public static <S> StateCont<Unit, S> action(Runnable a) {
        return $(f -> { a.run(); return f.apply(Unit.U); });
    }

    /**
     * Transfer a continuation by feeding the intermediate value to the given
     * function and yield the result of function application.
     *
     * @param f the function that accept a intermediate value and compute
     *          a new value
     * @return the continuation that yield the result of function application
     */
    public <B> StateCont<B, S> map(Function<? super A, ? extends B> f) {
        return $(c -> run(x -> c.apply(f.apply(x))));
    }

    /**
     * Transfer a continuation by feeding the intermediate value to the given
     * function.
     *
     * @param k the function that accept a intermediate value and transfer
     *          to a new continuation
     * @return the new continuation applying the transfer function
     */
    public <B> StateCont<B, S> bind(Function<? super A, ? extends $<µ<S>, B>> k) {
        return $(c -> run(a -> narrow(k.apply(a)).run(c)));
    }

    /**
     * Transfer a continuation by discarding the intermediate value.
     *
     * @param b the new continuation transformation
     * @return a continuation that transfers this continuation to the given continuation
     */
    public <B> StateCont<B, S> then($<µ<S>, B> b) {
        return $(c -> run(a -> narrow(b).run(c)));
    }

    /**
     * Transfer a continuation by discarding the intermediate value.
     *
     * @param b the new continuation transformation
     * @return a continuation that transfers this continuation to the given continuation
     */
    public <B> StateCont<B, S> then(Supplier<? extends $<µ<S>, B>> b) {
        return $(c -> run(a -> narrow(b.get()).run(c)));
    }

    /**
     * Apply a function to transform the result of a computation.
     *
     * @param f the function that transform the result of computation
     * @return a continuation applying the transfer function
     */
    public <R> StateCont<A, S> mapCont(Function<MonadState<R, S>, ? extends $<MonadState.µ<S>, R>> f) {
        return StateCont.<A,S,R>$(c -> f.apply(run(c)));
    }

    /**
     * Apply a function to transform the continuation passed to a CPS
     * computation.
     *
     * @param f the transfer function
     * @return a continuation applying the transfer function
     */
    public <B, R> StateCont<B, S>
    withCont(Function<Function<? super B, ? extends $<MonadState.µ<S>, R>>,
                      Function<? super A, ? extends $<MonadState.µ<S>, R>>> f) {
        return StateCont.<B,S,R>$(c -> run(f.apply(c)));
    }

    /**
     * Fetch the current value of the state within the monad.
     */
    public static <S> StateCont<S, S> get() {
        return lift(MonadState.get());
    }

    /**
     * Fetch the current value of the state and bind the value to the given function.
     */
    public static <B, S> StateCont<B, S> get(Function<? super S, ? extends $<µ<S>, B>> f) {
        return StateCont.<S>get().bind(f);
    }

    /**
     * Sets the state within the monad.
     */
    public static <S> StateCont<Unit, S> put(S s) {
        return lift(MonadState.put(s));
    }

    /**
     * Updates the state to the result of applying a function to the current state.
     */
    public static <S> StateCont<Unit, S> modify(Function<S, S> f) {
        return lift(MonadState.modify(f));
    }

    /**
     * The exit function type.
     */
    public interface Exit<A, S> {
        /**
         * Escape from CPS computation with a value.
         *
         * @param value the escaped value
         */
        <B> StateCont<B, S> escape(A value);
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
    public static <A, S> StateCont<A, S> callCC(Function<Exit<A, S>, ? extends $<µ<S>, A>> f) {
        // Note: the compacted code is as:
        //     $(c -> f.apply(a -> $(__ -> c.apply(a))).run(c))
        // but generic method can not be represented as a lambda expression
        return $(c -> {
            Exit<A, S> exit = new Exit<A, S>() {
                @Override public <B> StateCont<B, S> escape(A a) {
                    return $(__ -> c.apply(a));
                }
            };
            return narrow(f.apply(exit)).run(c);
        });
    }

    /**
     * Promote a state computation to a stateful CPS computation.
     *
     * @param m the state computation
     * @return the prompted stateful CPS computation
     */
    public static <A, S> StateCont<A, S> lift($<MonadState.µ<S>, A> m) {
        return $(MonadState.narrow(m)::bind);
    }

    /**
     * Delimits the continuation of any {@link #shift(Function) shift} inside current
     * continuation.
     */
    public static <A, S> StateCont<A, S> reset($<µ<S>, A> m) {
        return $(k -> narrow(m).eval().bind(k));
    }

    /**
     * Captures the continuation up to the nearest enclosing {@link #reset}
     * and passes it to the given function.
     */
    public static <A, R, S> StateCont<A, S>
    shift(Function<Function<A, MonadState<R, S>>, ? extends $<µ<S>, R>> f) {
        return StateCont.<A,S,R>$(k -> narrow(f.apply(a -> MonadState.narrow(k.apply(a)))).eval());
    }

    // Monad

    public static final class µ<S> implements Monad<µ<S>> {
        @Override
        public <A> StateCont<A, S> pure(A a) {
            return StateCont.pure(a);
        }

        @Override
        public <A, B> StateCont<B, S> map($<µ<S>, A> a, Function<? super A, ? extends B> f) {
            return narrow(a).map(f);
        }

        @Override
        public <A, B> StateCont<B, S> bind($<µ<S>, A> a, Function<? super A, ? extends $<µ<S>, B>> k) {
            return narrow(a).bind(k);
        }

        @Override
        public <A, B> StateCont<B, S> seqR($<µ<S>, A> a, $<µ<S>, B> b) {
            return narrow(a).then(b);
        }

        @Override
        public <A, B> StateCont<B, S> seqR($<µ<S>, A> a, Supplier<? extends $<µ<S>, B>> b) {
            return narrow(a).then(b);
        }
    }

    public static <A, S> StateCont<A, S> narrow($<µ<S>, A> value) {
        return (StateCont<A, S>)value;
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

    public static <A, S> StateCont<Seq<A>, S> flatM(Seq<? extends $<µ<S>, A>> ms) {
        return narrow(StateCont.<S>tclass().flatM(ms));
    }

    public static <A, S> StateCont<Unit, S> sequence(Foldable<? extends $<µ<S>, A>> ms) {
        return narrow(StateCont.<S>tclass().sequence(ms));
    }

    public static <A, B, S> StateCont<Seq<B>, S> mapM(Seq<A> xs, Function<? super A, ? extends $<µ<S>, B>> f) {
        return narrow(StateCont.<S>tclass().mapM(xs, f));
    }

    public static <A, B, S> StateCont<Unit, S> mapM_(Foldable<A> xs, Function<? super A, ? extends $<µ<S>, B>> f) {
        return narrow(StateCont.<S>tclass().mapM_(xs, f));
    }

    public static <A, S> StateCont<Seq<A>, S> filterM(Seq<A> xs, Function<? super A, ? extends $<µ<S>, Boolean>> p) {
        return narrow(StateCont.<S>tclass().filterM(xs, p));
    }

    public static <A, B, S> StateCont<B, S> foldM(B r0, Foldable<A> xs, BiFunction<B, ? super A, ? extends $<µ<S>, B>> f) {
        return narrow(StateCont.<S>tclass().foldM(r0, xs, f));
    }

    public static <A, S> StateCont<Seq<A>, S> replicateM(int n, $<µ<S>, A> a) {
        return narrow(StateCont.<S>tclass().replicateM(n, a));
    }

    public static <A, S> StateCont<Unit, S> replicateM_(int n, $<µ<S>, A> a) {
        return narrow(StateCont.<S>tclass().replicateM_(n, a));
    }

    // Generator

    private static class Yield<A, S> {
        final A result;
        final S state;
        final Function<A, MonadState<Optional<Yield<A, S>>, S>> cont;

        Yield(A a, S s, Function<A, MonadState<Optional<Yield<A, S>>, S>> k) {
            result = a;
            state = s;
            cont = k;
        }

        Optional<Yield<A, S>> next() {
            return cont.apply(result).eval(state);
        }

        Optional<Yield<A, S>> send(A value) {
            return cont.apply(value).eval(state);
        }
    }

    Optional<Yield<A, S>> runYield(S s) {
        return reset(map(__ -> Optional.<Yield<A,S>>empty())).eval().eval(s);
    }

    public static <A, S> StateCont<A, S> yield(A a) {
        return StateCont.<A, Optional<Yield<A, S>>, S>shift(k ->
            StateCont.<S>get().map(s -> Optional.of(new Yield<>(a, s, k))));
    }

    public static <A, S> StateCont<A, S> yield(Supplier<A> a) {
        return StateCont.<A, Optional<Yield<A, S>>, S>shift(k ->
            StateCont.<S>get().map(s -> Optional.of(new Yield<>(a.get(), s, k))));
    }

    @SuppressWarnings("unchecked")
    public static <A, S> StateCont<A, S> yieldFrom(Generator<A> g) {
        if (g instanceof Gen) {
            return ((Gen<A,S>)g).cont;
        } else {
            return g.foldRight(finish(), (x, r) -> StateCont.<A,S>yield(x).then(r));
        }
    }

    public static <A, S> StateCont<A, S> yieldList(Seq<A> xs) {
        return xs.map(StateCont::<A,S>yield).foldRight(finish(), StateCont::then);
    }

    public static <A, S> StateCont<Unit, S> sendTo(Generator.Channel<A> target, A value) {
        return action(() -> target.send(value));
    }

    private static final StateCont<?,?> _finish = pure(null);

    @SuppressWarnings("unchecked")
    public static <A, S> StateCont<A, S> finish() {
        return (StateCont<A,S>)_finish; // no NPE since null will be discarded
    }

    public static <A, S> Generator<A> generator(S s, $<µ<S>, A> k) {
        return new Gen<>(s, narrow(k));
    }

    private static class Gen<A, S> implements Generator<A> {
        private final S state;
        private final StateCont<A, S> cont;

        Gen(S s, StateCont<A, S> k) {
            state = s;
            cont = k;
        }

        @Override
        public Iterator<A> iterator() {
            return start();
        }

        @Override
        public Channel<A> start() {
            return new Channel<A>() {
                private Optional<Yield<A, S>> ch = cont.runYield(state);

                @Override
                public boolean hasNext() {
                    return ch.isPresent();
                }

                @Override
                public A next() {
                    Yield<A, S> y = ch.get();
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
            return generator(state, foldRight(finish(), (x, r) ->
                p.test(x) ? StateCont.<A,S>yield(x).then(r) : r.get()
            ));
        }

        @Override
        public <B> Generator<B> map(Function<? super A, ? extends B> f) {
            return generator(state, foldRight(finish(), (x, r) ->
                StateCont.<B,S>yield(f.apply(x)).then(r)
            ));
        }

        @Override
        public <B> Generator<B> bind(Function<? super A, ? extends $<µ, B>> f) {
            return generator(state, foldRight(finish(), (x, r) ->
                StateCont.<B,S>yieldFrom(Generator.narrow(f.apply(x))).then(r)
            ));
        }

        @Override
        public <R> R foldRight(BiFunction<? super A, Supplier<R>, R> f, Supplier<R> r) {
            return foldr(cont.runYield(state), f, r);
        }

        private static <A, R, S>
        R foldr(Optional<Yield<A, S>> yield, BiFunction<? super A, Supplier<R>, R> f, Supplier<R> r) {
            if (yield.isPresent()) {
                Yield<A, S> y = yield.get();
                return f.apply(y.result, () -> foldr(y.next(), f, r));
            } else {
                return r.get();
            }
        }

        @Override
        @SuppressWarnings("unchecked")
        public <B, C> Generator<C> zip(Generator<B> b, BiFunction<? super A, ? super B, ? extends C> f) {
            StateCont<B, S> kb = yieldFrom(b);
            return generator(state, zip_(cont.runYield(state), kb.runYield(state), f));
        }

        private static <A, B, C, S> StateCont<C,S> zip_(Optional<Yield<A,S>> ya, Optional<Yield<B,S>> yb,
                    BiFunction<? super A, ? super B, ? extends C> f) {
            if (ya.isPresent() && yb.isPresent()) {
                Yield<A,S> a = ya.get(); Yield<B,S> b = yb.get();
                return StateCont.<C,S>yield(f.apply(a.result, b.result)).then(() -> zip_(a.next(), b.next(), f));
            } else {
                return finish();
            }
        }
    }
}
