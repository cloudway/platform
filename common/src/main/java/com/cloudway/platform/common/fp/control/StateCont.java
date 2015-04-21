/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.control;

import java.util.Iterator;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

import com.cloudway.platform.common.fp.data.Foldable;
import com.cloudway.platform.common.fp.data.Maybe;
import com.cloudway.platform.common.fp.data.Seq;
import com.cloudway.platform.common.fp.data.Traversable;
import com.cloudway.platform.common.fp.data.Unit;
import com.cloudway.platform.common.fp.$;

/**
 * A stateful CPS computation.
 *
 * @param <A> the type of intermediate result of computation
 * @param <S> the type of state used during computation
 *
 * @see Cont
 */
public final class StateCont<S, A> extends ContT.Monadic<StateCont.µ<S>, MonadState.µ<S>, A> {
    private <R> StateCont(K<A, MonadState.µ<S>, R> k) {
        super(k);
    }

    /**
     * Run the CPS computation to get final result.
     *
     * @param k the function that accept computation value and return final result
     * @return the final result of computation
     */
    public <R> MonadState<S, R> run(Function<? super A, ? extends $<MonadState.µ<S>, R>> k) {
        return MonadState.narrow(StateCont.<S>tclass().runCont(this, k));
    }

    /**
     * The result of running a CPS computation with 'return' as the final
     * continuation.
     */
    public MonadState<S, A> eval() {
        return run(MonadState::<S,A>pure);
    }

    /**
     * Yield a pure computation that results in the given value.
     *
     * @param a the pure value of the computation result
     * @return the continuation that yield the pure value
     */
    public static <S, A> StateCont<S, A> pure(A a) {
        return narrow(StateCont.<S>tclass().pure(a));
    }

    /**
     * Returns a do nothing computation.
     */
    public static <S> StateCont<S, Unit> unit() {
        return pure(Unit.U);
    }

    /**
     * Yield a thunk that has a lazily evaluated computation.
     *
     * @param a a thunk that eventually produce computation result
     * @return the continuation that yield the computation.
     */
    public static <S, A> StateCont<S, A> lazy(Supplier<A> a) {
        return narrow(StateCont.<S>tclass().lazy(a));
    }

    /**
     * Yield an action computation that has no result.
     */
    public static <S> StateCont<S, Unit> action(Runnable a) {
        return narrow(StateCont.<S>tclass().action(a));
    }

    /**
     * Promote a state computation to a stateful CPS computation.
     *
     * @param m the state computation
     * @return the prompted stateful CPS computation
     */
    public static <S, A> StateCont<S, A> lift($<MonadState.µ<S>, A> m) {
        return narrow(StateCont.<S>tclass().lift(m));
    }

    /**
     * Transfer a continuation by feeding the intermediate value to the given
     * function and yield the result of function application.
     *
     * @param f the function that accept a intermediate value and compute
     *          a new value
     * @return the continuation that yield the result of function application
     */
    public <B> StateCont<S, B> map(Function<? super A, ? extends B> f) {
        return narrow(StateCont.<S>tclass().map(this, f));
    }

    /**
     * Transfer a continuation by feeding the intermediate value to the given
     * function.
     *
     * @param k the function that accept a intermediate value and transfer
     *          to a new continuation
     * @return the new continuation applying the transfer function
     */
    public <B> StateCont<S, B> bind(Function<? super A, ? extends $<µ<S>, B>> k) {
        return narrow(StateCont.<S>tclass().bind(this, k));
    }

    /**
     * Transfer a continuation by discarding the intermediate value.
     *
     * @param b the new continuation transformation
     * @return a continuation that transfers this continuation to the given continuation
     */
    public <B> StateCont<S, B> then($<µ<S>, B> b) {
        return narrow(StateCont.<S>tclass().seqR(this, b));
    }

    /**
     * Transfer a continuation by discarding the intermediate value.
     *
     * @param b the new continuation transformation
     * @return a continuation that transfers this continuation to the given continuation
     */
    public <B> StateCont<S, B> then(Supplier<? extends $<µ<S>, B>> b) {
        return narrow(StateCont.<S>tclass().seqR(this, b));
    }

    /**
     * Apply a function to transform the result of a computation.
     *
     * @param f the function that transform the result of computation
     * @return a continuation applying the transfer function
     */
    public <R> StateCont<S, A> mapCont(Function<MonadState<S, R>, ? extends $<MonadState.µ<S>, R>> f) {
        return narrow(StateCont.<S>tclass().<R,A>mapCont(this, r -> f.apply(MonadState.narrow(r))));
    }

    /**
     * Apply a function to transform the continuation passed to a CPS
     * computation.
     *
     * @param f the transfer function
     * @return a continuation applying the transfer function
     */
    public <B, R> StateCont<S, B>
    withCont(Function<Function<? super B, ? extends $<MonadState.µ<S>, R>>,
                      Function<? super A, ? extends $<MonadState.µ<S>, R>>> f) {
        return narrow(StateCont.<S>tclass().withCont(this, f));
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
    public static <S, B> StateCont<S, B> get(Function<? super S, ? extends $<µ<S>, B>> f) {
        return StateCont.<S>get().bind(f);
    }

    /**
     * Sets the state within the monad.
     */
    public static <S> StateCont<S, Unit> put(S s) {
        return lift(MonadState.put(s));
    }

    /**
     * Updates the state to the result of applying a function to the current state.
     */
    public static <S> StateCont<S, Unit> modify(Function<S, S> f) {
        return lift(MonadState.modify(f));
    }

    /**
     * The exit function type.
     */
    public interface Exit<S, A> {
        /**
         * Escape from CPS computation with a value.
         *
         * @param value the escaped value
         */
        <B> StateCont<S, B> escape(A value);
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
    public static <S, A> StateCont<S, A> callCC(Function<Exit<S, A>, ? extends $<µ<S>, A>> f) {
        return narrow(StateCont.<S>tclass().callCC(e ->
            f.apply(new Exit<S, A>() {
                @Override public <B> StateCont<S, B> escape(A a) {
                    return narrow(e.escape(a));
                }
            })
        ));
    }

    /**
     * Delimits the continuation of any {@link #shift(Function) shift} inside current
     * continuation.
     */
    public static <S, A> StateCont<S, A> reset($<µ<S>, A> m) {
        return narrow(StateCont.<S>tclass().reset(m));
    }

    /**
     * Captures the continuation up to the nearest enclosing {@link #reset}
     * and passes it to the given function.
     */
    public static <S, A, R> StateCont<S, A>
    shift(Function<Function<A, MonadState<S, R>>, ? extends $<µ<S>, R>> f) {
        return narrow(StateCont.<S>tclass().<R, A>shift(k ->
            f.apply(a -> MonadState.narrow(k.apply(a)))));
    }

    // Monad

    public static class µ<S> extends ContT<µ<S>, MonadState.µ<S>> {
        private µ() {
            super(MonadState.tclass());
        }

        @Override
        protected <R, A> StateCont<S, A> $(K<A, MonadState.µ<S>, R> k) {
            return new StateCont<>(k);
        }
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

    public static <S, A> StateCont<S, A> narrow($<µ<S>, A> value) {
        return (StateCont<S,A>)value;
    }

    public static <S, A, R> MonadState<S, R> runCont(
            Function<? super A, ? extends $<MonadState.µ<S>, R>> f, $<µ<S>, A> m) {
        return narrow(m).run(f);
    }

    public static <S, A> MonadState<S, A> evalCont($<µ<S>, A> m) {
        return narrow(m).eval();
    }

    // Convenient static monad methods

    public static <T, S, A> StateCont<S, ? extends Traversable<T, A>>
    flatM(Traversable<T, ? extends $<µ<S>, A>> ms) {
        return narrow(StateCont.<S>tclass().flatM(ms));
    }

    @SuppressWarnings("unchecked")
    public static <S, A> StateCont<S, Seq<A>> flatM(Seq<? extends $<µ<S>, A>> ms) {
        return (StateCont<S, Seq<A>>)StateCont.<S>tclass().flatM(ms);
    }

    public static <T, S, A, B> StateCont<S, ? extends Traversable<T, B>>
    mapM(Traversable<T, A> xs, Function<? super A, ? extends $<µ<S>, B>> f) {
        return narrow(StateCont.<S>tclass().mapM(xs, f));
    }

    @SuppressWarnings("unchecked")
    public static <S, A, B> StateCont<S, Seq<B>>
    mapM(Seq<A> xs, Function<? super A, ? extends $<µ<S>, B>> f) {
        return (StateCont<S, Seq<B>>)StateCont.<S>tclass().mapM(xs, f);
    }

    public static <S, A> StateCont<S, Unit> sequence(Foldable<? extends $<µ<S>, A>> ms) {
        return narrow(StateCont.<S>tclass().sequence(ms));
    }

    public static <S, A, B> StateCont<S, Unit>
    mapM_(Foldable<A> xs, Function<? super A, ? extends $<µ<S>, B>> f) {
        return narrow(StateCont.<S>tclass().mapM_(xs, f));
    }

    public static <S, A> StateCont<S, Seq<A>>
    filterM(Seq<A> xs, Function<? super A, ? extends $<µ<S>, Boolean>> p) {
        return narrow(StateCont.<S>tclass().filterM(xs, p));
    }

    public static <S, A, B> StateCont<S, B>
    foldM(B r0, Foldable<A> xs, BiFunction<B, ? super A, ? extends $<µ<S>, B>> f) {
        return narrow(StateCont.<S>tclass().foldM(r0, xs, f));
    }

    public static <S, A> StateCont<S, Seq<A>> replicateM(int n, $<µ<S>, A> a) {
        return narrow(StateCont.<S>tclass().replicateM(n, a));
    }

    public static <S, A> StateCont<S, Unit> replicateM_(int n, $<µ<S>, A> a) {
        return narrow(StateCont.<S>tclass().replicateM_(n, a));
    }

    // Generator

    private static class Yield<S, A> {
        final A result;
        final S state;
        final Function<A, MonadState<S, Maybe<Yield<S, A>>>> cont;

        Yield(A a, S s, Function<A, MonadState<S, Maybe<Yield<S, A>>>> k) {
            result = a;
            state = s;
            cont = k;
        }

        Maybe<Yield<S, A>> next() {
            return cont.apply(result).eval(state);
        }

        Maybe<Yield<S, A>> send(A value) {
            return cont.apply(value).eval(state);
        }
    }

    Maybe<Yield<S, A>> runYield(S s) {
        return reset(map(__ -> Maybe.<Yield<S,A>>empty())).eval().eval(s);
    }

    public static <S, A> StateCont<S, A> yield(A a) {
        return StateCont.<S, A, Maybe<Yield<S,A>>>shift(k ->
            StateCont.<S>get().map(s -> Maybe.of(new Yield<>(a, s, k))));
    }

    public static <S, A> StateCont<S, A> yield(Supplier<A> a) {
        return StateCont.<S, A, Maybe<Yield<S, A>>>shift(k ->
            StateCont.<S>get().map(s -> Maybe.of(new Yield<>(a.get(), s, k))));
    }

    @SuppressWarnings("unchecked")
    public static <S, A> StateCont<S, A> yieldFrom(Generator<A> g) {
        if (g instanceof Gen) {
            return ((Gen<S,A>)g).cont;
        } else {
            return g.foldRight(finish(), (x, r) -> StateCont.<S,A>yield(x).then(r));
        }
    }

    public static <S, A> StateCont<S, A> yieldList(Seq<A> xs) {
        return xs.map(StateCont::<S,A>yield).foldRight(finish(), StateCont::then);
    }

    public static <S, A> StateCont<S, Unit> sendTo(Generator.Channel<A> target, A value) {
        return action(() -> target.send(value));
    }

    private static final StateCont<?, ?> _finish = pure(null);

    @SuppressWarnings("unchecked")
    public static <S, A> StateCont<S, A> finish() {
        return (StateCont<S,A>)_finish; // no NPE since null will be discarded
    }

    public static <S, A> Generator<A> generator(S s, $<µ<S>, A> k) {
        return new Gen<>(s, narrow(k));
    }

    private static class Gen<S, A> implements Generator<A> {
        private final S state;
        private final StateCont<S, A> cont;

        Gen(S s, StateCont<S, A> k) {
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
                private Maybe<Yield<S, A>> ch = cont.runYield(state);

                @Override
                public boolean hasNext() {
                    return ch.isPresent();
                }

                @Override
                public A next() {
                    Yield<S, A> y = ch.get();
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
                p.test(x) ? StateCont.<S,A>yield(x).then(r) : r.get()
            ));
        }

        @Override
        public <B> Generator<B> map(Function<? super A, ? extends B> f) {
            return generator(state, foldRight(finish(), (x, r) ->
                StateCont.<S,B>yield(f.apply(x)).then(r)
            ));
        }

        @Override
        public <B> Generator<B> bind(Function<? super A, ? extends $<µ, B>> f) {
            return generator(state, foldRight(finish(), (x, r) ->
                StateCont.<S,B>yieldFrom(Generator.narrow(f.apply(x))).then(r)
            ));
        }

        @Override
        public <R> R foldRight(BiFunction<? super A, Supplier<R>, R> f, Supplier<R> r) {
            return foldr(cont.runYield(state), f, r);
        }

        private static <S, A, R>
        R foldr(Maybe<Yield<S, A>> yield, BiFunction<? super A, Supplier<R>, R> f, Supplier<R> r) {
            if (yield.isPresent()) {
                Yield<S, A> y = yield.get();
                return f.apply(y.result, () -> foldr(y.next(), f, r));
            } else {
                return r.get();
            }
        }

        @Override
        @SuppressWarnings("unchecked")
        public <B, C> Generator<C> zip(Generator<B> b, BiFunction<? super A, ? super B, ? extends C> f) {
            StateCont<S, B> kb = yieldFrom(b);
            return generator(state, zip_(cont.runYield(state), kb.runYield(state), f));
        }

        private static <S, A, B, C> StateCont<S, C> zip_(Maybe<Yield<S,A>> ya, Maybe<Yield<S,B>> yb,
                    BiFunction<? super A, ? super B, ? extends C> f) {
            if (ya.isPresent() && yb.isPresent()) {
                Yield<S,A> a = ya.get(); Yield<S,B> b = yb.get();
                return StateCont.<S,C>yield(f.apply(a.result, b.result)).then(() -> zip_(a.next(), b.next(), f));
            } else {
                return finish();
            }
        }
    }
}
