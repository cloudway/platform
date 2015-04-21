/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.control;

import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

import com.cloudway.platform.common.fp.$;
import com.cloudway.platform.common.fp.data.Fn;

/**
 * An abstract factory class for CPS transformer monad.
 *
 * @param <T> the CPS monad typeclass
 * @param <M> the inner monad typeclass
 */
public abstract class ContT<T extends ContT<T,M>, M extends Monad<M>>
    implements Monad<T>, MonadTrans<T, M>
{
    /**
     * The monadic datatype that encapsulate a CPS transfer function. This class
     * may be overridden by concrete CPS transformer monad.
     *
     * @param <T> the CPS monad typeclass
     * @param <M> the inner monad typeclass
     * @param <A> the computation value type
     */
    public static abstract class Monadic<T, M, A> implements $<T, A> {
        /**
         * The CPS transfer function: (a -> m r) -> m r
         */
        @FunctionalInterface
        protected interface K<A, M, R> {
            $<M, R> apply(Function<A, $<M, R>> f);
        }

        private final K<A, M, ?> kf;

        /**
         * Construct a CPS transformer monad.
         */
        protected <R> Monadic(K<A, M, R> f) {
            this.kf = f;
        }
    }

    private final M nm;

    /**
     * Construct a CPS transformer monad.
     *
     * @param nm the inner monad
     */
    protected ContT(M nm) {
        this.nm = nm;
    }

    /**
     * Implemented by concrete class to instantiate a new CPS monad.
     */
    protected abstract <R, A> $<T, A> $(Monadic.K<A, M, R> k);

    /**
     * Run the CPS computation to get the final result.
     *
     * @param k the function that accept computation value and return final result
     * @return the final result of computation
     */
    @SuppressWarnings("unchecked")
    public <R, A> $<M, R> runCont($<T, A> m, Function<? super A, ? extends $<M, R>> k) {
        return ((Monadic.K<A,M,R>)((Monadic<T,M,A>)m).kf).apply((Function<A, $<M, R>>)k);
    }

    /**
     * The result of running a CPS computation with 'return' as the final
     * continuation.
     *
     * <pre>{@code
     *     yield(x).eval() == x
     * }</pre>
     */
    public <R> $<M, R> evalCont($<T, R> m) {
        return runCont(m, nm::pure);
    }

    /**
     * Execute the continuation, discard the final result.
     *
     * @param k the function that accept computation value and return nothing
     */
    public <A> void execCont($<T, A> m, Consumer<? super A> k) {
        runCont(m, x -> { k.accept(x); return null; });
    }

    /**
     * Yield a pure computation that results in the given value.
     *
     * @param a the pure value of the computation result
     * @return the continuation that yield the pure value
     */
    @Override
    public <A> $<T, A> pure(A a) {
        return $(f -> f.apply(a));
    }

    /**
     * Yield a thunk that has a lazily evaluated computation.
     *
     * @param a a thunk that eventually produce computation result
     * @return the continuation that yield the computation.
     */
    @Override
    public <A> $<T, A> lazy(Supplier<A> a) {
        Supplier<A> t = Fn.lazy(a);
        return $(f -> f.apply(t.get()));
    }

    /**
     * Lift a computation from the argument monad to the constructed monad.
     */
    @Override
    public <A> $<T, A> lift($<M, A> m) {
        return $(k -> nm.bind(m, k));
    }

    /**
     * Returns the inner monad typeclass.
     */
    @Override
    public M inner() {
        return nm;
    }

    /**
     * Transfer a continuation by feeding the intermediate value to the given
     * function and yield the result of function application.
     *
     * @param f the function that accept a intermediate value and compute
     *          a new value
     * @return the continuation that yield the result of function application
     */
    @Override
    public <A, B> $<T, B> map($<T, A> m, Function<? super A, ? extends B> f) {
        return $(c -> runCont(m, c.compose(f)));
    }

    /**
     * Transfer a continuation by feeding the intermediate value to the given
     * function.
     *
     * @param k the function that accept a intermediate value and transfer
     *          to a new continuation
     * @return the new continuation applying the transfer function
     */
    @Override
    public <A, B> $<T, B> bind($<T, A> m, Function<? super A, ? extends $<T, B>> k) {
        return $(c -> runCont(m, a -> runCont(k.apply(a), c)));
    }

    /**
     * Sequential application.
     */
    @Override
    public <A, B> $<T, B> ap($<T, Function<? super A, ? extends B>> f, $<T, A> v) {
        return $(c -> runCont(f, g -> runCont(v, c.compose(g))));
    }

    /**
     * Transfer a continuation by discarding the intermediate value.
     */
    @Override
    public <A, B> $<T, B> seqR($<T, A> m, $<T, B> n) {
        return $(c -> runCont(m, a -> runCont(n, c)));
    }

    /**
     * Transfer a continuation by discarding the intermediate value.
     */
    @Override
    public <A, B> $<T, B> seqR($<T, A> m, Supplier<? extends $<T, B>> n) {
        return $(c -> runCont(m, a -> runCont(n.get(), c)));
    }

    /**
     * Apply a function to transform the result of a computation.
     *
     * @param f the function that transform the result of computation
     * @return a continuation applying the transfer function
     */
    public <R, A> $<T, A> mapCont($<T, A> m, Function<? super $<M, R>, ? extends $<M, R>> f) {
        return $((Function<A, $<M, R>> c) -> f.apply(runCont(m, c)));
    }

    /**
     * Apply a function to transform the continuation passed to a CPS
     * computation.
     *
     * @param f the transfer function
     * @return a continuation applying the transfer function
     */
    public <R, A, B> $<T, B> withCont($<T, A> m,
            Function<Function<? super B, ? extends $<M, R>>,
                     Function<? super A, ? extends $<M, R>>> f) {
        return $((Function<B, $<M, R>> c) -> runCont(m, f.apply(c)));
    }

    /**
     * The exit function type.
     */
    public interface Exit<T, A> {
        /**
         * Escape from CPS computation with a value.
         *
         * @param value the escaped value
         */
        <B> $<T, B> escape(A value);
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
    public <A> $<T, A> callCC(Function<Exit<T, A>, ? extends $<T, A>> f) {
        // Note: the compacted code is as:
        //     $(c -> run(f.apply(a -> $(__ -> c.apply(a)), c))
        // but generic method can not be represented as a lambda expression
        return $(c -> {
            Exit<T, A> exit = new Exit<T, A>() {
                @Override public <B> $<T, B> escape(A a) {
                    return $(__ -> c.apply(a));
                }
            };
            return runCont(f.apply(exit), c);
        });
    }

    /**
     * Delimits the continuation of any {@link #shift(Function) shift} inside current
     * continuation.
     */
    public <R> $<T, R> reset($<T, R> a) {
        return lift(evalCont(a));
    }

    /**
     * Captures the continuation up to the nearest enclosing {@link #reset}
     * and passes it to the given function.
     */
    public <R, A> $<T, A> shift(Function<Function<A, ? extends $<M, R>>, ? extends $<T, R>> f) {
        return $((Function<A, $<M, R>> k) -> evalCont(f.apply(k)));
    }

    // Monad Stacking

    /**
     * Stack monad on another monad.
     *
     * @param nm the inner monad
     * @return a stacked monad
     */
    public static <M extends Monad<M>> On<M> on(M nm) {
        return new On<>(nm);
    }

    /**
     * The stacked monad typeclass.
     */
    public static final class On<M extends Monad<M>> extends ContT<On<M>, M> {
        private On(M nm) {
            super(nm);
        }

        @Override
        protected <R, A> $<On<M>, A> $(Monadic.K<A, M, R> f) {
            return new Monadic<On<M>, M, A>(f) {
                @Override public On<M> getTypeClass() {
                    return On.this;
                }
            };
        }
    }
}
