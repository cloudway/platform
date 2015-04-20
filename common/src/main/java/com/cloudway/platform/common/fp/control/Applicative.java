/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.control;

import java.util.function.BiFunction;
import java.util.function.Function;

import com.cloudway.platform.common.fp.$;
import com.cloudway.platform.common.fp.data.Fn;
import com.cloudway.platform.common.fp.data.Functor;
import com.cloudway.platform.common.fp.function.TriFunction;

/**
 * An {@code Applicative} describes a structure intermediate between a functor
 * and a monad (technically, a strong lax monoidal functor). Compared with
 * monads, this interface lacks the full power of the binding operator, but
 *
 * <ul>
 * <li>it has more instances.</li>
 * <li>it is sufficient for many uses, e.g. context-free parsing, or the
 * {@code Traversable} class.</li>
 * <li>instances can perform analysis of computations before they are executed,
 * and thus produce shared optimizations.</li>
 * </ul>
 *
 * <p>A minimal complete definition must include implementations of these methods
 * satisfying the following laws:
 *
 * <p><strong>identity</strong>
 * <pre>{@code pure id <*> v = v}</pre>
 *
 * <p><Strong>composition</Strong>
 * <pre>{@code pure (.) <*> u <*> v <*> w = u <*> (v <*> w)}</pre>
 *
 * <p><strong>homomorphism</strong>
 * <pre>{@code pure f <*> pure x = pure (f x)}</pre>
 *
 * <p><strong>interchange</strong>
 * <pre>{@code u <*> pure y = pure ($ y) <*> u}</pre>
 *
 * <p>The other methods have the following default definitions, which may be
 * overridden with equivalent specialized implementations:
 *
 * <pre>{@code
 * u *> v = pure (const id) <*> u <*> v
 * u <* v = pure const <*> u <*> v
 * }</pre>
 *
 * <p>As a consequence of these laws, the {@link Functor} instance for f will
 * satisfy:
 * <pre>{@code fmap f = pure f <*> x}</pre>
 *
 * <p>If {@code f} is also a Monad, it should satisfy
 * <pre>{@code
 * pure = return
 * (<*>) = ap
 * }</pre>
 */
public interface Applicative<F> extends Functor<F> {
    /**
     * Lift a value.
     *
     * <pre>{@code pure :: a -> f a}</pre>
     */
    <A> $<F,A> pure(A a);

    /**
     * Sequential application.
     *
     * <pre>{@code (<*>) :: f (a -> b) -> f a -> f b}</pre>
     */
    <A, B> $<F,B> ap($<F, Function<? super A, ? extends B>> fs, $<F,A> a);

    /**
     * Sequential application using a binary function.
     */
    default <A, B, C> $<F,C>
    ap2(BiFunction<? super A, ? super B, ? extends C> f, $<F,A> a, $<F,B> b) {
        return ap(map(a, Fn.curry(f)), b);
    }

    /**
     * Sequential application using a ternary function.
     */
    default <A, B, C, D> $<F,D>
    ap3(TriFunction<? super A, ? super B, ? super C, ? extends D> f, $<F,A> a, $<F,B> b, $<F,C> c) {
        return ap(ap(map(a, Fn.curry3(f)), b), c);
    }

    /**
     * Sequence actions, discarding the value of the first argument.
     *
     * <pre>{@code (*>) :: f a -> f b -> f b}</pre>
     */
    default <A, B> $<F,B> seqR($<F,A> a, $<F,B> b) {
        return ap(fill(a, Fn.<B>id()), b);
    }

    /**
     * Sequence actions, discarding the value of the second argument.
     *
     * <pre>{@code (<*) :: f a -> f b -> f a}</pre>
     */
    default <A, B> $<F,A> seqL($<F,A> a, $<F,B> b) {
        return ap2((x, y) -> x, a, b);
    }

    /**
     * Lift a function to actions. This function may be used as a value for
     * {@link Functor#map} in a {@link Functor} instance.
     *
     * <pre>{@code liftA :: (a -> b) -> f a -> f b}</pre>
     */
    default <A, B> Function<$<F,A>, $<F,B>> liftA(Function<? super A, ? extends B> f) {
        return a -> ap(pure(f), a);
    }

    /**
     * Lift a binary function to actions.
     *
     * <pre>{@code liftA2 :: (a -> b -> c) -> f a -> f b -> f c}</pre>
     */
    default <A, B, C> BiFunction<$<F,A>, $<F,B>, $<F,C>>
    liftA2(BiFunction<? super A, ? super B, ? extends C> f) {
        return (a, b) -> ap2(f, a, b);
    }

    /**
     * Lift a ternary function to actions.
     *
     * <pre>{@code liftA3 :: (a -> b -> c -> d) -> f a -> f b -> f c}</pre>
     */
    default <A, B, C, D> TriFunction<$<F,A>, $<F,B>, $<F,C>, $<F,D>>
    liftA3(TriFunction<? super A, ? super B, ? super C, ? extends D> f) {
        return (a, b, c) -> ap3(f, a, b, c);
    }
}
