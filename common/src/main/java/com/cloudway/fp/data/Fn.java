/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.data;

import java.util.Objects;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.BiFunction;
import java.util.function.Supplier;

import com.cloudway.fp.function.TriFunction;

/**
 * Transformations on functions.
 */
public final class Fn {
    private Fn() {}

    /**
     * The identity transformation.
     *
     * @return the identity transformation.
     */
    public static <A> Function<A, A> id() {
        return a -> a;
    }

    /**
     * Returns a function that ignores its argument to constantly produce
     * the given value.
     *
     * @param b the value to return when the returned function is applied
     * @return a function that ignores its argument to constantly produce
     * the given value
     */
    public static <A, B> Function<A, B> pure(B b) {
        return a -> b;
    }

    /**
     * Returns the given function argument as-is. This is useful for composing
     * method references.
     *
     * @param f the function to be return
     * @return the given function argument
     */
    public static <A, B> Function<A, B> f(Function<A, B> f) {
        return f;
    }

    /**
     * Convert a Consumer to a Function.
     *
     * @param f the Consumer to be converted
     * @return the Function executing consumer and return Unit
     */
    public static <A> Function<A, Unit> f_(Consumer<A> f) {
        return a -> { f.accept(a); return Unit.U; };
    }

    /**
     * Function composition.
     *
     * @param f the function to compose with other one
     * @param g the function to apply before <tt>f</tt> is applied
     * @return a composed function that first applies the function <tt>g</tt>
     * and then applies function <tt>f</tt>
     */
    public static <A, B, C> Function<A, C> compose(Function<B, C> f, Function<A, B> g) {
        return f.compose(g);
    }

    /**
     * Function argument flipping.
     *
     * @param f the function to flip
     * @return the given function flipped
     */
    public static <A, B, C> BiFunction<B, A, C>
    flip(BiFunction<? super A, ? super B, ? extends C> f) {
        return (b, a) -> f.apply(a, b);
    }

    /**
     * Curry a function of arity-2.
     *
     * @param f the function to curry
     * @return a curried form of the given function
     */
    public static <A, B, C> Function<A, Function<B, C>>
    curry(BiFunction<? super A, ? super B, ? extends C> f) {
        return a -> b -> f.apply(a, b);
    }

    /**
     * Curry a function of arity-2.
     *
     * @param f the function to curry
     * @param a an argument to the curried function
     * @return a curried form of the given function
     */
    public static <A, B, C> Function<B, C>
    curry(BiFunction<? super A, ? super B, ? extends C> f, A a) {
        return b -> f.apply(a, b);
    }

    /**
     * Curry a function of arity-3.
     *
     * @param f the function to curry
     * @return a curried form of the given function
     */
    public static <A, B, C, D> Function<A, Function<B, Function<C, D>>>
    curry3(TriFunction<? super A, ? super B, ? super C, ? extends D> f) {
        return a -> b -> c -> f.apply(a, b, c);
    }

    /**
     * Uncurry a function of arity-2.
     *
     * @param f the function to uncurry
     * @return an uncurried function
     */
    public static <A, B, C> BiFunction<A, B, C>
    uncurry(Function<? super A, ? extends Function<B, C>> f) {
        return (a, b) -> f.apply(a).apply(b);
    }

    /**
     * Partial application of the second argument to the supplied function
     * to get a function of type {@code A -> C}. Same as {@code flip(f).apply(b)}.
     *
     * @param f the function to partially apply
     * @param b the value to apply to the function
     * @return a new function based on {@code f} with its second argument applied.
     */
    public static <A, B, C> Function<A, C>
    partial(BiFunction<? super A, ? super B, ? extends C> f, B b) {
        return a -> f.apply(a, b);
    }

    /**
     * Returns the least fixed point of the given function, i.e. the least
     * defined {@code x} such that {@code f x = x}.
     *
     * @param f the function to get the fixed point
     * @return the least fixed point of the function
     */
    public static <A> A fix(Function<Supplier<A>, ? extends A> f) {
        Ref<Supplier<A>> x = new Ref<>();
        return x.set(lazy(() -> f.apply(x.get()))).get();
    }

    /**
     * Returns the least fixed point of the given binary operator.
     */
    public static <A, B> Function<A, B>
    fix(BiFunction<Function<? super A, ? extends B>, ? super A, ? extends B> f) {
        return fix(rec -> x -> f.apply(y -> rec.get().apply(y), x));
    }

    /**
     * Returns the least fixed point of the given ternary operator.
     */
    public static <A, B, C> BiFunction<A, B, C>
    fix(TriFunction<BiFunction<? super A, ? super B, ? extends C>, ? super A, ? super B, ? extends C> f) {
        return fix(rec -> (x, y) -> f.apply((a, b) -> rec.get().apply(a, b), x, y));
    }

    /**
     * Maps the function in first argument to the function in the second argument.
     *
     * @param <A> the result type of the first function which is argument type of
     *            the second function
     * @param <B> the result type of the return value
     * @param <T> the argument type of the first and return value
     * @param mf a function whose argument type is the same as the argument type
     *           of the return value
     * @param f  a function whose argument type is the same as the return type of
     *           {@code mf} and yields the return value
     * @return   a function that chains the given functions together such that the
     *           result of applying {@code mf} to the argument is given to {@code f}
     */
    public static <T, A, B> Function<T, B>
    map(Function<T, A> mf, Function<? super A, ? extends B> f) {
        return mf.andThen(f);
    }

    /**
     * Binds the function in the first argument to the function in the second
     * argument.
     *
     * @param <A> the result type of the first function which is also the argument
     *            type of the second function
     * @param <B> the result type of the returned function
     * @param <T> the argument type of the first and result function
     * @param mf a function whose argument type is the same as the argument type
     *           of the return value
     * @param f  a function whose argument type is the same as the return type of
     *           {@code mf} and yields the return value
     * @return   a function that chains the given functions together such that the
     *           result of applying {@code mf} to the argument is given to {@code f},
     *           yielding a function that is applied to the argument again.
     */
    public static <T, A, B> Function<T, B>
    bind(Function<T, A> mf, Function<? super A, ? extends Function<T, B>> f) {
        return t -> f.apply(mf.apply(t)).apply(t);
    }

    /**
     * Maps the function to the result of a supplier.
     *
     * @param <A> the return type of the supplier
     * @param <B> the return type of the final result
     * @param sup a supplier whose return value is mapped by the given function
     * @param f a function that maps the return value of the supplier
     * @return a supplier that maps result for the given supplier and function
     */
    public static <A, B> Supplier<B>
    map(Supplier<A> sup, Function<? super A, ? extends B> f) {
        return () -> f.apply(sup.get());
    }

    /**
     * Binds the function to the result of a supplier.
     *
     * @param <A> the return type of the supplier
     * @param <B> the return type of the final result
     * @param sup a supplier whose return value is bind by the given function
     * @param f a function that binds the return value of the supplier
     * @return a supplier that bind result for the given supplier and function
     */
    public static <A, B> Supplier<B>
    bind(Supplier<A> sup, Function<? super A, ? extends Supplier<B>> f) {
        return () -> f.apply(sup.get()).get();
    }

    /**
     * Create a lazy evaluation thunk from the given supplier.
     *
     * @param thunk a thunk to be lazily evaluated
     * @return a thunk for lazy evaluation
     */
    public static <T> Supplier<T> lazy(Supplier<T> thunk) {
        Objects.requireNonNull(thunk);
        if (thunk instanceof LazyThunk) {
            return thunk;
        } else {
            return new LazyThunk<>(thunk);
        }
    }

    private static class LazyThunk<T> implements Supplier<T>, Forcible<Supplier<T>> {
        private Supplier<T> thunk;
        private T value;

        LazyThunk(Supplier<T> t) {
            thunk = t;
        }

        @Override
        public T get() {
            if (thunk != null) {
                value = thunk.get();
                thunk = null;
            }
            return value;
        }

        @Override
        public Supplier<T> force() {
            Forcible.force(get());
            return this;
        }
    }

    /**
     * Evaluate each action in the sequence from left to right, and collect
     * the result.
     */
    public static <T, A> Function<T, Seq<A>>
    flatM(Seq<Function<T, A>> ms) {
        return ms.foldRight_(pure(Seq.nil()), liftM2(Seq::cons));
    }

    /**
     * The {@code mapM} is analogous to {@link Seq#map(Function) map} except that
     * its result is encapsulated in a function.
     */
    public static <T, A, B> Function<T, Seq<B>>
    mapM(Seq<A> xs, Function<? super A, ? extends Function<T, B>> f) {
        return flatM(xs.map(f));
    }

    /**
     * Generalizes {@link Seq#zip(Seq,BiFunction)} to arbitrary monads.
     * Bind the given function to the given computations with a final join.
     */
    public static <T, A, B, C> Function<T, Seq<C>>
    zipM(Seq<A> xs, Seq<B> ys, BiFunction<? super A, ? super B, Function<T, C>> f) {
        return flatM(Seq.zip(xs, ys, f));
    }

    /**
     * The {@code foldM} is analogous to {@link Seq#foldLeft(Object,BiFunction) foldLeft},
     * except that its result is encapsulated in a function. Note that {@code foldM}
     * works from left-to-right over the lists arguments. If right-to-left evaluation
     * is required, the input list should be reversed.
     */
    public static <T, A, B> Function<T, B>
    foldM(B r0, Foldable<A> xs, BiFunction<B, ? super A, Function<T, B>> f) {
        return xs.foldLeft(pure(r0), (m, x) -> bind(m, r -> f.apply(r, x)));
    }

    /**
     * Promotes a function of arity-1 to a higher-order function.
     *
     * @param f the function to promote
     * @return a function of arity-1 promoted to compose with two functions
     */
    public static <T, A, B> Function<Function<T, A>, Function<T, B>>
    liftM(Function<? super A, ? extends B> f) {
        return m -> map(m, f);
    }

    /**
     * Promotes a function of arity-2 to a higher-order function.
     *
     * @param f the function to promote
     * @return a function of arity-2 promoted to compose with two functions
     */
    public static <T, A, B, C> BiFunction<Function<T, A>, Function<T, B>, Function<T, C>>
    liftM2(BiFunction<? super A, ? super B, ? extends C> f) {
        return (m1, m2) -> bind(m1, x1 -> map(m2, x2 -> f.apply(x1, x2)));
    }

    /**
     * Promotes a function of arity-3 to a higher-order function.
     *
     * @param f the function to promote
     * @return a function of arity-3 promoted to compose with two functions
     */
    public static <T, A, B, C, D> TriFunction<Function<T, A>, Function<T, B>, Function<T, C>, Function<T, D>>
    liftM3(TriFunction<? super A, ? super B, ? super C, ? extends D> f) {
        return (m1, m2, m3) -> bind(m1, x1 -> bind(m2, x2 -> map(m3, x3 -> f.apply(x1, x2, x3))));
    }
}
