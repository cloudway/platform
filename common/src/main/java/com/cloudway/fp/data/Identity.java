/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.data;

import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;

import com.cloudway.fp.$;
import com.cloudway.fp.control.Applicative;
import com.cloudway.fp.control.Monad;

/**
 * The identity monad.
 *
 * <p>This trivial type constructor serves two purposes:
 * <ul>
 * <li>It can be used with functions parameterized by functor or monad classes.</li>
 * <li>It can be used as a base monad to which a series of monad transformers may
 *     be applied to construct a composite monad. </li>
 * </ul>
 *
 * @param <A> the type of identity value
 */
public final class Identity<A> implements $<Identity.µ, A>, Foldable<A>, Traversable<Identity.µ, A> {
    private final A value;

    private Identity(A a) {
        this.value = a;
    }

    /**
     * Wraps a value in a {@code Identity} monad.
     */
    public static <A> Identity<A> of(A a) {
        return new Identity<>(a);
    }

    /**
     * Returns the wrapped value in this {@code Identity}.
     *
     * @return the wrapped value in this {@code Identity}
     */
    public A get() {
        return value;
    }

    /**
     * Returns the wrapped value in the given generic {@code Identity} monad.
     *
     * @param m a generic {@code Identity} monad.
     * @return the wrapped value in the given generic {@code Identity} monad
     */
    public static <A> A run($<µ, A> m) {
        return narrow(m).get();
    }

    /**
     * Transform the computation inside an {@code Identity}.
     *
     * <pre>{@code mapIdentity :: (a -> b) -> Identity a -> Identity b}</pre>
     */
    public static <A, B> Function<$<µ, A>, $<µ, B>>
    mapIdentity(Function<? super A, ? extends B> f) {
        return a -> narrow(a).map(f);
    }

    /**
     * Transfer the {@code Identity} value by applying the given function and
     * wraps the result in a new {@code Identity}.
     */
    public <B> Identity<B> map(Function<? super A, ? extends B> f) {
        return of(f.apply(value));
    }

    /**
     * Transfer the {@code Identity} value by applying the given function.
     */
    public <B> Identity<B> bind(Function<? super A, ? extends $<µ, B>> f) {
        return narrow(f.apply(value));
    }

    @Override
    public <R> R foldMap(Monoid<R> monoid, Function<? super A, ? extends R> f) {
        return f.apply(value);
    }

    @Override
    public <R> R foldRight(BiFunction<? super A, Supplier<R>, R> f, Supplier<R> r) {
        return f.apply(value, r);
    }

    @Override
    public <R> R foldRight_(R z, BiFunction<? super A, R, R> f) {
        return f.apply(value, z);
    }

    @Override
    public Maybe<A> foldRight(BiFunction<A, A, A> f) {
        return Maybe.of(value);
    }

    @Override
    public <R> R foldLeft(BiFunction<Supplier<R>, ? super A, R> f, Supplier<R> r) {
        return f.apply(r, value);
    }

    @Override
    public <R> R foldLeft(R z, BiFunction<R, ? super A, R> f) {
        return f.apply(z, value);
    }

    @Override
    public Maybe<A> foldLeft(BiFunction<A, A, A> f) {
        return Maybe.of(value);
    }

    @Override
    public <F, B> $<F, Identity<B>>
    traverse(Applicative<F> m, Function<? super A, ? extends $<F, B>> f) {
        return m.map(f.apply(value), Identity::of);
    }

    // Monad

    /**
     * The {@code Identity} typeclass definition.
     */
    public static final class µ implements Monad<µ> {
        @Override
        public <A> Identity<A> pure(A a) {
            return of(a);
        }

        @Override
        public <A, B> Identity<B> map($<µ, A> a, Function<? super A, ? extends B> f) {
            return narrow(a).map(f);
        }

        @Override
        public <A, B> Identity<B> bind($<µ, A> a, Function<? super A, ? extends $<µ, B>> k) {
            return narrow(a).bind(k);
        }

        @Override
        public <A, B> Identity<B> ap($<µ, Function<? super A, ? extends B>> fs, $<µ, A> a) {
            return of(narrow(fs).get().apply(narrow(a).get()));
        }
    }

    public static <A> Identity<A> narrow($<µ, A> value) {
        return (Identity<A>)value;
    }

    public static final µ tclass = new µ();

    @Override
    public µ getTypeClass() {
        return tclass;
    }
}
