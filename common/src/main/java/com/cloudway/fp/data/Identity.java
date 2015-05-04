/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.data;

import java.util.Objects;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;

import com.cloudway.fp.$;
import com.cloudway.fp.control.Applicative;
import com.cloudway.fp.control.MonadFix;

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
public abstract class Identity<A> implements $<Identity.µ, A>,
    Foldable<A>, Traversable<Identity.µ, A>, Forcible<Identity<A>>
{
    Identity() {
        // no public instantiation
    }

    /**
     * Wraps a value in a {@code Identity} monad.
     */
    public static <A> Identity<A> of(A a) {
        return new Strict<>(a);
    }

    /**
     * Wraps a lazy value in a {@code Identity} monad.
     */
    public static <A> Identity<A> lazy(Supplier<A> a) {
        return new Lazy<>(a);
    }

    /**
     * Delay evaluate a computation.
     */
    public static <A> Identity<A> delay(Supplier<$<µ, A>> m) {
        return new Lazy<>(() -> narrow(m.get()).get());
    }

    /**
     * Returns the wrapped value in this {@code Identity}.
     *
     * @return the wrapped value in this {@code Identity}
     */
    public abstract A get();

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
    public abstract <B> Identity<B> map(Function<? super A, ? extends B> f);

    /**
     * Transfer the {@code Identity} value by applying the given function.
     */
    public abstract <B> Identity<B> bind(Function<? super A, ? extends $<µ, B>> f);

    /**
     * Sequential application.
     */
    public abstract <B> Identity<B> ap($<µ, Function<? super A, ? extends B>> f);

    // Foldable and Traversable

    @Override
    public <R> R foldMap(Monoid<R> monoid, Function<? super A, ? extends R> f) {
        return f.apply(get());
    }

    @Override
    public <R> R foldRight(BiFunction<? super A, Supplier<R>, R> f, Supplier<R> r) {
        return f.apply(get(), r);
    }

    @Override
    public <R> R foldRight_(R z, BiFunction<? super A, R, R> f) {
        return f.apply(get(), z);
    }

    @Override
    public Maybe<A> foldRight(BiFunction<A, A, A> f) {
        return Maybe.of(get());
    }

    @Override
    public <R> R foldLeft(BiFunction<Supplier<R>, ? super A, R> f, Supplier<R> r) {
        return f.apply(r, get());
    }

    @Override
    public <R> R foldLeft(R z, BiFunction<R, ? super A, R> f) {
        return f.apply(z, get());
    }

    @Override
    public Maybe<A> foldLeft(BiFunction<A, A, A> f) {
        return Maybe.of(get());
    }

    @Override
    public <F, B> $<F, Identity<B>>
    traverse(Applicative<F> m, Function<? super A, ? extends $<F, B>> f) {
        return m.map(f.apply(get()), Identity::of);
    }

    @Override
    public Identity<A> force() {
        Forcible.force(get());
        return this;
    }

    // Implementation

    private static class Strict<A> extends Identity<A> {
        private final A value;

        Strict(A a) {
            value = a;
        }

        @Override
        public A get() {
            return value;
        }

        @Override
        public <B> Identity<B> map(Function<? super A, ? extends B> f) {
            return new Strict<>(f.apply(value));
        }

        @Override
        public <B> Identity<B> bind(Function<? super A, ? extends $<µ, B>> f) {
            return narrow(f.apply(value));
        }

        @Override
        public <B> Identity<B> ap($<µ, Function<? super A, ? extends B>> f) {
            return new Strict<>(narrow(f).get().apply(value));
        }
    }

    private static class Lazy<A> extends Identity<A> {
        private Supplier<A> thunk;
        private A value;

        Lazy(Supplier<A> a) {
            thunk = a;
        }

        @Override
        public A get() {
            if (thunk != null) {
                value = thunk.get();
                thunk = null;
            }
            return value;
        }

        @Override
        public <B> Identity<B> map(Function<? super A, ? extends B> f) {
            return new Lazy<>(() -> f.apply(get()));
        }

        @Override
        public <B> Identity<B> bind(Function<? super A, ? extends $<µ, B>> f) {
            return new Lazy<>(() -> narrow(f.apply(get())).get());
        }

        @Override
        public <B> Identity<B> ap($<µ, Function<? super A, ? extends B>> f) {
            return new Lazy<>(() -> narrow(f).get().apply(get()));
        }
    }

    // Monad

    /**
     * The {@code Identity} typeclass definition.
     */
    public static final class µ implements MonadFix<µ> {
        @Override
        public <A> Identity<A> pure(A a) {
            return of(a);
        }

        @Override
        public <A> Identity<A> lazy(Supplier<A> a) {
            return Identity.lazy(a);
        }

        @Override
        public <A> Identity<A> delay(Supplier<$<µ, A>> a) {
            return Identity.delay(a);
        }

        @Override
        public <A, B> Identity<B> map($<µ, A> m, Function<? super A, ? extends B> f) {
            return narrow(m).map(f);
        }

        @Override
        public <A, B> Identity<B> bind($<µ, A> m, Function<? super A, ? extends $<µ, B>> k) {
            return narrow(m).bind(k);
        }

        @Override
        public <A, B> Identity<B> ap($<µ, Function<? super A, ? extends B>> f, $<µ, A> m) {
            return narrow(m).ap(f);
        }

        @Override
        public <A> Identity<A> mfix(Function<Supplier<A>, ? extends $<µ, A>> f) {
            return of(Fn.fix(x -> run(f.apply(x))));
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

    // Object

    public boolean equals(Object obj) {
        if (obj == this)
            return true;
        if (!(obj instanceof Identity))
            return false;
        Identity<?> other = (Identity<?>)obj;
        return Objects.equals(get(), other.get());
    }

    public int hashCode() {
        return Objects.hashCode(get());
    }

    public String toString() {
        return "Identity " + get();
    }
}
