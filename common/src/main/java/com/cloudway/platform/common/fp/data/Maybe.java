/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.data;

import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

import com.cloudway.platform.common.fp.control.ConditionCase;
import com.cloudway.platform.common.fp.function.ExceptionFunction;
import com.cloudway.platform.common.fp.function.ExceptionSupplier;
import com.cloudway.platform.common.fp.$;
import com.cloudway.platform.common.fp.control.Applicative;
import com.cloudway.platform.common.fp.control.Monad;

import static com.cloudway.platform.common.fp.control.Conditionals.Any;
import static com.cloudway.platform.common.fp.control.Conditionals.with;

/**
 * A container object which may or may not contain a non-null value.
 * If a value is present, {@code isPresent()} will return {@code true} and
 * {@code get()} will return the value.
 *
 * <p>Additional methods that depend on the presence or absence of a contained
 * value are provided, such as {@link #orElse(java.lang.Object) orElse()}
 * (return a default value if value not present) and
 * {@link #ifPresent(java.util.function.Consumer) ifPresent()} (execute a block
 * of code if the value is present).
 */
public final class Maybe<A> implements $<Maybe.µ, A>, Foldable<A>, Traversable<Maybe.µ, A> {
    /**
     * Common instance for {@code empty()}.
     */
    private static final Maybe<?> EMPTY = new Maybe<>();

    /**
     * If non-null, the value; if null, indicates no value is present
     */
    private final A value;

    /**
     * Constructs an empty instance.
     */
    private Maybe() {
        this.value = null;
    }

    /**
     * Returns an empty {@code Maybe} instance.  No value is present for this
     * Maybe.
     *
     * @param <A> type of the non-existent value
     * @return an empty {@code Maybe}
     */
    public static <A> Maybe<A> empty() {
        @SuppressWarnings("unchecked")
        Maybe<A> t = (Maybe<A>)EMPTY;
        return t;
    }

    /**
     * Construct an instance with the value present.
     *
     * @param value the non-null value to be present
     * @throws NullPointerException if value is null
     */
    private Maybe(A value) {
        this.value = Objects.requireNonNull(value);
    }

    /**
     * Returns a {@code Maybe} with the specified present non-null value.
     *
     * @param <A> the class of the value
     * @param value the value to be present, which must be non-null
     * @return a {@code Maybe} with the value present
     * @throws NullPointerException if value is null
     */
    public static <A> Maybe<A> of(A value) {
        return new Maybe<>(value);
    }

    /**
     * Returns an {@code Maybe} describing the specified value, if non-null,
     * otherwise returns an empty {@code Maybe}.
     *
     * @param <A> the class of the value
     * @param value the possibly-null value to describe
     * @return a {@code Maybe} with a present value if the specified value
     * is non-null, otherwise an empty {@code Maybe}
     */
    public static <A> Maybe<A> ofNullable(A value) {
        return value == null ? empty() : of(value);
    }

    /**
     * Convert a {@code java.util.Optional} to {@code Maybe}.
     *
     * @param optional a {@code java.util.Optional} object
     * @return a {@code Maybe} object that equivalent to the given {@code Optional}
     */
    public static <A> Maybe<A> fromOptional(Optional<A> optional) {
        return optional.isPresent() ? of(optional.get()) : empty();
    }

    /**
     * Convert this {@code Maybe} to {@code java.util.Optional}.
     *
     * @return an {@code java.util.Optional} object that equivalent to this
     * {@code Maybe}
     */
    public Optional<A> toOptional() {
        return Optional.ofNullable(value);
    }

    /**
     * If a value is present in this {@code Maybe}, returns the value
     * otherwise throws {@code NoSuchElementException}.
     *
     * @return the non-null value held by this {@code Maybe}
     * @throws NoSuchElementException if there is no value present
     */
    public A get() {
        if (value == null) {
            throw new NoSuchElementException("No value present");
        }
        return value;
    }

    /**
     * Returns {@code true} if there is a value present, otherwise {@code false}.
     *
     * @return {@code true} if there is a value present, otherwise {@code false}
     */
    public boolean isPresent() {
        return value != null;
    }

    /**
     * Returns {@code true} if this {@code Maybe} is empty, otherwise {@code false}.
     *
     * @return {@code true} if this {@code Maybe} is empty, otherwise {@code false}
     */
    public boolean isAbsent() {
        return value == null;
    }

    /**
     * If a value is present, invoke the specified consumer with the value,
     * otherwise do nothing.
     *
     * @param consumer block to be executed if a value is present
     * @throws NullPointerException if value is present and {@code consumer} is null
     */
    public void ifPresent(Consumer<? super A> consumer) {
        if (value != null) {
            consumer.accept(value);
        }
    }

    /**
     * If a value is present, and the value matches the given predicate,
     * return an {@code Maybe} describing the value, otherwise return an
     * empty {@code Maybe}.
     *
     * @param predicate a predicate to apply to the value, if present
     * @return a {@code Maybe} describing the value of this {@code Maybe}
     * if a value is present and the value matches the given predicate,
     * otherwise an empty {@code Maybe}
     * @throws NullPointerException if the predicate is null
     */
    public Maybe<A> filter(Predicate<? super A> predicate) {
        return isPresent() && predicate.test(value) ? this : empty();
    }

    /**
     * If a value is present, apply the provided mapping function to it,
     * and if the result is non-null, return an {@code Maybe} describing the
     * result. Otherwise return an empty {@code Maybe}.
     *
     * @param <B> the type of the result of the mapping function
     * @param mapper a mapping function to apply to the value, if present
     * @return a {@code Maybe} describing the result of applying a mapping
     * function to the value of this {@code Maybe}, if a value is present
     * otherwise an empty {@code Maybe}
     * @throws NullPointerException if the mapping function is null
     */
    public <B> Maybe<B> map(Function<? super A, ? extends B> mapper) {
        return isPresent() ? ofNullable(mapper.apply(value)) : empty();
    }

    /**
     * If a value is present, apply the provided {@code Maybe}-bearing
     * mapping function to it, return that result, otherwise return an empty
     * {@code Maybe}.  This method is similar to {@link #map(Function)},
     * but the provided mapper is one whose result is already an {@code Maybe},
     * and if invoked, {@code flatMap} does not wrap it with an additional
     * {@code Maybe}.
     *
     * @param <B> the type parameter to the {@code Maybe} returned by
     * @param mapper a mapping function to apply to the value, if present
     * @return the result of applying a {@code Maybe}-bearing mapping function
     * to the value of this {@code Maybe}, if a value is present, otherwise
     * an empty {@code Maybe}
     * @throws NullPointerException if the mapping function is null or returns
     * a null result
     */
    public <B> Maybe<B> flatMap(Function<? super A, Maybe<B>> mapper) {
        return isPresent() ? Objects.requireNonNull(mapper.apply(value)) : empty();
    }

    /**
     * Map each element of a structure to an action, evaluate these actions
     * from left to right, and collect the results.
     */
    @Override
    public <F, B> $<F, Maybe<B>> traverse(Applicative<F> m, Function<? super A, ? extends $<F,B>> f) {
        return isPresent() ? m.map(f.apply(value), Maybe::of) : m.pure(empty());
    }

    /**
     * Return the value if present, otherwise return {@code other}.
     *
     * @param other the value to be returned if there is no value presnet, may
     * be null
     * @return the value, if present, otherwise {@code other}
     */
    public A orElse(A other) {
        return value != null ? value : other;
    }

    /**
     * Return the value if present, otherwise invoke {@code other} and return
     * the result of that invocation.
     *
     * @param other a {@code Supplier} whose result is returned if no value
     * is present
     * @return the value if present otherwise the result of {@code other.get()}
     * @throws NullPointerException if value is not present and {@code other} is
     * null
     */
    public A orElseGet(Supplier<? extends A> other) {
        return value != null ? value : other.get();
    }

    /**
     * Return the contained value, if present, otherwise throw an exception
     * to be created by the provided supplier.
     *
     * @param <X> Type of the exception to be thrown
     * @param exceptionSupplier The supplier which will return the exception to
     * be thrown
     * @return the present value
     * @throws X if there is no value present
     * @throws NullPointerException if no value is present and
     * {@code exceptionSupplier} is null
     */
    public <X extends Throwable> A orElseThrow(Supplier<? extends X> exceptionSupplier) throws X {
        if (value != null) {
            return value;
        } else {
            throw exceptionSupplier.get();
        }
    }

    /**
     * Indicates whether some other object is "equal to" this Maybe. The
     * other object is considered equal if:
     * <ul>
     * <li>it is also a {@code Maybe} and;
     * <li>both instances have no value present or;
     * <li>the present values are "equal to" each other via {@code equals()}.
     * </ul>
     *
     * @param obj an object to be tested for equality
     * @return {code true} if the other object is "equal to" this object
     * otherwise {@code false}
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!(obj instanceof Maybe))
            return false;
        Maybe<?> other = (Maybe<?>)obj;
        return Objects.equals(value, other.value);
    }

    /**
     * Returns the hash code value of the present value, if any, or 0 (zero) if
     * no value is present.
     *
     * @return hash code value of the present value or 0 if no value is present
     */
    @Override
    public int hashCode() {
        return Objects.hashCode(value);
    }

    /**
     * Returns a non-empty string representation of this Maybe suitable for
     * debugging. The exact presentation format is unspecified and may vary
     * between implementations and versions.
     *
     * @return the string representation of this instance
     */
    @Override
    public String toString() {
        return value != null
            ? String.format("Just %s", value)
            : "Nothing";
    }

    // Pattern Matching

    /**
     * Returns a conditional case that evaluate to perform action on the
     * {@code Maybe} being tested which is present a value.
     */
    public static <T, R, X extends Throwable> ConditionCase<Maybe<T>, R, X>
    Just(ExceptionFunction<? super T, ? extends R, X> mapper) {
        return t -> t.isPresent()
            ? () -> mapper.evaluate(t.get())
            : null;
    }

    /**
     * Returns a conditional case that evaluate to perform action on the
     * {@code Maybe} being tested which is not presents value.
     */
    public static <R, X extends Throwable> ConditionCase<Maybe<?>, R, X>
    Nothing(ExceptionSupplier<R, X> mapper) {
        return t -> t.isPresent() ? null : mapper;
    }

    // Foldable

    @Override
    public <R> R foldRight(BiFunction<? super A, Supplier<R>, R> f, Supplier<R> r) {
        return isPresent() ? f.apply(value, r) : r.get();
    }

    @Override
    public <R> R foldRight_(R z, BiFunction<? super A, R, R> f) {
        return isPresent() ? f.apply(value, z) : z;
    }

    @Override
    public Maybe<A> foldRight(BiFunction<A, A, A> f) {
        return this;
    }

    @Override
    public <R> R foldLeft(BiFunction<Supplier<R>, ? super A, R> f, Supplier<R> r) {
        return isPresent() ? f.apply(r, value) : r.get();
    }

    @Override
    public <R> R foldLeft(R z, BiFunction<R, ? super A, R> f) {
        return isPresent() ? f.apply(z, value) : z;
    }

    @Override
    public Maybe<A> foldLeft(BiFunction<A, A, A> f) {
        return this;
    }

    // Type Classes

    public static <A> Monoid<Maybe<A>> monoid(Monoid<A> ma) {
        return Monoid.monoid_(empty(), (a1, a2) ->
            with(a1, a2).<Maybe<A>>get()
              .when(Any(x -> Nothing(() -> x)))
              .when(Nothing(() -> Any(y -> y)))
              .when(Just(x -> Just(y -> of(ma.append(x, y)))))
              .get());
    }

    public static final class µ implements Monad<µ> {
        @Override
        public <A> Maybe<A> pure(A a) {
            return of(a);
        }

        @Override
        public <A> Maybe<A> fail(String s) {
            return empty();
        }

        @Override
        public <A, B> Maybe<B> map($<µ, A> a, Function<? super A, ? extends B> f) {
            return narrow(a).map(f);
        }

        @Override
        public <A, B> Maybe<B> bind($<µ, A> a, Function<? super A, ? extends $<µ, B>> k) {
            return narrow(a).flatMap(x -> narrow(k.apply(x)));
        }

        @Override
        public <A, B> Maybe<B> ap($<µ, Function<? super A, ? extends B>> fs, $<µ, A> a) {
            Maybe<Function<? super A, ? extends B>> mf = narrow(fs);
            return mf.isPresent() ? map(a, mf.get()) : empty();
        }

        @Override
        public <A, B> Maybe<B> seqR($<µ, A> a, $<µ, B> b) {
            return narrow(a).isPresent() ? narrow(b) : empty();
        }

        @Override
        public <A, B> $<µ, B> seqR($<µ, A> a, Supplier<? extends $<µ, B>> b) {
            return narrow(a).isPresent() ? narrow(b.get()) : empty();
        }
    }

    public static <A> Maybe<A> narrow($<µ, A> value) {
        return (Maybe<A>)value;
    }

    public static final µ tclass = new µ();

    @Override
    public µ getTypeClass() {
        return tclass;
    }

    // Convenient static monad methods

    public static <T, A> Maybe<? extends Traversable<T, A>>
    flatM(Traversable<T, ? extends $<µ, A>> ms) {
        return narrow(tclass.flatM(ms));
    }

    @SuppressWarnings("unchecked")
    public static <A> Maybe<Seq<A>> flatM(Seq<? extends $<µ, A>> ms) {
        return (Maybe<Seq<A>>)tclass.flatM(ms);
    }

    public static <T, A, B> Maybe<? extends Traversable<T, B>>
    mapM(Traversable<T, A> xs, Function<? super A, ? extends $<µ, B>> f) {
        return narrow(tclass.mapM(xs, f));
    }

    @SuppressWarnings("unchecked")
    public static <A, B> Maybe<Seq<B>>
    mapM(Seq<A> xs, Function<? super A, ? extends $<µ, B>> f) {
        return (Maybe<Seq<B>>)tclass.mapM(xs, f);
    }

    public static <A> Maybe<Unit> sequence(Foldable<? extends $<µ, A>> ms) {
        return narrow(tclass.sequence(ms));
    }

    public static <A, B> Maybe<Unit>
    mapM_(Foldable<A> xs, Function<? super A, ? extends $<µ, B>> f) {
        return narrow(tclass.mapM_(xs, f));
    }

    public static <A, S> Maybe<Seq<A>>
    filterM(Seq<A> xs, Function<? super A, ? extends $<µ, Boolean>> p) {
        return narrow(tclass.filterM(xs, p));
    }

    public static <A, B> Maybe<B>
    foldM(B r0, Foldable<A> xs, BiFunction<B, ? super A, ? extends $<µ, B>> f) {
        return narrow(tclass.foldM(r0, xs, f));
    }

    public static <A> Maybe<Seq<A>> replicateM(int n, $<µ, A> a) {
        return narrow(tclass.replicateM(n, a));
    }

    public static <A> Maybe<Unit> replicateM_(int n, $<µ, A> a) {
        return narrow(tclass.replicateM_(n, a));
    }

    // Utilities

    /**
     * Returns the first of two given parameters that is not {@code null},
     * if either is, or otherwise throws a {@link NullPointerException}.
     *
     * @return {@code first} if it is non-null; otherwise {@code second} if it is non-null
     * @throws NullPointerException if both {@code first} and {@code second} are null.
     */
    public static <T> T firstNonNull(T first, T second) {
        return first != null ? first : Objects.requireNonNull(second);
    }

    /**
     * Given two parameters, returns the first parameter if it is non-null,
     * or return the second parameter if the first parameter is {@code null}.
     *
     * @return {@code first} if it is non-null; otherwise {@code second} if
     * {@code first} is {@code null}.
     */
    public static <T> T or(T first, T second) {
        return first != null ? first : second;
    }

    /**
     * Returns the first parameter if it is non-null, otherwise invoke {@code second}
     * and return the result of that invocation.
     *
     * @return {@code first} if it is non-null; otherwise invoke {@code second} and
     * return the result of that invocation.
     */
    public static <T> T or(T first, Supplier<? extends T> second) {
        return first != null ? first : second.get();
    }

    /**
     * Adapts a {@link Supplier} to return optional value where the invocation of supplier
     * may return null value or throws exception.
     *
     * @return an adapted supplier
     */
    public static <T> Supplier<Maybe<T>> adapt(Supplier<? extends T> supplier) {
        return () -> {
            try {
                return ofNullable(supplier.get());
            } catch (Exception ex) {
                return empty();
            }
        };
    }

    /**
     * Adapts a {@link Function} to return optional value where the invocation of function
     * may return null value or throws exception.
     *
     * @return an adapted function
     */
    public static <T, R> Function<T, Maybe<R>> adapt(Function<? super T, ? extends R> f) {
        return (t) -> {
            try {
                return ofNullable(f.apply(t));
            } catch (Exception ex) {
                return empty();
            }
        };
    }
}
