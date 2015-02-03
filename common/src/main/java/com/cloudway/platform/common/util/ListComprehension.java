/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util;

import java.util.Collection;
import java.util.function.DoubleFunction;
import java.util.function.Function;
import java.util.function.IntFunction;
import java.util.function.LongFunction;
import java.util.stream.DoubleStream;
import java.util.stream.IntStream;
import java.util.stream.LongStream;
import java.util.stream.Stream;

/**
 * A utility class that build a DSL for list comprehension.
 */
public final class ListComprehension {
    private ListComprehension() {}

    /**
     * A polymorphism interface that responsible to build a list comprehension.
     *
     * @param <R> the type of result stream element
     * @param <S> the type of stream
     */
    @FunctionalInterface
    public interface Qualifier<R, S> {
        /**
         * Build the final stream.
         */
        default S build() {
            throw new UnsupportedOperationException();
        }

        /**
         * Internal method used to build downstream from given builder.
         */
        S __build(Builder<R, S> builder);
    }

    /**
     * The internal interface used to create polymorphism list comprehension.
     */
    private interface Builder<R, S> {
        /**
         * Build downstream from given qualifier.
         */
        default S build(Qualifier<R, S> q) {
            return q != null ? q.__build(this) : identity();
        }

        /**
         * Build the identity element.
         */
        S identity();

        /**
         * Build the element contains the result value.
         */
        S yield(R value);
    }

    /**
     * Builder implementations.
     */
    @SuppressWarnings("rawtypes")
    private enum Builders implements Builder {
        STREAM {
            @Override
            public Stream identity() {
                return Stream.empty();
            }

            @Override
            public Stream yield(Object value) {
                return Stream.of(value);
            }
        },

        SEQ {
            @Override
            public Seq identity() {
                return Seq.nil();
            }

            @Override
            public Seq yield(Object value) {
                return Seq.of(value);
            }
        }
    }

    /**
     * Generate list comprehension from source stream.
     */
    public static <T, R> Qualifier<R, Stream<R>>
    From(Stream<T> stream, Function<? super T, ? extends Qualifier<R, Stream<R>>> mapper) {
        return new Qualifier<R, Stream<R>>() {
            @Override
            @SuppressWarnings("unchecked")
            public Stream<R> build() {
                return __build(Builders.STREAM);
            }

            @Override
            public Stream<R> __build(Builder<R, Stream<R>> builder) {
                return stream.flatMap(t -> builder.build(mapper.apply(t)));
            }
        };
    }

    /**
     * Generate list comprehension from source Integer stream.
     */
    public static <R> Qualifier<R, Stream<R>>
    From(IntStream stream, IntFunction<? extends Qualifier<R, Stream<R>>> mapper) {
        return new Qualifier<R, Stream<R>>() {
            @Override
            @SuppressWarnings("unchecked")
            public Stream<R> build() {
                return __build(Builders.STREAM);
            }

            @Override
            public Stream<R> __build(Builder<R, Stream<R>> builder) {
                return stream.boxed().flatMap(t -> builder.build(mapper.apply(t)));
            }
        };
    }

    /**
     * Generate list comprehension from source Long stream.
     */
    public static <R> Qualifier<R, Stream<R>>
    From(LongStream stream, LongFunction<? extends Qualifier<R, Stream<R>>> mapper) {
        return new Qualifier<R, Stream<R>>() {
            @Override
            @SuppressWarnings("unchecked")
            public Stream<R> build() {
                return __build(Builders.STREAM);
            }

            @Override
            public Stream<R> __build(Builder<R, Stream<R>> builder) {
                return stream.boxed().flatMap(t -> builder.build(mapper.apply(t)));
            }
        };
    }

    /**
     * Generate list comprehension from source Double stream.
     */
    public static <R> Qualifier<R, Stream<R>>
    From(DoubleStream stream, DoubleFunction<? extends Qualifier<R, Stream<R>>> mapper) {
        return new Qualifier<R, Stream<R>>() {
            @Override
            @SuppressWarnings("unchecked")
            public Stream<R> build() {
                return __build(Builders.STREAM);
            }

            @Override
            public Stream<R> __build(Builder<R, Stream<R>> builder) {
                return stream.boxed().flatMap(t -> builder.build(mapper.apply(t)));
            }
        };
    }

    /**
     * Generate list comprehension from source collection.
     */
    public static <T, R> Qualifier<R, Stream<R>>
    From(Collection<T> col, Function<? super T, ? extends Qualifier<R, Stream<R>>> mapper) {
        return From(col.stream(), mapper);
    }

    /**
     * Generate list comprehension from source sequence.
     */
    public static <T, R> Qualifier<R, Seq<R>>
    From(Seq<T> seq, Function<? super T, ? extends Qualifier<R, Seq<R>>> mapper) {
        return new Qualifier<R, Seq<R>>() {
            @Override
            @SuppressWarnings("unchecked")
            public Seq<R> build() {
                return __build(Builders.SEQ);
            }

            @Override
            public Seq<R> __build(Builder<R, Seq<R>> builder) {
                return seq.flatMap(t -> builder.build(mapper.apply(t)));
            }
        };
    }

    /**
     * Generate list comprehension from source integer sequence.
     */
    public static <R> Qualifier<R, Seq<R>>
    From(IntSeq seq, IntFunction<? extends Qualifier<R, Seq<R>>> mapper) {
        return new Qualifier<R, Seq<R>>() {
            @Override
            @SuppressWarnings("unchecked")
            public Seq<R> build() {
                return __build(Builders.SEQ);
            }

            @Override
            public Seq<R> __build(Builder<R, Seq<R>> builder) {
                return seq.flatMapToObj(t -> builder.build(mapper.apply(t)));
            }
        };
    }

    /**
     * Do pattern matching on argument of qualifier function.
     */
    public static <T, R, S> Function<T, Qualifier<R, S>>
    With(ConditionCase<? super T, ? extends Qualifier<R, S>, RuntimeException> cond) {
        return cond::lift;
    }

    /**
     * Introduce a local variable.
     */
    public static <T, R, S> Qualifier<R, S>
    Let(T value, Function<? super T, ? extends Qualifier<R, S>> mapper) {
        return builder -> builder.build(mapper.apply(value));
    }

    /**
     * Filter the stream by a predicate.
     */
    public static <R, S> Qualifier<R, S> Where(boolean test, Qualifier<R, S> next) {
        return builder -> test && next != null ? next.__build(builder) : builder.identity();
    }

    /**
     * Yield a result.
     */
    public static <R, S> Qualifier<R, S> Yield(R result) {
        return builder -> builder.yield(result);
    }
}
