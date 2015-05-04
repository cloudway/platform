/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.control;

import java.util.Collection;
import java.util.function.BiFunction;
import java.util.function.DoubleFunction;
import java.util.function.Function;
import java.util.function.IntFunction;
import java.util.function.LongFunction;
import java.util.function.Supplier;
import java.util.stream.DoubleStream;
import java.util.stream.IntStream;
import java.util.stream.LongStream;
import java.util.stream.Stream;

import com.cloudway.fp.data.Fn;
import com.cloudway.fp.data.Maybe;
import com.cloudway.fp.data.Ref;
import com.cloudway.fp.data.IntSeq;
import com.cloudway.fp.data.Seq;
import com.cloudway.fp.data.Tuple;
import com.cloudway.fp.data.Unit;
import com.cloudway.fp.io.IO;
import com.cloudway.fp.io.VoidIO;
import com.cloudway.fp.$;

/**
 * This class contains keywords for functional DSL.
 */
public final class Syntax {
    private Syntax() {}

    /**
     * A polymorphism interface that responsible to build a list comprehension.
     *
     * @param <R> the type of result stream element
     * @param <S> the type of stream
     */
    public static abstract class Qualifier<R, S> {
        /**
         * Select the final stream.
         */
        protected S select() {
            throw new UnsupportedOperationException();
        }

        /**
         * Internal method used to build downstream from given builder.
         */
        protected abstract S build(Builder<R, S> builder);
    }

    /**
     * The internal interface used to create polymorphism list comprehension.
     */
    public interface Builder<R, S> {
        /**
         * Build downstream from given qualifier.
         */
        default S build(Qualifier<R, S> q) {
            return q != null ? q.build(this) : empty();
        }

        /**
         * Build the empty element.
         */
        S empty();

        /**
         * Build the element contains the result value.
         */
        S pure(R value);
    }

    /**
     * Builder implementations.
     */
    @SuppressWarnings("rawtypes")
    private enum Builders implements Builder {
        STREAM {
            @Override
            public Stream empty() {
                return Stream.empty();
            }

            @Override
            public Stream pure(Object value) {
                return Stream.of(value);
            }
        },

        SEQ {
            @Override
            public Seq empty() {
                return Seq.nil();
            }

            @Override
            public Seq pure(Object value) {
                return Seq.of(value);
            }
        },

        MAYBE {
            @Override
            public Maybe empty() {
                return Maybe.empty();
            }

            @Override
            public Maybe pure(Object value) {
                return Maybe.of(value);
            }
        },

        FUNCTION {
            @Override
            public Function empty() {
                return Fn.id();
            }

            @Override
            public Function pure(Object value) {
                return Fn.pure(value);
            }
        },
    }

    /**
     * Generate list comprehension from source stream.
     */
    public static <T, R> Qualifier<R, Stream<R>>
    from(Stream<T> stream, Function<? super T, ? extends Qualifier<R, Stream<R>>> mapper) {
        return new Qualifier<R, Stream<R>>() {
            @Override
            @SuppressWarnings("unchecked")
            protected Stream<R> select() {
                return build(Builders.STREAM);
            }

            @Override
            protected Stream<R> build(Builder<R, Stream<R>> builder) {
                return stream.flatMap(t -> builder.build(mapper.apply(t)));
            }
        };
    }

    /**
     * Generate list comprehension from source Integer stream.
     */
    public static <R> Qualifier<R, Stream<R>>
    from(IntStream stream, IntFunction<? extends Qualifier<R, Stream<R>>> mapper) {
        return new Qualifier<R, Stream<R>>() {
            @Override
            @SuppressWarnings("unchecked")
            protected Stream<R> select() {
                return build(Builders.STREAM);
            }

            @Override
            protected Stream<R> build(Builder<R, Stream<R>> builder) {
                return stream.boxed().flatMap(t -> builder.build(mapper.apply(t)));
            }
        };
    }

    /**
     * Generate list comprehension from source Long stream.
     */
    public static <R> Qualifier<R, Stream<R>>
    from(LongStream stream, LongFunction<? extends Qualifier<R, Stream<R>>> mapper) {
        return new Qualifier<R, Stream<R>>() {
            @Override
            @SuppressWarnings("unchecked")
            protected Stream<R> select() {
                return build(Builders.STREAM);
            }

            @Override
            protected Stream<R> build(Builder<R, Stream<R>> builder) {
                return stream.boxed().flatMap(t -> builder.build(mapper.apply(t)));
            }
        };
    }

    /**
     * Generate list comprehension from source Double stream.
     */
    public static <R> Qualifier<R, Stream<R>>
    from(DoubleStream stream, DoubleFunction<? extends Qualifier<R, Stream<R>>> mapper) {
        return new Qualifier<R, Stream<R>>() {
            @Override
            @SuppressWarnings("unchecked")
            protected Stream<R> select() {
                return build(Builders.STREAM);
            }

            @Override
            protected Stream<R> build(Builder<R, Stream<R>> builder) {
                return stream.boxed().flatMap(t -> builder.build(mapper.apply(t)));
            }
        };
    }

    /**
     * Generate list comprehension from source collection.
     */
    public static <T, R> Qualifier<R, Stream<R>>
    from(Collection<T> col, Function<? super T, ? extends Qualifier<R, Stream<R>>> mapper) {
        return from(col.stream(), mapper);
    }

    /**
     * Generate list comprehension from source sequence.
     */
    public static <T, R> Qualifier<R, Seq<R>>
    from(Seq<T> seq, Function<? super T, ? extends Qualifier<R, Seq<R>>> mapper) {
        return new Qualifier<R, Seq<R>>() {
            @Override
            @SuppressWarnings("unchecked")
            protected Seq<R> select() {
                return build(Builders.SEQ);
            }

            @Override
            protected Seq<R> build(Builder<R, Seq<R>> builder) {
                return seq.flatMap(t -> builder.build(mapper.apply(t)));
            }
        };
    }

    /**
     * Generate list comprehension from source integer sequence.
     */
    public static <R> Qualifier<R, Seq<R>>
    from(IntSeq seq, IntFunction<? extends Qualifier<R, Seq<R>>> mapper) {
        return new Qualifier<R, Seq<R>>() {
            @Override
            @SuppressWarnings("unchecked")
            protected Seq<R> select() {
                return build(Builders.SEQ);
            }

            @Override
            protected Seq<R> build(Builder<R, Seq<R>> builder) {
                return seq.flatMapToObj(t -> builder.build(mapper.apply(t)));
            }
        };
    }

    /**
     * Generate comprehension from {@code Maybe}.
     */
    public static <T, R> Qualifier<R, Maybe<R>>
    from(Maybe<T> maybe, Function<? super T, ? extends Qualifier<R, Maybe<R>>> mapper) {
        return new Qualifier<R, Maybe<R>>() {
            @Override
            @SuppressWarnings("unchecked")
            protected Maybe<R> select() {
                return build(Builders.MAYBE);
            }

            @Override
            protected Maybe<R> build(Builder<R, Maybe<R>> builder) {
                return maybe.flatMap(t -> builder.build(mapper.apply(t)));
            }
        };
    }

    /**
     * Generate comprehension from functor.
     */
    public static <T, W, R> Qualifier<R, Function<T, R>>
    from(Function<T, W> h, Function<? super W, ? extends Qualifier<R, Function<T, R>>> f) {
        return new Qualifier<R, Function<T, R>>() {
            @Override
            @SuppressWarnings("unchecked")
            protected Function<T, R> select() {
                return build(Builders.FUNCTION);
            }

            @Override
            protected Function<T, R> build(Builder<R, Function<T, R>> builder) {
                return Fn.bind(h, t -> builder.build(f.apply(t)));
            }
        };
    }

    /**
     * A lead word used to create and select comprehensions.
     */
    public static final class select {
        private select() {}

        /**
         * @see Syntax#from(Stream,Function)
         */
        public static <T, R> Stream<R>
        from(Stream<T> stream, Function<? super T, ? extends Qualifier<R, Stream<R>>> mapper) {
            return Syntax.from(stream, mapper).select();
        }

        /**
         * @see Syntax#from(IntStream,IntFunction)
         */
        public static <R> Stream<R>
        from(IntStream stream, IntFunction<? extends Qualifier<R, Stream<R>>> mapper) {
            return Syntax.from(stream, mapper).select();
        }

        /**
         * @see Syntax#from(LongStream,LongFunction)
         */
        public static <R> Stream<R>
        from(LongStream stream, LongFunction<? extends Qualifier<R, Stream<R>>> mapper) {
            return Syntax.from(stream, mapper).select();
        }

        /**
         * @see Syntax#from(DoubleStream,DoubleFunction)
         */
        public static <R> Stream<R>
        from(DoubleStream stream, DoubleFunction<? extends Qualifier<R, Stream<R>>> mapper) {
            return Syntax.from(stream, mapper).select();
        }

        /**
         * @see Syntax#from(Collection,Function)
         */
        public static <T, R> Stream<R>
        from(Collection<T> col, Function<? super T, ? extends Qualifier<R, Stream<R>>> mapper) {
            return Syntax.from(col, mapper).select();
        }

        /**
         * @see Syntax#from(Seq,Function)
         */
        public static <T, R> Seq<R>
        from(Seq<T> seq, Function<? super T, ? extends Qualifier<R, Seq<R>>> mapper) {
            return Syntax.from(seq, mapper).select();
        }

        /**
         * @see Syntax#from(IntSeq,IntFunction)
         */
        public static <R> Seq<R>
        from(IntSeq seq, IntFunction<? extends Qualifier<R, Seq<R>>> mapper) {
            return Syntax.from(seq, mapper).select();
        }

        /**
         * @see Syntax#from(Maybe,Function)
         */
        public static <T, R> Maybe<R>
        from(Maybe<T> maybe, Function<? super T, ? extends Qualifier<R, Maybe<R>>> mapper) {
            return Syntax.from(maybe, mapper).select();
        }

        /**
         * @see Syntax#from(Function,Function)
         */
        public static <T, W, R> Function<T, R>
        from(Function<T, W> h, Function<? super W, ? extends Qualifier<R, Function<T, R>>> f) {
            return Syntax.from(h, f).select();
        }
    }

    /**
     * Filter the stream by a predicate.
     */
    public static <R, S> Qualifier<R, S> where(boolean test, Qualifier<R, S> next) {
        return new Qualifier<R, S>() {
            @Override
            protected S build(Builder<R, S> builder) {
                return test && next != null ? next.build(builder) : builder.empty();
            }
        };
    }

    /**
     * Yield a result.
     */
    public static <R, S> Qualifier<R, S> yield(R result) {
        return new Qualifier<R, S>() {
            @Override
            protected S build(Builder<R, S> builder) {
                return builder.pure(result);
            }
        };
    }

    /**
     * Yield a result directly.
     */
    public static <R, S> Qualifier<R, S> from(S result) {
        return new Qualifier<R, S>() {
            @Override
            protected S select() {
                return result;
            }

            @Override
            protected S build(Builder<R, S> builder) {
                return result;
            }
        };
    }

    /**
     * Introduce a local variable.
     */
    public static <T, R> R let(T t, Function<? super T, ? extends R> f) {
        return f.apply(t);
    }

    /**
     * Simulate a 'do-while' loop.
     */
    public static <T> T loop(Function<Supplier<T>, T> body) {
        Ref<Supplier<T>> h = new Ref<>();
        return h.set(Fn.lazy(() -> body.apply(h.get()))).get();
    }

    // Do notation helper methods. These methods simply call '>>=' or '>>'
    // on monads.

    /**
     * Helper method to chain monad actions together.
     */
    public static <M extends Monad<M>, A, B>
    $<M, B> do_($<M, A> m, Function<? super A, ? extends $<M, B>> k) {
        return m.getTypeClass().bind(m, k);
    }

    /**
     * Helper method to chain monad actions together.
     */
    public static <M extends Monad<M>, A, B, C>
    $<M, C> do_($<M, Tuple<A, B>> m, BiFunction<? super A, ? super B, ? extends $<M, C>> k) {
        return m.getTypeClass().bind(m, k);
    }

    /**
     * Helper method to chain monad actions together, discard intermediate result.
     */
    public static <M extends Monad<M>, A, B>
    $<M, B> do_($<M, A> a, $<M, B> b) {
        return a.getTypeClass().seqR(a, b);
    }

    /**
     * Helper method to chain monad actions together, discard intermediate result.
     */
    public static <M extends Monad<M>, A, B>
    $<M, B> do_($<M, A> a, Supplier<? extends $<M, B>> b) {
        return a.getTypeClass().seqR(a, b);
    }

    /**
     * Evaluate each action in the array from left to right, and ignore the
     * result.
     */
    @SafeVarargs
    public static <M extends Monad<M>>
    $<M, Unit> do_($<M, ?> first, $<M, ?> second, $<M, ?>... rest) {
        M tc = first.getTypeClass();
        $<M, ?> result = tc.seqR(first, second);
        for ($<M, ?> next : rest) {
            result = tc.seqR(result, next);
        }
        return tc.seqR(result, tc.unit());
    }

    /**
     * Helper method to wrap an action.
     */
    public static <A> A do_(A a) {
        return a;
    }

    /**
     * Helper method to wrap a lazy action.
     */
    public static <A> Supplier<A> do_(Supplier<A> a) {
        return a;
    }

    /**
     * Returns {@code pure ()} if the given guard condition is true, otherwise
     * return {@code mzero}.
     */
    public static <F extends Alternative<F>> $<F, Unit> guard(F m, boolean b) {
        return m.guard(b);
    }

    /**
     * Conditional execution of monad action.
     */
    public static <F extends Applicative<F>>
    $<F, Unit> when(boolean test, $<F, Unit> then) {
        return test ? then : then.getTypeClass().pure(Unit.U);
    }

    /**
     * The reverse of when.
     */
    public static <F extends Applicative<F>>
    $<F, Unit> unless(boolean test, $<F, Unit> orElse) {
        return test ? orElse.getTypeClass().pure(Unit.U) : orElse;
    }

    /**
     * Combine the results of alternatives.
     */
    @SafeVarargs
    public static <F extends Alternative<F>, A>
    $<F, A> choice($<F, A> first, $<F, A>... rest) {
        F tc = first.getTypeClass();
        $<F, A> result = first;
        for ($<F, A> next : rest) {
            result = tc.mplus(result, next);
        }
        return result;
    }

    /**
     * Repeats the action infinitely.
     */
    public static <M extends Monad<M>, A, B> $<M, B> forever($<M, A> a) {
        return loop(r -> a.getTypeClass().seqR(a, r));
    }

    /**
     * Helper method to wrap an IO action.
     */
    public static <A> IO<A> io(IO<A> a) {
        return a;
    }

    /**
     * Helper method to wrap an IO action that has no return value.
     */
    public static IO<Unit> io_(VoidIO a) {
        return a;
    }
}
