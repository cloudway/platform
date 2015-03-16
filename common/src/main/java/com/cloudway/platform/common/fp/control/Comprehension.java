/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.control;

import java.util.Collection;
import java.util.Optional;
import java.util.function.DoubleFunction;
import java.util.function.Function;
import java.util.function.IntFunction;
import java.util.function.LongFunction;
import java.util.function.Supplier;
import java.util.stream.DoubleStream;
import java.util.stream.IntStream;
import java.util.stream.LongStream;
import java.util.stream.Stream;

import com.cloudway.platform.common.fp.data.Either;
import com.cloudway.platform.common.fp.data.Fn;
import com.cloudway.platform.common.fp.data.IntSeq;
import com.cloudway.platform.common.fp.data.Seq;
import com.cloudway.platform.common.fp.data.Unit;
import com.cloudway.platform.common.fp.io.IO;
import com.cloudway.platform.common.fp.io.VoidIO;

/**
 * A utility class that build a DSL for list comprehension.
 */
public final class Comprehension {
    private Comprehension() {}

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

        OPTIONAL {
            @Override
            public Optional empty() {
                return Optional.empty();
            }

            @Override
            public Optional pure(Object value) {
                return Optional.of(value);
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
     * Generate comprehension from optional.
     */
    public static <T, R> Qualifier<R, Optional<R>>
    from(Optional<T> optional, Function<? super T, ? extends Qualifier<R, Optional<R>>> mapper) {
        return new Qualifier<R, Optional<R>>() {
            @Override
            @SuppressWarnings("unchecked")
            protected Optional<R> select() {
                return build(Builders.OPTIONAL);
            }

            @Override
            protected Optional<R> build(Builder<R, Optional<R>> builder) {
                return optional.flatMap(t -> builder.build(mapper.apply(t)));
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
         * @see Comprehension#from(Stream,Function)
         */
        public static <T, R> Stream<R>
        from(Stream<T> stream, Function<? super T, ? extends Qualifier<R, Stream<R>>> mapper) {
            return Comprehension.from(stream, mapper).select();
        }

        /**
         * @see Comprehension#from(IntStream,IntFunction)
         */
        public static <R> Stream<R>
        from(IntStream stream, IntFunction<? extends Qualifier<R, Stream<R>>> mapper) {
            return Comprehension.from(stream, mapper).select();
        }

        /**
         * @see Comprehension#from(LongStream,LongFunction)
         */
        public static <R> Stream<R>
        from(LongStream stream, LongFunction<? extends Qualifier<R, Stream<R>>> mapper) {
            return Comprehension.from(stream, mapper).select();
        }

        /**
         * @see Comprehension#from(DoubleStream,DoubleFunction)
         */
        public static <R> Stream<R>
        from(DoubleStream stream, DoubleFunction<? extends Qualifier<R, Stream<R>>> mapper) {
            return Comprehension.from(stream, mapper).select();
        }

        /**
         * @see Comprehension#from(Collection,Function)
         */
        public static <T, R> Stream<R>
        from(Collection<T> col, Function<? super T, ? extends Qualifier<R, Stream<R>>> mapper) {
            return Comprehension.from(col, mapper).select();
        }

        /**
         * @see Comprehension#from(Seq,Function)
         */
        public static <T, R> Seq<R>
        from(Seq<T> seq, Function<? super T, ? extends Qualifier<R, Seq<R>>> mapper) {
            return Comprehension.from(seq, mapper).select();
        }

        /**
         * @see Comprehension#from(IntSeq,IntFunction)
         */
        public static <R> Seq<R>
        from(IntSeq seq, IntFunction<? extends Qualifier<R, Seq<R>>> mapper) {
            return Comprehension.from(seq, mapper).select();
        }

        /**
         * @see Comprehension#from(Optional,Function)
         */
        public static <T, R> Optional<R>
        from(Optional<T> optional, Function<? super T, ? extends Qualifier<R, Optional<R>>> mapper) {
            return Comprehension.from(optional, mapper).select();
        }

        /**
         * @see Comprehension#from(Function,Function)
         */
        public static <T, W, R> Function<T, R>
        from(Function<T, W> h, Function<? super W, ? extends Qualifier<R, Function<T, R>>> f) {
            return Comprehension.from(h, f).select();
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

    // Do notation helper methods.  These methods simply call {@code bind} or
    // {@code andThen} on monads.

    /**
     * Helper method to chain optional actions together.
     */
    public static <A, B> Optional<B>
    do_(Optional<A> a, Function<? super A, Optional<B>> f) {
        return a.flatMap(f);
    }

    /**
     * Helper method to chain either actions together.
     */
    public static <T, A, B> Either<T, B>
    do_(Either<T, A> a, Function<? super A, Either<T, B>> f) {
        return a.flatMap(f);
    }

    /**
     * Helper method to chain IO actions together.
     */
    public static <A, B> IO<B> do_(IO<A> a, Function<? super A, ? extends IO<B>> f) {
        return a.bind(f);
    }

    /**
     * Helper method to chain IO actions together, discard intermediate result.
     */
    public static <A, B> IO<B> do_(IO<A> a, IO<B> b) {
        return a.andThen(b);
    }

    /**
     * Helper method to chain IO actions together, discard intermediate result.
     */
    public static <A, B> IO<B> do_(IO<A> a, Supplier<IO<B>> b) {
        return a.andThen(b);
    }

    /**
     * Conditional execution of IO action.
     */
    public static IO<Unit> when(boolean test, IO<Unit> then) {
        return test ? then : IO.unit;
    }

    /**
     * The reverse of when.
     */
    public static IO<Unit> unless(boolean test, IO<Unit> orElse) {
        return test ? IO.unit : orElse;
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

    /**
     * Helper method to chain state actions together.
     */
    public static <A, B, S> MonadState<B, S>
    do_(MonadState<A, S> a, Function<? super A, MonadState<B, S>> f) {
        return a.bind(f);
    }

    /**
     * Helper method to chain state actions together, discard intermediate result.
     */
    public static <A, B, S> MonadState<B, S>
    do_(MonadState<A, S> a, MonadState<B, S> b) {
        return a.andThen(b);
    }

    /**
     * Helper method to chain state actions together, discard intermediate result.
     */
    public static <A, B, S> MonadState<B, S>
    do_(MonadState<A, S> a, Supplier<MonadState<B, S>> b) {
        return a.andThen(b);
    }

    /**
     * Conditional execution of state action.
     */
    public static <S> MonadState<Unit, S> when(boolean test, MonadState<Unit, S> then) {
        return test ? then : MonadState.unit();
    }

    /**
     * The reverse of when.
     */
    public static <S> MonadState<Unit, S> unless(boolean test, MonadState<Unit, S> orElse) {
        return test ? MonadState.unit() : orElse;
    }

    /**
     * Helper method to chain stateful IO actions together.
     */
    public static <A, B, S> StateIO<B, S>
    do_(StateIO<A, S> a, Function<? super A, StateIO<B, S>> f) {
        return a.bind(f);
    }

    /**
     * Helper method to chain stateful IO actions together, discard intermediate result.
     */
    public static <A, B, S> StateIO<B, S>
    do_(StateIO<A, S> a, StateIO<B, S> b) {
        return a.andThen(b);
    }

    /**
     * Helper method to chain stateful IO actions together, discard intermediate result.
     */
    public static <A, B, S> StateIO<B, S>
    do_(StateIO<A, S> a, Supplier<StateIO<B, S>> b) {
        return a.andThen(b);
    }

    /**
     * Conditional execution of IO action.
     */
    public static <S> StateIO<Unit, S> when(boolean test, StateIO<Unit, S> then) {
        return test ? then : StateIO.unit();
    }

    /**
     * The reverse of when.
     */
    public static <S> StateIO<Unit, S> unless(boolean test, StateIO<Unit, S> orElse) {
        return test ? StateIO.unit() : orElse;
    }

    /**
     * Helper method to chain trampoline actions together.
     */
    public static <A, B> Trampoline<B>
    do_(Trampoline<A> a, Function<? super A, Trampoline<B>> f) {
        return a.bind(f);
    }

    /**
     * Helper method to chain trampoline actions together, discard intermediate result.
     */
    public static <A, B> Trampoline<B>
    do_(Trampoline<A> a, Trampoline<B> b) {
        return a.bind(__ -> b);
    }

    /**
     * Helper method to chain trampoline actions together, discard intermediate result.
     */
    public static <A, B> Trampoline<B>
    do_(Trampoline<A> a, Supplier<Trampoline<B>> b) {
        return a.bind(__ -> Trampoline.suspend(b));
    }

    /**
     * Conditional execution of trampoline action.
     */
    public static Trampoline<Unit> when(boolean test, Trampoline<Unit> then) {
        return test ? then : Trampoline.pure(Unit.U);
    }

    /**
     * The reverse of when.
     */
    public static Trampoline<Unit> unless(boolean test, Trampoline<Unit> orElse) {
        return test ? Trampoline.pure(Unit.U) : orElse;
    }

    /**
     * Helper method to chain CPS actions together.
     */
    public static <A, B> Cont<B>
    do_(Cont<A> a, Function<? super A, Cont<B>> f) {
        return a.bind(f);
    }

    /**
     * Helper method to chain CPS actions together, discard intermediate result.
     */
    public static <A, B> Cont<B>
    do_(Cont<A> a, Cont<B> b) {
        return a.andThen(b);
    }

    /**
     * Helper method to chain CPS actions together, discard intermediate result.
     */
    public static <A, B> Cont<B>
    do_(Cont<A> a, Supplier<Cont<B>> b) {
        return a.andThen(b);
    }

    /**
     * Conditional execution of continuation action.
     */
    public static Cont<Unit> when(boolean test, Cont<Unit> then) {
        return test ? then : Cont.unit();
    }

    /**
     * The reverse of when.
     */
    public static Cont<Unit> unless(boolean test, Cont<Unit> orElse) {
        return test ? Cont.unit() : orElse;
    }

    /**
     * Helper method to chain CPS actions together.
     */
    public static <A, B, S> StateCont<B, S>
    do_(StateCont<A, S> a, Function<? super A, StateCont<B, S>> f) {
        return a.bind(f);
    }

    /**
     * Helper method to chain CPS actions together, discard intermediate result.
     */
    public static <A, B, S> StateCont<B, S>
    do_(StateCont<A, S> a, StateCont<B, S> b) {
        return a.andThen(b);
    }

    /**
     * Helper method to chain CPS actions together, discard intermediate result.
     */
    public static <A, B, S> StateCont<B, S>
    do_(StateCont<A, S> a, Supplier<StateCont<B, S>> b) {
        return a.andThen(b);
    }

    /**
     * Conditional execution of continuation action.
     */
    public static <S> StateCont<Unit, S> when(boolean test, StateCont<Unit, S> then) {
        return test ? then : StateCont.unit();
    }

    /**
     * The reverse of when.
     */
    public static <S> StateCont<Unit, S> unless(boolean test, StateCont<Unit, S> orElse) {
        return test ? StateCont.unit() : orElse;
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
}
