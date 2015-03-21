/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.control;

import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

final class Generators {
    private Generators() {}

    private static final Empty<?> _EMPTY = new Empty<>();

    @SuppressWarnings("unchecked")
    static <A> Generator<A> empty() {
        return (Generator<A>)_EMPTY;
    }

    static class Empty<A> implements Generator<A> {
        @Override
        public Iterator<A> iterator() {
            return start();
        }

        @Override
        public Channel<A> start() {
            return new Channel<A>() {
                @Override public boolean hasNext() {
                    return false;
                }
                @Override public A next() {
                    throw new NoSuchElementException();
                }
                @Override public A send(A value) {
                    throw new UnsupportedOperationException();
                }
            };
        }

        @Override
        public Generator<A> filter(Predicate<? super A> predicate) {
            return empty();
        }

        @Override
        public <B> Generator<B> map(Function<? super A, ? extends B> mapper) {
            return empty();
        }

        @Override
        public <B> Generator<B> bind(Function<? super A, ? extends Generator<B>> mapper) {
            return empty();
        }

        @Override
        public <R> R foldRight(R identity, BiFunction<? super A, Supplier<R>, R> accumulator) {
            return identity;
        }

        @Override
        public <B, C> Generator<C> zip(Generator<B> b, BiFunction<? super A, ? super B, ? extends C> zipper) {
            return empty();
        }
    }

    static class Pure<A> implements Generator<A> {
        private final A value;

        Pure(A a) {
            value = a;
        }

        @Override
        public Iterator<A> iterator() {
            return start();
        }

        @Override
        public Channel<A> start() {
            return new Channel<A>() {
                private boolean hasNext = true;

                @Override
                public boolean hasNext() {
                    return hasNext;
                }

                @Override
                public A next() {
                    if (hasNext()) {
                        hasNext = false;
                        return value;
                    }
                    throw new NoSuchElementException();
                }

                @Override
                public A send(A value) {
                    throw new UnsupportedOperationException();
                }
            };
        }

        @Override
        public Generator<A> filter(Predicate<? super A> predicate) {
            return predicate.test(value) ? this : empty();
        }

        @Override
        public <B> Generator<B> map(Function<? super A, ? extends B> f) {
            return new Pure<>(f.apply(value));
        }

        @Override
        public <B> Generator<B> bind(Function<? super A, ? extends Generator<B>> f) {
            return f.apply(value);
        }

        @Override
        public <R> R foldRight(R z, BiFunction<? super A, Supplier<R>, R> f) {
            return f.apply(value, () -> z);
        }

        @Override
        public <B, C> Generator<C> zip(Generator<B> b, BiFunction<? super A, ? super B, ? extends C> f) {
            if (b instanceof Pure) {
                return new Pure<>(f.apply(value, ((Pure<B>)b).value));
            } else {
                return b.zip(this, (y, x) -> f.apply(x, y));
            }
        }
    }
}
