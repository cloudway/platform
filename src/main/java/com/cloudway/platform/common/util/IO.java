/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util;

import java.io.IOException;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Stream;

public final class IO
{
    private IO() {}

    // Function interface wrappers

    @FunctionalInterface
    public static interface Runnable {
        void run() throws IOException;
    }

    public static java.lang.Runnable wrap(Runnable other) {
        return () -> {
            try {
                other.run();
            } catch (IOException ex) {
                throw new RuntimeIOException(ex);
            }
        };
    }

    public static void caught(Runnable action)
        throws IOException
    {
        try {
            action.run();
        } catch (RuntimeIOException ex) {
            throw ex.getCause();
        }
    }

    public static void ignore(Runnable action) {
        try {
            action.run();
        } catch (IOException|RuntimeIOException ex) {
            // ignored
        }
    }

    @FunctionalInterface
    public static interface Supplier<T> {
        T get() throws IOException;
    }

    public static <T> java.util.function.Supplier<T> wrap(Supplier<? extends T> other) {
        return () -> {
            try {
                return other.get();
            } catch (IOException ex) {
                throw new RuntimeIOException(ex);
            }
        };
    }

    public static <T> T caught(Supplier<? extends T> action)
        throws IOException
    {
        try {
            return action.get();
        } catch (RuntimeIOException ex) {
            throw ex.getCause();
        }
    }

    public static <T> Optional<T> ignore(Supplier<? extends T> action) {
        try {
            return Optional.ofNullable(action.get());
        } catch (IOException|RuntimeIOException ex) {
            return Optional.empty();
        }
    }

    @FunctionalInterface
    public static interface Function<T,R> {
        R apply(T t) throws IOException;
    }

    public static <T,R> java.util.function.Function<T,R> wrap(Function<? super T, ? extends R> other) {
        return (T t) -> {
            try {
                return other.apply(t);
            } catch (IOException ex) {
                throw new RuntimeIOException(ex);
            }
        };
    }

    @FunctionalInterface
    public static interface Consumer<T> {
        void accept(T t) throws IOException;
    }

    public static <T> java.util.function.Consumer<T> wrap(Consumer<? super T> other) {
        return (T t) -> {
            try {
                other.accept(t);
            } catch (IOException ex) {
                throw new RuntimeIOException(ex);
            }
        };
    }

    @FunctionalInterface
    public static interface BiConsumer<T, U> {
        void accept(T t, U u) throws IOException;
    }

    public static <T, U> java.util.function.BiConsumer<T, U> wrap(BiConsumer<? super T, ? super U> other) {
        return (T t, U u) -> {
            try {
                other.accept(t, u);
            } catch (IOException ex) {
                throw new RuntimeIOException(ex);
            }
        };
    }

    public static interface Predicate<T> {
        boolean test(T t) throws IOException;
    }

    public static <T> java.util.function.Predicate<T> wrap(Predicate<? super T> other) {
        return (T t) -> {
            try {
                return other.test(t);
            } catch (IOException ex) {
                throw new RuntimeIOException(ex);
            }
        };
    }

    public static <T> void forEach(Stream<T> stream, Consumer<? super T> action)
        throws IOException
    {
        try {
            stream.forEach(wrap(action));
        } catch (RuntimeIOException ex) {
            throw ex.getCause();
        }
    }

    public static <T> void forEach(Iterable<T> collection, Consumer<? super T> action)
        throws IOException
    {
        try {
            collection.forEach(wrap(action));
        } catch (RuntimeIOException ex) {
            throw ex.getCause();
        }
    }

    public static<K,V> void forEach(Map<K,V> map, BiConsumer<? super K,? super V> action)
        throws IOException
    {
        try {
            map.forEach(wrap(action));
        } catch (RuntimeIOException ex) {
            throw ex.getCause();
        }
    }
}
