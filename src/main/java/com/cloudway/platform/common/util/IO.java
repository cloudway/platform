/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util;

import java.io.IOException;
import java.util.function.Supplier;

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

    public static <T> java.util.function.Supplier wrap(Supplier<T> other) {
        return () -> {
            try {
                return other.get();
            } catch (IOException ex) {
                throw new RuntimeIOException(ex);
            }
        };
    }

    public static <T> T caught(Supplier<T> action)
        throws IOException
    {
        try {
            return action.get();
        } catch (RuntimeIOException ex) {
            throw ex.getCause();
        }
    }

    @FunctionalInterface
    public static interface Function<T,R> {
        R apply(T t) throws IOException;
    }

    public static <T,R> java.util.function.Function<T,R> wrap(Function<T,R> other) {
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

    public static <T> java.util.function.Consumer<T> wrap(Consumer<T> other) {
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

    public static <T, U> java.util.function.BiConsumer<T, U> wrap(BiConsumer<T,U> other) {
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

    public static <T> java.util.function.Predicate<T> wrap(Predicate<T> other) {
        return (T t) -> {
            try {
                return other.test(t);
            } catch (IOException ex) {
                throw new RuntimeIOException(ex);
            }
        };
    }
}
