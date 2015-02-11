/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container;

import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

import com.cloudway.platform.common.fp.function.TriConsumer;
import com.cloudway.platform.common.fp.function.TriFunction;

import org.jmock.api.Action;
import org.jmock.api.Invocation;
import org.jmock.lib.action.CustomAction;

public final class Actions {
    private Actions() {}

    public static <T> Action apply(String description, Supplier<?> action) {
        return new CustomAction(description) {
            @Override public Object invoke(Invocation inv) throws Throwable {
                return action.get();
            }
        };
    }

    public static <T> Action apply(String description, Function<? super T, ?> action) {
        return new CustomAction(description) {
            @Override public Object invoke(Invocation inv) {
                @SuppressWarnings("unchecked") T t = (T)inv.getParameter(0);
                return action.apply(t);
            }
        };
    }

    public static <T, U> Action apply(String description, BiFunction<? super T, ? super U, ?> action) {
        return new CustomAction(description) {
            @Override public Object invoke(Invocation inv) throws Throwable {
                @SuppressWarnings("unchecked") T t = (T)inv.getParameter(0);
                @SuppressWarnings("unchecked") U u = (U)inv.getParameter(1);
                return action.apply(t, u);
            }
        };
    }

    public static <T, U, V> Action apply(String description, TriFunction<? super T, ? super U, ? super V, ?> action) {
        return new CustomAction(description) {
            @Override public Object invoke(Invocation inv) throws Throwable {
                @SuppressWarnings("unchecked") T t = (T)inv.getParameter(0);
                @SuppressWarnings("unchecked") U u = (U)inv.getParameter(1);
                @SuppressWarnings("unchecked") V v = (V)inv.getParameter(2);
                return action.apply(t, u, v);
            }
        };
    }

    public static <T> Action perform(String description, Runnable action) {
        return new CustomAction(description) {
            @Override public Object invoke(Invocation inv) throws Throwable {
                action.run();
                return null;
            }
        };
    }

    public static <T> Action perform(String description, Consumer<? super T> action) {
        return new CustomAction(description) {
            @Override public Object invoke(Invocation inv) {
                @SuppressWarnings("unchecked") T t = (T)inv.getParameter(0);
                action.accept(t);
                return null;
            }
        };
    }

    public static <T, U> Action perform(String description, BiConsumer<? super T, ? super U> action) {
        return new CustomAction(description) {
            @Override public Object invoke(Invocation inv) throws Throwable {
                @SuppressWarnings("unchecked") T t = (T)inv.getParameter(0);
                @SuppressWarnings("unchecked") U u = (U)inv.getParameter(1);
                action.accept(t, u);
                return null;
            }
        };
    }

    public static <T, U, V> Action perform(String description, TriConsumer<? super T, ? super U, ? super V> action) {
        return new CustomAction(description) {
            @Override public Object invoke(Invocation inv) throws Throwable {
                @SuppressWarnings("unchecked") T t = (T)inv.getParameter(0);
                @SuppressWarnings("unchecked") U u = (U)inv.getParameter(1);
                @SuppressWarnings("unchecked") V v = (V)inv.getParameter(2);
                action.accept(t, u, v);
                return null;
            }
        };
    }
}
