/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.Map;
import java.util.stream.Stream;

public final class IO
{
    private IO() {}

    public static void perform(IOAction action)
        throws IOException
    {
        try {
            action.perform();
        } catch (UncheckedIOException ex) {
            throw ex.getCause();
        }
    }

    public static <T> T produce(IOSupplier<? extends T> action)
        throws IOException
    {
        try {
            return action.produce();
        } catch (UncheckedIOException ex) {
            throw ex.getCause();
        }
    }

    public static <T> void forEach(Stream<T> stream, IOConsumer<? super T> action)
        throws IOException
    {
        try {
            stream.forEach(IOConsumer.wrap(action));
        } catch (UncheckedIOException ex) {
            throw ex.getCause();
        }
    }

    public static <T> void forEach(Iterable<T> collection, IOConsumer<? super T> action)
        throws IOException
    {
        try {
            collection.forEach(IOConsumer.wrap(action));
        } catch (UncheckedIOException ex) {
            throw ex.getCause();
        }
    }

    public static<K,V> void forEach(Map<K,V> map, IOBiConsumer<? super K, ? super V> action)
        throws IOException
    {
        try {
            map.forEach(IOBiConsumer.wrap(action));
        } catch (UncheckedIOException ex) {
            throw ex.getCause();
        }
    }

    public static Conditionals.ActionBrancher<IOException> with() {
        return Conditionals.actionBrancher();
    }

    public static <T> Conditionals.ActionSwitcher<T, IOException> with(T value) {
        return Conditionals.actionSwitcher(value);
    }

    public static <T, U> Conditionals.BiActionSwitcher<T, U, IOException> with(T t, U u) {
        return Conditionals.biActionSwitcher(t, u);
    }
}
