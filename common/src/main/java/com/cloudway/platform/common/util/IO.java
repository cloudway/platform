/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Stream;

public final class IO
{
    private IO() {}

    public static void caught(IOAction action)
        throws IOException
    {
        try {
            action.perform();
        } catch (UncheckedIOException ex) {
            throw ex.getCause();
        }
    }

    public static void ignore(IOAction action) {
        try {
            action.perform();
        } catch (IOException | UncheckedIOException ex) {
            // ignored
        }
    }

    public static <T> T caught(IOSupplier<? extends T> action)
        throws IOException
    {
        try {
            return action.produce();
        } catch (UncheckedIOException ex) {
            throw ex.getCause();
        }
    }

    public static <T> Optional<T> ignore(IOSupplier<? extends T> action) {
        try {
            return Optional.ofNullable(action.produce());
        } catch (IOException|UncheckedIOException ex) {
            return Optional.empty();
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

    public static<K,V> void forEach(Map<K,V> map, BiIOConsumer<? super K,? super V> action)
        throws IOException
    {
        try {
            map.forEach(BiIOConsumer.wrap(action));
        } catch (UncheckedIOException ex) {
            throw ex.getCause();
        }
    }

    public static Conditionals.ActionConditional<IOException> with() {
        return Conditionals.actionConditional();
    }

    public static <T> Conditionals.ActionSwitcher<T, IOException> with(T value) {
        return Conditionals.actionSwitcher(value);
    }
}
