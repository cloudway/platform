/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.parser;


import java.util.function.BiFunction;
import java.util.function.Supplier;
import com.cloudway.fp.data.Seq;

/**
 * Represents a stream of tokens. A {@code Stream} instance is responsible
 * for maintaining the "position within the stream".
 *
 * @param <T> the token type
 */
public interface Stream<T> {
    /**
     * Load initial stream from external resource.
     */
    default Stream<T> load() {
        return this;
    }

    /**
     * Deconstruct current token and remaining stream to the given function,
     * or in case of an empty stream, apply the empty function.
     *
     * @param consumer the function to apply with current token and remaining stream
     * @param empty the function to apply when the stream is empty
     */
    <R> R uncons(BiFunction<T, Stream<T>, R> consumer, Supplier<R> empty);

    /**
     * Create a character stream from a string.
     */
    static Stream<Character> of(String input) {
        return new Streams.CharStream(input);
    }

    /**
     * Create a stream from a list.
     */
    static <T> Stream<T> of(Seq<T> list) {
        return new Streams.ListStream<>(list);
    }

    /**
     * Create a character stream from a {@code java.io.Reader}.
     */
    static Stream<Character> of(java.io.Reader reader) {
        return new Streams.ChunkStream(reader);
    }
}
