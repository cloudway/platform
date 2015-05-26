/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.parser;


import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;

import com.cloudway.fp.data.Foldable;
import com.cloudway.fp.data.Seq;

/**
 * Represents a stream of tokens. A {@code Stream} instance is responsible
 * for maintaining the "position within the stream".
 *
 * @param <T> the token type
 */
public interface Stream<T> extends Foldable<T> {
    /**
     * Load initial stream from external resource.
     */
    default Stream<T> load() {
        return this;
    }

    /**
     * Deconstruct current token and remaining stream to the given function,
     * or in case of an empty stream, apply the empty function. If the parse
     * error occurred, apply the given error handler.
     *
     * <p>The default implementation is assume that no parse error occurs and
     * ignore the error handler.</p>
     *
     * @param consumer the function to apply with current token and remaining stream
     * @param empty the function to apply when the stream is empty
     * @param error the error handler to apply when parse error occurred
     */
    default <R> R uncons(BiFunction<T, Stream<T>, R> consumer, Supplier<R> empty,
                         Function<ParseError, R> error) {
        return uncons(consumer, empty);
    }

    /**
     * Deconstruct current token and remaining stream to the given function,
     * or in case of an empty stream, apply the empty function. If the parse
     * error occurred, rethrow the exception.
     *
     * @param consumer the function to apply with current token and remaining stream
     * @param empty the function to apply when the stream is empty
     * @throws ParseError if parse error occurred.
     */
    default <R> R uncons(BiFunction<T, Stream<T>, R> consumer, Supplier<R> empty) {
        return uncons(consumer, empty, e -> { throw e; });
    }

    /**
     * Reduce the token stream, from right to left.
     */
    @Override
    default <R> R foldRight(BiFunction<? super T, Supplier<R>, R> f, Supplier<R> r) {
        return uncons((x, xs) -> f.apply(x, () -> xs.foldRight(f, r)), r);
    }

    /**
     * Get the current source code position within the stream. The position
     * is encoded in a 32 bit integer that contains both column and line number
     * information.
     */
    default int getPosition() {
        return SourcePos.NOPOS;
    }

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
        return new Streams.ChunkStream(reader, SourcePos.STARTPOS);
    }
}
