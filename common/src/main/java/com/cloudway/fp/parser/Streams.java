/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.parser;

import java.io.IOException;
import java.io.Reader;
import java.io.UncheckedIOException;
import java.util.Arrays;
import java.util.function.BiFunction;
import java.util.function.Supplier;

import com.cloudway.fp.data.Seq;

final class Streams {
    private Streams() {}

    /**
     * The string input stream.
     */
    static final class CharStream implements Stream<Character> {
        private final String input;
        private final int offset;

        CharStream(String input) {
            this(input, 0);
        }

        private CharStream(String input, int offset) {
            this.input = input;
            this.offset = offset;
        }

        @Override
        public <R> R uncons(BiFunction<Character, Stream<Character>, R> f, Supplier<R> e) {
            if (offset >= input.length()) {
                return e.get();
            } else {
                return f.apply(input.charAt(offset), new CharStream(input, offset + 1));
            }
        }
    }

    /**
     * The List input stream.
     */
    static final class ListStream<T> implements Stream<T> {
        private final Seq<T> list;

        ListStream(Seq<T> list) {
            this.list = list;
        }

        @Override
        public <R> R uncons(BiFunction<T, Stream<T>, R> f, Supplier<R> e) {
            if (list.isEmpty()) {
                return e.get();
            } else {
                return f.apply(list.head(), new ListStream<>(list.tail()));
            }
        }
    }

    /**
     * The chunked input stream.
     */
    static class ChunkStream implements Stream<Character> {
        private static final int CHUNK_SIZE = 8192;
        private static final ChunkStream EOF = new ChunkStream(null);

        private final Reader reader;

        ChunkStream(Reader reader) {
            this.reader = reader;
        }

        @Override
        public Stream<Character> load() {
            return makeChunk(makeChunkList(reader));
        }

        @Override
        public <R> R uncons(BiFunction<Character, Stream<Character>, R> f, Supplier<R> e) {
            return e.get();
        }

        private static Stream<Character> makeChunk(Seq<char[]> chunks) {
            if (chunks.isEmpty()) {
                return EOF;
            } else {
                return new Chunk(chunks, 0);
            }
        }

        private static Seq<char[]> makeChunkList(Reader reader) {
            try {
                char[] payload = new char[CHUNK_SIZE];
                int length = reader.read(payload);
                if (length <= 0) {
                    return Seq.nil();
                } else {
                    if (length != payload.length) {
                        payload = Arrays.copyOf(payload, length);
                    }
                    return Seq.cons(payload, () -> makeChunkList(reader));
                }
            } catch (IOException ex) {
                throw new UncheckedIOException(ex);
            }
        }

        static class Chunk implements Stream<Character> {
            private final Seq<char[]> chunks;
            private final int offset;

            Chunk(Seq<char[]> chunks, int offset) {
                this.chunks = chunks;
                this.offset = offset;
            }

            @Override
            public <R> R uncons(BiFunction<Character, Stream<Character>, R> f, Supplier<R> e) {
                char[] cs = chunks.head();
                char c = cs[offset];
                if (offset + 1 < cs.length) {
                    return f.apply(c, new Chunk(chunks, offset + 1));
                } else {
                    return f.apply(c, makeChunk(chunks.tail()));
                }
            }
        }
    }
}
