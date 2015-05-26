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
        private final int pos;

        CharStream(String input) {
            this(input, 0, SourcePos.STARTPOS);
        }

        private CharStream(String input, int offset, int pos) {
            this.input = input;
            this.offset = offset;
            this.pos = pos;
        }

        @Override
        public <R> R uncons(BiFunction<Character, Stream<Character>, R> f, Supplier<R> e) {
            if (offset >= input.length()) {
                return e.get();
            } else {
                char c = input.charAt(offset);
                int nextpos = SourcePos.nextchar(pos, c);
                return f.apply(c, new CharStream(input, offset + 1, nextpos));
            }
        }

        @Override
        public int getPosition() {
            return pos;
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

        @Override
        public <R> R foldRight(BiFunction<? super T, Supplier<R>, R> f, Supplier<R> r) {
            return list.foldRight(f, r);
        }

        @Override
        public <R> R foldRight_(R z, BiFunction<? super T, R, R> f) {
            return list.foldRight_(z, f);
        }

        @Override
        public <R> R foldLeft(R z, BiFunction<R, ? super T, R> f) {
            return list.foldLeft(z, f);
        }

        @Override
        public Seq<T> asList() {
            return list;
        }
    }

    /**
     * The chunked input stream.
     */
    static class ChunkStream implements Stream<Character> {
        private static final int CHUNK_SIZE = 8192;

        private final Reader reader;
        private final int pos;

        ChunkStream(Reader reader, int pos) {
            this.reader = reader;
            this.pos = pos;
        }

        @Override
        public Stream<Character> load() {
            return makeChunk(makeChunkList(reader), pos);
        }

        @Override
        public <R> R uncons(BiFunction<Character, Stream<Character>, R> f, Supplier<R> e) {
            return e.get();
        }

        @Override
        public int getPosition() {
            return pos;
        }

        private static Stream<Character> makeChunk(Seq<char[]> chunks, int pos) {
            if (chunks.isEmpty()) {
                return new ChunkStream(null, pos);
            } else {
                return new Chunk(chunks, 0, pos);
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
            private final int pos;

            Chunk(Seq<char[]> chunks, int offset, int pos) {
                this.chunks = chunks;
                this.offset = offset;
                this.pos = pos;
            }

            @Override
            public <R> R uncons(BiFunction<Character, Stream<Character>, R> f, Supplier<R> e) {
                char[] cs = chunks.head();
                char c = cs[offset];
                int nextpos = SourcePos.nextchar(pos, c);
                if (offset + 1 < cs.length) {
                    return f.apply(c, new Chunk(chunks, offset + 1, nextpos));
                } else {
                    return f.apply(c, makeChunk(chunks.tail(), nextpos));
                }
            }

            @Override
            public int getPosition() {
                return pos;
            }
        }
    }
}
