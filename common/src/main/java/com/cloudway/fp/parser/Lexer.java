/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.parser;

import java.io.Reader;

/**
 * A lexer that translate source code into token stream.
 *
 * @param <T> the token type
 */
public interface Lexer<T> {
    /**
     * Returns a stream of tokens that scanned by the lexer.
     *
     * @param input the string source input
     */
    Stream<T> getTokenStream(String input);

    /**
     * Returns a stream of tokens that scanned by the lexer.
     *
     * @param input the source input from a Reader
     */
    Stream<T> getTokenStream(Reader input);

    /**
     * Returns a scanner to scan tokens imperatively.
     *
     * @param input the string source input
     */
    Scanner<T> getScanner(String input);

    /**
     * Returns a scanner to scan tokens imperatively.
     *
     * @param input the source input from a reader
     */
    Scanner<T> getScanner(Reader input);
}
