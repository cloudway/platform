/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.parser;

import java.util.Iterator;

/**
 * A scanner that scans tokens imperatively.
 *
 * @param <T> the token type
 */
public interface Scanner<T> extends Iterator<T> {
    /**
     * Returns the lexeme text associated with current token.
     */
    String text();

    /**
     * Returns the line number associated with current token.
     */
    int line();

    /**
     * Returns the column number associated with current token.
     */
    int column();
}
