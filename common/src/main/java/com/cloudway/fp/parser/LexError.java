/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.parser;

/**
 * The exception type represents lexical analysis errors.
 */
@SuppressWarnings("ExceptionClassNameDoesntEndWithException")
public class LexError extends RuntimeException {
    private static final long serialVersionUID = 3075088474004684963L;

    public LexError(String message) {
        super(message);
    }
}
