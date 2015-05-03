/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.parser;

import com.cloudway.fp.$;
import com.cloudway.fp.control.Monad;
import com.cloudway.fp.data.PSet;

/**
 * The {@code LanguageDef} is a record that contains all parameterizable
 * features of the {@link TokenParserT token parser}.
 */
public interface LanguageDef {
    /**
     * Describes the start of a block comment. Use the empty string if the
     * language doesn't support block comments.
     */
    String commentStart();

    /**
     * Describes the end of a block comment. Use the empty string if the
     * language doesn't support block comments.
     */
    String commentEnd();

    /**
     * Describes the start of a line comment. Use the empty string if the
     * language doesn't support line comments.
     */
    String commentLine();

    /**
     * Set to {@code true} if the language supports nested block comments.
     */
    boolean nestedComments();

    /**
     * Returns a parser that should accept any start characters of identifiers.
     */
    <P, S, U, M extends Monad<M>>
    $<P, Character> identStart(TokenParserTC<P, S, U, M> pt);

    /**
     * Returns a parser that should accept any legal tail characters of identifiers.
     */
    <P, S, U, M extends Monad<M>>
    $<P, Character> identPart(TokenParserTC<P, S, U, M> pt);

    /**
     * Returns a parser that should accept any start characters of operators.
     */
    <P, S, U, M extends Monad<M>>
    $<P, Character> opStart(TokenParserTC<P, S, U, M> pt);

    /**
     * Returns a parser that should accept any legal tail characters of operators.
     * Note that this parser should even be defined if the language doesn't
     * support user-defined operators, or otherwise the {@code reservedOp} parser
     * won't work correctly.
     */
    <P, S, U, M extends Monad<M>>
    $<P, Character> opPart(TokenParserTC<P, S, U, M> pt);

    /**
     * Returns the list of reserved identifiers.
     */
    PSet<String> reservedNames();

    /**
     * Returns the list of reserved operators.
     */
    PSet<String> reservedOpNames();

    /**
     * Returns {@code true} if the language is case sensitive.
     */
    boolean caseSensitive();
}
