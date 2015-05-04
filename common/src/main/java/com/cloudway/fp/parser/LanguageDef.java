/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.parser;

import com.cloudway.fp.$;
import com.cloudway.fp.data.HashPSet;
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
    default String commentStart() {
        return "";
    }

    /**
     * Describes the end of a block comment. Use the empty string if the
     * language doesn't support block comments.
     */
    default String commentEnd() {
        return "";
    }

    /**
     * Describes the start of a line comment. Use the empty string if the
     * language doesn't support line comments.
     */
    default String commentLine() {
        return "";
    }

    /**
     * Set to {@code true} if the language supports nested block comments.
     */
    default boolean nestedComments() {
        return true;
    }

    /**
     * Returns a parser that should accept any start characters of identifiers.
     */
    default <P> $<P, Character> identStart(TokenParserTC<P, ?, ?> pt) {
        return pt.mplus(pt.letter(), pt.chr('_'));
    }

    /**
     * Returns a parser that should accept any legal tail characters of identifiers.
     */
    default <P> $<P, Character> identPart(TokenParserTC<P, ?, ?> pt) {
        return pt.mplus(pt.alphaNum(), pt.chr('_'));
    }

    /**
     * Returns a parser that should accept any start characters of operators.
     */
    default <P> $<P, Character> opStart(TokenParserTC<P, ?, ?> pt) {
        return opPart(pt);
    }

    /**
     * Returns a parser that should accept any legal tail characters of operators.
     * Note that this parser should even be defined if the language doesn't
     * support user-defined operators, or otherwise the {@code reservedOp} parser
     * won't work correctly.
     */
    default <P> $<P, Character> opPart(TokenParserTC<P, ?, ?> pt) {
        return pt.oneOf(":!#$%&*+./<=>?@\\^|-~");
    }

    /**
     * Returns the list of reserved identifiers.
     */
    default PSet<String> reservedNames() {
        return HashPSet.empty();
    }

    /**
     * Returns the list of reserved operators.
     */
    default PSet<String> reservedOpNames() {
        return HashPSet.empty();
    }

    /**
     * Returns {@code true} if the language is case sensitive.
     */
    default boolean caseSensitive() {
        return true;
    }
}
