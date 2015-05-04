/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.parser;

import com.cloudway.fp.$;

/**
 * This enum defines some language definitions that can be used to instantiate
 * a {@link TokenParser token parser}.
 */
public enum Languages implements LanguageDef {
    /**
     * Minimal language definition.
     *
     * <p>The {@code Empty} language definition has no reserved names or
     * operators, is case sensitive and doesn't accept comments, identifiers
     * or operators.
     */
    Empty,

    /**
     * This is a minimal token definition for Java style languages. It defines
     * the style of comments valid identifiers and case sensitivity. It does
     * not define any reserved words or operators.
     */
    Java {
        @Override
        public String commentStart() {
            return "/*";
        }

        @Override
        public String commentEnd() {
            return "*/";
        }

        @Override
        public String commentLine() {
            return "//";
        }

        @Override
        public <P> $<P, Character> identStart(TokenParserTC<P, ?, ?> pt) {
            return pt.satisfy(Character::isJavaIdentifierStart);
        }

        @Override
        public <P > $<P, Character> identPart(TokenParserTC<P, ?, ?> pt) {
            return pt.satisfy(Character::isJavaIdentifierPart);
        }

        @Override
        public <P> $<P, Character> opPart(TokenParserTC<P, ?, ?> pt) {
            return pt.fail("operator not allowed");
        }
    },

    /**
     * This is a minimal token definition for Haskell style languages. It
     * defines the style of comments, valid identifiers and case sensitivity.
     * It does not define any reserved words or operators.
     */
    Haskell {
        @Override
        public String commentStart() {
            return "{-";
        }

        @Override
        public String commentEnd() {
            return "-}";
        }

        @Override
        public String commentLine() {
            return "--";
        }

        @Override
        public <P> $<P, Character> identStart(TokenParserTC<P, ?, ?> pt) {
            return pt.letter();
        }

        @Override
        public <P> $<P, Character> identPart(TokenParserTC<P, ?, ?> pt) {
            return pt.mplus(pt.alphaNum(), pt.oneOf("_'"));
        }
    }
}
