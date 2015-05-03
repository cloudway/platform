/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.parser;

import com.cloudway.fp.$;
import com.cloudway.fp.control.Monad;
import com.cloudway.fp.data.HashPSet;
import com.cloudway.fp.data.PSet;

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
    Empty {
        @Override
        public String commentStart() {
            return "";
        }

        @Override
        public String commentEnd() {
            return "";
        }

        @Override
        public String commentLine() {
            return "";
        }

        @Override
        public boolean nestedComments() {
            return true;
        }

        @Override
        public <P, S, U, M extends Monad<M>>
        $<P, Character> identStart(TokenParserTC<P, S, U, M> pt) {
            return pt.mplus(pt.letter(), pt.chr('_'));
        }

        @Override
        public <P, S, U, M extends Monad<M>>
        $<P, Character> identPart(TokenParserTC<P, S, U, M> pt) {
            return pt.mplus(pt.alphaNum(), pt.chr('_'));
        }

        @Override
        public <P, S, U, M extends Monad<M>>
        $<P, Character> opStart(TokenParserTC<P, S, U, M> pt) {
            return opPart(pt);
        }

        @Override
        public <P, S, U, M extends Monad<M>>
        $<P, Character> opPart(TokenParserTC<P, S, U, M> pt) {
            return pt.oneOf(":!#$%&*+./<=>?@\\^|-~");
        }

        @Override
        public PSet<String> reservedNames() {
            return HashPSet.empty();
        }

        @Override
        public PSet<String> reservedOpNames() {
            return HashPSet.empty();
        }

        @Override
        public boolean caseSensitive() {
            return true;
        }
    },

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
        public boolean nestedComments() {
            return true;
        }

        @Override
        public <P, S, U, M extends Monad<M>>
        $<P, Character> identStart(TokenParserTC<P, S, U, M> pt) {
            return pt.satisfy(Character::isJavaIdentifierStart);
        }

        @Override
        public <P, S, U, M extends Monad<M>>
        $<P, Character> identPart(TokenParserTC<P, S, U, M> pt) {
            return pt.satisfy(Character::isJavaIdentifierPart);
        }

        @Override
        public <P, S, U, M extends Monad<M>>
        $<P, Character> opStart(TokenParserTC<P, S, U, M> pt) {
            return opPart(pt);
        }

        @Override
        public <P, S, U, M extends Monad<M>>
        $<P, Character> opPart(TokenParserTC<P, S, U, M> pt) {
            return pt.oneOf(":!#$%&*+./<=>?@\\^|-~");
        }

        @Override
        public PSet<String> reservedNames() {
            return HashPSet.empty();
        }

        @Override
        public PSet<String> reservedOpNames() {
            return HashPSet.empty();
        }

        @Override
        public boolean caseSensitive() {
            return true;
        }
    }
}
