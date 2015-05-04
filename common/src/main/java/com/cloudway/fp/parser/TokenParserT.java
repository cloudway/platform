/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.parser;

import com.cloudway.fp.$;
import com.cloudway.fp.control.Monad;
import com.cloudway.fp.data.Either;

/**
 * The parser to parse lexical elements (tokens).
 *
 * @param <ST> the user state type
 * @param <M> the inner monad type
 */
public class TokenParserT<ST, M extends Monad<M>>
    extends TokenParserTC<TokenParserT<ST, M>, ST, M>
{
    protected TokenParserT(LanguageDef ldf, M nm) {
        super(ldf, nm);
    }

    /**
     * Create a lexical parsers that are defined using the definitions in the
     * language definition.
     *
     * @param ldf the language definition
     * @param nm the inner monad
     * @return the lexical parser
     */
    public static <ST, M extends Monad<M>> TokenParserT<ST, M> on(LanguageDef ldf, M nm) {
        return new TokenParserT<>(ldf, nm);
    }

    /**
     * Run the lexical parser with the given stream input.
     */
    public static <ST, M extends Monad<M>, A> $<M, Either<ParseError, A>>
    parse($<TokenParserT<ST, M>, A> p, ST st, String name, Stream<Character> input) {
        return p.getTypeClass().runParser(p, st, name, input);
    }

    /**
     * Run the lexical parser with the given string input.
     */
    public static <ST, M extends Monad<M>, A> $<M, Either<ParseError, A>>
    parse($<TokenParserT<ST, M>, A> p, ST st, String name, String input) {
        return parse(p, st, name, Stream.of(input));
    }
}
