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
    extends TokenParserTC<TokenParserT<ST, M>, String, ST, M>
{
    private TokenParserT(LanguageDef ldf, M nm) {
        super(ldf, nm);
    }

    @Override
    protected <A> $<TokenParserT<ST, M>, A> $(ParseFunction<String, ST, M, A, ?> f) {
        return new Monadic<TokenParserT<ST, M>, String, ST, M, A>(f) {
            @Override public TokenParserT<ST, M> getTypeClass() {
                return TokenParserT.this;
            }
        };
    }

    @Override
    protected Stream<String, Character> stream() {
        return CharStream.INSTANCE;
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
     * Run the lexical parser.
     */
    public static <ST, M extends Monad<M>, A> $<M, Either<ParseError, A>>
    run($<TokenParserT<ST, M>, A> p, ST st, String name, String input) {
        return p.getTypeClass().runParsecT(p, st, name, input);
    }
}
