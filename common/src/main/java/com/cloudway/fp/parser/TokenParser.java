/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.parser;

import com.cloudway.fp.$;
import com.cloudway.fp.data.Either;
import com.cloudway.fp.data.Identity;

/**
 * A standalone lexical parser.
 *
 * @param <ST> the user state type
 */
public class TokenParser<ST> extends TokenParserTC<TokenParser<ST>, String, ST, Identity.µ> {
    /**
     * Construct a lexical parser.
     *
     * @param ldf the language definition
     */
    public TokenParser(LanguageDef ldf) {
        super(ldf, Identity.tclass);
    }

    /**
     * Run the lexical parser.
     */
    public static <ST, A>Either<ParseError, A>
    run($<TokenParser<ST>, A> p, ST st, String name, String input) {
        return Identity.run(p.getTypeClass().runParsecT(p, st, name, input));
    }

    @Override
    protected <A> $<TokenParser<ST>, A> $(ParseFunction<String, ST, Identity.µ, A, ?> f) {
        return new Monadic<TokenParser<ST>, String, ST, Identity.µ, A>(f) {
            @Override public TokenParser<ST> getTypeClass() {
                return TokenParser.this;
            }
        };
    }

    @Override
    protected Stream<String, Character> stream() {
        return CharStream.INSTANCE;
    }
}
