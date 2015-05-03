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
 * The lexical parser that accept arbitrary stream type.
 *
 * @param <S> the stream type
 * @param <U> the user state type
 */
public class GenTokenParser<S, U> extends TokenParserTC<GenTokenParser<S, U>, S, U, Identity.µ> {
    private final Stream<S, Character> stream;

    /**
     * Construct a lexical parser.
     *
     * @param stream the stream tokenizer
     * @param ldf the language definition
     */
    public GenTokenParser(Stream<S, Character> stream, LanguageDef ldf) {
        super(ldf, Identity.tclass);
        this.stream = stream;
    }

    /**
     * Run the lexical parser.
     */
    public static <S, ST, A>Either<ParseError, A>
    run($<GenTokenParser<S, ST>, A> p, ST st, String name, S input) {
        return Identity.run(p.getTypeClass().runParsecT(p, st, name, input));
    }

    @Override
    protected <A> $<GenTokenParser<S, U>, A> $(ParseFunction<S, U, Identity.µ, A, ?> f) {
        return new Monadic<GenTokenParser<S, U>, S, U, Identity.µ, A>(f) {
            @Override public GenTokenParser<S, U> getTypeClass() {
                return GenTokenParser.this;
            }
        };
    }

    @Override
    protected Stream<S, Character> stream() {
        return stream;
    }
}
