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
 * A standalone parser that accept a string as input stream.
 *
 * @param <ST> the user state type
 */
public class CharParser<ST> extends CharParserTC<CharParser<ST>, ST, Identity.µ> {
    private CharParser() {
        super(Identity.tclass);
    }

    private static final CharParser<?> _TCLASS = new CharParser<>();

    @SuppressWarnings("unchecked")
    public static <ST> CharParser<ST> get() {
        return (CharParser<ST>)_TCLASS;
    }

    public static <ST, A> Either<ParseError, A>
    run($<CharParser<ST>, A> p, ST st, String name, String input) {
        return Identity.run(p.getTypeClass().runParsecT(p, st, name, input));
    }

    private static final class Parser<ST, A> extends Monadic<CharParser<ST>, String, ST, Identity.µ, A> {
        Parser(ParseFunction<String, ST, Identity.µ, A, ?> f) {
            super(f);
        }

        @Override
        @SuppressWarnings("unchecked")
        public CharParser<ST> getTypeClass() {
            return get();
        }
    }

    @Override
    protected <A> $<CharParser<ST>, A> $(ParseFunction<String, ST, Identity.µ, A, ?> f) {
        return new Parser<>(f);
    }
}
