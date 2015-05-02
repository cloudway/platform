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
 * A parser monadic transformer that accept a string an input stream.
 *
 * @param <ST> the user state type
 * @param <M> the inner monad type
 */
public class CharParserT<ST, M extends Monad<M>>
    extends CharParserTC<CharParserT<ST, M>, ST, M>
{
    private CharParserT(M nm) {
        super(nm);
    }

    @Override
    protected <A> $<CharParserT<ST, M>, A> $(ParseFunction<String, ST, M, A, ?> f) {
        return new Monadic<CharParserT<ST, M>, String, ST, M, A>(f) {
            @Override public CharParserT<ST, M> getTypeClass() {
                return CharParserT.this;
            }
        };
    }

    /**
     * Stack parser monad on another monad.
     *
     * @param nm the inner monad
     * @return the stacked parser monad
     */
    public static <ST, M extends Monad<M>> CharParserT<ST, M> on(M nm) {
        return new CharParserT<>(nm);
    }

    /**
     * Run the parser with given input.
     */
    public static <ST, M extends Monad<M>, A> $<M, Either<ParseError, A>>
    run($<CharParserT<ST, M>, A> p, ST st, String name, String input) {
        return p.getTypeClass().runParsecT(p, st, name, input);
    }
}
