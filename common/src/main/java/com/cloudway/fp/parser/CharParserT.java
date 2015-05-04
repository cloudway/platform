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
 * A character parser monadic transformer.
 *
 * @param <ST> the user state type
 * @param <M> the inner monad type
 */
public class CharParserT<ST, M extends Monad<M>>
    extends CharParserTC<CharParserT<ST, M>, ST, M>
{
    protected CharParserT(M nm) {
        super(nm);
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
     * Run the parser with the given stream input.
     */
    public static <ST, M extends Monad<M>, A> $<M, Either<ParseError, A>>
    parse($<CharParserT<ST, M>, A> p, ST st, String name, Stream<Character> input) {
        return p.getTypeClass().runParser(p, st, name, input);
    }

    /**
     * Run the parser with the given string input.
     */
    public static <ST, M extends Monad<M>, A> $<M, Either<ParseError, A>>
    parse($<CharParserT<ST, M>, A> p, ST st, String name, String input) {
        return parse(p, st, name, Stream.of(input));
    }
}
