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
 * A generalized parser that accept token and user state type as parameters.
 *
 * @param <T> the token type
 * @param <ST> the user state type
 * @param <M> the inner monad type
 */
public class GenParserT<T, ST, M extends Monad<M>>
    extends GenParserTC<GenParserT<T, ST, M>, T, ST, M>
{
    /**
     * Construct a new parser stacked on the given monad.
     */
    protected GenParserT(M nm) {
        super(nm);
    }

    /**
     * Stack parser monad on another monad.
     *
     * @param nm the inner monad
     * @return the stacked parser monad
     */
    public static <T, ST, M extends Monad<M>> GenParserT<T, ST, M> on(M nm) {
        return new GenParserT<>(nm);
    }

    /**
     * Run the parser with the given input stream.
     */
    public static <T, ST, M extends Monad<M>, A> $<M, Either<ParseError, A>>
    parse($<GenParserT<T, ST, M>, A> p, ST st, String name, Stream<T> input) {
        return p.getTypeClass().runParser(p, st, name, input);
    }
}
