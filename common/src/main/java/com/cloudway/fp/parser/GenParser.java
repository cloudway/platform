/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.parser;

import com.cloudway.fp.$;
import com.cloudway.fp.control.Trampoline;
import com.cloudway.fp.data.Either;

/**
 * A generalized parser that accept token and user state type as parameters.
 *
 * @param <T> the token type
 * @param <ST> the user state type
 */
public class GenParser<T, ST> extends GenParserTC<GenParser<T, ST>, T, ST, Trampoline.µ> {
    /**
     * Construct a new parser.
     */
    protected GenParser() {
        super(Trampoline.tclass);
    }

    private static final GenParser<?,?> _TCLASS = new GenParser<>();

    /**
     * Returns the single instance of the {@code GenParser}.
     */
    @SuppressWarnings("unchecked")
    public static <T, ST> GenParser<T, ST> get() {
        return (GenParser<T, ST>)_TCLASS;
    }

    /**
     * Run the parser with the given input stream.
     */
    public static <T, ST, A> Either<ParseError, A>
    parse($<GenParser<T, ST>, A> p, ST st, String name, Stream<T> input) {
        return Trampoline.run(p.getTypeClass().runParser(p, st, name, input));
    }
}
