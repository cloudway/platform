/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.parser;

import com.cloudway.fp.$;
import com.cloudway.fp.control.Trampoline;
import com.cloudway.fp.data.Either;
import com.cloudway.fp.data.Unit;

/**
 * A standalone character parser.
 */
public class CharParser extends CharParserTC<CharParser, Unit, Trampoline.µ> {
    protected CharParser() {
        super(Trampoline.tclass);
    }

    private static final CharParser _TCLASS = new CharParser();

    /**
     * Returns the single instance of the {@code CharParser}.
     */
    public static CharParser get() {
        return _TCLASS;
    }

    /**
     * Run the {@code CharParser} with the given stream input.
     */
    public static <A> Either<ParseError, A>
    parse($<CharParser, A> p, String name, Stream<Character> input) {
        return Trampoline.run(p.getTypeClass().runParser(p, Unit.U, name, input));
    }

    /**
     * Run the {@code CharParser} with the given string input.
     */
    public static <A> Either<ParseError, A>
    parse($<CharParser, A> p, String name, String input) {
        return parse(p, name, Stream.of(input));
    }
}
