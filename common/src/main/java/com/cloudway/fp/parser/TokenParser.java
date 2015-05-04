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
 * A standalone lexical parser.
 */
public class TokenParser extends TokenParserTC<TokenParser, Unit, Trampoline.µ> {
    /**
     * Construct a lexical parser.
     *
     * @param language the language definition
     */
    public TokenParser(LanguageDef language) {
        super(language, Trampoline.tclass);
    }

    /**
     * Run the lexical parser with given stream input.
     */
    public static <A> Either<ParseError, A>
    parse($<TokenParser, A> p, String name, Stream<Character> input) {
        return Trampoline.run(p.getTypeClass().runParser(p, Unit.U, name, input));
    }

    /**
     * Run the lexical parser with the given string input.
     */
    public static <A>Either<ParseError, A>
    parse($<TokenParser, A> p, String name, String input) {
        return parse(p, name, Stream.of(input));
    }
}
