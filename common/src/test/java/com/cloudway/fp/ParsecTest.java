/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp;

import org.junit.Test;
import static org.junit.Assert.*;

import com.cloudway.fp.data.Fn;
import com.cloudway.fp.data.Unit;
import com.cloudway.fp.parser.CharParser;

import static com.cloudway.fp.control.Syntax.alternative;
import static com.cloudway.fp.control.Syntax.do_;

// @formatter:off

public class ParsecTest {
    private static final CharParser<Unit> pt = CharParser.get();

    @Test
    public void messageTest() {
        $<CharParser<Unit>, String> goal =
            do_(reserved("send"),
            do_(phrase(), message ->
            do_(reserved("to"),
            do_(phrase(), someone ->
            do_(pt.pure(String.format("Hello, %s! %s.", someone, message)))))));

        String input = "send \"welcome to earth\" to martin";
        String res = CharParser.run(goal, Unit.U, "(unknown)", input).getOrThrow(Fn.id());
        assertEquals("Hello, martin! welcome to earth.", res);
    }

    static $<CharParser<Unit>, String> phrase() {
        return do_(pt.spaces(),
               do_(alternative(quotedPhrase(), pt.manyChar(pt.noneOf(" "))), content ->
               do_(pt.spaces(),
               do_(pt.pure(content)))));
    }

    static $<CharParser<Unit>, String> quotedPhrase() {
        return do_(pt.chr('"'),
               do_(pt.manyChar(quotedChar()), content ->
               do_(pt.label(pt.chr('"'), "quote at end of phrase"),
               do_(pt.pure(content)))));
    }

    static $<CharParser<Unit>, Character> quotedChar() {
        return alternative(
            pt.noneOf("\""),
            pt.try_(do_(pt.str("\"\""), pt.pure('"')))
        );
    }

    static $<CharParser<Unit>, Unit> reserved(String word) {
        return do_(pt.spaces(),
               do_(pt.str(word),
               do_(pt.label(significantSpaces(), word))));
    }

    static $<CharParser<Unit>, Unit> significantSpaces() {
        return do_(pt.space(), pt.spaces());
    }
}
