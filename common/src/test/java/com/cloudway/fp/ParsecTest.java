/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp;

import org.junit.Test;
import static org.junit.Assert.*;

import com.cloudway.fp.data.Fn;
import com.cloudway.fp.data.Maybe;
import com.cloudway.fp.data.Seq;
import com.cloudway.fp.data.Tuple;
import com.cloudway.fp.data.Unit;
import com.cloudway.fp.parser.CharParser;
import com.cloudway.fp.parser.GenTokenParser;
import com.cloudway.fp.parser.Languages;
import com.cloudway.fp.parser.Stream;
import com.cloudway.fp.parser.TokenParser;

import static com.cloudway.fp.control.Syntax.alternative;
import static com.cloudway.fp.control.Syntax.do_;

// @formatter:off

public class ParsecTest {
    private static final CharParser<Unit> pt = CharParser.get();

    @Test
    public void charParserTest() {
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
               do_(pt.label("quote at end of phrase", pt.chr('"')),
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
               do_(pt.label(word, significantSpaces()))));
    }

    static $<CharParser<Unit>, Unit> significantSpaces() {
        return do_(pt.space(), pt.spaces());
    }

    @Test
    public void tokenParserTest() {
        TokenParser<Unit> pt = new TokenParser<>(Languages.Empty);

        $<TokenParser<Unit>, String> phrase =
            alternative(pt.stringLiteral(), pt.identifier());

        $<TokenParser<Unit>, String> goal =
            do_(pt.reserved("send"),
            do_(phrase, message ->
            do_(pt.reserved("to"),
            do_(phrase, someone ->
            do_(pt.pure(String.format("Hello, %s! %s.", someone, message)))))));

        String input = "send \"welcome to earth\" to martin";
        String res = TokenParser.run(goal, Unit.U, "(unknown)", input).getOrThrow(Fn.id());
        assertEquals("Hello, martin! welcome to earth.", res);
    }

    @Test
    public void genTokenParserTest() {
        Stream<Seq<Character>, Character> stream =
            cs-> cs.isEmpty()
                 ? Maybe.empty()
                 : Maybe.of(Tuple.of(cs.head(), cs.tail()));

        GenTokenParser<Seq<Character>, Unit> pt = new GenTokenParser<>(stream, Languages.Empty);

        $<GenTokenParser<Seq<Character>, Unit>, String> phrase =
            alternative(pt.stringLiteral(), pt.identifier());

        $<GenTokenParser<Seq<Character>, Unit>, String> goal =
            do_(pt.reserved("send"),
            do_(phrase, message ->
            do_(pt.reserved("to"),
            do_(phrase, someone ->
            do_(pt.pure(String.format("Hello, %s! %s.", someone, message)))))));

        String input = "send \"welcome to earth\" to martin";
        String res = GenTokenParser.run(goal, Unit.U, "(unknown)", Seq.wrap(input)).getOrThrow(Fn.id());
        assertEquals("Hello, martin! welcome to earth.", res);
    }
}
