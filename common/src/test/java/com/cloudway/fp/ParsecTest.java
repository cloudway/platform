/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp;

import java.util.function.BinaryOperator;
import java.util.function.UnaryOperator;

import org.junit.Test;
import static org.junit.Assert.*;

import com.cloudway.fp.data.Fn;
import com.cloudway.fp.data.Seq;
import com.cloudway.fp.data.Unit;
import com.cloudway.fp.parser.CharParser;
import com.cloudway.fp.parser.Expr;
import com.cloudway.fp.parser.Expr.Assoc;
import com.cloudway.fp.parser.Expr.Operator;
import com.cloudway.fp.parser.Languages;
import com.cloudway.fp.parser.Stream;
import com.cloudway.fp.parser.TokenParser;

import static com.cloudway.fp.control.Syntax.choice;
import static com.cloudway.fp.control.Syntax.do_;
import static com.cloudway.fp.parser.Expr.buildExpressionParser;

// @formatter:off

public class ParsecTest {
    static final class CharParserTest extends CharParser {
        public void test() {
            $<CharParser, String> goal =
                do_(reserved("send"),
                do_(phrase(), message ->
                do_(reserved("to"),
                do_(phrase(), someone ->
                do_(pure(String.format("Hello, %s! %s.", someone, message)))))));

            String input = "send \"welcome to earth\" to martin";
            String res = parse(goal, "(unknown)", input).getOrThrow(Fn.id());
            assertEquals("Hello, martin! welcome to earth.", res);

            res = parse(goal, "(unknown)", Stream.of(Seq.wrap(input))).getOrThrow(Fn.id());
            assertEquals("Hello, martin! welcome to earth.", res);
        }

        private $<CharParser, String> phrase() {
            return do_(spaces(),
                   do_(choice(quotedPhrase(), manyChar(noneOf(" "))), content ->
                   do_(spaces(),
                   do_(pure(content)))));
        }

        private $<CharParser, String> quotedPhrase() {
            return do_(chr('"'),
                   do_(manyChar(quotedChar()), content ->
                   do_(label("quote at end of phrase", chr('"')),
                   do_(pure(content)))));
        }

        private $<CharParser, Character> quotedChar() {
            return choice(
                noneOf("\""),
                try_(do_(str("\"\""), pure('"')))
            );
        }

        private $<CharParser, Unit> reserved(String word) {
            return do_(spaces(),
                   do_(str(word),
                   do_(label(word, significantSpaces()))));
        }

        private $<CharParser, Unit> significantSpaces() {
            return do_(space(), spaces());
        }
    }

    @Test
    public void charParserTest() {
        new CharParserTest().test();
    }

    @Test
    public void tokenParserTest() {
        TokenParser pt = new TokenParser(Languages.Empty);

        $<TokenParser, String> phrase =
            choice(pt.stringLiteral(), pt.identifier());

        $<TokenParser, String> goal =
            do_(pt.reserved("send"),
            do_(phrase, message ->
            do_(pt.reserved("to"),
            do_(phrase, someone ->
            do_(pt.pure(String.format("Hello, %s! %s.", someone, message)))))));

        String input = "send \"welcome to earth\" to martin";
        String res = TokenParser.parse(goal, "(unknown)", input).getOrThrow(Fn.id());
        assertEquals("Hello, martin! welcome to earth.", res);

        res = TokenParser.parse(goal, "(unknown)", Stream.of(Seq.wrap(input))).getOrThrow(Fn.id());
        assertEquals("Hello, martin! welcome to earth.", res);
    }

    static final class ExprTest extends TokenParser {
        private final Seq<Seq<Operator<TokenParser, Integer>>> table;
        private final $<TokenParser, Integer> goal;

        public ExprTest() {
            super(Languages.Java);

            this.table = Seq.of(
                Seq.of(prefix("-", x -> -x), prefix("+", x -> x)),
                Seq.of(postfix("++", x -> x + 1)),
                Seq.of(binary("*", (x, y) -> x * y, Assoc.Left),
                       binary("/", (x, y) -> x / y, Assoc.Left)),
                Seq.of(binary("+", (x, y) -> x + y, Assoc.Left),
                       binary("-", (x, y) -> x - y, Assoc.Left)));

            this.goal = do_(whiteSpace(),
                        do_(expr(), x ->
                        do_(eof(),
                        do_(pure(x)))));
        }

        private $<TokenParser, Integer> expr() {
            return label("expression",
                buildExpressionParser(this, table, delay(this::term)));
        }

        private $<TokenParser, Integer> term() {
            return label("term", choice(
                natural(),
                parens(delay(this::expr))));
        }

        private Operator<TokenParser, Integer>
        binary(String name, BinaryOperator<Integer> fun, Assoc assoc) {
            return Expr.infix(name, do_(reservedOp(name), pure(fun)), assoc);
        }

        private Operator<TokenParser, Integer>
        prefix(String name, UnaryOperator<Integer> fun) {
            return Expr.prefix(name, do_(reservedOp(name), pure(fun)));
        }

        private Operator<TokenParser, Integer>
        postfix(String name, UnaryOperator<Integer> fun) {
            return Expr.postfix(name, do_(reservedOp(name), pure(fun)));
        }

        public int eval(String expression) {
            return parse(goal, "", expression).getOrThrow(Fn.id());
        }
    }

    @Test
    public void exprTest() {
        ExprTest test = new ExprTest();
        assertEquals(7, test.eval("1+2*3"));
        assertEquals(9, test.eval("(1+2)*/*comment*/3"));
        assertEquals(-1, test.eval("1+-2"));
        assertEquals(2, test.eval("1++"));
        assertEquals(-1, test.eval("-2++"));
        assertEquals(4, test.eval("1+++2"));
    }
}
