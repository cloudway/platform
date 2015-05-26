/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp;

import java.util.Objects;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.function.UnaryOperator;

import org.junit.Test;
import static org.junit.Assert.*;

import com.cloudway.fp.control.ConditionCase;
import com.cloudway.fp.data.Fn;
import com.cloudway.fp.data.Maybe;
import com.cloudway.fp.data.Seq;
import com.cloudway.fp.data.Unit;
import com.cloudway.fp.parser.CharParser;
import com.cloudway.fp.parser.Expr;
import com.cloudway.fp.parser.Expr.Assoc;
import com.cloudway.fp.parser.Expr.Operator;
import com.cloudway.fp.parser.GenParser;
import com.cloudway.fp.parser.Languages;
import com.cloudway.fp.parser.LexBuilder;
import com.cloudway.fp.parser.Lexer;
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
                attempt(do_(str("\"\""), pure('"')))
            );
        }

        private $<CharParser, Unit> reserved(String word) {
            return do_(spaces(), str(word), label(word, significantSpaces()));
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
            return label("expression", buildExpressionParser(this, table, delay(this::term)));
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

    static final class LexerTest extends GenParser<LexerTest.Token, Unit> {
        interface Token {}

        enum Symbol implements Token {
            ADD("+"),
            SUB("-"),
            MUL("*"),
            DIV("/"),
            INC("++"),
            LP("("),
            RP(")");

            private final String name;

            Symbol(String name) {
                this.name = name;
            }

            public String toString() {
                return name;
            }
        }

        static class Const implements Token {
            final int value;

            public Const(String s) {
                this.value = Integer.parseInt(s);
            }

            public int getValue() {
                return value;
            }

            public String toString() {
                return String.valueOf(value);
            }
        }

        static <R> ConditionCase<Token, R, RuntimeException>
        Const(Function<Integer, R> mapper) {
            return t -> t instanceof Const
                ? () -> mapper.apply(((Const)t).getValue())
                : null;
        }

        static Lexer<Token> lexer = new LexBuilder<Token>()
            .action("/\\*", LexerTest::comment) // comment block
            .ignore("//.*")                     // single line comments
            .rule("\\d+",  Const::new)          // number literal
            .literal("+",  Symbol.ADD)          // symbols...
            .literal("-",  Symbol.SUB)
            .literal("*",  Symbol.MUL)
            .literal("/",  Symbol.DIV)
            .literal("++", Symbol.INC)
            .literal("(",  Symbol.LP)
            .literal(")",  Symbol.RP)
            .ignore("\\s+")                     // whitespaces
            .ignore(".")                        // discard bad characters
            .build();

        private static Maybe<Token> comment(LexBuilder.InputBuffer s) {
            int c;
            while ((c = s.input()) != -1) {
                if (c == '*') {
                    while ((c = s.input()) == '*')
                        ;
                    if (c == '/' || c == -1)
                        break;
                }
            }
            return Maybe.empty();
        }

        private final Seq<Seq<Operator<GenParser<Token, Unit>, Integer>>> table;
        private final $<GenParser<Token, Unit>, Integer> goal;

        public LexerTest() {
            this.table = Seq.of(
                Seq.of(prefix(Symbol.SUB, x -> -x), prefix(Symbol.ADD, x -> x)),
                Seq.of(postfix(Symbol.INC, x -> x + 1)),
                Seq.of(binary(Symbol.MUL, (x, y) -> x * y, Assoc.Left),
                       binary(Symbol.DIV, (x, y) -> x / y, Assoc.Left)),
                Seq.of(binary(Symbol.ADD, (x, y) -> x + y, Assoc.Left)),
                Seq.of(binary(Symbol.SUB, (x, y) -> x - y, Assoc.Left))
            );

            this.goal = do_(expr(), x -> do_(eof(), do_(pure(x))));
        }

        public int eval(String expression) {
            return parse(goal, Unit.U, "", lexer.getTokenStream(expression)).getOrThrow(Fn.id());
        }

        private $<GenParser<Token, Unit>, Integer> expr() {
            return label("expression", buildExpressionParser(this, table, delay(this::term)));
        }

        private $<GenParser<Token, Unit>, Integer> term() {
            return label("term", choice(
                number(),
                between(token(Symbol.LP), token(Symbol.RP), delay(this::expr))
            ));
        }

        public $<GenParser<Token, Unit>, Integer> number() {
            return token(Const(Fn.id()));
        }

        private Operator<GenParser<Token, Unit>, Integer>
        binary(Symbol symbol, BinaryOperator<Integer> fun, Assoc assoc) {
            return Expr.infix(symbol.name(), do_(token(symbol), pure(fun)), assoc);
        }

        private Operator<GenParser<Token, Unit>, Integer>
        prefix(Symbol symbol, UnaryOperator<Integer> fun) {
            return Expr.prefix(symbol.name(), do_(token(symbol), pure(fun)));
        }

        private Operator<GenParser<Token, Unit>, Integer>
        postfix(Symbol symbol, UnaryOperator<Integer> fun) {
            return Expr.postfix(symbol.name(), do_(token(symbol), pure(fun)));
        }
    }

    @Test
    public void lexerTest() {
        LexerTest test = new LexerTest();
        assertEquals(7, test.eval("1+2*3"));
        assertEquals(9, test.eval("(1+2)*/*comment*/3"));
        assertEquals(-1, test.eval("1+-2"));
        assertEquals(2, test.eval("1++"));
        assertEquals(-1, test.eval("-2++"));
        assertEquals(4, test.eval("1+++2"));
        assertEquals(2, test.eval("4/2"));
    }

    /* Complex lexer test */
    private static final Lexer<String> URI_LEXER = new LexBuilder<String>()
        .macro("uri_reference",   "{absolute_uri}|{relative_uri}")
        .macro("absolute_uri",    "{scheme}:({hier_part}|{opaque_part})(#{fragment})?")
        .macro("relative_uri",    "({net_path}|{abs_path}|{rel_path})(\\?{query})?(#{fragment})?")

        .macro("hier_part",       "({net_path}|{abs_path})(\\?{query})?")
        .macro("opaque_part",     "{uri_no_slash}{uric}*")

        .macro("uri_no_slash",    "{unreserved}|{escaped}|[;?:@&=+$,]")

        .macro("net_path",        "//{authority}{abs_path}?")
        .macro("abs_path",        "/{path_segments}")
        .macro("rel_path",        "{rel_segment}{abs_path}?")

        .macro("rel_segment",     "({unreserved}|{escaped}|[;@&=+$,])+")

        .macro("scheme",          "{alpha}({alpha}|{digit}|[+\\-.])*")

        .macro("authority",       "{server}|{reg_name}")

        .macro("reg_name",        "({unreserved}|{escaped}|[$,;:@&=+])+")

        .macro("server",          "(({userinfo}@)?{hostport})?")
        .macro("userinfo",        "({unreserved}|{escaped}|[;:&=+$,])*")

        .macro("hostport",        "{host}(:{port})?")
        .macro("host",            "{hostname}|{IPV4address}")
        .macro("hostname",        "({domainlabel}\\.)*{toplabel}\\.?")
        .macro("domainlabel",     "{alphanum}|{alphanum}({alphanum}|-)*{alphanum}")
        .macro("toplabel",        "{alpha}|{alpha}({alphanum}|-)*{alphanum}")
        .macro("IPV4address",     "{digit}+\\.{digit}+\\.{digit}+\\.{digit}+")
        .macro("port",            "{digit}*")

        .macro("path",            "({abs_path}|{opaque_part})?")
        .macro("path_segments",   "{segment}(/{segment})*")
        .macro("segment",         "{pchar}*(;{param})*")
        .macro("param",           "{pchar}*")
        .macro("pchar",           "{unreserved}|{escaped}|[:@&=+$,]")

        .macro("query",           "{uric}*")
        .macro("fragment",        "{uric}*")

        .macro("uric",            "{reserved}|{unreserved}|{escaped}")
        .macro("reserved",        "[;/?:@&=+$,]")
        .macro("unreserved",      "{alphanum}|{mark}")
        .macro("mark",            "[\\-_.!~*\']")
        .macro("escaped",         "%{hex}{hex}")
        .macro("hex",             "[0-9a-fA-F]")
        .macro("alphanum",        "[a-zA-Z0-9]")
        .macro("alpha",           "[a-zA-Z]")
        .macro("digit",           "[0-9]")

        .rule("{uri_reference}",  Fn.id())

        .build();

    private static void testURI(String uri) {
        Seq<String> toks = URI_LEXER.getTokenStream(uri).asList();
        assertEquals(uri, toks.head());
        assertTrue(toks.tail().isEmpty());
    }

    @Test
    public void complexLexerTest() {
        testURI("ftp://ftp.is.co.za/rfc/rfc1808.txt");
        testURI("gopher://spinaltap.micro.umn.edu/00/Weather/California/Los%20Angeles");
        testURI("http://www.math.uio.no/faq/compression-faq/part1.html");
        testURI("mailto:mduerst@ifi.unizh.ch");
        testURI("news:comp.infosystems.www.servers.unix");
        testURI("telnet://melvyl.ucop.edu/");
    }

    // A full featured CSV Parser from "Real World Haskell"
    public static class CSVParser extends CharParser {
        public Seq<Seq<String>> parse(String input) {
            return parse(csvFile(), "(unknown)", input).getOrThrow(Fn.id());
        }

        public $<CharParser, Seq<Seq<String>>> csvFile() {
            return endBy(line(), eol());
        }

        public $<CharParser, Seq<String>> line() {
            return sepBy(cell(), chr(','));
        }

        public $<CharParser, String> cell() {
            return choice(quotedCell(), manyChar(noneOf(",\n\r")));
        }

        public $<CharParser, String> quotedCell() {
            return do_(chr('"'),
                   do_(manyChar(quotedChar()), content ->
                   do_(label("quote at end of cell", chr('"')),
                   do_(pure(content)))));
        }

        public $<CharParser, Character> quotedChar() {
            return choice(
                noneOf("\""),
                attempt(seqR(str("\"\""), pure('"')))
            );
        }

        public $<CharParser, String> eol() {
            return label("end of line", choice(
                attempt(str("\n\r")),
                attempt(str("\r\n")),
                str("\n"),
                str("\r")
            ));
        }
    }

    private static final String CSV_INPUT =
        "\"Product\",\"Price\"\n" +
        "\"O'Reilly Socks\",10\n" +
        "\"Shirt with \"\"Haskell\"\" text\",20\n" +
        "\"Shirt, \"\"O'Reilly\"\" version\",20\n" +
        "\"Haskell Caps\",15\n";

    private static final Seq<Seq<String>> CSV_OUTPUT = Seq.of(
        Seq.of("Product", "Price"),
        Seq.of("O'Reilly Socks", "10"),
        Seq.of("Shirt with \"Haskell\" text", "20"),
        Seq.of("Shirt, \"O'Reilly\" version", "20"),
        Seq.of("Haskell Caps", "15")
    );

    @Test
    public void csvTest() {
        CSVParser parser = new CSVParser();
        Seq<Seq<String>> result;

        result = parser.parse(CSV_INPUT);
        assertTrue(csvEquals(CSV_OUTPUT, result));
    }

    private static boolean csvEquals(Seq<Seq<String>> a, Seq<Seq<String>> b) {
        return a.count() == b.count() && all(Seq.zip(a, b, ParsecTest::cellsEquals));
    }

    private static boolean cellsEquals(Seq<String> xs, Seq<String> ys) {
        return xs.count() == ys.count() && all(Seq.zip(xs, ys, Objects::equals));
    }

    private static boolean all(Seq<Boolean> xs) {
        return xs.foldLeft(true, (Boolean x, Boolean y) -> x && y);
    }
}
