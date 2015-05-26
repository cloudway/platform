/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.scheme;

import java.io.Reader;
import java.math.BigInteger;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Predicate;

import com.cloudway.fp.$;
import com.cloudway.fp.control.Trampoline;
import com.cloudway.fp.data.Either;
import com.cloudway.fp.data.Fn;
import com.cloudway.fp.data.HashPMap;
import com.cloudway.fp.data.Maybe;
import com.cloudway.fp.data.MutablePMap;
import com.cloudway.fp.data.Rational;
import com.cloudway.fp.data.Seq;
import com.cloudway.fp.data.Unit;
import com.cloudway.fp.data.Vector;
import com.cloudway.fp.parser.GenParserTC;
import com.cloudway.fp.parser.LexBuilder;
import com.cloudway.fp.parser.Lexer;
import com.cloudway.fp.parser.ParseError;
import com.cloudway.fp.parser.Stream;

import static com.cloudway.fp.control.Conditionals.with;
import static com.cloudway.fp.control.Syntax.do_;
import static com.cloudway.fp.scheme.LispVal.Void.VOID;
import static com.cloudway.fp.scheme.SchemeParser.Token.*;

// @formatter:off

public class SchemeParser extends GenParserTC<SchemeParser, LispVal, Unit, Trampoline.µ> {
    public SchemeParser() {
        super(Trampoline.tclass);
    }

    // Symbol table

    private final MutablePMap<String, Atom> symtab = new MutablePMap<>(HashPMap.empty());
    private final AtomicInteger tcounter = new AtomicInteger();

    public Atom getsym(String name) {
        return symtab.compute(name, (k, v) -> v == null ? new Atom(name) : v);
    }

    public Atom newsym() {
        return new Atom(" t." + tcounter.incrementAndGet());
    }

    // Lexical Analyzer

    enum Token implements LispVal {
        LP("("),
        RP(")"),
        LB("["),
        RB("]"),
        DOT("."),
        VECTOR("#("),
        QUOTE("'"),
        QUASIQUOTE("`"),
        UNQUOTE(","),
        UNQUOTE_SPLICING(",@");

        private final String name;

        Token(String name) {
            this.name = name;
        }

        @Override
        public String show() {
            return name;
        }

        public String toString() {
            return name;
        }
    }

    private final Lexer<LispVal> lexer = new LexBuilder<LispVal>()
        .macro("symbol",    "[!$%&*+\\-/:.<=>?@^_~]")
        .macro("letter",    "[a-zA-Z]")
        .macro("digit",     "[0-9]")
        .macro("c",         "{letter}|{symbol}|{digit}")
        .macro("b",         "[01]")
        .macro("o",         "[0-7]")
        .macro("d",         "[0-9]")
        .macro("x",         "[0-9a-fA-F]")

        .rule("#[tT]",      Fn.pure(Bool.TRUE))
        .rule("#[fF]",      Fn.pure(Bool.FALSE))

        .rule("[-+]?{d}+(/{d}+)?",   s -> new Num(parseNum(s, 10)))
        .rule("#b[-+]?{b}+(/{b}+)?", s -> new Num(parseNum(s.substring(2), 2)))
        .rule("#o[-+]?{o}+(/{o}+)?", s -> new Num(parseNum(s.substring(2), 8)))
        .rule("#d[-+]?{d}+(/{d}+)?", s -> new Num(parseNum(s.substring(2), 10)))
        .rule("#x[-+]?{x}+(/{x}+)?", s -> new Num(parseNum(s.substring(2), 16)))

        .action("#[bodx]{c}*", s -> { s.fail("invalid number format");
                                      return Maybe.empty();
                                    })

        .rule("[-+]?({d}+|{d}+\\.{d}*|{d}*\\.{d}+)([eE][-+]?{d}+)?",
            s -> new Num(Double.parseDouble(s)))

        .action("\"", SchemeParser::parseString)

        .literal("(",  LP)
        .literal(")",  RP)
        .literal("[",  LB)
        .literal("]",  RB)
        .literal(".",  DOT)
        .literal("#(", VECTOR)
        .literal("'",  QUOTE)
        .literal("`",  QUASIQUOTE)
        .literal(",",  UNQUOTE)
        .literal(",@", UNQUOTE_SPLICING)

        .rule("{c}+",       s -> getsym(s.toLowerCase()))
        .rule("\\|{c}*\\|", s -> getsym(s.substring(1, s.length() - 1)))

        .ignore("\\s+")
        .ignore(";.*")

        .build();

    private static Number parseNum(String input, int radix) {
        if (input.indexOf('/') != -1) {
            return Rational.valueOf(input, radix);
        }

        int     len      = input.length();
        long    value    = 0;
        boolean overflow = false;
        int     sign     = 1;
        char    c;
        int     i;

        i = 0;
        if ((c = input.charAt(i)) == '+' || c == '-') {
            sign = c == '-' ? -1 : 1;
            i++;
        }

        for (; i < len && !overflow; i++) {
            overflow = (value*radix)/radix != value;
            value = value * radix + digitToInt(input.charAt(i));
            overflow |= (value - 1 < -1);
        }

        if (overflow) {
            return new BigInteger(input, radix);
        } else {
            return sign * value;
        }
    }

    private static int digitToInt(char c) {
        return c >= 'a' ? c - 'a' + 10:
               c >= 'A' ? c - 'A' + 10
                        : c - '0';
    }

    private static Maybe<LispVal> parseString(LexBuilder.InputBuffer s) {
        StringBuilder buf = new StringBuilder();
        int c;

        loop: while ((c = s.input()) != -1 && c != '\n' && c != '"') {
            if (c == '\\') {
                switch (c = s.input()) {
                case 'a':   c = '\u0007'; break;
                case 'b':   c = '\b';     break;
                case 't':   c = '\t';     break;
                case 'n':   c = '\n';     break;
                case 'v':   c = '\u000b'; break;
                case 'f':   c = '\f';     break;
                case 'r':   c = '\r';     break;
                case '"':   c = '"';      break;
                case '\\':  c = '\\';     break;

                case -1:
                case '\n':
                    break loop;

                default:
                    s.fail("unknown escape sequence \\%c in string", c);
                    return Maybe.empty();
                }
            }
            buf.append((char)c);
        }

        if (c != '"') {
            s.fail("unterminated string literal");
            return Maybe.empty();
        } else {
            return Maybe.of(new Text(buf.toString()));
        }
    }

    public Stream<LispVal> getStream(String input) {
        return lexer.getTokenStream(input);
    }

    public Stream<LispVal> getStream(Reader input) {
        return lexer.getTokenStream(input);
    }

    // Syntax Parser

    private final $<SchemeParser, LispVal> goal = p_toplevel();

    public Either<ParseError, LispVal> parse(String name, Stream<LispVal> input) {
        return Trampoline.run(runParser(goal, Unit.U, name, input));
    }

    public Either<ParseError, LispVal> parseExpr(Stream<LispVal> input) {
        return Trampoline.run(runParser(p_expr(), Unit.U, "", input));
    }

    private $<SchemeParser, LispVal> p_toplevel() {
        return seqL(choice(p_sequence(), pure(VOID)), eof());
    }

    private $<SchemeParser, LispVal> p_sequence() {
        return map(some(p_expr()), this::make_sequence);
    }

    private LispVal make_sequence(Seq<LispVal> exps) {
        return exps.isEmpty()        ? VOID :
               exps.tail().isEmpty() ? exps.head()
                                     : list(getsym("begin"), exps);
    }

    private $<SchemeParser, LispVal> p_expr() {
        return label("expression", delay(() -> choice(
            p_simple_datum(),
            p_abbreviation(),
            p_list(),
            p_vector())));
    }

    private $<SchemeParser, LispVal> p_simple_datum() {
        return label("", choice(
            token(Atom.class),
            token(Text.class),
            token(Num.class),
            token(Bool.class)
        ));
    }

    private $<SchemeParser, LispVal> p_abbreviation() {
        return choice(
            do_(token(QUOTE),
            do_(p_expr(), datum -> pure(list(getsym("quote"), datum))))
            ,
            do_(token(QUASIQUOTE),
            do_(p_expr(), datum -> pure(list(getsym("quasiquote"), datum))))
            ,
            do_(token(UNQUOTE),
            do_(p_expr(), datum -> pure(list(getsym("unquote"), datum))))
            ,
            do_(token(UNQUOTE_SPLICING),
            do_(p_expr(), datum -> pure(list(getsym("unquote-splicing"), datum))))
        );
    }

    private $<SchemeParser, LispVal> p_list() {
        return parens(choice(
            bind(some(p_expr()), hd -> choice(dotted(hd), pure(new List(hd)))),
            pure(List.NIL)));
    }

    private $<SchemeParser, LispVal> dotted(Seq<LispVal> head) {
        return do_(token(DOT),
               do_(p_expr(), tail ->
               do_(pure(cons(head, tail)))));
    }

    private static LispVal cons(Seq<LispVal> xs, LispVal y) {
        return with(y).<LispVal>get()
            .when(LispVal.List(ys ->
                isUnquote(ys) ? new DottedList(xs, y) : new List(xs.append(ys))))
            .when(LispVal.DottedList((ys, yt) ->
                new DottedList(xs.append(ys), yt)))
            .orElseGet(() ->
                new DottedList(xs, y));
    }

    private static boolean isUnquote(Seq<LispVal> form) {
        if (!form.isEmpty() && form.head().isAtom()) {
            String tag = ((Atom)form.head()).name;
            return "unquote".equals(tag) || "unquote-splicing".equals(tag);
        } else {
            return false;
        }
    }

    private $<SchemeParser, LispVal> p_vector() {
        return between(
            token(VECTOR), token(RP),
            map(manyAccum(p_expr(), Vector.empty(), Vector::snoc), Vec::new)
        );
    }

    private <A> $<SchemeParser, A> parens($<SchemeParser, A> p) {
        return choice(between(token(LP), token(RP), p),
                      between(token(LB), token(RB), p));
    }

    @Override
    public $<SchemeParser, LispVal> satisfy(Predicate<? super LispVal> p) {
        return tokenPrim(p, LispVal::show);
    }

    private static List list(Atom id, LispVal arg) {
        return new List(Seq.of(id, arg));
    }

    private static List list(Atom id, Seq<LispVal> args) {
        return new List(Seq.cons(id, args));
    }
}
