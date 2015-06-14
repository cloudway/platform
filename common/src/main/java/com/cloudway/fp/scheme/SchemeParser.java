/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.scheme;

import java.io.Reader;
import java.math.BigInteger;
import java.util.function.BiFunction;
import java.util.function.Supplier;

import com.cloudway.fp.data.Either;
import com.cloudway.fp.data.Fn;
import com.cloudway.fp.data.HashPMap;
import com.cloudway.fp.data.Maybe;
import com.cloudway.fp.data.MutablePMap;
import com.cloudway.fp.data.PMap;
import com.cloudway.fp.data.Rational;
import com.cloudway.fp.data.Seq;
import com.cloudway.fp.data.Vector;
import com.cloudway.fp.parser.LexBuilder;
import com.cloudway.fp.parser.Lexer;
import com.cloudway.fp.parser.Scanner;

import static com.cloudway.fp.scheme.SchemeParser.Token.*;

// @formatter:off

public class SchemeParser {
    // Symbol table

    private final MutablePMap<String, Symbol> symtab
        = new MutablePMap<>(HashPMap.empty());
    private final MutablePMap<String, KeySym> keytab
        = new MutablePMap<>(HashPMap.empty());

    public Symbol getsym(String name) {
        return symtab.compute(name, (k, v) -> v == null ? new Symbol(name) : v);
    }

    public KeySym getkeysym(String name) {
        return keytab.compute(name, (k, v) -> v == null ? new KeySym(name) : v);
    }

    // Lexical Analyzer

    enum Token implements LispVal {
        EOI("end of input"),

        SYMBOL("symbol"),

        LP("("), RP(")"),
        LB("["), RB("]"),
        LC("{"), RC("}"),

        SLP(" ("),
        SLB(" ["),
        SLC(" {"),

        DOT("."),
        VECTOR("#("),
        BOX("#&"),

        QUOTE("'"),
        QUASIQUOTE("`"),
        UNQUOTE(","),
        UNQUOTE_SPLICING(",@"),

        DIRECTIVE("#!"),
        COMMENT("#;");

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

    private static final Lexer<LispVal> lexer = new LexBuilder<LispVal>()
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

        .rule("#\\\\.",        s -> new Char(s.charAt(2)))
        .rule("#\\\\x{x}+",    s -> parseCharCode(s.substring(3)))
        .action("#\\\\[a-z]+", SchemeParser::parseCharConst)

        .action("\"", SchemeParser::parseString)

        // These rules are used in parsing curly-infix-expressions. In some
        // lexical construct the whitespace characters are significant,
        // e.g. 'sin(x)' is different from 'sin (x)'
        .rule("\\s\\(", Fn.pure(SLP))
        .rule("\\s\\[", Fn.pure(SLB))
        .rule("\\s\\{", Fn.pure(SLC))

        .literal("(",  LP)
        .literal(")",  RP)
        .literal("[",  LB)
        .literal("]",  RB)
        .literal("{",  LC)
        .literal("}",  RC)

        .literal(".",  DOT)
        .literal("#(", VECTOR)
        .literal("#&", BOX)

        .literal("'",  QUOTE)
        .literal("`",  QUASIQUOTE)
        .literal(",",  UNQUOTE)
        .literal(",@", UNQUOTE_SPLICING)

        .rule("({c}*|\\|[^|]*\\|)+", Fn.pure(SYMBOL))

        .literal("#;",  COMMENT)
        .rule("#!{c}+", Fn.pure(DIRECTIVE))

        .ignore("\\s+")
        .ignore(";.*")
        .action("#\\|", SchemeParser::parseComment)

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
            return Maybe.of(new CText(buf.toString()));
        }
    }

    private static final PMap<String, LispVal> CHAR_CONSTS = HashPMap.<String, LispVal>empty()
        .put("nul",         new Char('\0'))
        .put("alarm",       new Char('\u0007'))
        .put("backspace",   new Char('\u0008'))
        .put("tab",         new Char('\t'))
        .put("linefeed",    new Char('\n'))
        .put("newline",     new Char('\n'))
        .put("vtab",        new Char('\u000b'))
        .put("page",        new Char('\u000c'))
        .put("return",      new Char('\r'))
        .put("esc",         new Char('\u001b'))
        .put("space",       new Char(' '))
        .put("delete",      new Char('\u007f'));

    private static Maybe<LispVal> parseCharConst(LexBuilder.InputBuffer s) {
        String name = s.lexeme().substring(2);
        Maybe<LispVal> c = CHAR_CONSTS.lookup(name);
        if (c.isAbsent())
            s.fail("illegal character constant " + name);
        return c;
    }

    private static LispVal parseCharCode(String text) {
        return new Char((char)Integer.parseInt(text, 16)); // FIXME range check
    }

    private static Maybe<LispVal> parseComment(LexBuilder.InputBuffer s) {
        int nested = 1;
        int state  = 'x';
        int c;

        while ((c = s.input()) != -1) {
            if (c == '|') {
                if (state == '#') {
                    nested++;
                    state = 'x';
                } else {
                    state = '|';
                }
            } else if (c == '#') {
                if (state == '|') {
                    if (--nested == 0)
                        return Maybe.empty();
                    state = 'x';
                } else {
                    state = '#';
                }
            } else {
                state = 'x';
            }
        }

        s.fail("end of file in comment");
        return Maybe.empty();
    }

    // Syntax Parser

    public Seq<Either<LispError, LispVal>> parse(String name, Reader input) {
        Parser parser = new Parser(name, lexer.getScanner(input));
        return parser.p_top_level();
    }

    public Seq<Either<LispError, LispVal>> parse(String input) {
        Parser parser = new Parser("", lexer.getScanner(input));
        return parser.p_top_level();
    }

    public Either<LispError, LispVal> parseExpr(Reader input) {
        try {
            Parser parser = new Parser("", lexer.getScanner(input));
            return Either.right(parser.p_expression());
        } catch (LispError ex) {
            return Either.left(ex);
        } catch (Exception ex) {
            return Either.left(new LispError(ex));
        }
    }

    private final class Parser {
        private final String filename;
        private final Scanner<LispVal> scanner;
        private LispVal token;

        private boolean foldcase = false;

        public Parser(String filename, Scanner<LispVal> scanner) {
            this.filename = filename;
            this.scanner = scanner;
        }

        private void advance() {
            if (scanner.hasNext()) {
                token = scanner.next();
            } else {
                token = EOI;
            }
        }

        private <A> A error(String message) {
            throw new LispError.Parser(filename, scanner.line(), scanner.column(), message);
        }

        private <A> A expect(String name) {
            return error("unexpected " + token.show() + ", expecting " + name);
        }

        private void scan(Supplier<LispVal> pexp) {
            advance();

            while (true) {
                if (token == COMMENT) {
                    advance();
                    pexp.get();
                } else if (token == DIRECTIVE) {
                    process_directive(scanner.text().substring(2));
                    advance();
                } else {
                    return;
                }
            }
        }

        private void process_directive(String directive) {
            switch (directive) {
            case "fold-case":
                foldcase = true;
                break;

            case "no-fold-case":
                foldcase = false;
                break;

            default:
                // ignore unknown directive
            }
        }

        private boolean scan(Token t, Supplier<LispVal> pexp) {
            if (token == t) {
                scan(pexp);
                return true;
            } else {
                return false;
            }
        }

        private void expect(Token t, Supplier<LispVal> pexp) {
            if (token == t) {
                scan(pexp);
            } else {
                expect(t.show());
            }
        }

        public Seq<Either<LispError, LispVal>> p_top_level() {
            try {
                if (token == null)
                    scan(this::p_expression);

                if (token == EOI)
                    return Seq.nil();

                return Seq.cons(Either.right(p_expression()), this::p_top_level);
            } catch (LispError ex) {
                return Seq.of(Either.left(ex));
            } catch (Exception ex) {
                return Seq.of(Either.left(new LispError(ex)));
            }
        }

        public LispVal p_expression() {
            return p_term(this::p_expression);
        }

        private LispVal p_term(Supplier<LispVal> pexp) {
            if (token instanceof Token) {
                switch ((Token)token) {
                case SYMBOL:
                    return p_symbol(pexp);

                case QUOTE:
                case QUASIQUOTE:
                case UNQUOTE:
                case UNQUOTE_SPLICING:
                    return p_abbreviation(pexp);

                case VECTOR:
                    scan(pexp);
                    return p_vector(pexp);

                case BOX:
                    scan(pexp);
                    return p_box(pexp);

                case LP: case SLP:
                    scan(pexp);
                    return p_list(pexp, RP);

                case LB: case SLB:
                    scan(pexp);
                    return p_list(pexp, RB);

                case LC: case SLC:
                    scan(pexp);
                    return p_curly_infix_list();

                default:
                    return expect("expression");
                }
            } else {
                LispVal val = token;
                scan(pexp);
                return val;
            }
        }

        private LispVal p_symbol(Supplier<LispVal> pexp) {
            String text = scanner.text();

            boolean keyword = text.endsWith(":") && !":".equals(text);
            if (keyword ) {
                text = text.substring(0, text.length() - 1);
            }

            if (text.indexOf('|') != -1) {
                int len = text.length();
                StringBuilder buf = new StringBuilder(len);
                boolean q = false;

                for (int i = 0; i < len; i++) {
                    char c = text.charAt(i);
                    if (c == '|') {
                        q = !q;
                    } else if (q || !foldcase) {
                        buf.append(c);
                    } else {
                        buf.append(Character.toLowerCase(c));
                    }
                }
                text = buf.toString();
            } else {
                if (foldcase) {
                    text = text.toLowerCase();
                }
            }

            scan(pexp);
            return keyword ? getkeysym(text) : getsym(text);
        }

        private LispVal p_abbreviation(Supplier<LispVal> pexp) {
            String tag;

            switch ((Token)token) {
            case QUOTE:
                tag = "quote";
                break;

            case QUASIQUOTE:
                tag = "quasiquote";
                break;

            case UNQUOTE:
                tag = "unquote";
                break;

            case UNQUOTE_SPLICING:
                tag = "unquote-splicing";
                break;

            default:
                throw new Error();
            }

            scan(pexp);
            return Pair.of(getsym(tag), pexp.get());
        }

        private LispVal p_vector(Supplier<LispVal> pexp) {
            Vector<LispVal> vec = Vector.empty();
            while (!scan(RP, pexp)) {
                vec = vec.snoc(pexp.get());
            }
            return new Vec(vec);
        }

        private LispVal p_box(Supplier<LispVal> pexp) {
            return new Box(pexp.get());
        }

        private LispVal p_list(Supplier<LispVal> pexp, Token delim) {
            Seq<LispVal> hd = Seq.nil();
            LispVal tl = LispVal.Nil;

            while (token != delim && token != DOT) {
                hd = Seq.cons(pexp.get(), hd);
            }

            if (!hd.isEmpty() && scan(DOT, pexp)) {
                tl = pexp.get();
            }

            expect(delim, pexp);
            return make_reverse_list(hd, tl);
        }

        private LispVal make_reverse_list(Seq<LispVal> hd, LispVal tl) {
            LispVal res = tl;
            while (!hd.isEmpty()) {
                res = new Pair(hd.head(), res);
                hd = hd.tail();
            }
            return res;
        }

        private LispVal p_curly_infix_list() {
            return p_neoteric_list(RC, this::p_neoteric, this::transform_infix_list);
        }

        private LispVal
        p_neoteric_list(Token delim, Supplier<LispVal> pexp, BiFunction<Seq<LispVal>, LispVal, LispVal> cont) {
            Seq<LispVal> hd = Seq.nil();
            LispVal tl = LispVal.Nil;

            while (token != delim && token != DOT) {
                hd = Seq.cons(pexp.get(), hd);
            }

            if (scan(DOT, pexp)) {
                tl = pexp.get();
            }

            expect(delim, pexp);
            return cont.apply(hd, tl);
        }

        private LispVal p_neoteric() {
            LispVal term = p_term(this::p_neoteric);
            while (token == LP || token == LB || token == LC) {
                term = p_neoteric_term(term, this::p_neoteric);
            }
            return term;
        }

        private LispVal p_neoteric_term(LispVal lhs, Supplier<LispVal> pexp) {
            LispVal rhs;

            switch ((Token)token) {
            case LP:
                scan(pexp);
                rhs = p_neoteric_list(RP, pexp, this::make_reverse_list);
                return new Pair(lhs, rhs);

            case LC:
                scan(pexp);
                rhs = p_neoteric_list(RC, pexp, this::transform_infix_list);
                return rhs.isNil() ? Pair.of(lhs) : Pair.of(lhs, rhs);

            case LB:
                scan(pexp);
                rhs = p_neoteric_list(RB, pexp, this::make_reverse_list);
                return new Pair(getsym("$bracket-apply$"), new Pair(lhs, rhs));

            default:
                return lhs;
            }
        }

        private LispVal transform_infix_list(Seq<LispVal> hd, LispVal tl) {
            if (hd.isEmpty())
                return tl;

            if (tl.isNil()) {
                int len = hd.length();
                if (len == 0)
                    return Nil;
                if (len == 1)
                    return hd.head();
                if (len == 2)
                    return make_reverse_list(hd, Nil);
                if (len % 2 != 0)
                    return transform_infix(hd);
            }

            return new Pair(getsym("$nfx$"), make_reverse_list(hd, tl));
        }

        private LispVal transform_infix(Seq<LispVal> exps) {
            LispVal operator = null;
            LispVal operands = Pair.of(exps.head());

            for (Seq<LispVal> p = exps.tail(); !p.isEmpty(); p = p.tail().tail()) {
                LispVal op = p.head();
                if (operator == null) {
                    operator = op;
                } else if (!Primitives.equal(op, operator)) {
                    operator = null;
                    break;
                }
                operands = new Pair(p.tail().head(), operands);
            }

            if (operator != null) {
                return new Pair(operator, operands);
            }

            return new Pair(getsym("$nfx$"), make_reverse_list(exps, Nil));
        }
    }
}
