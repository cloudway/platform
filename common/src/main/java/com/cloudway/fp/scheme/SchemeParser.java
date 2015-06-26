/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.scheme;

import java.io.Reader;
import java.lang.reflect.Array;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.function.BiFunction;
import java.util.function.Supplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.cloudway.fp.data.Either;
import com.cloudway.fp.data.Fn;
import com.cloudway.fp.data.HashPMap;
import com.cloudway.fp.data.Maybe;
import com.cloudway.fp.data.MutablePMap;
import com.cloudway.fp.data.PMap;
import com.cloudway.fp.data.Rational;
import com.cloudway.fp.data.Seq;
import com.cloudway.fp.parser.LexBuilder;
import com.cloudway.fp.parser.Lexer;
import com.cloudway.fp.parser.Scanner;

import static com.cloudway.fp.scheme.SchemeParser.Token.*;
import com.cloudway.fp.scheme.numsys.Complex;
import com.cloudway.fp.scheme.numsys.Num;

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
        HASH("#hash("),
        BOX("#&"),
        CLASS("#!"),

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

        .macro("dec",       "{d}+(/{d}+)?")
        .macro("bin",       "{b}+(/{b}+)?")
        .macro("oct",       "{o}+(/{o}+)?")
        .macro("hex",       "{x}+(/{x}+)?")
        .macro("float",     "({d}+|{d}+\\.{d}*|{d}*\\.{d}+)([eE][-+]?{d}+)?")

        .rule("#[tT]",      Fn.pure(Bool.TRUE))
        .rule("#[fF]",      Fn.pure(Bool.FALSE))

        .rule("([-+]?{dec})?[-+]{dec}?i",   s -> parseComplex(s, 10))
        .rule("#b([-+]?{bin})?[-+]{bin}?i", s -> parseComplex(s.substring(2), 2))
        .rule("#o([-+]?{oct})?[-+]{oct}?i", s -> parseComplex(s.substring(2), 8))
        .rule("#d([-+]?{dec})?[-+]{dec}?i", s -> parseComplex(s.substring(2), 10))
        .rule("#x([-+]?{hex})?[-+]{hex}?i", s -> parseComplex(s.substring(2), 16))

        .rule("[-+]?{dec}",   s -> parseNum(s, 10))
        .rule("#b[-+]?{bin}", s -> parseNum(s.substring(2), 2))
        .rule("#o[-+]?{oct}", s -> parseNum(s.substring(2), 8))
        .rule("#d[-+]?{dec}", s -> parseNum(s.substring(2), 10))
        .rule("#x[-+]?{hex}", s -> parseNum(s.substring(2), 16))

        .action("#[bodx]{c}*", s -> { s.fail("invalid number format");
                                      return Maybe.empty();
                                    })

        .rule("[-+]?{float}", s -> Num.make(Double.parseDouble(s)))
        .rule("([-+]?{float})?[-+]{float}?i", SchemeParser::parseFloatComplex)

        .literal("+inf.0", Num.make(Double.POSITIVE_INFINITY))
        .literal("-inf.0", Num.make(Double.NEGATIVE_INFINITY))
        .literal("+nan.0", Num.make(Double.NaN))
        .literal("-nan.0", Num.make(Double.NaN))

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
        .literal("#hash(", HASH)
        .literal("#&", BOX)

        .rule("#![a-zA-Z0-9_$.]+(\\[\\])*(/[a-zA-Z0-9_$]+)*", Fn.pure(CLASS))

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

    private static final Pattern INTEGER_PATTERN
        = Pattern.compile("[-+]?\\p{Alnum}+(/\\p{Alnum}+)?");

    private static final Pattern COMPLEX_PATTERN
        = Pattern.compile("([-+]?\\p{Alnum}+(/\\p{Alnum}+)?)?[-+](\\p{Alnum}+(/\\p{Alnum}+)?)?i");

    private static final String FLOAT_PATTERN_STRING =
        "([-+]?(\\d+|\\d+\\.\\d*|\\d*\\.\\d+)([eE][-+]?\\d+)?)";

    private static final Pattern FLOAT_PATTERN
        = Pattern.compile(FLOAT_PATTERN_STRING);

    private static final Pattern FLOAT_COMPLEX_PATTERN
        = Pattern.compile(FLOAT_PATTERN_STRING + "?([-+]" + FLOAT_PATTERN_STRING + "?)i");

    public static Num parseNumber(String input, int radix) {
        if ("+inf.0".equals(input))
            return Num.make(Double.POSITIVE_INFINITY);
        if ("-inf.0".equals(input))
            return Num.make(Double.NEGATIVE_INFINITY);
        if ("+nan.0".equals(input) || "-nan.0".equals(input))
            return Num.make(Double.NaN);

        if (COMPLEX_PATTERN.matcher(input).matches())
            return parseComplex(input, radix);

        if (FLOAT_COMPLEX_PATTERN.matcher(input).matches())
            return parseFloatComplex(input);

        if (INTEGER_PATTERN.matcher(input).matches())
            return parseNum(input, radix);

        if (FLOAT_PATTERN.matcher(input).matches())
            return Num.make(Double.parseDouble(input));

        throw new NumberFormatException("For input string: " + input);
    }

    private static Num parseComplex(String input, int radix) {
        int i = input.lastIndexOf('+');
        if (i == -1)
            i = input.lastIndexOf('-');

        String real_part = input.substring(0, i);
        String imag_part = input.substring(i, input.length()-1);

        Num real, imag;

        if (real_part.isEmpty()) {
            real = Num.ZERO;
        } else {
            real = parseNum(real_part, radix);
        }

        if ("+".equals(imag_part)) {
            imag = Num.make(1);
        } else if ("-".equals(imag_part)) {
            imag = Num.make(-1);
        } else {
            imag = parseNum(imag_part, radix);
        }

        return new Complex(real, imag).lower();
    }

    private static Num parseFloatComplex(String input) {
        Matcher m = FLOAT_COMPLEX_PATTERN.matcher(input);
        if (!m.matches())
            throw new NumberFormatException("For input string: " + input);

        // notice the group index
        String real_part = m.group(1);
        String imag_part = m.group(4);

        double real, imag;

        if (real_part == null) {
            real = 0;
        } else {
            real = Double.parseDouble(real_part);
        }

        if ("+".equals(imag_part)) {
            imag = 1;
        } else if ("-".equals(imag_part)) {
            imag = -1;
        } else {
            imag = Double.parseDouble(imag_part);
        }

        // noinspection FloatingPointEquality
        return imag == 0 ? Num.make(real) : new Complex(real, imag);
    }

    private static Num parseNum(String input, int radix) {
        if (input.indexOf('/') != -1) {
            return Num.make(Rational.valueOf(input, radix)).lower();
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
            int d = digitToInt(input.charAt(i));
            if (d < 0 || d >= radix)
                throw new NumberFormatException("For input string: \"" + input + "\"");
            overflow = (value*radix)/radix != value;
            value = value * radix + d;
            overflow |= (value - 1 < -1);
        }

        if (overflow) {
            return Num.make(new BigInteger(input, radix));
        }

        value = sign * value;
        if ((int)value == value) {
            return Num.make((int)value);
        } else {
            return Num.make(value);
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

        loop: while ((c = s.input()) != -1 && c != '"') {
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

    private final class Parser {
        private final String filename;
        private final Scanner<LispVal> scanner;
        private LispVal token;

        private boolean foldcase = false;

        // Internal state used to read a single expression from input stream.
        // If this value is set to 0, and an expression has been read, more
        // tokens should be kept in stream for next read.
        private int nested = 0;

        Parser(String filename, Scanner<LispVal> scanner) {
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
                    nested++;
                    pexp.get();
                    nested--;
                } else if (token == DIRECTIVE) {
                    process_directive(scanner.text().substring(2));
                    advance();
                } else {
                    return;
                }
            }
        }

        private void more(Supplier<LispVal> pexp) {
            if (nested > 0) {
                scan(pexp);
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
                more(pexp);
            } else {
                expect(t.show());
            }
        }

        Seq<Either<LispError, LispVal>> p_top_level() {
            try {
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

        private LispVal p_expression() {
            return p_term(this::p_expression);
        }

        private LispVal p_term(Supplier<LispVal> pexp) {
            if (token instanceof Token) {
                switch ((Token)token) {
                case SYMBOL:
                    return p_symbol(pexp);

                case CLASS:
                    return p_class(pexp);

                case QUOTE:
                case QUASIQUOTE:
                case UNQUOTE:
                case UNQUOTE_SPLICING:
                    return p_abbreviation(pexp);

                case VECTOR:
                    scan(pexp);
                    return p_vector(pexp);

                case HASH:
                    scan(pexp);
                    return p_hashtable(pexp);

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
                    scan(this::p_neoteric);
                    return p_curly_infix_list();

                default:
                    return expect("expression");
                }
            } else {
                LispVal val = token;
                more(pexp);
                return val;
            }
        }

        private LispVal p_symbol(Supplier<LispVal> pexp) {
            String name = scanner.text();
            more(pexp);

            boolean keyword = name.endsWith(":") && !":".equals(name);
            if (keyword ) {
                name = name.substring(0, name.length() - 1);
            }

            if (name.indexOf('|') != -1) {
                int len = name.length();
                StringBuilder buf = new StringBuilder(len);
                boolean q = false;

                for (int i = 0; i < len; i++) {
                    char c = name.charAt(i);
                    if (c == '|') {
                        q = !q;
                    } else if (q || !foldcase) {
                        buf.append(c);
                    } else {
                        buf.append(Character.toLowerCase(c));
                    }
                }
                name = buf.toString();
            } else {
                if (foldcase) {
                    name = name.toLowerCase();
                }
            }

            return keyword ? getkeysym(name) : getsym(name);
        }

        private LispVal p_class(Supplier<LispVal> pexp) {
            String name = scanner.text().substring(2);
            more(pexp);

            String fields = null;
            int sep = name.indexOf('/');
            if (sep != -1) {
                fields = name.substring(sep + 1);
                name = name.substring(0, sep);
            }

            try {
                LispVal val = new JClass(loadClass(name));
                if (fields != null)
                    val = p_fields(val, fields);
                return val;
            } catch (Exception ex) {
                return error(name + ": class not found");
            }
        }

        private LispVal p_fields(LispVal val, String fields) {
            for (int i = 0, j; i < fields.length(); i = j + 1) {
                if ((j = fields.indexOf('/', i)) < 0)
                    j = fields.length();
                String field = fields.substring(i, j);
                val = Pair.cons(getsym("get-field"), Pair.list(val, getkeysym(field)));
            }
            return val;
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
            return Pair.list(getsym(tag), pexp.get());
        }

        private LispVal p_vector(Supplier<LispVal> pexp) {
            ArrayList<LispVal> vec = new ArrayList<>();

            nested++;
            while (token != RP) {
                vec.add(pexp.get());
            }

            nested--;
            expect(RP, pexp);
            return new Vec(vec.toArray(new LispVal[vec.size()]));
        }

        private LispVal p_hashtable(Supplier<LispVal> pexp) {
            LinkedHashMap<LispVal, LispVal> map = new LinkedHashMap<>();

            nested++;
            while (token != RP) {
                if (token == LP || token == SLP)
                    scan(pexp);
                else
                    expect("(");

                LispVal key = pexp.get();
                expect(DOT, pexp);
                LispVal val = pexp.get();
                expect(RP, pexp);
                map.put(key, val);
            }
            nested--;

            expect(RP, pexp);
            return new JObject(map);
        }

        private LispVal p_box(Supplier<LispVal> pexp) {
            return new Box(pexp.get());
        }

        private LispVal p_list(Supplier<LispVal> pexp, Token delim) {
            Seq<LispVal> hd = Seq.nil();
            LispVal tl = LispVal.Nil;

            nested++;
            while (token != delim && token != DOT) {
                hd = Seq.cons(pexp.get(), hd);
            }

            if (!hd.isEmpty() && scan(DOT, pexp)) {
                tl = pexp.get();
            }

            nested--;
            expect(delim, pexp);
            return make_reverse_list(hd, tl);
        }

        private LispVal make_reverse_list(Seq<LispVal> hd, LispVal tl) {
            LispVal res = tl;
            while (!hd.isEmpty()) {
                res = Pair.cons(hd.head(), res);
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

            nested++;
            while (token != delim && token != DOT) {
                hd = Seq.cons(pexp.get(), hd);
            }

            if (scan(DOT, pexp)) {
                tl = pexp.get();
            }

            nested--;
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
                return Pair.cons(lhs, rhs);

            case LC:
                scan(pexp);
                rhs = p_neoteric_list(RC, pexp, this::transform_infix_list);
                return rhs.isNil() ? Pair.list(lhs) : Pair.list(lhs, rhs);

            case LB:
                scan(pexp);
                rhs = p_neoteric_list(RB, pexp, this::make_reverse_list);
                return Pair.cons(getsym("$bracket-apply$"), Pair.cons(lhs, rhs));

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

            return Pair.cons(getsym("$nfx$"), make_reverse_list(hd, tl));
        }

        private LispVal transform_infix(Seq<LispVal> exps) {
            LispVal operator = null;
            LispVal operands = Pair.list(exps.head());

            for (Seq<LispVal> p = exps.tail(); !p.isEmpty(); p = p.tail().tail()) {
                LispVal op = p.head();
                if (operator == null) {
                    operator = op;
                } else if (!Primitives.equal(op, operator)) {
                    operator = null;
                    break;
                }
                operands = Pair.cons(p.tail().head(), operands);
            }

            if (operator != null) {
                return Pair.cons(operator, operands);
            }

            return Pair.cons(getsym("$nfx$"), make_reverse_list(exps, Nil));
        }
    }

    static Class<?> loadClass(String name) throws ClassNotFoundException {
        Class<?> type;

        int dim = 0;
        while (name.endsWith("[]")) {
            dim++;
            name = name.substring(0, name.length()-2);
        }

        type = getPrimitiveClass(name);
        if (type == null) {
            if (name.indexOf('.') == -1)
                name = "java.lang." + name;
            type = Class.forName(name);
        }

        if (dim == 0) {
            return type;
        } else {
            return Array.newInstance(type, new int[dim]).getClass();
        }
    }

    private static final String[] PRIMITIVE_NAMES = {
        "boolean", "byte", "char", "double", "float", "int", "long", "short", "void"
    };

    private static final Class<?>[] PRIMITIVE_TYPES = {
        Boolean.TYPE, Byte.TYPE, Character.TYPE, Double.TYPE, Float.TYPE,
        Integer.TYPE, Long.TYPE, Short.TYPE, Void.TYPE
    };

    private static Class<?> getPrimitiveClass(String name) {
        int i = Arrays.binarySearch(PRIMITIVE_NAMES, name);
        return (i >= 0) ? PRIMITIVE_TYPES[i] : null;
    }
}
