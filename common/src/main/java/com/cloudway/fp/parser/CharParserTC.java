/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.parser;

import java.util.function.BiFunction;
import java.util.function.Predicate;

import com.cloudway.fp.$;
import com.cloudway.fp.control.Monad;
import com.cloudway.fp.control.Trampoline;
import com.cloudway.fp.data.Fn;
import com.cloudway.fp.data.Maybe;
import com.cloudway.fp.data.Seq;
import com.cloudway.fp.data.Tuple;
import com.cloudway.fp.data.Unit;

import static com.cloudway.fp.control.Trampoline.suspend;

/**
 * A {@link CharParserT} typeclass definition.
 *
 * @param <P> the parser monad typeclass
 * @param <ST> the user state type
 * @param <M> the inner monad typeclass
 */
public abstract class CharParserTC<P, ST, M extends Monad<M>>
    extends ParsecT<P, String, Character, ST, M>
{
    protected CharParserTC(M nm) {
        super(nm);
    }

    // The string input stream
    private enum CharStream implements Stream<String, Character> {
        INSTANCE;

        @Override
        public Maybe<Tuple<Character, String>> uncons(String s) {
            return s.isEmpty()
                   ? Maybe.empty()
                   : Maybe.of(Tuple.of(s.charAt(0), s.substring(1)));
        }
    }

    @Override
    protected Stream<String, Character> stream() {
        return CharStream.INSTANCE;
    }

    /**
     * The parser {@code satisfy} succeeds for any character for which the
     * supplied predicate returns {@code true}. Returns the character that is
     * actually parsed.
     */
    public $<P, Character> satisfy(Predicate<Character> p) {
        return tokenPrim(String::valueOf,
                         (pos, c, cs) -> pos.updatePosChar(c),
                         (c) -> p.test(c) ? Maybe.of(c) : Maybe.empty());
    }

    /**
     * Succeeds if the current character is in the supplied list of characters.
     * Returns the parsed character.
     */
    public $<P, Character> oneOf(String cs) {
        return satisfy(c -> cs.indexOf(c) != -1);
    }

    /**
     * As the dual of {@link #oneOf}, {@code noneOf} succeeds if the current
     * character not in the supplied list of characters. Returns the parsed
     * character.
     */
    public $<P, Character> noneOf(String cs) {
        return satisfy(c -> cs.indexOf(c) == -1);
    }

    /**
     * Skips zero or more white space characters.
     */
    public $<P, Unit> spaces() {
        return label(skipMany(space()), "white space");
    }

    /**
     * Parses a white space character (any character which satisfies
     * {@code Character.isSpace}). Returns the parsed character.
     */
    public $<P, Character> space() {
        return label(satisfy(Character::isWhitespace), "space");
    }

    /**
     * Parses a newline character ('\n'). Returns a newline character.
     */
    public $<P, Character> newline() {
        return label(chr('\n'), "lf new-line");
    }

    /**
     * Parses a carriage return character ('\r') followed by a newline character
     * ('\n'). Returns a newline character.
     */
    public $<P, Character> crlf() {
        return label(seqR(chr('\r'), chr('\n')), "crlf newline");
    }

    /**
     * Parses a CRLF (see {@link #crlf}) or LF (see {@link #newline}) end-of-line.
     * Returns a newline character ('\n').
     */
    public $<P, Character> endOfLine() {
        return label(mplus(newline(), crlf()), "new-line");
    }

    /**
     * Parses a tab character ('\t'). Returns a tab character.
     */
    public $<P, Character> tab() {
        return label(chr('\t'), "tab");
    }

    /**
     * Parses an upper case letter (a character between 'A' and 'Z').
     * Returns the parsed character.
     */
    public $<P, Character> upper() {
        return label(satisfy(Character::isUpperCase), "uppercase letter");
    }

    /**
     * Parses a lower case character (a character between 'a' and 'z').
     * Returns the parsed character.
     */
    public $<P, Character> lower() {
        return label(satisfy(Character::isLowerCase), "lowercase letter");
    }

    /**
     * Parses a letter or digit (a character between '0' and '9').
     * Returns the parsed character.
     */
    public $<P, Character> alphaNum() {
        return label(satisfy(Character::isLetterOrDigit), "letter or digit");
    }

    /**
     * Parses a letter (an upper case or lower case character). Returns the
     * parsed character.
     */
    public $<P, Character> letter() {
        return label(satisfy(Character::isLetter), "letter");
    }

    /**
     * Parses a digit. Returns the parsed character.
     */
    public $<P, Character> digit() {
        return label(satisfy(Character::isDigit), "digit");
    }

    /**
     * Parses a hexadecimal digit (a digit or letter between 'a' and 'f'
     * or 'A' and 'F'). Returns the parsed character.
     */
    public $<P, Character> hexDigit() {
        return label(satisfy(cc -> {
            char c = cc;
            return c >= '0' && c <= '9'
                || c >= 'a' && c <= 'f'
                || c >= 'A' && c <= 'F';
        }), "hexadecimal digit");
    }

    /**
     * Parses a octal digit (a character between '0' and '7'). Returns
     * the parsed character.
     */
    public $<P, Character> octDigit() {
        return label(satisfy(c -> c >= '0' && c <= '7'), "octal digit");
    }

    /**
     * Parses a single character. Returns the parsed character.
     */
    public $<P, Character> chr(char c) {
        return label(satisfy(x -> x == c), String.valueOf(c));
    }

    /**
     * This parser succeeds for any character. Returns the parsed character.
     */
    public $<P, Character> anyChar() {
        return satisfy(c -> true);
    }

    public $<P, String> manyChar($<P, Character> p) {
        return map(many(p), CharParserTC::join);
    }

    public $<P, String> manyChar1($<P, Character> p) {
        return map(many1(p), CharParserTC::join);
    }

    private static String join(Seq<Character> cs) {
        StringBuilder buf = new StringBuilder();
        while (!cs.isEmpty()) {
            buf.append(cs.head());
            cs = cs.tail();
        }
        return buf.toString();
    }

    /**
     * Parses a sequence of characters. Returns the parsed string.
     */
    public $<P, String> str(String cs) {
        if (cs.isEmpty()) {
            return $((s, cok, cerr, eok, eerr) -> suspend(() -> eok.apply("", s, unknownError(s))));
        } else {
            return $((s, cok, cerr, eok, eerr) -> s.as((input, pos, u) -> {
                BiFunction<String, String, Trampoline<$<M, Object>>> walk = Fn.fix((rec, toks, rs) -> {
                    if (toks.isEmpty()) {
                        SourcePos pos_ = pos.updatePosString(cs);
                        State<String, ST> s_ = new State<>(rs, pos_, u);
                        return suspend(() -> cok.apply(cs, s_, new ParseError(pos_)));
                    } else {
                        return suspend(() -> stream().uncons(rs).map(t -> t.as((x, xs) ->
                            toks.charAt(0) == x
                                ? rec.apply(toks.substring(1), xs)
                                : cerr.apply(errExpect(pos, x, cs))
                        )).orElseGet(() -> cerr.apply(errEof(pos, cs))));
                    }
                });

                return suspend(() -> stream().uncons(input).map(t -> t.as((x, xs) ->
                    cs.charAt(0) == x
                        ? walk.apply(cs.substring(1), xs)
                        : cerr.apply(errExpect(pos, x, cs))
                    )).orElseGet(() -> cerr.apply(errEof(pos, cs))));
            }));
        }
    }

    private static ParseError errEof(SourcePos pos, String cs) {
        return new ParseError(pos, new Message.SysUnExpect(""))
                   .setMessage(new Message.Expect(cs));
    }

    private static ParseError errExpect(SourcePos pos, char x, String cs) {
        return new ParseError(pos, new Message.SysUnExpect(String.valueOf(x)))
                   .setMessage(new Message.Expect(cs));
    }
}
