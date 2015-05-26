/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.parser;

import java.util.function.Predicate;

import com.cloudway.fp.$;
import com.cloudway.fp.control.Monad;
import com.cloudway.fp.data.Seq;
import com.cloudway.fp.data.Unit;

/**
 * A {@link CharParserT} typeclass definition.
 *
 * @param <P> the parser monad typeclass
 * @param <U> the user state type
 * @param <M> the inner monad typeclass
 */
public abstract class CharParserTC<P, U, M extends Monad<M>>
    extends ParserT<P, Character, U, M>
{
    protected CharParserTC(M nm) {
        super(nm);
    }

    /**
     * The parser {@code satisfy} succeeds for any character for which the
     * supplied predicate returns {@code true}. Returns the character that is
     * actually parsed.
     */
    public $<P, Character> satisfy(Predicate<Character> p) {
        return tokenPrim(p, String::valueOf);
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
        return label("white space", skipMany(space()));
    }

    /**
     * Parses a white space character (any character which satisfies
     * {@code Character.isSpace}). Returns the parsed character.
     */
    public $<P, Character> space() {
        return label("space", satisfy(Character::isWhitespace));
    }

    /**
     * Parses a newline character ('\n'). Returns a newline character.
     */
    public $<P, Character> newline() {
        return label("lf new-line", chr('\n'));
    }

    /**
     * Parses a carriage return character ('\r') followed by a newline character
     * ('\n'). Returns a newline character.
     */
    public $<P, Character> crlf() {
        return label("crlf newline", seqR(chr('\r'), chr('\n')));
    }

    /**
     * Parses a CRLF (see {@link #crlf}) or LF (see {@link #newline}) end-of-line.
     * Returns a newline character ('\n').
     */
    public $<P, Character> endOfLine() {
        return label("new-line", mplus(newline(), crlf()));
    }

    /**
     * Parses a tab character ('\t'). Returns a tab character.
     */
    public $<P, Character> tab() {
        return label("tab", chr('\t'));
    }

    /**
     * Parses an upper case letter (a character between 'A' and 'Z').
     * Returns the parsed character.
     */
    public $<P, Character> upper() {
        return label("uppercase letter", satisfy(Character::isUpperCase));
    }

    /**
     * Parses a lower case character (a character between 'a' and 'z').
     * Returns the parsed character.
     */
    public $<P, Character> lower() {
        return label("lowercase letter", satisfy(Character::isLowerCase));
    }

    /**
     * Parses a letter or digit (a character between '0' and '9').
     * Returns the parsed character.
     */
    public $<P, Character> alphaNum() {
        return label("letter or digit", satisfy(Character::isLetterOrDigit));
    }

    /**
     * Parses a letter (an upper case or lower case character). Returns the
     * parsed character.
     */
    public $<P, Character> letter() {
        return label("letter", satisfy(Character::isLetter));
    }

    /**
     * Parses a digit. Returns the parsed character.
     */
    public $<P, Character> digit() {
        return label("digit", satisfy(Character::isDigit));
    }

    /**
     * Parses a hexadecimal digit (a digit or letter between 'a' and 'f'
     * or 'A' and 'F'). Returns the parsed character.
     */
    public $<P, Character> hexDigit() {
        return label("hexadecimal digit",
            satisfy(cc -> {
                char c = cc;
                return c >= '0' && c <= '9'
                    || c >= 'a' && c <= 'f'
                    || c >= 'A' && c <= 'F';
            }));
    }

    /**
     * Parses a octal digit (a character between '0' and '7'). Returns
     * the parsed character.
     */
    public $<P, Character> octDigit() {
        return label("octal digit", satisfy(c -> c >= '0' && c <= '7'));
    }

    /**
     * Parses a single character. Returns the parsed character.
     */
    public $<P, Character> chr(char c) {
        return label(String.valueOf(c), satisfy(x -> x == c));
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

    public $<P, String> someChar($<P, Character> p) {
        return map(some(p), CharParserTC::join);
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
     * Parses a sequence of characters.
     */
    public $<P, String> str(String s) {
        return seqR(tokens(Seq.wrap(s), cs -> cs.show("", "", "")), pure(s));
    }
}
