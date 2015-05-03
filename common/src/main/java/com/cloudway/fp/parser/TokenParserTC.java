/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.parser;

import com.cloudway.fp.$;
import com.cloudway.fp.control.Monad;
import com.cloudway.fp.data.Either;
import com.cloudway.fp.data.Seq;
import com.cloudway.fp.data.Tuple;
import com.cloudway.fp.data.Unit;

// @formatter:off

/**
 * The {@link TokenParserT} typeclass definition.
 *
 * @param <P> the parser monad typeclass
 * @param <S> the stream type
 * @param <U> the user state type
 * @param <M> the inner monad
 */
public abstract class TokenParserTC<P, S, U, M extends Monad<M>> extends CharParserTC<P, S, U, M> {
    private final LanguageDef ldf;

    // cache
    private final $<P, Character> idStart;
    private final $<P, Character> idPart;
    private final $<P, Character> opStart;
    private final $<P, Character> opPart;

    /**
     * Construct a {@code TokenParserTC}.
     *
     * @param ldf the language definition used by this parser.
     * @param nm the inner monad typeclass
     */
    protected TokenParserTC(LanguageDef ldf, M nm) {
        super(nm);
        this.ldf = ldf;

        idStart = ldf.identStart(this);
        idPart  = ldf.identPart(this);
        opStart = ldf.opStart(this);
        opPart  = ldf.opPart(this);
    }

    /**
     * This lexeme parser parses a legal identifier. Returns the identifier
     * string. This parser will fail on identifiers that are reserved words.
     * Legal identifier (start) characters and reserved words are defined in
     * the {@link LanguageDef}. An identifier is treated as a single token
     * using {@code try}.
     */
    public $<P, String> identifier() {
        return lexeme(try_(bind(ident(), name ->
            ldf.reservedNames().contains(name)
                ? unexpected("reserved word " + name)
                : pure(name))));
    }

    private $<P, String> ident() {
        return label("identifier",
            bind(idStart, (Character c) ->
            bind(manyChar(idPart), (String cs) ->
            pure(c + cs))));

    }

    /**
     * The lexeme parser parses symbol name, but it also checks that the name
     * is not a prefix of a valid identifier. A reserved word is treated as a
     * single token using {@code try}.
     */
    public $<P, Unit> reserved(String name) {
        return lexeme(try_(
            seqR(caseString(name), label("end of " + name, notFollowedBy(idPart)))));
    }

    private $<P, String> caseString(String name) {
        if (ldf.caseSensitive()) {
            return str(name);
        } else {
            return seqR(Seq.wrap(name).foldRight(unit(), (c, cs) ->
                            seqR(label(name, caseChar(c)), cs)),
                        pure(name));
        }
    }

    private $<P, Character> caseChar(char c) {
        return Character.isLetter(c)
            ? mplus(chr(Character.toLowerCase(c)), chr(Character.toUpperCase(c)))
            : chr(c);
    }

    /**
     * The lexeme parser parses a legal operator. Returns the name of the
     * operator.  This parser will fail on any operators that are reserved
     * operators. Legal  operator (start) characters and reserved operators
     * are defined in the {@link LanguageDef}. An operator is treated as a
     * single token using {@code try}.
     */
    public $<P, String> operator() {
        return lexeme(try_(bind(oper(), name ->
            ldf.reservedOpNames().contains(name)
                ? unexpected("reserved operator " + name)
                : pure(name))));
    }

    private $<P, String> oper() {
        return label("operator",
            bind(opStart, (Character c) ->
            bind(manyChar(opPart), (String cs) ->
            pure(c + cs))));
    }

    /**
     * The lexeme parser parses symbol name, but it also checks that the name
     * is not a prefix of a valid operator. A {@code reservedOp} is treated as
     * a single token using {@code try}.
     */
    public $<P, Unit> reservedOp(String name) {
        return lexeme(try_(seqR(str(name), label("end of " + name, notFollowedBy(opPart)))));
    }

    /**
     * This lexeme parser parses a single literal character. Returns the literal
     * character value. This parser deals correctly with escape sequences. The
     * literal character is parsed according to the grammar rules defined in the
     * Haskell report (which matches most programming languages quite closely).
     */
    public $<P, Character> charLiteral() {
        return label("character",
            lexeme(between(chr('\''),
                           label("end of character", chr('\'')),
                           charChar())));
    }

    private $<P, Character> charChar() {
        return label("literal character", mplus(charLetter(), charEscape()));
    }

    private $<P, Character> charLetter() {
        return satisfy(c -> c != '\'' && c != '\\' && c > '\026');
    }

    private $<P, Character> charEscape() {
        return seqR(chr('\\'), escapeCode());
    }

    /**
     * This lexeme parser parses a literal string. Returns the literal string
     * value. This parser deals correctly with escape sequences and gaps. The
     * literal string is parsed according to the grammar rules defined in the
     * Haskell report (which matches most programming languages quite closely).
     */
    public $<P, String> stringLiteral() {
        return label("string literal",
            lexeme(between(chr('"'),
                           label("end of string", chr('"')),
                           manyChar(stringChar()))));
    }

    private $<P, Character> stringChar() {
        return label("string character", mplus(stringLetter(), stringEscape()));
    }

    private $<P, Character> stringLetter() {
        return satisfy(c -> c != '"' && c != '\\' && c > '\026');
    }

    private $<P, Character> stringEscape() {
        return seqR(chr('\\'), escapeCode());
    }

    private $<P, Character> escapeCode() {
        return label("escape code", mplus(charEsc(), charNum(), charCtrl()));
    }

    private $<P, Character> charNum() {
        return bind(mplus(decimal(),
                          seqR(chr('o'), number(8, octDigit())),
                          seqR(chr('x'), number(16, hexDigit()))), code ->
                    pure((char)(int)code));
    }

    private $<P, Character> charCtrl() {
        return seqR(chr('^'), bind(upper(), code -> pure((char)(code - 'A' + 1))));
    }

    private $<P, Character> charEsc() {
        return choice(escMap.map(t -> t.as((c, code) -> seqR(chr(c), pure(code)))));
    }

    private static final Seq<Tuple<Character, Character>> escMap
        = Seq.wrap("bfnrt\\\"\'").zip(Seq.wrap("\b\f\n\r\t\\\"\'"));

    /**
     * This lexeme parser parses a natural number (a positive whole number).
     * Returns the value of the number. The number can be specified in 'decimal',
     * 'hexadecimal' or 'octal'. The number is parsed according to the grammar
     * rules in the Haskell report.
     */
    public $<P, Integer> natural() {
        return label("natural", lexeme(mplus(zeroNumber(), decimal())));
    }

    /**
     * The lexeme parser parses an integer (a whole number). This parser is
     * like {@link #natural} except that is can be prefixed with sign (i.e.
     * '-' or '+'). Returns the value of the number. The number can be specified
     * in 'decimal', 'hexadecimal' or 'octal'. The number is parsed according
     * to the grammar rules in the Haskell report.
     */
    public $<P, Integer> integer() {
        return label("integer",
            lexeme(bind(lexeme(sign()), s ->
                   bind(mplus(zeroNumber(), decimal()), n ->
                   pure(s == '-' ? -n : n)))));
    }

    /**
     * Parses a positive whole number in the decimal system. Returns the value
     * of the number.
     */
    public $<P, Integer> decimal() {
        return number(10, digit());
    }

    /**
     * Parses a positive whole number in the hexadecimal system. The number
     * should be prefixed with "0x" or "0X". Returns the value of the number.
     */
    public $<P, Integer> hexadecimal() {
        return seqR(oneOf("xX"), number(16, hexDigit()));
    }

    /**
     * Parses a positive whole number in the octal system. The number should
     * be prefixed with "0o" or "0O". Returns the value of the number.
     */
    public $<P, Integer> octal() {
        return seqR(oneOf("oO"), number(8, octDigit()));
    }

    private $<P, Integer> number(int base, $<P, Character> baseDigit) {
        return bind(many1(baseDigit), digits ->
               pure(digits.foldLeft(0, (x, d) -> base * x + d - '0')));
    }

    private $<P, Integer> zeroNumber() {
        return label("", seqR(chr('0'), mplus(hexadecimal(), octal(), decimal(), pure(0))));
    }

    private $<P, Character> sign() {
        return mplus(oneOf("-+"), pure('+'));
    }

    /**
     * This lexeme parser parses a floating point value. Returns the value of
     * the number. The number is parsed according to the grammar rules defined
     * in the Haskell report.
     */
    public $<P, Double> floating() {
        return label("float", lexeme(bind(decimal(), this::fractExponent)));
    }

    /**
     * This lexeme parser parses either {@link #natural()} or a {@link #floating()}.
     * Returns the value of the number. This parser deals with any overlap in
     * the grammar rules for naturals and floats. The number is parsed according
     * to the grammar rules defined in the Haskell report.
     */
    public $<P, Either<Integer, Double>> naturalOrFloating() {
        return label("number", lexeme(natFloat()));
    }

    private $<P, Either<Integer, Double>> natFloat() {
        return mplus(bind(mplus(hexadecimal(), octal()), n -> pure(Either.left(n))),
                     decimalFloat(),
                     fractFloat(0),
                     pure(Either.left(0)));
    }

    private $<P, Either<Integer, Double>> decimalFloat() {
        return bind(decimal(), n -> option(Either.left(n), fractFloat(n)));
    }

    private $<P, Either<Integer, Double>> fractFloat(int n) {
        return bind(fractExponent(n), f -> pure(Either.right(f)));
    }

    private $<P, Double> fractExponent(int n) {
        return mplus(
            bind(fraction(), fract ->
            bind(option(1.0, exponent()), expo ->
            pure((n + fract) * expo))),

            bind(exponent(), expo ->
            pure(n * expo)));
    }

    private $<P, Double> fraction() {
        return label("fraction",
            seqR(chr('.'), bind(label("fraction", many1(digit())), digits ->
                           pure(digits.foldRight_(0.0, (d, f) -> (f + d - '0') / 10.0)))));
    }

    private $<P, Double> exponent() {
        return label("exponent",
            seqR(oneOf("eE"), bind(sign(), s ->
                              bind(label("exponent", decimal()), e ->
                              pure(power(s, e))))));
    }

    private static double power(char sign, double e) {
        return Math.pow(10, sign == '-' ? -e : e);
    }

    /**
     * Lexeme parser parses given string and skips trailing white space.
     */
    public $<P, String> symbol(String name) {
        return lexeme(str(name));
    }

    /**
     * First applies the given parser and then the {@link #whiteSpace} parser,
     * returning the value of the parser. Every lexical token (lexeme) is
     * defined using this method, this way every parse starts at a point without
     * white space. Parsers that use {@code lexeme} are called "lexeme parsers"
     * in this document.
     *
     * <p>The only point where the {@code whiteSpace} parser should be called
     * explicitly is the start of the main parser in order to skip any leading
     * white space.</p>
     */
    public <A> $<P, A> lexeme($<P, A> p) {
        return bind(p, x -> seqR(whiteSpace(), pure(x)));
    }

    /**
     * Parses any white space. White space consists of zero or more occurrences
     * of a 'space', a line comment or a block (multi line) comment. Block
     * comments may be nested. How comments are started and ended is defined in
     * the {@link LanguageDef language definition}.
     */
    public $<P, Unit> whiteSpace() {
        boolean noLine = ldf.commentLine().isEmpty();
        boolean noMulti = ldf.commentStart().isEmpty();
        if (noLine && noMulti) {
            return skipMany(label("", simpleSpace()));
        } else if (noLine) {
            return skipMany(label("", mplus(simpleSpace(), multiLineComment())));
        } else if (noMulti) {
            return skipMany(label("", mplus(simpleSpace(), oneLineComment())));
        } else {
            return skipMany(label("", mplus(simpleSpace(), oneLineComment(), multiLineComment())));
        }
    }

    private $<P, Unit> simpleSpace() {
        return skipMany1(satisfy(Character::isWhitespace));
    }

    private $<P, Unit> oneLineComment() {
        return seqR(try_(str(ldf.commentLine())),
               seqR(skipMany(satisfy(c -> c != '\n')),
               unit()));
    }

    private $<P, Unit> multiLineComment() {
        return seqR(try_(str(ldf.commentStart())), this::inComment);
    }

    private $<P, Unit> inComment() {
        return ldf.nestedComments() ?  inCommentMulti() : inCommentSingle();
    }

    private $<P, Unit> inCommentMulti() {
        String startEnd = ldf.commentEnd() + ldf.commentStart();
        return label("end of comment",
            mplus(seqR(try_(str(ldf.commentEnd())), unit()),
                  seqR(multiLineComment(), this::inCommentMulti),
                  seqR(skipMany1(noneOf(startEnd)), this::inCommentMulti),
                  seqR(oneOf(startEnd), this::inCommentMulti)));
    }

    private $<P, Unit> inCommentSingle() {
        String startEnd = ldf.commentEnd() + ldf.commentStart();
        return label("end of comment",
            mplus(seqR(try_(str(ldf.commentEnd())), unit()),
                  seqR(skipMany1(noneOf(startEnd)), this::inCommentSingle),
                  seqR(oneOf(startEnd), this::inCommentSingle)));
    }

    /**
     * Lexeme parser parses given parser enclosed in parenthesis, returning the
     * value of the parser.
     */
    public <A> $<P, A> parens($<P, A> p) {
        return between(symbol("("), symbol(")"), p);
    }

    /**
     * Lexeme parser parses the given parser enclosed in braces ('{' and '}'),
     * returning the value of the parser.
     */
    public <A> $<P, A> braces($<P, A> p) {
        return between(symbol("{"), symbol("}"), p);
    }

    /**
     * Lexeme parser parses the given parser enclosed in angle brackets ('&lt;'
     * and '&gt;'), returning the value the parser.
     */
    public <A> $<P, A> angles($<P, A> p) {
        return between(symbol("<"), symbol(">"), p);
    }

    /**
     * Lexeme parser parses the given parser enclosed in brackets ('[' and ']'),
     * returning the value of the parser.
     */
    public <A> $<P, A> brackets($<P, A> p) {
        return between(symbol("["), symbol("]"), p);
    }

    /**
     * Lexeme parser parses the character ';' and skips any trailing white space.
     * Return the string ";".
     */
    public $<P, String> semi() {
        return symbol(";");
    }

    /**
     * Lexeme parser parses the character ',' and skips any trailing white space.
     * Returns the string ",".
     */
    public $<P, String> comma() {
        return symbol(",");
    }

    /**
     * Lexeme parser parses the character ':' and skips any trailing white space.
     * Returns the string ":".
     */
    public $<P, String> colon() {
        return symbol(":");
    }

    /**
     * Lexeme parser parses the character '.' and skips any trailing white space.
     * Returns the string ".".
     */
    public $<P, String> dot() {
        return symbol(".");
    }

    /**
     * Lexeme parser parses zero or more occurrences of given parser separated
     * by {@link #semi()}. Returns a list of values returned by parser.
     */
    public <A> $<P, Seq<A>> semiSep($<P, A> p) {
        return sepBy(p, semi());
    }

    /**
     * Lexeme parser parses one or more occurrences of given parser separated
     * by {@link #semi()}. Returns a list of values returned by parser.
     */
    public <A> $<P, Seq<A>> semiSep1($<P, A> p) {
        return sepBy1(p, semi());
    }

    /**
     * Lexeme parser parses zero or more occurrences of given parser separated
     * by {@link #comma()}. Returns a list of values returned by parser.
     */
    public <A> $<P, Seq<A>> commaSep($<P, A> p) {
        return sepBy(p, comma());
    }

    /**
     * Lexeme parser parses one or more occurrences of given parser separated
     * by {@link #comma()}. Returns a list of values returned by parser.
     */
    public <A> $<P, Seq<A>> commaSep1($<P, A> p) {
        return sepBy1(p, comma());
    }
}
