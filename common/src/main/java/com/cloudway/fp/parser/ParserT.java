/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.parser;

import java.util.Objects;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

import com.cloudway.fp.$;
import com.cloudway.fp.control.ForwardingMonad;
import com.cloudway.fp.control.Monad;
import com.cloudway.fp.control.MonadPlus;
import com.cloudway.fp.control.monad.trans.MonadExcept;
import com.cloudway.fp.control.monad.trans.MonadReader;
import com.cloudway.fp.control.monad.trans.MonadTrans;
import com.cloudway.fp.data.Either;
import com.cloudway.fp.data.Fn;
import com.cloudway.fp.data.Maybe;
import com.cloudway.fp.data.Ref;
import com.cloudway.fp.data.Seq;
import com.cloudway.fp.data.Unit;
import com.cloudway.fp.function.TriFunction;

// @formatter:off

/**
 * Parser monad transformer.
 *
 * @param <P> the concrete parser type
 * @param <T> the token type
 * @param <U> the user state type
 * @param <M> the underlying monad type
 */
public abstract class ParserT<P, T, U, M extends Monad<M>>
    implements MonadPlus<P>, MonadTrans<P, M>
{
    /**
     * A record of parser state that maintain the input, the source position,
     * and user data.
     */
    protected static class State<T, U> {
        final Stream<T> input;
        final SourcePos pos;
        final U user;

        public State(Stream<T> input, SourcePos pos, U user) {
            this.input = input;
            this.pos = pos;
            this.user = user;
        }

        /**
         * Returns the current parser input.
         */
        public Stream<T> getInput() {
            return input;
        }

        /**
         * Returns the current source position.
         */
        public SourcePos getPosition() {
            return pos;
        }

        /**
         * Returns an updated source position by adding delta to the given
         * position.
         */
        public SourcePos updatePos(int newPos) {
            newPos = SourcePos.addDelta(pos.getPosition(), input.getPosition(), newPos);
            return new SourcePos(pos.getName(), newPos);
        }

        /**
         * Returns the user state.
         */
        public U getState() {
            return user;
        }

        /**
         * Deconstruct the State object.
         */
        public <R> R as(TriFunction<Stream<T>, SourcePos, ? super U, ? extends R> f) {
            return f.apply(input, pos, user);
        }
    }

    private static class Consumed<A> {
        final boolean isEmpty;
        final A reply;

        Consumed(boolean isEmpty, A reply) {
            this.isEmpty = isEmpty;
            this.reply = reply;
        }
    }

    private interface Reply<T, U, A> {
        <R> R apply(TriFunction<A, State<T, U>, ParseError, R> ok, Function<ParseError, R> err);
    }

    private static class Ok<T, U, A> implements Reply<T, U, A> {
        final A value;
        final State<T, U> state;
        final ParseError error;

        Ok(A a, State<T, U> s, ParseError e) {
            this.value = a;
            this.state = s;
            this.error = e;
        }

        @Override
        public <R> R apply(TriFunction<A, State<T, U>, ParseError, R> ok,
                           Function<ParseError, R> err) {
            return ok.apply(value, state, error);
        }
    }

    private static class Error<T, U, A> implements Reply<T, U, A> {
        final ParseError error;

        Error(ParseError e) {
            this.error = e;
        }

        @Override
        public <R> R apply(TriFunction<A, State<T, U>, ParseError, R> ok,
                           Function<ParseError, R> err) {
            return err.apply(error);
        }
    }

    static ParseError unknownError(State<?,?> state) {
        return new ParseError(state.pos);
    }

    static ParseError unexpectError(State<?,?> state, String msg) {
        return new ParseError(state.pos, new Message.SysUnExpect(msg));
    }

    static ParseError updateErrorPos(ParseError e, State<?,?> s) {
        return e.setErrorPos(s.updatePos(e.getErrorPos().getPosition()));
    }

    /**
     * The parser is implemented as a function.
     */
    @FunctionalInterface
    protected interface ParseFunction<T, U, M extends Monad<M>, A, B> {
        $<M, B> apply(State<T, U> state,
            TriFunction<A, State<T, U>, ParseError, $<M, B>> cok, // consumed ok
            Function<ParseError, $<M, B>> cerr,                   // consumed err
            TriFunction<A, State<T, U>, ParseError, $<M, B>> eok, // empty ok
            Function<ParseError, $<M, B>> eerr);                  // empty err
    }

    /**
     * Pack the parser function in a monadic data.
     */
    protected class Monadic<A> implements $<P, A> {
        private final ParseFunction<T,U,M,A,?> pf;

        protected Monadic(ParseFunction<T,U,M,A,?> f) {
            this.pf = f;
        }

        @Override
        @SuppressWarnings("unchecked")
        public P getTypeClass() {
            return (P)ParserT.this;
        }
    }

    protected final M nm;

    /**
     * Construct a parser transformer monad.
     *
     * @param nm the inner monad
     */
    protected ParserT(M nm) {
        this.nm = nm;
    }

    /**
     * Helper method to construct a parser monad.
     */
    protected <A> $<P, A> $(ParseFunction<T,U,M,A,?> f) {
        return new Monadic<>(f);
    }

    /**
     * Unpack the parser function.
     */
    @SuppressWarnings("unchecked")
    protected final <A, B> ParseFunction<T,U,M,A,B> unpack($<P, A> p) {
        return (ParseFunction<T,U,M,A,B>)((Monadic<A>)p).pf;
    }

    /**
     * Unpack and execute the parser function.
     */
    protected final <A, B> $<M, B> unParser($<P, A> p, State<T, U> s,
            TriFunction<A, State<T, U>, ParseError, $<M, B>> cok,
            Function<ParseError, $<M, B>> cerr,
            TriFunction<A, State<T, U>, ParseError, $<M, B>> eok,
            Function<ParseError, $<M, B>> eerr) {
        return nm.delay(() -> this.<A,B>unpack(p).apply(s, cok, cerr, eok, eerr));
    }

    /**
     * Low-level unpacking of the ParsecT type.
     */
    protected final <A> $<M, Consumed<$<M, Reply<T, U, A>>>> runPT($<P, A> p, State<T, U> s) {
        return this.<A, Consumed<$<M, Reply<T, U, A>>>>unpack(p).apply(s,
            (a, s1, e) -> nm.pure(new Consumed<>(false, nm.pure(new Ok<>(a, s1, e)))),
            (e)        -> nm.pure(new Consumed<>(false, nm.pure(new Error<>(e)))),
            (a, s1, e) -> nm.pure(new Consumed<>(true,  nm.pure(new Ok<>(a, s1, e)))),
            (e)        -> nm.pure(new Consumed<>(true,  nm.pure(new Error<>(e)))));
    }

    /**
     * Low-level creation of the ParserT type.
     */
    protected final <A> $<P, A> mkPT(Function<State<T, U>, $<M, Consumed<$<M, Reply<T, U, A>>>>> k) {
        return $((s, cok, cerr, eok, eerr) ->
            nm.bind(k.apply(s), cons ->
                !cons.isEmpty
                    ? nm.bind(cons.reply, rep -> rep.apply(cok, cerr))
                    : nm.bind(cons.reply, rep -> rep.apply(eok, eerr))
            ));
    }

    /**
     * Running a parser.
     */
    public <A> $<M, Either<ParseError, A>> runParser($<P, A> p, U u, String name, Stream<T> input) {
        State<T, U> s = new State<>(input.load(), new SourcePos(name), u);
        return nm.bind(runPT(p, s), res ->
               nm.map(res.reply, (Reply<T,U,A> rep) ->
               rep.apply((x, s1, e) -> Either.<ParseError,A>right(x),
                                       Either::<ParseError,A>left)));
    }

    /**
     * Construct a pure computation that results in the given value.
     *
     * @param x the final result
     * @return the parser monad that hold the final result
     */
    @Override
    public <A> $<P, A> pure(A x) {
        return $((s, cok, cerr, eok, eerr) -> eok.apply(x, s, unknownError(s)));
    }

    private final $<P, Unit> _unit = pure(Unit.U);

    /**
     * Lift a unit value.
     */
    @Override
    public $<P, Unit> unit() {
        return _unit;
    }

    /**
     * Construct a pure computation that results in the given lazy evaluation
     * thunk.
     *
     * @param x a thunk that eventually produce computation result
     * @return the parser monad that hold the computation
     */
    @Override
    public <A> $<P, A> lazy(Supplier<A> x) {
        Supplier<A> t = Fn.lazy(x);
        return $((s, cok, cerr, eok, eerr) -> eok.apply(t.get(), s, unknownError(s)));
    }

    /**
     * Delay evaluate the given parser when this parser involved in a recursive
     * computation.
     */
    @Override
    public <A> $<P, A> delay(Supplier<$<P, A>> x) {
        Supplier<$<P, A>> t = Fn.lazy(x);
        return $((s, cok, cerr, eok, eerr) ->
            unParser(t.get(), s, cok, cerr, eok, eerr));
    }

    /**
     * Promote an inner computation to a parser monad.
     */
    @Override
    public <A> $<P, A> lift($<M, A> m) {
        return $((s, cok, cerr, eok, eerr) ->
            nm.bind(m, a -> eok.apply(a, s, unknownError(s))));
    }

    /**
     * Returns the inner monad typeclass.
     */
    @Override
    public M inner() {
        return nm;
    }

    /**
     * Transfer a computation by feeding the value to the given function and
     * wrap the result to new parser.
     */
    @Override
    public <A, B> $<P, B> map($<P, A> p, Function<? super A, ? extends B> f) {
        return $((s, cok, cerr, eok, eerr) ->
            unParser(p, s, (x, s1, e) -> cok.apply(f.apply(x), s1, e), cerr,
                           (x, s1, e) -> eok.apply(f.apply(x), s1, e), eerr));
    }

    /**
     * Transfer a computation by feeding the value to the given function and
     * wrap the result to new parser.
     */
    @Override
    public <A, B> $<P, B> bind($<P, A> m, Function<? super A, ? extends $<P, B>> k) {
        return $((s, cok, cerr, eok, eerr) ->
            unParser(m, s,
                (x, s1, e1) ->
                    unParser(k.apply(x), s1, cok, cerr,
                             (x2, s2, e2) -> cok.apply(x2, s2, e1.merge(e2)),
                             (e2) -> cerr.apply(e1.merge(e2))),
                cerr,
                (x, s1, e1) ->
                    unParser(k.apply(x), s1, cok, cerr,
                             (x2, s2, e2) -> eok.apply(x2, s2, e1.merge(e2)),
                             (e2) -> eerr.apply(e1.merge(e2))),
                eerr));
    }

    /**
     * Returns the identity parser.
     */
    @Override
    public <A> $<P, A> mzero() {
        return $((s, cok, cerr, eok, eerr) -> eerr.apply(unknownError(s)));
    }

    /**
     * Alternatively combine two parsers.
     */
    @Override
    public <A> $<P, A> mplus($<P, A> m, $<P, A> n) {
        return $((s, cok, cerr, eok, eerr) ->
            unParser(m, s, cok, cerr, eok, e ->
            unParser(n, s, cok, cerr,
                     (y, s1, e1) -> eok.apply(y, s1, e.merge(e1)),
                     (e1)        -> eerr.apply(e.merge(e1)))));
    }

    // Combinators

    /**
     * Fail with a message.
     */
    @Override
    public <A> $<P, A> fail(String msg) {
        return $((s, cok, cerr, eok, eerr) ->
            eerr.apply(new ParseError(s.pos, new Message.Fail(msg)))
        );
    }

    /**
     * Always fails with an unexpected error without consuming any input.
     *
     * <p>The parsers {@link #fail}, {@link #label}, and {@code unexpected} are
     * the three parsers used to generate error messages. </p>
     */
    public <A> $<P, A> unexpected(String msg) {
        return $((s, cok, cerr, eok, eerr) ->
            eerr.apply(new ParseError(s.pos, new Message.UnExpect(msg)))
        );
    }

    /**
     * The {@code label} parser behaves as given parser, but whenever the
     * parser fails (without consuming any input), it replaces expect
     * error messages with the given expect error message.
     *
     * <p>This is normally used at the end of a set alternatives where we
     * want to return an error message in terms of a higher level construct
     * rather than returning all possible characters. For example, if the
     * an expression parser fail, the error message is: '...: expecting
     * expression'. Without the {@code label} combinator, the message would
     * like '...: expecting "let" or letter', which is less friendly.</p>
     */
    public <A> $<P, A> label(String msg, $<P, A> p) {
        return $((s, cok, cerr, eok, eerr) -> unParser(p, s, cok, cerr,
            (x, s1, e) -> eok.apply(x, s1, setExpectError(e, msg)),
            (e) -> eerr.apply(setExpectError(e, msg))));
    }

    private static ParseError setExpectError(ParseError err, String msg) {
        Seq<Message> messages = err.getMessages().append(new Message.Expect(msg));
        return new ParseError(err.getErrorPos(), messages.distinct());
    }

    /**
     * The parser {@code attempt} behaves like given parser, except that it
     * pretends that it hasn't consumed any input when an error occurs.
     *
     * <p>This combinator is used whenever arbitrary look ahead is needed. Since
     * it pretends that it hasn't consumed any input when given parser fails,
     * the (&lt;|&gt;) combinator will try its second alternative even when the
     * first parser failed while consuming input.</p>
     *
     * <p>The {@code attempt} combinator can for example be used to distinguish
     * identifiers and reserved words. Both reserved words and identifiers
     * are a sequence of letters. Whenever we expect a certain reserved word
     * where we can also expect an identifier we have to use the {@code attempt}
     * combinator. Suppose we write:</p>
     *
     * <pre>{@code
     *     expr       = letExpr <|> identifier <?> "expression"
     *     letExpr    = do { string "let"; ... }
     *     identifier = many1 letter
     * }</pre>
     *
     * <p>If the user writes "lexical", the parser fails with: {@code unexpected
     * 'x', expecting 't' in "let"}.  Indeed, since the (&lt;|&gt;) combinator
     * only tries alternatives when the first alternative hasn't consumed input,
     * the identifier parser is never tried (because the prefix "le" of the
     * {@code string "let"} parser is already consumed). The right behaviour can
     * be obtained by adding the {@code attempt} combinator:</p>
     *
     * <pre>{@code
     *     expr       = letExpr <|> identifier <?> "expression"
     *     letExpr    = do { attempt (string "let"); ... }
     *     identifier = many1 letter
     * }</pre>
     */
    public <A> $<P, A> attempt($<P, A> p) {
        return $((s, cok, cerr, eok, eerr) -> unParser(p, s, cok, eerr, eok, eerr));
    }

    /**
     * Alternatively combine two parsers.
     */
    public <A> $<P, A> choice($<P, A> m, $<P, A> n) {
        return mplus(m, n);
    }

    /**
     * Tries to apply the parsers in the list in order, until one of them succeeds.
     * Returns the value of the succeeding parser.
     */
    @SafeVarargs
    public final <A> $<P, A> choice($<P, A> first, $<P, A>... rest) {
        $<P, A> result = first;
        for ($<P, A> next : rest) {
            result = mplus(result, next);
        }
        return result;
    }

    /**
     * Tries to apply the parsers in the list in order, until one of them succeeds.
     * Returns the value of the succeeding parser.
     */
    public <A> $<P, A> choice(Seq<$<P, A>> ps) {
        return ps.foldRight_(mzero(), this::mplus);
    }

    /**
     * The parser {@code many} applies the given parser zero or more times.
     * Returns a list of the returned values.
     */
    public <A> $<P, Seq<A>> many($<P, A> p) {
        return map(manyAccum(p, Seq.<A>nil(), Fn.flip(Seq::cons)), Seq::reverse);
    }

    /**
     * The parser {@code skipMany} applies the given parser zero or more times,
     * skipping its result.
     */
    public <A> $<P, Unit> skipMany($<P, A> p) {
        return manyAccum(p, Unit.U, (xs, x) -> Unit.U);
    }

    /**
     * Applies the given parser one or more times. Returns a list of the returned
     * values.
     */
    public <A> $<P, Seq<A>> some($<P, A> p) {
        return map(someAccum(p, Seq.<A>nil(), Fn.flip(Seq::cons)), Seq::reverse);
    }

    /**
     * Applies the given parser one or more times, skipping its result.
     */
    public <A> $<P, Unit> skipSome($<P, A> p) {
        return someAccum(p, Unit.U, (xs, x) -> Unit.U);
    }

    /**
     * Applies the given parser zero or more times, accumulate results.
     */
    public <A, R> $<P, R> manyAccum($<P, A> p, R z, BiFunction<R, A, R> acc) {
        return $((s, cok, cerr, eok, eerr) -> {
            Function<R, TriFunction<A, State<T, U>, ParseError, $<M, Object>>> walk =
                Fn.fix((rec, xs) -> (x, s1, err) ->
                    unParser(p, s1, rec.apply(acc.apply(xs, x)),
                             cerr, manyErr(),
                             e -> cok.apply(acc.apply(xs, x), s1, e)));

            return unParser(p, s, walk.apply(z),
                            cerr, manyErr(),
                            e -> eok.apply(z, s, e));
        });
    }

    private <A> TriFunction<A, State<T, U>, ParseError, $<M, Object>> manyErr() {
        return (a, s, e) -> {
            throw new RuntimeException("combinator 'many' is applied to a parser that accepts an empty string");
        };
    }

    public <A, R> $<P, R> someAccum($<P, A> p, R z, BiFunction<R, A, R> acc) {
        return bind(p, a -> manyAccum(p, acc.apply(z, a), acc));
    }

    /**
     * Tries to apply the given parser. If parser fails without consuming
     * input, it returns the given value, otherwise the value returned by
     * parser.
     */
    public <A> $<P, A> option(A x, $<P, A> p) {
        return mplus(p, pure(x));
    }

    /**
     * Tries to apply given parser. It will parse or nothing. It only fails if
     * the parser fails after consuming input. It discards the result of parser.
     */
    public <A> $<P, Unit> option($<P, A> p) {
        return mplus(seqR(p, unit()), unit());
    }

    /**
     * Tries to apply given parser. If the parser fails without consuming input,
     * it returns {@code Nothing}, otherwise it returns {@code Just} the value
     * returned by parser.
     */
    @Override
    public <A> $<P, Maybe<A>> optional($<P, A> p) {
        return option(Maybe.empty(), map(p, Maybe::of));
    }

    /**
     * Parses {@code open}, followed by parser and {@code close}. Returned the
     * value returned by parser.
     *
     * <pre>{@code braces = between (symbol "{") (symbol "}")}</pre>
     */
    public <A> $<P, A> between($<P, ?> open, $<P, ?> close, $<P, A> p) {
        return seqR(open, bind(p, x -> seqR(close, pure(x))));
    }

    /**
     * Parses zero or more occurrences of parser, separated by {@code sep}.
     * Returns a list of values returned by parser.
     */
    public <A> $<P, Seq<A>> sepBy($<P, A> p, $<P, ?> sep) {
        return mplus(sepBy1(p, sep), pure(Seq.nil()));
    }

    /**
     * Parses one or more occurrences of parser, separated by {@code sep}.
     * Returns a list of values returned by parser.
     */
    public <A> $<P, Seq<A>> sepBy1($<P, A> p, $<P, ?> sep) {
        return liftM2(Seq::cons, p, many(seqR(sep, p)));
    }

    /**
     * Parses one or more occurrences of parser, separated and optionally ended
     * by {@code sep}. Returns a list of values returned by parser.
     */
    public <A> $<P, Seq<A>> sepEndBy($<P, A> p, $<P, ?> sep) {
        return mplus(sepEndBy1(p, sep), pure(Seq.nil()));
    }

    /**
     * Parses one or more occurrences of parser, separated and optionally ended
     * by {@code sep}. Returns a list of values returned by parser.
     */
    public <A> $<P, Seq<A>> sepEndBy1($<P, A> p, $<P, ?> sep) {
        return bind(p, x ->
            mplus(seqR(sep, bind(sepEndBy(p, sep), xs -> pure(Seq.cons(x, xs)))),
                  pure(Seq.of(x))));
    }

    /**
     * Parses zero or more occurrences of parser, separated and ended by {@code sep}.
     * Returns a list of values returned by parser.
     */
    public <A> $<P, Seq<A>> endBy($<P, A> p, $<P, ?> sep) {
        return many(bind(p, x -> seqR(sep, pure(x))));
    }

    /**
     * Parses one or more occurrences of parser, separated and ended by {@code sep}.
     * Returns a list of values returned by parser.
     */
    public <A> $<P, Seq<A>> endBy1($<P, A> p, $<P, ?> sep) {
        return some(bind(p, x -> seqR(sep, pure(x))));
    }

    /**
     * Parses {@code n} occurrences of parser. If {@code n} is smaller or equal
     * to zero, the parser equals to return []. Returns a list of {@code n}
     * values returned by parser.
     */
    public <A> $<P, Seq<A>> count(int n, $<P, A> p) {
        return replicateM(n, p);
    }

    /**
     * Parses zero or more occurrences of parser, separated by {@code op}.
     * Returns a value obtained by a left associative application of all
     * functions returned by {@code op} to the values returned by parser.
     * If there are zero occurrences of parser, the given default value is
     * returned.
     */
    public <A> $<P, A> chainl($<P, A> p, $<P, BiFunction<A, A, A>> op, A x) {
        return mplus(chainl1(p, op), pure(x));
    }

    /**
     * Parses one or more occurrences of parser, separated by {@code op}.
     * Returns a value obtained by a left associative application of all
     * functions returned by {@code op} to the values returned by parser.
     * This parser can for example be used to eliminate left recursion
     * which typically occurs in expression grammars.
     *
     * <pre>{@code
     *     expr     = term `chainl1 addop
     *     term     = factor `chainl1` mulop
     *     factor   = parens expr <|> integer
     *
     *     mulop    =   do { symbol "*"; return (*) }
     *              <|> do { symbol "/"; return (div) }
     *
     *     addop    =   do { symbol "+"; return (+) }
     *              <|> do { symbol "-"; return (-) }
     * }</pre>
     */
    public <A> $<P, A> chainl1($<P, A> p, $<P, BiFunction<A, A, A>> op) {
        return bind(p, Fn.<A, $<P, A>>fix((rec, x) ->
            mplus(bind(op, f -> bind(p, y -> rec.apply(f.apply(x, y)))), pure(x))));
    }

    /**
     * Parses zero or more occurrences of parser, separated by {@code op}.
     * Returns a value obtained by a right associative application of all
     * functions returned by {@code op} to the values returned by parser.
     * If there are no occurrences of parser, the given default value is
     * returned.
     */
    public <A> $<P, A> chainr($<P, A> p, $<P, BiFunction<A, A, A>> op, A x) {
        return mplus(chainr1(p, op), pure(x));
    }

    /**
     * Parses zero or more occurrences of parser, separated by {@code op}.
     * Returns a value obtained by a right associative application of all
     * functions returned by {@code op} to the values returned by parser.
     * If there are no occurrences of parser, the given default value is
     * returned.
     */
    public <A> $<P, A> chainr1($<P, A> p, $<P, BiFunction<A, A, A>> op) {
        Ref<Supplier<$<P, A>>> scan = new Ref<>();
        Function<A, $<P, A>> rest = Fn.fix((rec, x) ->
            mplus(bind(op, f -> bind(scan.get().get(), y -> pure(f.apply(x, y)))), pure(x)));
        return scan.set(() -> bind(p, rest)).get();
    }

    /**
     * This parser only succeeds at the end of the input. This is not a primitive
     * parser but it is defined using {@link #notFollowedBy}.
     */
    public $<P, Unit> eof() {
        return label("end of input", notFollowedBy(anyToken()));
    }

    /**
     * This parser only succeeds when given parser fails. This parser does not
     * consume any input. This parser can be used to implement the "longest
     * match" rule. For example, when recognizing keywords (for example "let"),
     * we want to make sure that a keyword is not followed by a legal identifier
     * character, in which case the keyword is actually an identifier (for example
     * "lets"). We can program this behaviour as follows:
     *
     * <pre>{@code
     *     keywordLet = attempt (do { string "let"
     *                              ; notFollowedBy alphaNum
     *                              })
     * }</pre>
     */
    public <A> $<P, Unit> notFollowedBy($<P, A> p) {
        return attempt(mplus(bind(attempt(p), c -> unexpected(String.valueOf(c))), unit()));
    }

    /**
     * Applies parser zero or more times until parser {@code end} succeeds.
     * Returns the list of values returned by parser. This parser can be used
     * to scan comments:
     *
     * <pre>{@code
     *     simpleComment = do { string "<!--"
     *                        ; manyTill anyChar (attempt (string "-->"))
     *                        }
     * }</pre>
     */
    public <A> $<P, Seq<A>> manyTill($<P, A> p, $<P, ?> end) {
        return Fn.fix(scan -> mplus(
            seqR(end, pure(Seq.nil())),
            bind(p, x -> bind(scan.get(), xs -> pure(Seq.cons(x, xs))))));
    }

    /**
     * The parser {@code lookAhead} parses given parser without consuming any input.
     *
     * <p>If the given parser fails and consumes some input, so does {@code lookAhead}.
     * Combine with {@link #attempt} if this is undesirable.</p>
     */
    public <A> $<P, A> lookAhead($<P, A> p) {
        return $((s, cok, cerr, eok, eerr) -> {
            TriFunction<A, State<T, U>, ParseError, $<M, Object>> eok_ =
                (a, s1, e) -> eok.apply(a, s, unknownError(s));
            return unParser(p, s, eok_, cerr, eok_, eerr);
        });
    }

    /**
     * The parser {@code anyToken} accepts any kind of token. It is for example
     * used to implement {@link #eof}. Returns the accepted token.
     */
    public $<P, T> anyToken() {
        return tokenPrim(t -> true, String::valueOf);
    }

    /**
     * The parser {@code tokenPrime} accepts a token with result when the function
     * {@code test} returns {@code Just x}. The token can be shown using
     * {@code showTok}. The position of the next token should be returned when
     * {@code nextPos} is called with the current source position, the current
     * token and the rest of the tokens.
     *
     * <p>This is the most primitive combinator for accepting tokens.</p>
     */
    public $<P, T> tokenPrim(Predicate<? super T> p, Function<T, String> showToken) {
        return $((s, cok, cerr, eok, eerr) -> s.as((input, pos, user) ->
            input.uncons((c, cs) -> {
                if (p.test(c)) {
                    SourcePos pos_ = s.updatePos(cs.getPosition());
                    State<T, U> s_ = new State<>(cs, pos_, user);
                    return cok.apply(c, s_, new ParseError(pos_));
                } else {
                    return eerr.apply(unexpectError(s, showToken.apply(c)));
                }
            },
            () -> eerr.apply(unexpectError(s, "")),
            e  -> eerr.apply(updateErrorPos(e, s)))));
    }

    public $<P, Unit> tokens(Seq<T> tts, Function<Seq<T>, String> showTokens) {
        if (tts.isEmpty()) {
            return $((s, cok, cerr, eok, eerr) -> eok.apply(Unit.U, s, unknownError(s)));
        } else {
            return $((s, cok, cerr, eok, eerr) -> s.as((input, pos, u) -> {
                BiFunction<Seq<T>, Stream<T>, $<M, Object>> walk
                    = Fn.fix((rec, toks, rs) -> {
                        if (toks.isEmpty()) {
                            SourcePos pos_ = s.updatePos(rs.getPosition());
                            State<T, U> s_ = new State<>(rs, pos_, u);
                            return cok.apply(Unit.U, s_, new ParseError(pos_));
                        } else {
                            return rs.uncons((x, xs) ->
                                Objects.equals(toks.head(), x)
                                    ? rec.apply(toks.tail(), xs)
                                    : cerr.apply(errExpect(showTokens, pos, x, tts)),
                                () -> cerr.apply(errEof(showTokens, pos, tts)),
                                e  -> cerr.apply(updateErrorPos(e, s)));
                        }
                    });

                return input.uncons((x, xs) ->
                    Objects.equals(tts.head(), x)
                        ? walk.apply(tts.tail(), xs)
                        : eerr.apply(errExpect(showTokens, pos, x, tts)),
                    () -> eerr.apply(errEof(showTokens, pos, tts)),
                    e  -> eerr.apply(updateErrorPos(e, s)));
            }));
        }
    }

    private static <T> ParseError
    errEof(Function<Seq<T>, String> show, SourcePos pos, Seq<T> toks) {
        ParseError e = new ParseError(pos, new Message.SysUnExpect(""));
        return e.addMessage(new Message.Expect(show.apply(toks)));
    }

    private static <T> ParseError
    errExpect(Function<Seq<T>, String> show, SourcePos pos, T token, Seq<T> tokens) {
        ParseError e = new ParseError(pos, new Message.SysUnExpect(show.apply(Seq.of(token))));
        return e.addMessage(new Message.Expect(show.apply(tokens)));
    }

    // Parser state combinators

    /**
     * Returns the current input.
     */
    public $<P, Stream<T>> getInput() {
        return map(getParserState(), State::getInput);
    }

    /**
     * Continues parsing with given input. The {@link #getInput} and {@code
     * setInput} functions can for example be used to deal with #include files.
     */
    public $<P, Unit> setInput(Stream<T> input) {
        return seqR(updateParserState(s -> new State<>(input.load(), s.pos, s.user)), unit());
    }

    /**
     * Returns the current source position.
     */
    public $<P, SourcePos> getPosition() {
        return map(getParserState(), State::getPosition);
    }

    /**
     * Set the current source position.
     */
    public $<P, Unit> setPosition(SourcePos pos) {
        return seqR(updateParserState(s -> new State<>(s.input, pos, s.user)), unit());
    }

    /**
     * Returns the current user state.
     */
    public $<P, U> getState() {
        return map(getParserState(), State::getState);
    }

    /**
     * Set the user state.
     */
    public $<P, Unit> putState(U user) {
        return seqR(updateParserState(s -> new State<>(s.input, s.pos, user)), unit());
    }

    /**
     * Applies given function to the user state.
     */
    public $<P, Unit> modifyState(Function<U, U> f) {
        return seqR(updateParserState(s -> new State<>(s.input, s.pos, f.apply(s.user))), unit());
    }

    /**
     * returns the full parser state as a {@code State} record.
     */
    protected $<P, State<T, U>> getParserState() {
        return updateParserState(Fn.id());
    }

    /**
     * Set the full parser state.
     */
    protected $<P, State<T, U>> setParserState(State<T, U> st) {
        return updateParserState(Fn.pure(st));
    }

    /**
     * Applies function to the parser state.
     */
    protected $<P, State<T, U>> updateParserState(Function<State<T, U>, State<T, U>> f) {
        return $((s, cok, cerr, eok, eerr) -> {
            State<T, U> s1 = f.apply(s);
            return eok.apply(s1, s1, unknownError(s1));
        });
    }

    // Lifting other operations

    @Override
    @SuppressWarnings("unchecked")
    public <R> MonadReader<P, R> liftReader() {
        MonadReader<M, R> inner;
        if (nm instanceof MonadReader) {
            inner = (MonadReader<M,R>)nm;
        } else if (nm instanceof MonadTrans) {
            inner = (MonadReader<M,R>)((MonadTrans<P,M>)nm).liftReader();
        } else {
            throw new UnsupportedOperationException("liftReader");
        }
        return new LiftReader<>(inner);
    }

    private class LiftReader<R> implements MonadReader<P, R>, ForwardingMonad<P> {
        private final MonadReader<M, R> inner;

        LiftReader(MonadReader<M, R> inner) {
            this.inner = inner;
        }

        @Override
        public Monad<P> delegate() {
            return ParserT.this;
        }

        @Override
        public <A> $<P, A> reader(Function<? super R, ? extends A> f) {
            return lift(inner.reader(f));
        }

        @Override
        public $<P, R> ask() {
            return lift(inner.ask());
        }

        @Override
        public <A> $<P, A> local(Function<R, R> f, $<P, A> p) {
            return mkPT(s -> inner.local(f, runPT(p, s)));
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <E> MonadExcept<P, E> liftExcept() {
        MonadExcept<M, E> inner;
        if (nm instanceof MonadExcept) {
            inner = (MonadExcept<M,E>)nm;
        } else if (nm instanceof MonadTrans) {
            inner = (MonadExcept<M,E>)((MonadTrans<P,M>)nm).liftExcept();
        } else {
            throw new UnsupportedOperationException("liftExcept");
        }
        return new LiftExcept<>(inner);
    }

    private class LiftExcept<E> implements MonadExcept<P, E>, ForwardingMonad<P> {
        private final MonadExcept<M, E> inner;

        LiftExcept(MonadExcept<M, E> inner) {
            this.inner = inner;
        }

        @Override
        public Monad<P> delegate() {
            return ParserT.this;
        }

        @Override
        public <A> $<P, A> throwE(E e) {
            return lift(inner.throwE(e));
        }

        @Override
        public <A> $<P, A> catchE(Function<? super E, ? extends $<P, A>> h, $<P, A> p) {
            return mkPT(s -> inner.catchE(e -> runPT(h.apply(e), s), runPT(p, s)));
        }
    }
}
