/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.parser;

import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;

import com.cloudway.fp.$;
import com.cloudway.fp.control.ConditionCase;
import com.cloudway.fp.control.ForwardingMonad;
import com.cloudway.fp.control.Monad;
import com.cloudway.fp.control.MonadPlus;
import com.cloudway.fp.control.Trampoline;
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

import static com.cloudway.fp.control.Conditionals.with;
import static com.cloudway.fp.control.Trampoline.immediate;
import static com.cloudway.fp.control.Trampoline.suspend;

// @formatter:off

/**
 * Parser monad transformer.
 *
 * @param <P> the concrete parser type
 * @param <S> the stream type
 * @param <T> the token type
 * @param <U> the user state type
 * @param <M> the underlying monad type
 */
public abstract class ParsecT<P, S, T, U, M extends Monad<M>>
    implements MonadPlus<P>, MonadTrans<P, M>
{
    /**
     * A record of parser state that maintain the input, the source position,
     * and user data.
     */
    public static class State<S, U> {
        final S input;
        final SourcePos pos;
        final U user;

        public State(S input, SourcePos pos, U user) {
            this.input = input;
            this.pos = pos;
            this.user = user;
        }

        /**
         * Returns the current parser input.
         */
        public S getInput() {
            return input;
        }

        /**
         * Returns the current source position.
         */
        public SourcePos getPosition() {
            return pos;
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
        public <R> R as(TriFunction<? super S, SourcePos, ? super U, ? extends R> f) {
            return f.apply(input, pos, user);
        }
    }

    private static class Consumed<A> {
        final A reply;

        Consumed(A reply) {
            this.reply = reply;
        }

        boolean isEmpty() {
            return false;
        }
    }

    private static class Empty<A> extends Consumed<A> {
        Empty(A data) {
            super(data);
        }

        @Override
        boolean isEmpty() {
            return true;
        }
    }

    private interface Reply<S, U, A> {}

    private static class Ok<S, U, A> implements Reply<S, U, A> {
        final A value;
        final State<S, U> state;
        final ParseError error;

        Ok(A a, State<S, U> s, ParseError e) {
            this.value = a;
            this.state = s;
            this.error = e;
        }
    }

    private static class Error<S, U, A> implements Reply<S, U, A> {
        final ParseError error;

        Error(ParseError e) {
            this.error = e;
        }
    }

    private static <S, U, A, R> ConditionCase<Reply<S, U, A>, R, RuntimeException>
    Ok(TriFunction<? super A, ? super State<S, U>, ParseError, ? extends R> mapper) {
        return t -> {
            if (t instanceof Ok) {
                Ok<S, U, A> ok = (Ok<S,U,A>)t;
                return () -> mapper.apply(ok.value, ok.state, ok.error);
            } else {
                return null;
            }
        };
    }

    private static <S, U, A, R> ConditionCase<Reply<S, U, A>, R, RuntimeException>
    Error(Function<ParseError, ? extends R> mapper) {
        return t -> {
            if (t instanceof Error) {
                Error<S, U, A> err = (Error<S,U,A>)t;
                return () -> mapper.apply(err.error);
            } else {
                return null;
            }
        };
    }

    protected static ParseError unknownError(State<?,?> state) {
        return new ParseError(state.pos);
    }

    /**
     * The parser is implemented as a function.
     */
    @FunctionalInterface
    protected interface ParseFunction<S, U, M extends Monad<M>, A, B> {
        Trampoline<$<M, B>> apply(State<S, U> state,
            TriFunction<A, State<S, U>, ParseError, Trampoline<$<M, B>>> cok, // consumed ok
            Function<ParseError, Trampoline<$<M, B>>> cerr,                   // consumed err
            TriFunction<A, State<S, U>, ParseError, Trampoline<$<M, B>>> eok, // empty ok
            Function<ParseError, Trampoline<$<M, B>>> eerr);                  // empty err
    }

    /**
     * Pack the parser function in a monadic data.
     */
    protected static abstract class Monadic<P, S, U, M extends Monad<M>, A> implements $<P, A> {
        private final ParseFunction<S, U, M, A, ?> pf;

        protected Monadic(ParseFunction<S, U, M, A, ?> f) {
            this.pf = f;
        }
    }

    private final M nm;

    /**
     * Construct a parser transformer monad.
     *
     * @param nm the inner monad
     */
    protected ParsecT(M nm) {
        this.nm = nm;
    }

    /**
     * Implemented by subclass to construct a parser monad.
     */
    protected abstract <A> $<P, A> $(ParseFunction<S, U, M, A, ?> f);

    /**
     * Implemented by subclass to return underlying stream implementation.
     */
    protected abstract Stream<S, T> stream();

    /**
    * Unpack the parser function.
     */
    @SuppressWarnings("unchecked")
    private <A, B> ParseFunction<S, U, M, A, B> unpack($<P, A> p) {
        return (ParseFunction<S,U,M,A,B>)((Monadic<P,S,U,M,A>)p).pf;
    }

    /**
     * Unpack and execute the parser function.
     */
    private <A, B> Trampoline<$<M, B>> unParser($<P, A> p, State<S, U> s,
            TriFunction<A, State<S, U>, ParseError, Trampoline<$<M, B>>> cok,
            Function<ParseError, Trampoline<$<M, B>>> cerr,
            TriFunction<A, State<S, U>, ParseError, Trampoline<$<M, B>>> eok,
            Function<ParseError, Trampoline<$<M, B>>> eerr) {
        return suspend(() -> this.<A,B>unpack(p).apply(s, cok, cerr, eok, eerr));
    }

    /**
     * Low-level unpacking of the ParsecT type.
     */
    private <A> $<M, Consumed<$<M, Reply<S, U, A>>>> runPT($<P, A> p, State<S, U> s) {
        return this.<A, Consumed<$<M, Reply<S, U, A>>>>unpack(p).apply(s,
            (a, s1, e) -> immediate(nm.pure(new Consumed<>(nm.pure(new Ok<>(a, s1, e))))),
            (e)        -> immediate(nm.pure(new Consumed<>(nm.pure(new Error<>(e))))),
            (a, s1, e) -> immediate(nm.pure(new Empty<>(nm.pure(new Ok<>(a, s1, e))))),
            (e)        -> immediate(nm.pure(new Empty<>(nm.pure(new Error<>(e))))))
            .run();
    }

    /**
     * Low-level creation of the ParsecT type.
     */
    private <A> $<P, A> mkPT(Function<State<S, U>, $<M, Consumed<$<M, Reply<S, U, A>>>>> k) {
        return $((s, cok, cerr, eok, eerr) ->
            Trampoline.lazy(() -> nm.bind(k.apply(s), cons -> {
                if (!cons.isEmpty()) {
                    return nm.bind(cons.reply, rep ->
                        with(rep).<$<M,Object>>get()
                          .when(Ok((a, s1, e) -> cok.apply(a, s1, e).run()))
                          .when(Error(e -> cerr.apply(e).run()))
                          .get());
                } else {
                    return nm.bind(cons.reply, rep ->
                        with(rep).<$<M,Object>>get()
                          .when(Ok((a, s1, e) -> eok.apply(a, s1, e).run()))
                          .when(Error(e -> eerr.apply(e).run()))
                          .get());
                }
            })));
    }

    /**
     * Running a parser.
     */
    public <A> $<M, Either<ParseError, A>> runParsecT($<P, A> p, U u, String name, S s) {
        return nm.bind(runPT(p, new State<>(s, new SourcePos(name), u)), res ->
               nm.map(res.reply, (Reply<S,U,A> r) ->
                   with(r).<Either<ParseError, A>>get()
                     .when(Ok((x, s1, e) -> Either.right(x)))
                     .when(Error(Either::left))
                     .get()));
    }

    /**
     * Construct a pure computation that results in the given value.
     *
     * @param x the final result
     * @return the parser monad that hold the final result
     */
    @Override
    public <A> $<P, A> pure(A x) {
        return $((s, cok, cerr, eok, eerr) -> suspend(() ->
            eok.apply(x, s, unknownError(s))));
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
        return $((s, cok, cerr, eok, eerr) -> suspend(() ->
            eok.apply(t.get(), s, unknownError(s))));
    }

    /**
     * Delay evaluate the given parser when this parser involved in a recursive
     * computation.
     */
    @Override
    public <A> $<P, A> delay(Supplier<$<P, A>> x) {
        Supplier<$<P, A>> t = Fn.lazy(x);
        return $((s, cok, cerr, eok, eerr) -> suspend(() ->
            unParser(t.get(), s, cok, cerr, eok, eerr)));
    }

    /**
     * Promote an inner computation to a parser monad.
     */
    @Override
    public <A> $<P, A> lift($<M, A> m) {
        return $((s, cok, cerr, eok, eerr) -> Trampoline.lazy(() ->
            nm.bind(m, a -> eok.apply(a, s, unknownError(s)).run())));
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
            unParser(p, s, (x, s1, e) -> suspend(() -> cok.apply(f.apply(x), s1, e)), cerr,
                           (x, s1, e) -> suspend(() -> eok.apply(f.apply(x), s1, e)), eerr));
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
                             (x2, s2, e2) -> suspend(() -> cok.apply(x2, s2, e1.merge(e2))),
                             (e2) -> suspend(() -> cerr.apply(e1.merge(e2)))),
                cerr,
                (x, s1, e1) ->
                    unParser(k.apply(x), s1, cok, cerr,
                             (x2, s2, e2) -> suspend(() -> eok.apply(x2, s2, e1.merge(e2))),
                             (e2) -> suspend(() -> eerr.apply(e1.merge(e2)))),
                eerr));
    }

    /**
     * Returns the identity parser.
     */
    @Override
    public <A> $<P, A> mzero() {
        return $((s, cok, cerr, eok, eerr) -> suspend(() -> eerr.apply(unknownError(s))));
    }

    /**
     * Alternatively combine two parsers.
     */
    @Override
    public <A> $<P, A> mplus($<P, A> m, $<P, A> n) {
        return $((s, cok, cerr, eok, eerr) ->
            unParser(m, s, cok, cerr, eok, e ->
            unParser(n, s, cok, cerr,
                     (y, s1, e1) -> suspend(() -> eok.apply(y, s1, e.merge(e1))),
                     (e1)        -> suspend(() -> eerr.apply(e.merge(e1))))));
    }

    /**
     * Combine alternatives.
     */
    @SafeVarargs
    public final <A> $<P, A> mplus($<P, A> first, $<P, A>... rest) {
        $<P, A> result = first;
        for ($<P, A> next : rest) {
            result = mplus(result, next);
        }
        return result;
    }

    // Combinators

    /**
     * Fail with a message.
     */
    @Override
    public <A> $<P, A> fail(String msg) {
        return $((s, cok, cerr, eok, eerr) -> suspend(() ->
            eerr.apply(new ParseError(s.pos, new Message.Fail(msg)))));
    }

    /**
     * Always fails with an unexpected error without consuming any input.
     *
     * <p>The parsers {@link #fail}, {@link #label}, and {@code unexpected} are
     * the three parsers used to generate error messages. </p>
     */
    public <A> $<P, A> unexpected(String msg) {
        return $((s, cok, cerr, eok, eerr) -> suspend(() ->
            eerr.apply(new ParseError(s.pos, new Message.UnExpect(msg)))));
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
    public <A> $<P, A> label($<P, A> p, String msg) {
        return labels(p, Seq.of(msg));
    }

    public <A> $<P, A> labels($<P, A> p, Seq<String> msgs) {
        return $((s, cok, cerr, eok, eerr) -> unParser(p, s, cok, cerr,
            (x, s1, e) -> suspend(() -> eok.apply(x, s1, setExpectErrors(e, msgs))),
            (e) -> suspend(() -> eerr.apply(setExpectErrors(e, msgs)))));
    }

    private static ParseError setExpectErrors(ParseError err, Seq<String> msgs) {
        if (msgs.isEmpty()) {
            return err.setMessage(new Message.Expect(""));
        } else {
            Seq<Message> messages = err.getMessages().append(msgs.map(Message.Expect::new));
            return new ParseError(err.getErrorPos(), messages.distinct());
        }
    }

    /**
     * The parser {@code try} behaves like given parser, except that it pretends
     * that it hasn't consumed any input when an error occurs.
     *
     * <p>This combinator is used whenever arbitrary look ahead is needed. Since
     * it pretends that it hasn't consumed any input when given parser fails,
     * the (&lt;|&gt;) combinator will try its second alternative even when the
     * first parser failed while consuming input.</p>
     *
     * <p>The {@code try} combinator can for example be used to distinguish
     * identifiers and reserved words. Both reserved words and identifiers
     * are a sequence of letters. Whenever we expect a certain reserved word
     * where we can also expect an identifier we have to use the {@code try}
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
     * be obtained by adding the {@code try} combinator:</p>
     *
     * <pre>{@code
     *     expr       = letExpr <|> identifier <?> "expression"
     *     letExpr    = do { try (string "let"); ... }
     *     identifier = many1 letter
     * }</pre>
     */
    public <A> $<P, A> try_($<P, A> p) {
        return $((s, cok, cerr, eok, eerr) -> unParser(p, s, cok, eerr, eok, eerr));
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
        return bind(accum(p, Seq.<A>nil(), Seq::cons), xs -> pure(xs.reverse()));
    }

    /**
     * The parser {@code skipMany} applies the given parser zero or more times,
     * skipping its result.
     */
    public <A> $<P, Unit> skipMany($<P, A> p) {
        return accum(p, Unit.U, (x, xs) -> xs);
    }

    /**
     * Applies the given parser one or more times. Returns a list of the returned
     * values.
     */
    public <A> $<P, Seq<A>> many1($<P, A> p) {
        return liftM2(Seq::cons, p, many(p));
    }

    /**
     * Applies the given parser one or more times, skipping its result.
     */
    public <A> $<P, Unit> skipMany1($<P, A> p) {
        return seqR(p, skipMany(p));
    }

    /**
     * Applies the given parser zero or more times, accumulate results.
     */
    public <A, R> $<P, R> accum($<P, A> p, R z, BiFunction<? super A, R, R> acc) {
        return $((s, cok, cerr, eok, eerr) -> {
            Function<R, TriFunction<A, State<S,U>, ParseError, Trampoline<$<M, Object>>>> walk =
                Fn.fix((rec, xs) -> (x, s1, err) ->
                    unParser(p, s1,
                        (x2, s2, e2) -> suspend(() -> rec.apply(acc.apply(x, xs)).apply(x2, s2, e2)),
                        cerr, manyErr(),
                        e -> suspend(() -> cok.apply(acc.apply(x, xs), s1, e))));

            return unParser(p, s,
                (x, s1, e) -> suspend(() -> walk.apply(z).apply(x, s1, e)),
                cerr, manyErr(),
                e -> suspend(() -> eok.apply(z, s, e)));
        });
    }

    private <A> TriFunction<A, State<S, U>, ParseError, Trampoline<$<M, Object>>> manyErr() {
        return (a, s, e) -> {
            throw new RuntimeException("combinator 'many' is applied to a parser that accepts an empty string");
        };
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
        return many1(bind(p, x -> seqR(sep, pure(x))));
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
        return label(notFollowedBy(anyToken()), "end of input");
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
     *     keywordLet = try (do { string "let"
     *                          ; notFollowedBy alphaNum
     *                          })
     * }</pre>
     */
    public <A> $<P, Unit> notFollowedBy($<P, A> p) {
        return try_(mplus(bind(try_(p), c -> unexpected(String.valueOf(c))), unit()));
    }

    /**
     * Applies parser zero or more times until parser {@code end} succeeds.
     * Returns the list of values returned by parser. This parser can be used
     * to scan comments:
     *
     * <pre>{@code
     *     simpleComment = do { string "<!--"
     *                        ; manyTill anyChar (try (string "-->"))
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
     * Combine with {@code try} if this is undesirable.</p>
     */
    public <A> $<P, A> lookAhead($<P, A> p) {
        return $((s, cok, cerr, eok, eerr) -> {
            TriFunction<A, State<S, U>, ParseError, Trampoline<$<M, Object>>> eok_ =
                (a, s1, e) -> suspend(() -> eok.apply(a, s, unknownError(s)));
            return unParser(p, s, eok_, cerr, eok_, eerr);
        });
    }

    /**
     * The parser {@code anyToken} accepts any kind of token. It is for example
     * used to implement {@link #eof}. Returns the accepted token.
     */
    public $<P, T> anyToken() {
        return tokenPrim(String::valueOf, (pos, tok, toks) -> pos, Maybe::of);
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
    public <A> $<P, A> tokenPrim(Function<T, String>  showToken,
            TriFunction<SourcePos, T, S, SourcePos>   nextPos,
            Function<T, Maybe<A>>                     test) {
        return tokenPrimEx(showToken, nextPos, Maybe.empty(), test);
    }

    public <A> $<P, A> tokenPrimEx(Function<T, String>          showToken,
            TriFunction<SourcePos, T, S, SourcePos>             nextPos,
            Maybe<TriFunction<SourcePos, T, S, Function<U, U>>> nextState,
            Function<T, Maybe<A>>                               test) {
        return $((s, cok, cerr, eok, eerr) -> s.as((input, pos, user) ->
            suspend(() -> stream().uncons(input).map(t -> t.as((c, cs) ->
                test.apply(c).map(x -> {
                    SourcePos newPos = nextPos.apply(pos, c, cs);
                    U newUser = nextState.map(f -> f.apply(pos, c, cs)).orElse(Fn.id()).apply(user);
                    State<S, U> newState = new State<>(cs, newPos, newUser);
                    return cok.apply(x, newState, new ParseError(newPos));
                }).orElseGet(() -> eerr.apply(unexpectError(pos, showToken.apply(c))))
            )).orElseGet(() -> eerr.apply(unexpectError(pos, ""))))));
    }

    private static ParseError unexpectError(SourcePos pos, String msg) {
        return new ParseError(pos, new Message.SysUnExpect(msg));
    }

    // Parser state combinators

    /**
     * Returns the current source position.
     */
    public $<P, SourcePos> getPosition() {
        return map(getParserState(), State::getPosition);
    }

    /**
     * Returns the current input.
     */
    public $<P, S> getInput() {
        return map(getParserState(), State::getInput);
    }

    /**
     * Set the current source position.
     */
    public $<P, Unit> setPosition(SourcePos pos) {
        return seqR(updateParserState(s -> new State<>(s.input, pos, s.user)), unit());
    }

    /**
     * Continues parsing with given input. The {@link #getInput} and {@code
     * setInput} functions can for example be used to deal with #include files.
     */
    public $<P, Unit> setInput(S input) {
        return seqR(updateParserState(s -> new State<>(input, s.pos, s.user)), unit());
    }

    /**
     * returns the full parser state as a {@code State} record.
     */
    public $<P, State<S, U>> getParserState() {
        return updateParserState(Fn.id());
    }

    /**
     * Set the full parser state.
     */
    public $<P, State<S, U>> setParserState(State<S, U> st) {
        return updateParserState(Fn.pure(st));
    }

    /**
     * Applies function to the parser state.
     */
    public $<P, State<S, U>> updateParserState(Function<State<S, U>, State<S, U>> f) {
        return $((s, cok, cerr, eok, eerr) -> {
            State<S, U> s1 = f.apply(s);
            return suspend(() -> eok.apply(s1, s1, unknownError(s1)));
        });
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
            return ParsecT.this;
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
            return ParsecT.this;
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
