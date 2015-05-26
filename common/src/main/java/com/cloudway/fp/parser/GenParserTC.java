/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.parser;

import java.util.Objects;
import java.util.function.Function;
import java.util.function.Predicate;

import com.cloudway.fp.$;
import com.cloudway.fp.control.ConditionCase;
import com.cloudway.fp.control.Monad;
import com.cloudway.fp.function.ExceptionSupplier;

/**
 * A generalized parser that add support for separate lexical analyzer.
 *
 * @param <P> the parser monad typeclass
 * @param <T> the token type
 * @param <U> the user state type
 * @param <M> the inner monad type
 *
 */
public class GenParserTC<P, T, U, M extends Monad<M>>
    extends ParserT<P, T, U, M>
{
    protected GenParserTC(M nm) {
        super(nm);
    }

    /**
     * The parser {@code satisfy} succeeds for any token for which the
     * supplied predicate returns {@code true}. Returns the token that is
     * actually parsed.
     */
    public $<P, T> satisfy(Predicate<? super T> p) {
        return tokenPrim(p, String::valueOf);
    }

    /**
     * Accepts the token by using Objects.equals.
     */
    public $<P, T> token(T token) {
        return satisfy(t -> Objects.equals(t, token));
    }

    /**
     * Accepts the token by type checking it against the given class.
     */
    public $<P, T> token(Class<? extends T> c) {
        return satisfy(c::isInstance);
    }

    /**
     * Accepts the token by type checking it against the given class and map
     * result with given function.
     */
    public <V extends T, A> $<P, A> token(Class<V> c, Function<? super V, ? extends A> f) {
        return map(satisfy(c::isInstance), t -> f.apply(c.cast(t)));
    }

    /**
     * Accepts the token by pattern matching it with the given conditional case.
     */
    public <R> $<P, R> token(ConditionCase<T, R, RuntimeException> mapper) {
        return $((s, cok, cerr, eok, eerr) -> s.as((input, pos, user) ->
            input.uncons((t, ts) -> {
                ExceptionSupplier<R, RuntimeException> sup = mapper.evaluate(t);
                if (sup != null) {
                    SourcePos pos_ = s.updatePos(ts.getPosition());
                    State<T, U> s_ = new State<>(ts, pos_, user);
                    return cok.apply(sup.produce(), s_, new ParseError(pos_));
                } else {
                    return eerr.apply(unexpectError(s, String.valueOf(t)));
                }
            },
            () -> eerr.apply(unexpectError(s, "")),
            e  -> eerr.apply(updateErrorPos(e, s)))));
    }
}
