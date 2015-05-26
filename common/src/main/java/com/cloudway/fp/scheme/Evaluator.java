/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.scheme;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Iterator;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;

import com.cloudway.fp.$;
import com.cloudway.fp.control.ConditionCase;
import com.cloudway.fp.control.Trampoline;
import com.cloudway.fp.control.monad.trans.ExceptTC;
import com.cloudway.fp.control.monad.trans.ReaderT;
import com.cloudway.fp.data.Either;
import com.cloudway.fp.data.Fn;
import com.cloudway.fp.data.HashPMap;
import com.cloudway.fp.data.Maybe;
import com.cloudway.fp.data.PMap;
import com.cloudway.fp.data.Rational;
import com.cloudway.fp.data.Ref;
import com.cloudway.fp.data.Seq;
import com.cloudway.fp.data.Tuple;
import com.cloudway.fp.data.Vector;
import com.cloudway.fp.function.TriFunction;
import com.cloudway.fp.parser.Stream;

import static com.cloudway.fp.control.Conditionals.with;
import static com.cloudway.fp.control.Syntax.do_;
import static com.cloudway.fp.control.Syntax.loop;
import static com.cloudway.fp.scheme.LispError.*;
import static com.cloudway.fp.scheme.LispVal.*;
import static com.cloudway.fp.scheme.LispVal.Void;

// @formatter:off

public class Evaluator extends ExceptTC<Evaluator, LispError, ReaderT<Env, Trampoline.µ>> {
    private final SchemeParser parser = new SchemeParser();
    private final $<Evaluator, LispVal> stdlib;

    public Evaluator() {
        super(ReaderT.on(Trampoline.tclass));
        stdlib = loadLib("stdlib.scm");
    }

    public $<Evaluator, LispVal> parse(String name, Stream<LispVal> input) {
        return parser.parse(name, input).<$<Evaluator, LispVal>>either(
            err -> throwE(new LispError.Parser(err)), this::eval);
    }

    public $<Evaluator, LispVal> parse(String input) {
        return parse("", parser.getStream(input));
    }

    public $<Evaluator, LispVal> parse(String name, Reader input) {
        return parse(name, parser.getStream(input));
    }

    private $<Evaluator, LispVal> loadLib(String name) {
        try (InputStream is = getClass().getResourceAsStream(name)) {
            return parse(name, new InputStreamReader(is, StandardCharsets.UTF_8));
        } catch (Exception ex) {
            return throwE(new LispError(ex));
        }
    }

    public Either<LispError, LispVal> run(Env env, $<Evaluator, LispVal> exp) {
        return Trampoline.run(ReaderT.run(runExcept(seqR(stdlib, exp)), env));
    }

    public Either<LispError, LispVal> run($<Evaluator, LispVal> exp) {
        return run(new Env(getPrimitives()), exp);
    }

    public Either<LispError, LispVal> evaluate(String expr) {
        return run(parse(expr));
    }

    public void runREPL() throws IOException {
        Env env = new Env(getPrimitives());
        run(env, stdlib);

        BufferedReader input = new BufferedReader(new InputStreamReader(System.in));
        String line;

        // A very simple Read-Eval-Print Loop
        while (true) {
            System.out.print("==> ");
            if ((line = input.readLine()) == null || "quit".equals(line.trim()))
                break;
            if (line.trim().isEmpty())
                continue;
            run(env, parse(line)).either(
                err -> {
                    System.err.println(err.getMessage());
                    return null;
                },
                val -> {
                    if (!(val instanceof Void))
                        System.out.println(val.show());
                    return null;
                }
            );
        }
    }

    public static void main(String[] args) throws IOException {
        new Evaluator().runREPL();
    }

    // =======================================================================

    public $<Evaluator, LispVal> eval(LispVal val) {
        return with(val).<$<Evaluator, LispVal>>get()
            .when(Text      (__ -> pure(val)))
            .when(Num       (__ -> pure(val)))
            .when(Bool      (__ -> pure(val)))
            .when(Promise   (__ -> pure(val)))
            .when(Void      (() -> pure(val)))
            .when(Atom      (this::getVar))
            .when(Cons      ((head, tail) -> evalList(val, head, tail)))
            .when(Vector    (v -> map(mapM(v, this::eval), Vec::new)))
            .orElseGet(() -> throwE(new BadSpecialForm("Unrecognized special form", val)));
    }

    private <T> $<Evaluator, T> badSyntax(String name, LispVal val) {
        return throwE(new BadSpecialForm(name + ": bad syntax", val));
    }

    private $<Evaluator, LispVal> evalList(LispVal val, LispVal tag, Seq<LispVal> args) {
        if (tag.isAtom()) {
            switch (((Atom)tag).name) {
            case "begin":
                return evalSequence(args);

            case "define":
                return with(args).<$<Evaluator, LispVal>>get()
                  .when(Seq.Pair((first, value) ->
                    first.isAtom() 
                        ? defineVar((Atom)first, value)
                        : defineFunc(val, first, Seq.of(value), false)))
                  .when(Seq.Cons((first, rest) ->
                        defineFunc(val, first, rest, false)))
                  .orElseGet(() -> badSyntax("define", val));

            case "defmacro":
                return with(args).<$<Evaluator, LispVal>>get()
                  .when(Seq.Cons((first, rest) ->
                        defineFunc(val, first, rest, true)))
                  .orElseGet(() -> badSyntax("defmacro", val));

            case "set!":
                return with(args).<$<Evaluator, LispVal>>get()
                  .when(Seq.Pair((id, value) ->
                    id.isAtom()
                      ? do_(eval(value), res -> setVar((Atom)id, res))
                      : throwE(new BadSpecialForm("set! not an identifier", id))))
                  .orElseGet(() -> badSyntax("set!", val));

            case "set-car!":
                return with(args).<$<Evaluator, LispVal>>get()
                  .when(Seq.Pair((id, value) ->
                    id.isAtom()
                      ? withEnv(env -> env.lookupRef((Atom)id).<$<Evaluator, LispVal>>either(
                          var -> do_(eval(value), res -> setCar(var, res)),
                          ()  -> unbound((Atom)id)))
                      : throwE(new BadSpecialForm("set-car! not an identifier", id))))
                  .orElseGet(() -> badSyntax("set-car!", val));

            case "set-cdr!":
                return with(args).<$<Evaluator, LispVal>>get()
                  .when(Seq.Pair((id, value) ->
                    id.isAtom()
                      ? withEnv(env -> env.lookupRef((Atom)id).<$<Evaluator, LispVal>>either(
                          var -> do_(eval(value), res -> setCdr(var, res)),
                          ()  -> unbound((Atom)id)))
                      : throwE(new BadSpecialForm("set-cdr! not an identifier", id))))
                  .orElseGet(() -> badSyntax("set-cdr!", val));

            case "if":
                return with(args).<$<Evaluator, LispVal>>get()
                  .when(Seq.Pair((pred, conseq) ->
                    do_(eval(pred), result -> isFalse(result) ? pure(Void.VOID) : eval(conseq))))
                  .when(Seq.Triple((pred, conseq, alt) ->
                    do_(eval(pred), result -> isFalse(result) ? eval(alt) : eval(conseq))))
                  .orElseGet(() -> badSyntax("if", val));

            case "cond":
                return evalCond(val, args);

            case "and":
                return evalAnd(args, Bool.TRUE);

            case "or":
                return evalOr(args, Bool.FALSE);

            case "lambda":
                return analyzeLambda(val, args);

            case "let":
                return !args.isEmpty() && args.head().isAtom()
                    ? analyzeLet("let", val, args.tail(), (ps, as, b) ->
                        evalNamedLet((Atom)args.head(), ps, as, b))
                    : analyzeLet("let", val, args, this::evalLet);

            case "let*":
                return analyzeLet("let*", val, args, this::evalLetStar);

            case "letrec":
                return analyzeLet("letrec", val, args, this::evalLetrec);

            case "letrec*":
                return analyzeLet("letrec*", val, args, this::evalLetrecStar);

            case "do":
                return evalDoLoop(val, args);

            case "quote":
                return with(args).<$<Evaluator, LispVal>>get()
                  .when(Seq.Single(this::pure))
                  .orElseGet(() -> badSyntax("quote", val));

            case "quasiquote":
                return with(args).<$<Evaluator, LispVal>>get()
                  .when(Seq.Single(datum -> withQuoteLevel(+1, evalUnquote(datum))))
                  .orElseGet(() -> badSyntax("quasiquote", val));

            case "delay":
                return with(args).<$<Evaluator, LispVal>>get()
                  .when(Seq.Single(exp ->
                      withEnv(env -> pure(new Promise(exp, env, parser.newsym())))))
                  .orElseGet(() -> badSyntax("delay", val));
            }
        }

        return do_(eval(tag), func ->
            with(func).<$<Evaluator, LispVal>>get()
              .when(Prim(f ->
                do_(mapM(args, this::eval), f)))
              .when(Func(f ->
                do_(mapM(args, this::eval), as ->
                do_(doApply(f.params, f.vararg, f.closure, as, f.body)))))
              .when(Macro(m ->
                withNewEnv(env -> bind(doApply(m.params, m.vararg, env, args, m.body), this::eval))))
              .orElseGet(() -> throwE(new NotFunction("Unrecognized function", func.show()))));
    }

    public $<Evaluator, LispVal> apply(LispVal func, Seq<LispVal> args) {
        return with(func).<$<Evaluator, LispVal>>get()
            .when(Prim(f -> f.apply(args)))
            .when(Func(f -> doApply(f.params, f.vararg, f.closure, args, f.body)))
            .orElseGet(() -> throwE(new NotFunction("Unrecognized function", func.show())));
    }

    private $<Evaluator, LispVal>
    doApply(Seq<Atom> params, Maybe<Atom> vararg, Env closure, Seq<LispVal> args, Seq<LispVal> body) {
        int plen = params.length();
        int alen = args.length();
        if (plen != alen && (vararg.isAbsent() || plen > alen)) {
            return throwE(new NumArgs(plen, args));
        }

        return liftReader().local(
            __ -> closure.extend(params, vararg, args),
            evalSequence(body));
    }

    private $<Evaluator, LispVal> withEnv(Function<Env, $<Evaluator, LispVal>> f) {
        return bind(lift(inner().ask()), f);
    }

    private $<Evaluator, LispVal> withNewEnv(Function<Env, $<Evaluator, LispVal>> f) {
        return bind(lift(inner().ask()), env -> f.apply(new Env(env)));
    }

    private $<Evaluator, LispVal> evalE(Seq<Atom> params, Seq<LispVal> args, $<Evaluator, LispVal> body) {
        return this.<Env>liftReader().local(
            env -> env.extend(params, Maybe.empty(), args),
            body);
    }

    private $<Evaluator, LispVal> defineVar(Atom var, LispVal expr) {
        return withEnv(env -> do_(eval(expr), value ->
                              do_action(() -> env.put(var, value))));
    }

    @SuppressWarnings("unchecked")
    private $<Evaluator, LispVal> defineFunc(LispVal val, LispVal first, Seq<LispVal> rest, boolean mac) {
        return with(first).<$<Evaluator, LispVal>>get()
            .when(Cons((var, formals) -> {
                if (var.isAtom() && formals.allMatch(LispVal::isAtom)) {
                    Seq<Atom> params = (Seq<Atom>)(Seq<?>)formals;
                    return withEnv(env ->
                        do_(makeLambda(params, Maybe.empty(), rest, mac), lambda ->
                        do_action(() -> env.put((Atom)var, lambda))));
                } else {
                    return badSyntax(mac ? "defmacro" : "define", val);
                }
            }))

            .when(DottedList((formals, vararg) -> {
                if (formals.allMatch(LispVal::isAtom) && vararg.isAtom()) {
                    Atom var = (Atom)formals.head();
                    Seq<Atom> params = (Seq<Atom>)(Seq<?>)formals.tail();
                    return withEnv(env ->
                        do_(makeLambda(params, Maybe.of((Atom)vararg), rest, mac), lambda ->
                        do_action(() -> env.put(var, lambda))));
                } else {
                    return badSyntax(mac ? "defmacro" : "define", val);
                }
            }))

            .orElseGet(() -> badSyntax(mac ? "defmacro" : "define", val));
    }
    
    private $<Evaluator, LispVal> getVar(Atom var) {
        return withEnv(env -> env.lookup(var).either(
            this::pure,
            () -> unbound(var)));
    }

    private $<Evaluator, LispVal> setVar(Atom var, LispVal value) {
        return withEnv(env -> env.lookupRef(var).either(
            slot -> do_action(() -> slot.set(value)),
            ()   -> unbound(var)));
    }

    private $<Evaluator, LispVal> setCar(Ref<LispVal> var, LispVal val) {
        return with(var.get()).<$<Evaluator, LispVal>>get()
            .when(Cons((__, xs) ->
                do_action(() -> var.set(new List(Seq.cons(val, xs))))))
            .when(DottedList((xs, xt) ->
                do_action(() -> var.set(new DottedList(Seq.cons(val, xs.tail()), xt)))))
            .orElseGet(() -> throwE(new TypeMismatch("pair", var.get())));
    }

    private $<Evaluator, LispVal> setCdr(Ref<LispVal> var, LispVal val) {
        return with(var.get()).<$<Evaluator, LispVal>>get()
            .when(Cons((x, __) ->
              with(val).<$<Evaluator, LispVal>>get()
                .when(List(ys ->
                    do_action(() -> var.set(new List(Seq.cons(x, ys))))))
                .when(DottedList((ys, yt) ->
                    do_action(() -> var.set(new DottedList(Seq.cons(x, ys), yt)))))
                .orElseGet(() ->
                    do_action(() -> var.set(new DottedList(Seq.of(x), val))))))
            .when(DottedList((xs, __) ->
              with(val).<$<Evaluator, LispVal>>get()
                .when(List(ys ->
                    do_action(() -> var.set(new List(xs.append(ys))))))
                .when(DottedList((ys, yt) ->
                    do_action(() -> var.set(new DottedList(xs.append(ys), yt)))))
                .orElseGet(() ->
                    do_action(() -> var.set(new DottedList(xs, val))))))
            .orElseGet(() -> throwE(new TypeMismatch("pair", var.get())));
    }

    private $<Evaluator, LispVal> unbound(Atom var) {
        return throwE(new UnboundVar("Undefined variable", var.show()));
    }

    private $<Evaluator, LispVal> evalCond(LispVal val, Seq<LispVal> form) {
        Seq<Tuple<LispVal, Either<Seq<LispVal>, LispVal>>> clauses = analyzeCondClauses(form);
        return clauses != null ? evalCond(clauses) : badSyntax("cond", val);
    }

    private $<Evaluator, LispVal> evalCond(Seq<Tuple<LispVal, Either<Seq<LispVal>, LispVal>>> clauses) {
        return clauses.head().as((cond, action) ->
            bind(eval(cond), tval -> {
                if (isTrue(tval)) {
                    return action.either(
                        exps   -> exps.isEmpty() ? pure(tval) : evalSequence(exps),
                        result -> apply(result, tval));
                } else if (clauses.tail().isEmpty()) {
                    return pure(Void.VOID);
                } else {
                    return evalCond(clauses.tail());
                }
            }));
    }

    private Seq<Tuple<LispVal, Either<Seq<LispVal>, LispVal>>> analyzeCondClauses(Seq<LispVal> args) {
        return
          with(args).<Seq<Tuple<LispVal, Either<Seq<LispVal>, LispVal>>>>get()
            .when(Seq.Single(last ->
              with(last).<Seq<Tuple<LispVal, Either<Seq<LispVal>, LispVal>>>>get()
                .when(LispVal.Cons((h, t) ->
                  h.equals(getsym("else"))
                    ? Seq.of(Tuple.of(Bool.TRUE, Either.left(t)))
                    : Seq.of(analyzeCondClause(Seq.cons(h, t)))))
                .orElse(null)))

            .when(Seq.Cons((first, rest) ->
              first.isList()
                ? Seq.cons(analyzeCondClause(((List)first).value),
                           () -> analyzeCondClauses(rest))
                : null))

            .orElse(null);
    }

    private Tuple<LispVal, Either<Seq<LispVal>, LispVal>> analyzeCondClause(Seq<LispVal> args) {
        return with(args).<Tuple<LispVal, Either<Seq<LispVal>, LispVal>>>get()
            .when(Seq.Triple((test, mid, exp) ->
                mid.equals(getsym("=>"))
                    ? Tuple.of(test, Either.right(exp))
                    : Tuple.of(test, Either.left(Seq.of(mid, exp)))))
            .when(Seq.Cons((test, exps) ->
                test.equals(getsym("else"))
                    ? null
                    : Tuple.of(test, Either.left(exps))))
            .orElse(null);
    }

    private $<Evaluator, LispVal> evalAnd(Seq<LispVal> args, LispVal ret) {
        return args.isEmpty()
               ? pure(ret)
               : do_(eval(args.head()), result ->
                   isFalse(result) ? pure(result)
                                   : evalAnd(args.tail(), result));
    }

    private $<Evaluator, LispVal> evalOr(Seq<LispVal> args, LispVal ret) {
        return args.isEmpty()
               ? pure(ret)
               : do_(eval(args.head()), result ->
                   isTrue(result) ? pure(result)
                                  : evalOr(args.tail(), result));
    }

    @SuppressWarnings("unchecked")
    private $<Evaluator, LispVal> analyzeLambda(LispVal val, Seq<LispVal> form) {
        if (form.isEmpty() || form.tail().isEmpty()) {
            return badSyntax("lambda", val);
        }

        Seq<LispVal> body = form.tail();

        return with(form.head()).<$<Evaluator, LispVal>>get()
            .when(Atom(vararg ->
              makeLambda(Seq.nil(), Maybe.of(vararg), body, false)))

            .when(List(params ->
              params.allMatch(LispVal::isAtom)
                ? makeLambda((Seq<Atom>)(Seq<?>)params, Maybe.empty(), body, false)
                : badSyntax("lambda", val)
            ))

            .when(DottedList((params, vararg) ->
              params.allMatch(LispVal::isAtom) && vararg.isAtom()
                ? makeLambda((Seq<Atom>)(Seq<?>)params, Maybe.of((Atom)vararg), body, false)
                : badSyntax("lambda", val)))

            .orElseGet(() -> badSyntax("lambda", val));
    }

    private $<Evaluator, LispVal>
    makeLambda(Seq<Atom> params, Maybe<Atom> vararg, Seq<LispVal> body, boolean macro) {
        return macro ? pure(new Macro(params, vararg, body))
                     : withEnv(env -> pure(new Func(params, vararg, body, env)));
    }

    private $<Evaluator, LispVal> analyzeLet(String name, LispVal val, Seq<LispVal> args,
        TriFunction<Seq<Atom>, Seq<LispVal>, Seq<LispVal>, $<Evaluator, LispVal>> trans)
    {
        if (!args.isEmpty() && !args.tail().isEmpty() && args.head().isList()) {
            Seq<LispVal> pairs = ((List)args.head()).value;
            if (pairs.allMatch(p -> p.isList() &&
                                    ((List)p).value.arity(2) &&
                                    ((List)p).value.head().isAtom())) {
                return trans.apply(pairs.map(p -> (Atom)((List)p).value.head()),
                                   pairs.map(p -> ((List)p).value.tail().head()),
                                   args.tail());
            }
        }

        return badSyntax(name, val);
    }

    private $<Evaluator, LispVal> evalLet(Seq<Atom> params, Seq<LispVal> args, Seq<LispVal> body) {
        return do_(mapM(args, this::eval), vals ->
               do_(evalE(params, vals, evalSequence(body))));
    }

    private $<Evaluator, LispVal> evalNamedLet(Atom tag, Seq<Atom> params, Seq<LispVal> args, Seq<LispVal> body) {
        return  do_(mapM(args, this::eval), vals ->
                evalE(Seq.of(tag), Seq.of(Void.VOID),
                do_(makeLambda(params, Maybe.empty(), body, false), f ->
                do_(setVar(tag, f),
                do_(apply(f, vals))))));
    }

    private $<Evaluator, LispVal> evalLetStar(Seq<Atom> params, Seq<LispVal> args, Seq<LispVal> body) {
        if (params.isEmpty() || params.tail().isEmpty()) {
            return evalLet(params, args, body);
        } else {
            return do_(eval(args.head()), val ->
                   evalE(Seq.of(params.head()), Seq.of(val),
                   evalLetStar(params.tail(), args.tail(), body)));
        }
    }

    private $<Evaluator, LispVal> evalLetrec(Seq<Atom> params, Seq<LispVal> args, Seq<LispVal> body) {
        return evalE(params, params.map(Fn.pure(Void.VOID)),
            do_(mapM(args, this::eval), vals ->
            do_(zipM(params, vals, this::setVar),
            do_(evalSequence(body)))));
    }

    private $<Evaluator, LispVal> evalLetrecStar(Seq<Atom> params, Seq<LispVal> args, Seq<LispVal> body) {
        return evalE(params, params.map(Fn.pure(Void.VOID)),
            do_(zipM(params, args, (n, v) -> do_(eval(v), x -> setVar(n, x))),
            do_(evalSequence(body))));
    }

    private $<Evaluator, LispVal> evalDoLoop(LispVal val, Seq<LispVal> form) {
        // (do ((var init step) ...)
        //     (test finish ...)
        //   command ...)
        //
        // ==>
        //
        // (letrec ((loop
        //   (lambda (var ...)
        //     (if test finish (begin command ... (loop step ...)))))
        //   (loop init ...)))

        boolean check = with(form).<Boolean>get()
          .when(Seq.Cons((x, y, ys) ->
              x.isList() && ((List)x).value.allMatch(p ->
                  p.isList() && ((List)p).value.head().isAtom() &&
                  (((List)p).value.arity(2) || ((List)p).value.arity(3))) &&
              y.isList() && !((List)y).value.isEmpty()))
          .orElse(false);

        if (!check) {
            return badSyntax("do", val);
        }

        Seq<Atom>    vars;
        Seq<LispVal> inits;
        Seq<LispVal> steps;
        LispVal      test;
        Seq<LispVal> finish;
        Seq<LispVal> body;

        @SuppressWarnings("unchecked")
        Seq<List> params = (Seq<List>)(Seq<?>)((List)form.head()).value;
        vars   = params.map(p -> (Atom)p.value.head());
        inits  = params.map(p -> p.value.tail().head());
        steps  = params.map(p -> {
                    Seq<LispVal> s = p.value.tail().tail();
                    return s.isEmpty() ? p.value.head() : makeSequence(s);
                 });

        test   = ((List)form.tail().head()).value.head();
        finish = ((List)form.tail().head()).value.tail();
        body   = form.tail().tail();

        return do_(mapM(inits, this::eval), vals ->
               evalE(vars, vals, loop(again ->
                 do_(eval(test), tval ->
                 isTrue(tval)
                   ? evalSequence(finish)
                   : do_(evalSequence(body),
                     do_(mapM(steps, this::eval), sval ->
                     do_(zipM(vars, sval, this::setVar),
                     again)))))));
    }

    private LispVal makeSequence(Seq<LispVal> args) {
        return args.isEmpty()        ? Void.VOID :
               args.tail().isEmpty() ? args.head()
                                     : new List(Seq.cons(getsym("begin"), args));
    }

    private $<Evaluator, LispVal> evalSequence(Seq<LispVal> seq) {
        return foldM(Void.VOID, seq, (r, x) -> eval(x));
    }

    private $<Evaluator, LispVal> do_action(Runnable action) {
        return seqR(action(action), pure(Void.VOID));
    }

    private Atom getsym(String name) {
        return parser.getsym(name);
    }

    // -----------------------------------------------------------------------

    private final Atom QQ = getsym("quasiquote");
    private final Atom UNQ = getsym("unquote");
    private final Atom UNQS = getsym("unquote-splicing");

    private $<Evaluator, LispVal> evalUnquote(LispVal val) {
        return with(val).<$<Evaluator, LispVal>>get()
            .when(Text       (__ -> pure(val)))
            .when(Num        (__ -> pure(val)))
            .when(Bool       (__ -> pure(val)))
            .when(Promise    (__ -> pure(val)))
            .when(Atom       (__ -> pure(val)))
            .when(Void       (() -> pure(val)))
            .when(TaggedList (QQ, this::evalQuasiQuote))
            .when(TaggedList (UNQ, datum -> unquote(datum, false)))
            .when(List       (this::unquoteList))
            .when(DottedList (this::unquoteDottedList))
            .when(Vector     (this::unquoteVector))
            .orElseGet(() -> throwE(new BadSpecialForm("Unrecognized special form", val)));
    }

    private <R> ConditionCase<LispVal, $<Evaluator, R>, RuntimeException>
    TaggedList(Atom tag, Function<LispVal, $<Evaluator, R>> mapper) {
        return t -> {
            if (t.isList()) {
                Seq<LispVal> xs = ((List)t).value;
                if (!xs.isEmpty() && tag.equals(xs.head())) {
                    return () -> xs.arity(2) ? mapper.apply(xs.tail().head())
                                             : badSyntax(tag.name, t);
                }
            }
            return null;
        };
    }

    private $<Evaluator, LispVal> withQuoteLevel(int delta, $<Evaluator, LispVal> action) {
        return this.<Env>liftReader().local(env -> env.updateQL(delta), action);
    }

    private $<Evaluator, LispVal> evalQuasiQuote(LispVal datum) {
        return withQuoteLevel(+1, map(evalUnquote(datum), v ->
                                  new List(Seq.of(QQ, v))));
    }

    private $<Evaluator, LispVal> unquote(LispVal datum, boolean splicing) {
        return withQuoteLevel(-1, withEnv(env ->
            env.getQL() < 0  ? throwE(new BadSpecialForm("unquote: not in quasiquote", datum)) :
            env.getQL() == 0 ? eval(datum)
                             : map(evalUnquote(datum), v ->
                               new List(Seq.of(splicing ? UNQS : UNQ, v)))));
    }

    private $<Evaluator, LispVal> unquoteList(Seq<LispVal> lst) {
        if (lst.noneMatch(this::isSplicing)) {
            return map(lst.traverse(this, this::evalUnquote), List::new);
        } else {
            return lst.foldLeft(pure(List.NIL), this::appendSplice);
        }
    }

    private $<Evaluator, LispVal> unquoteDottedList(Seq<LispVal> head, LispVal tail) {
        if (head.noneMatch(this::isSplicing) && !isSplicing(tail)) {
            return do_(head.traverse(this, this::evalUnquote), h ->
                   do_(evalUnquote(tail), t ->
                   do_(pure(cons(h, t)))));
        } else {
            return consSplice(head.foldLeft(pure(List.NIL), this::appendSplice), tail);
        }
    }

    private $<Evaluator, LispVal> appendSplice($<Evaluator, LispVal> res, LispVal val) {
        return with(val).<$<Evaluator, LispVal>>get()
            .when(TaggedList(UNQS, datum ->
                bind(res, r -> bind(unquote(datum, true), x -> append(r, x)))))
            .orElseGet(() ->
                bind(res, r -> bind(evalUnquote(val), x -> append(r, new List(Seq.of(x))))));
    }

    private $<Evaluator, LispVal> consSplice($<Evaluator, LispVal> head, LispVal tail) {
        return with(tail).<$<Evaluator, LispVal>>get()
            .when(TaggedList(UNQS, datum ->
                bind(head, h ->
                bind(unquote(datum, true), t ->
                pure(cons(((List)h).value, t))))))
            .orElseGet(() ->
                bind(head, h ->
                bind(evalUnquote(tail), t ->
                pure(cons(((List)h).value, t)))));
    }

    private $<Evaluator, LispVal> unquoteVector(Vector<LispVal> vec) {
        return map(vec.foldLeft(pure(Vector.empty()), this::vectorSplice), Vec::new);
    }

    private $<Evaluator, Vector<LispVal>> vectorSplice($<Evaluator, Vector<LispVal>> res, LispVal val) {
        return with(val).<$<Evaluator, Vector<LispVal>>>get()
            .when(TaggedList(UNQS, datum ->
                bind(res, r -> bind(unquote(datum, true), x ->
                    x.isList() ? pure(r.append(Vector.fromList(((List)x).value)))
                               : throwE(new TypeMismatch("list", x))))))
            .orElseGet(() -> bind(res, r -> bind(evalUnquote(val), x -> pure(r.snoc(x)))));
    }

    private boolean isSplicing(LispVal val) {
        return val.isList() && !((List)val).value.isEmpty() && UNQS.equals(((List)val).value.head());
    }

    // =======================================================================

    private final PMap<String, Function<Seq<LispVal>, $<Evaluator, LispVal>>> primitives =
        HashPMap.<String, Function<Seq<LispVal>, $<Evaluator, LispVal>>>empty()
            .put("boolean?",    prim(is(Bool.class)))
            .put("symbol?",     prim(is(Atom.class)))
            .put("number?",     prim(is(Num.class)))
            .put("integer?",    prim(this::isInteger))
            .put("real?",       prim(this::isReal))
            .put("rational?",   prim(this::isRational))
            .put("string?",     prim(is(Text.class)))
            .put("vector?",     prim(is(Vec.class)))
            .put("promise?",    prim(is(Promise.class)))
            .put("list?",       prim(this::isList))
            .put("pair?",       prim(this::isPair))
            .put("null?",       prim(this::isNull))
            .put("procedure?",  prim(this::isProcedure))

            .put("eq?",         prim(this::eqv))
            .put("eqv?",        prim(this::eqv))
            .put("equal?",      prim(this::equal))

            .put("=",           numBoolBinop((x, y) -> compare(x, y) == 0))
            .put("!=",          numBoolBinop((x, y) -> compare(x, y) != 0))
            .put("<",           numBoolBinop((x, y) -> compare(x, y) < 0))
            .put(">",           numBoolBinop((x, y) -> compare(x, y) > 0))
            .put("<=",          numBoolBinop((x, y) -> compare(x, y) <= 0))
            .put(">=",          numBoolBinop((x, y) -> compare(x, y) >= 0))

            .put("+",           numericBinop(Maybe.of(0), Evaluator::plus))
            .put("-",           numericBinop(Maybe.of(0), Evaluator::minus))
            .put("*",           numericBinop(Maybe.of(1), Evaluator::times))
            .put("/",           numericBinop(Maybe.of(1), Evaluator::divide))
            .put("modulo",      numericBinop(Maybe.empty(), Evaluator::mod))
            .put("remainder",   numericBinop(Maybe.empty(), Evaluator::rem))

            .put("floor",       math_prim(Math::floor))
            .put("ceiling",     math_prim(Math::ceil))
            .put("truncate",    math_prim(Math::rint))
            .put("round",       math_prim(Math::round))
            .put("numerator",   prim(Num.class, this::numerator))
            .put("denominator", prim(Num.class, this::denominator))
            .put("exp",         math_prim(Math::exp))
            .put("log",         math_prim(Math::log))
            .put("sin",         math_prim(Math::sin))
            .put("cos",         math_prim(Math::cos))
            .put("tan",         math_prim(Math::tan))
            .put("asin",        math_prim(Math::asin))
            .put("acos",        math_prim(Math::acos))
            .put("atan",        math_prim(Math::atan))
            .put("sqrt",        math_prim(Math::sqrt))
            .put("expt",        prim(Num.class, Num.class, this::pow))

            .put("cons",        prim(this::cons))
            .put("append",      this::append)
            .put("car",         prim(this::car))
            .put("cdr",         prim(this::cdr))

            .put("symbol->string", prim(Atom.class, this::symbolToString))
            .put("string->symbol", prim(Text.class, this::stringToSymbol))

            .put("string=?",     stringBoolBinop((x, y) -> x.compareTo(y) == 0))
            .put("string<?",     stringBoolBinop((x, y) -> x.compareTo(y) < 0))
            .put("string>?",     stringBoolBinop((x, y) -> x.compareTo(y) > 0))
            .put("string<=?",    stringBoolBinop((x, y) -> x.compareTo(y) <= 0))
            .put("string>=?",    stringBoolBinop((x, y) -> x.compareTo(y) >= 0))
            .put("string-ci=?",  stringBoolBinop((x, y) -> x.compareToIgnoreCase(y) == 0))
            .put("string-ci<?",  stringBoolBinop((x, y) -> x.compareToIgnoreCase(y) < 0))
            .put("string-ci>?",  stringBoolBinop((x, y) -> x.compareToIgnoreCase(y) > 0))
            .put("string-ci<=?", stringBoolBinop((x, y) -> x.compareToIgnoreCase(y) <= 0))
            .put("string-ci>=?", stringBoolBinop((x, y) -> x.compareToIgnoreCase(y) >= 0))

            .put("make-vector",   this::make_vector)
            .put("vector",        this::vector)
            .put("vector-length", prim(Vec.class, this::vector_length))
            .put("vector-ref",    prim(Vec.class, Num.class, this::vector_ref))
            .put("vector-set!",   prim(Vec.class, Num.class, LispVal.class, this::vector_set))
            .put("vector->list",  prim(Vec.class, this::vector_to_list))
            .put("list->vector",  prim(List.class, this::list_to_vector))
            .put("vector-fill!",  prim(Vec.class, LispVal.class, this::vector_fill))

            .put("eval",        prim(this::eval))
            .put("apply",       prim(this::apply))
            .put("force",       prim(this::force))
            .put("error",       this::error)
            .put("gensym",      prim(this::gensym))
            .put("not",         prim(this::not))

            .put("load",        prim(Text.class, this::load))
            .put("read",        prim(this::read))
            .put("write",       prim(this::write))
            .put("display",     prim(this::display))
            .put("newline",     prim(this::newline))
            .put("print",       prim(this::print))
        ;

    private PMap<Atom, Ref<LispVal>> getPrimitives() {
        return primitives.foldLeftKV(HashPMap.empty(), (map, id, f) ->
            map.put(getsym(id), new Ref<>(new Prim(f))));
    }

    // ---------------------------------------------------------------------

    private Function<LispVal, $<Evaluator, LispVal>> is(Class<? extends LispVal> t) {
        return obj -> pure(Bool.valueOf(t.isInstance(obj)));
    }

    private $<Evaluator, LispVal> isInteger(LispVal obj) {
        return pure(Bool.valueOf(obj instanceof Num &&
                            (((Num)obj).value instanceof Long ||
                             ((Num)obj).value instanceof BigInteger)));
    }

    private $<Evaluator, LispVal> isReal(LispVal obj) {
        return pure(Bool.valueOf(obj instanceof Num && ((Num)obj).value instanceof Double));
    }

    private $<Evaluator, LispVal> isRational(LispVal obj) {
        return pure(Bool.valueOf(obj instanceof Num && ((Num)obj).value instanceof Rational));
    }

    private $<Evaluator, LispVal> isList(LispVal obj) {
        return pure(Bool.valueOf(obj.isList()));
    }

    private $<Evaluator, LispVal> isPair(LispVal obj) {
        return pure(Bool.valueOf((obj instanceof List && !((List)obj).value.isEmpty()) ||
                                 (obj instanceof DottedList)));
    }

    private $<Evaluator, LispVal> isNull(LispVal obj) {
        return pure(Bool.valueOf(obj instanceof List && ((List)obj).value.isEmpty()));
    }

    private $<Evaluator, LispVal> isProcedure(LispVal obj) {
        return pure(Bool.valueOf(obj instanceof Func || obj instanceof Prim));
    }

    private static boolean isFalse(LispVal val) {
        return val == Bool.FALSE;
    }

    private static boolean isTrue(LispVal val) {
        return !isFalse(val);
    }

    private $<Evaluator, LispVal> not(LispVal val) {
        return pure(Bool.valueOf(isFalse(val)));
    }

    // ---------------------------------------------------------------------

    private $<Evaluator, LispVal> eqv(LispVal x, LispVal y) {
        return pure(Bool.valueOf(x.equals(y)));
    }

    private $<Evaluator, LispVal> equal(LispVal x, LispVal y) {
        return pure(Bool.valueOf(equals(x, y)));
    }

    private static boolean equals(LispVal x, LispVal y) {
        if (x == y) {
            return true;
        }

        return with(x, y).<Boolean>get()
            .when(List(xs -> List(ys -> listEquals(xs, ys))))
            .when(DottedList((xs, xt) -> DottedList((ys, yt) ->
                listEquals(xs, ys) && equals(xt, yt))))
            .when(Vector(xs -> Vector(ys -> vectorEquals(xs, ys))))
            .orElseGet(() -> x.equals(y));
    }

    private static boolean listEquals(Seq<LispVal> xs, Seq<LispVal> ys) {
        while (!xs.isEmpty() && !ys.isEmpty()) {
            if (!equals(xs.head(), ys.head()))
                return false;
            xs = xs.tail();
            ys = ys.tail();
        }
        return xs.isEmpty() && ys.isEmpty();
    }

    private static boolean vectorEquals(Vector<LispVal> x, Vector<LispVal> y) {
        if (x.size() != y.size())
            return false;

        Iterator<LispVal> i1 = x.iterator();
        Iterator<LispVal> i2 = y.iterator();
        while (i1.hasNext() && i2.hasNext()) {
            if (!equals(i1.next(), i2.next()))
                return false;
        }
        return true;
    }

    // ---------------------------------------------------------------------

    private <A> Function<Seq<LispVal>, $<Evaluator, LispVal>>
    boolBinop(Function<LispVal, $<Evaluator, A>> unpacker, BiFunction<A, A, Boolean> op) {
        return prim((x, y) ->
            do_(unpacker.apply(x), left ->
            do_(unpacker.apply(y), right ->
            do_(pure(Bool.valueOf(op.apply(left, right)))))));
    }

    private Function<Seq<LispVal>, $<Evaluator, LispVal>>
    numBoolBinop(BiFunction<Number, Number, Boolean> op) {
        return boolBinop(this::unpackNum, op);
    }

    private static int compare(Number x, Number y) {
        if (x instanceof Double || y instanceof Double) {
            return Double.compare(x.doubleValue(), y.doubleValue());
        } else if (x instanceof BigInteger || y instanceof BigInteger) {
            return toBigInteger(x).compareTo(toBigInteger(y));
        } else {
            return Long.compare(x.longValue(), y.longValue());
        }
    }

    // ---------------------------------------------------------------------

    private Function<Seq<LispVal>, $<Evaluator, LispVal>>
    numericBinop(Maybe<Number> seed, BiFunction<Number, Number, Number> op) {
        return args -> with(args).<$<Evaluator, LispVal>>get()
          .when(Seq.Nil(() -> seed.isPresent()
                            ? pure(new Num(seed.get()))
                            : throwE(new NumArgs(2, args))))
          .when(Seq.Single(x -> seed.isPresent()
                            ? map(unpackNum(x), y -> new Num(op.apply(seed.get(), y)))
                            : throwE(new NumArgs(2, args))))
          .orElseGet(() -> do_(mapM(args, this::unpackNum), xs ->
                           do_(pure(new Num(xs.foldLeft(op).get())))));
    }

    private $<Evaluator, Number> unpackNum(LispVal val) {
        return with(val).<$<Evaluator, Number>>get()
                 .when(Num(this::pure))
                 .orElseGet(() -> throwE(new TypeMismatch("number", val)));
    }

    private static Number plus(Number x, Number y) {
        if (x instanceof Double || y instanceof Double) {
            return x.doubleValue() + y.doubleValue();
        } else if (x instanceof Rational || y instanceof Rational) {
            return reduce(Rational.valueOf(x).add(Rational.valueOf(y)));
        } else if (x instanceof BigInteger || y instanceof BigInteger) {
            return reduce(toBigInteger(x).add(toBigInteger(y)));
        } else {
            long a = x.longValue(), b = y.longValue();
            long c = a + b;
            if (((a ^ c) & (b ^ c)) < 0) {
                return BigInteger.valueOf(a).add(BigInteger.valueOf(b));
            } else {
                return c;
            }
        }
    }

    private static Number minus(Number x, Number y) {
        if (x instanceof Double || y instanceof Double) {
            return x.doubleValue() - y.doubleValue();
        } else if (x instanceof Rational || y instanceof Rational) {
            return reduce(Rational.valueOf(x).subtract(Rational.valueOf(y)));
        } else if (x instanceof BigInteger || y instanceof BigInteger) {
            return reduce(toBigInteger(x).subtract(toBigInteger(y)));
        } else {
            long a = x.longValue(), b = y.longValue();
            long c = a - b;
            if (((a ^ b) & (a ^ c)) < 0) {
                return BigInteger.valueOf(a).add(BigInteger.valueOf(b));
            } else {
                return c;
            }
        }
    }

    private static Number times(Number x, Number y) {
        if (x instanceof Double || y instanceof Double) {
            return x.doubleValue() * y.doubleValue();
        } else if (x instanceof Rational || y instanceof Rational) {
            return reduce(Rational.valueOf(x).multiply(Rational.valueOf(y)));
        } else if (x instanceof BigInteger || y instanceof BigInteger) {
            return reduce(toBigInteger(x).multiply(toBigInteger(y)));
        } else {
            long a = x.longValue(), b = y.longValue();
            long c = a * b;
            if (b != 0 && c / b != a) {
                return BigInteger.valueOf(a).multiply(BigInteger.valueOf(b));
            } else {
                return c;
            }
        }
    }

    private static Number divide(Number x, Number y) {
        if (x instanceof Double || y instanceof Double) {
            return x.doubleValue() / y.doubleValue();
        } else if (x instanceof Rational || y instanceof Rational) {
            return reduce(Rational.valueOf(x).divide(Rational.valueOf(y)));
        } else if (x instanceof BigInteger || y instanceof BigInteger) {
            BigInteger a = toBigInteger(x), b = toBigInteger(y);
            BigInteger[] divMod = a.divideAndRemainder(b);
            return divMod[1].signum() == 0 ? reduce(divMod[0]) : Rational.make(a, b);
        } else {
            long a = x.longValue(), b = y.longValue();
            return (a % b == 0) ? (a / b) : Rational.make(a, b);
        }
    }

    private static Number mod(Number x, Number y) {
        if (x instanceof Double || y instanceof Double) {
            return x.doubleValue() % y.doubleValue();
        } else if (x instanceof Rational || y instanceof Rational) {
            return reduce(Rational.valueOf(x).remainder(Rational.valueOf(y)));
        } else if (x instanceof BigInteger || y instanceof BigInteger) {
            return reduce(toBigInteger(x).mod(toBigInteger(y)));
        } else {
            return Math.floorMod(x.longValue(), y.longValue());
        }
    }

    private static Number rem(Number x, Number y) {
        if (x instanceof Double || y instanceof Double) {
            return x.doubleValue() % y.doubleValue();
        } else if (x instanceof Rational || y instanceof Rational) {
            return reduce(Rational.valueOf(x).remainder(Rational.valueOf(y)));
        } else if (x instanceof BigInteger || y instanceof BigInteger) {
            return reduce(toBigInteger(x).remainder(toBigInteger(y)));
        } else {
            return x.longValue() % y.longValue();
        }
    }

    private $<Evaluator, LispVal> numerator(Num val) {
        if (val.value instanceof Rational) {
            return pure(new Num(reduce(((Rational)val.value).numerator())));
        } else if (val.value instanceof Double) {
            return pure(new Num(Rational.valueOf(val.value).numerator().doubleValue()));
        } else {
            return pure(val);
        }
    }

    private $<Evaluator, LispVal> denominator(Num val) {
        if (val.value instanceof Rational) {
            return pure(new Num(reduce(((Rational)val.value).denominator())));
        } else if (val.value instanceof Double) {
            return pure(new Num(Rational.valueOf(val.value).denominator().doubleValue()));
        } else {
            return pure(new Num(1L));
        }
    }

    private $<Evaluator, LispVal> pow(Num x, Num y) {
        Number a = x.value, b = y.value;
        if (!(a instanceof Double) &&
              ((b instanceof Integer) ||
               (b instanceof Long && b.intValue() == b.longValue()))) {
            int n = b.intValue();
            if (a instanceof Rational) {
                return pure(new Num(reduce(((Rational)a).pow(n))));
            } else {
                return pure(new Num(reduce(toBigInteger(a).pow(n))));
            }
        } else {
            return pure(new Num(Math.pow(a.doubleValue(), b.doubleValue())));
        }
    }

    private static BigInteger toBigInteger(Number n) {
        return (n instanceof BigInteger) ? (BigInteger)n : BigInteger.valueOf(n.longValue());
    }

    private static Number reduce(BigInteger n) {
        return n.bitLength() < 64 ? n.longValue() : n;
    }

    private static Number reduce(Rational ratio) {
        BigInteger n = ratio.numerator(), d = ratio.denominator();
        if (n.signum() == 0) {
            return 0L;
        } else if (n.bitLength() < 64 && d.equals(BigInteger.ONE)) {
            return n.longValue();
        } else {
            return ratio;
        }
    }

    private Function<Seq<LispVal>, $<Evaluator, LispVal>>
    math_prim(Function<Double, Number> f) {
        return prim(Num.class, x ->
            pure(new Num(f.apply(x.value.doubleValue()))));
    }

    // ---------------------------------------------------------------------

    private $<Evaluator, LispVal> cons(LispVal x, LispVal y) {
        return with(y).<$<Evaluator, LispVal>>get()
            .when(List(ys -> pure(new List(Seq.cons(x, ys)))))
            .when(DottedList((h, t) -> pure(new DottedList(Seq.cons(x, h), t))))
            .orElseGet(() -> pure(new DottedList(Seq.of(x), y)));
    }

    private static LispVal cons(Seq<LispVal> xs, LispVal y) {
        return with(y).<LispVal>get()
            .when(List(ys -> new List(xs.append(ys))))
            .when(DottedList((ys, yt) -> new DottedList(xs.append(ys), yt)))
            .orElseGet(() -> new DottedList(xs, y));
    }

    private $<Evaluator, LispVal> append(Seq<LispVal> args) {
        return with(args).<$<Evaluator, LispVal>>get()
            .when(Seq.Nil(() -> pure(List.NIL)))
            .when(Seq.Single(this::pure))
            .when(Seq.Pair(this::append))
            .when(Seq.Cons((x, y, ys) ->
                do_(append(x, y), hd ->
                do_(append(ys), tl ->
                do_(append(hd, tl))))))
            .get();
    }

    private $<Evaluator, LispVal> append(LispVal x, LispVal y) {
        if (x instanceof List) {
            Seq<LispVal> xs = ((List)x).value;
            return with(y).<$<Evaluator, LispVal>>get()
                .when(List(ys -> pure(new List(xs.append(ys)))))
                .when(DottedList((ys, yt) -> pure(new DottedList(xs.append(ys), yt))))
                .orElseGet(() -> pure(new DottedList(xs, y)));
        } else {
            return throwE(new TypeMismatch("list", x));
        }
    }

    private $<Evaluator, LispVal> car(LispVal list) {
        return with(list).<$<Evaluator, LispVal>>get()
            .when(Cons((x, xs) -> pure(x)))
            .when(DottedList((h, t) -> pure(h.head())))
            .orElseGet(() -> throwE(new TypeMismatch("pair", list)));
    }

    private $<Evaluator, LispVal> cdr(LispVal list) {
        return with(list).<$<Evaluator, LispVal>>get()
            .when(Cons((x, xs) -> pure(new List(xs))))
            .when(DottedList((h, t) ->
                pure(h.tail().isEmpty() ? t : new DottedList(h.tail(), t))))
            .orElseGet(() -> throwE(new TypeMismatch("pair", list)));
    }

    // ---------------------------------------------------------------------

    private $<Evaluator, LispVal> symbolToString(Atom atom) {
        return pure(new Text(atom.name));
    }

    private $<Evaluator, LispVal> stringToSymbol(Text text) {
        return pure(getsym(text.value));
    }

    private Function<Seq<LispVal>, $<Evaluator, LispVal>>
    stringBoolBinop(BiFunction<String, String, Boolean> op) {
        return boolBinop(this::unpackString, op);
    }

    private $<Evaluator, String> unpackString(LispVal val) {
        return with(val).<$<Evaluator, String>>get()
            .when(Text(this::pure))
            .orElseGet(() -> throwE(new TypeMismatch("string", val)));
    }

    // ---------------------------------------------------------------------

    private $<Evaluator, LispVal>
    with_valid_dim(LispVal k, Function<Integer, $<Evaluator, LispVal>> f) {
        if ((k instanceof Num) && (((Num)k).value instanceof Long)) {
            long n = ((Num)k).value.longValue();
            if (n >= 0 && n < Integer.MAX_VALUE) {
                return f.apply((int)n);
            }
        }
        return throwE(new TypeMismatch("exact-non-negative-integer", k));
    }

    private $<Evaluator, LispVal> make_vector(Seq<LispVal> args) {
        return with(args).<$<Evaluator, LispVal>>get()
            .when(Seq.Single(k ->
                with_valid_dim(k, n -> make_vector(n, new Num(0L)))))
            .when(Seq.Pair((k, fill) ->
                with_valid_dim(k, n -> make_vector(n, fill))))
            .orElseGet(() -> throwE(new NumArgs(2, args)));
    }

    private $<Evaluator, LispVal> make_vector(int k, LispVal fill) {
        return pure(new Vec(Vector.iterate(k, __ -> fill)));
    }

    private $<Evaluator, LispVal> vector(Seq<LispVal> args) {
        return pure(new Vec(Vector.fromList(args)));
    }

    private $<Evaluator, LispVal> vector_length(Vec vec) {
        return pure(new Num((long)vec.value.size()));
    }

    private $<Evaluator, LispVal> vector_ref(Vec vec, Num k) {
        return with_valid_dim(k, i -> pure(vec.value.at(i)));
    }

    private $<Evaluator, LispVal> vector_set(Vec vec, Num k, LispVal obj) {
        return with_valid_dim(k, i -> do_action(() -> vec.value = vec.value.update(i, obj)));
    }

    private $<Evaluator, LispVal> vector_to_list(Vec vec) {
        return pure(new List(vec.value.asList()));
    }

    private $<Evaluator, LispVal> list_to_vector(List lst) {
        return pure(new Vec(Vector.fromList(lst.value)));
    }

    private $<Evaluator, LispVal> vector_fill(Vec vec, LispVal fill) {
        return do_action(() -> vec.value = vec.value.map(Fn.pure(fill)));
    }

    // ---------------------------------------------------------------------

    private $<Evaluator, LispVal> apply(LispVal fun, LispVal args) {
        return args.isList() ? apply(fun, ((List)args).value)
                             : throwE(new TypeMismatch("list", args));
    }

    private $<Evaluator, LispVal> force(LispVal t) {
        if (t instanceof Promise) {
            Promise p = (Promise)t;
            Maybe<LispVal> v = p.env.lookup(p.tag);

            if (v.isPresent()) {
                return pure(v.get());
            } else {
                return this.<Env>liftReader().local(__ -> p.env,
                    do_(eval(p.exp), val ->
                    do_(action(() -> p.env.put(p.tag, val)),
                    do_(pure(val)))));
            }
        } else {
            return pure(t);
        }
    }

    private $<Evaluator, LispVal> error(Seq<LispVal> args) {
        return throwE(new LispError(args.map(LispVal::show).show(" ", "", "")));
    }

    private $<Evaluator, LispVal> gensym() {
        return pure(parser.newsym());
    }

    // ---------------------------------------------------------------------

    private $<Evaluator, LispVal> load(Text name) {
        try (InputStream is = getInputStream(name.value)) {
            if (is == null) {
                return throwE(new LispError(name.show() + ": not found"));
            } else {
                return parse(name.value, new InputStreamReader(is, StandardCharsets.UTF_8));
            }
        } catch (Exception ex) {
            return throwE(new LispError(ex));
        }
    }

    private InputStream getInputStream(String name) throws IOException {
        Path path = Paths.get(name);
        if (Files.isRegularFile(path)) {
            return Files.newInputStream(path);
        } else {
            return getClass().getResourceAsStream(name);
        }
    }

    private $<Evaluator, LispVal> read() {
        Reader input = new InputStreamReader(System.in);
        return parser.parseExpr(parser.getStream(input)).<$<Evaluator, LispVal>>either(
            err -> throwE(new LispError.Parser(err)), this::pure);
    }

    private $<Evaluator, LispVal> write(LispVal val) {
        return do_action(() -> System.out.print(val.show()));
    }

    private $<Evaluator, LispVal> display(LispVal val) {
        return do_action(() -> {
            if (val instanceof Text)
                System.out.print(((Text)val).value);
            else
                System.out.print(val.show());
        });
    }

    private $<Evaluator, LispVal> newline() {
        return do_action(System.out::println);
    }

    private $<Evaluator, LispVal> print(LispVal val) {
        return seqR(display(val), newline());
    }

    // ---------------------------------------------------------------------

    private Function<Seq<LispVal>, $<Evaluator, LispVal>>
    prim(Supplier<$<Evaluator, LispVal>> f) {
        return args -> {
            if (args.isEmpty()) {
                return f.get();
            } else {
                return throwE(new NumArgs(0, args));
            }
        };
    }

    private Function<Seq<LispVal>, $<Evaluator, LispVal>>
    prim(Function<LispVal, $<Evaluator, LispVal>> f) {
        return args -> {
            if (args.arity(1)) {
                return f.apply(args.head());
            } else {
                return throwE(new NumArgs(1, args));
            }
        };
    }

    private <A extends LispVal> Function<Seq<LispVal>, $<Evaluator, LispVal>>
    prim(Class<A> c, Function<A, $<Evaluator, LispVal>> f) {
        return args -> {
            if (args.arity(1)) {
                LispVal x = args.head();
                if (!c.isInstance(x))
                    return throwE(new TypeMismatch(c.getSimpleName(), x));
                return f.apply(c.cast(x));
            } else {
                return throwE(new NumArgs(1, args));
            }
        };
    }

    private Function<Seq<LispVal>, $<Evaluator, LispVal>>
    prim(BiFunction<LispVal, LispVal, $<Evaluator, LispVal>> f) {
        return args -> {
            if (args.arity(2)) {
                return f.apply(args.head(), args.tail().head());
            } else {
                return throwE(new NumArgs(2, args));
            }
        };
    }

    private <A extends LispVal, B extends LispVal> Function<Seq<LispVal>, $<Evaluator, LispVal>>
    prim(Class<A> c1, Class<B> c2, BiFunction<A, B, $<Evaluator, LispVal>> f) {
        return args -> {
            if (args.arity(2)) {
                LispVal x = args.head(), y = args.tail().head();
                if (!c1.isInstance(x))
                    return throwE(new TypeMismatch(c1.getSimpleName(), x));
                if (!c2.isInstance(y))
                    return throwE(new TypeMismatch(c2.getSimpleName(), y));
                return f.apply(c1.cast(x), c2.cast(y));
            } else {
                return throwE(new NumArgs(2, args));
            }
        };
    }


    private <A extends LispVal, B extends LispVal, C extends LispVal> Function<Seq<LispVal>, $<Evaluator, LispVal>>
    prim(Class<A> c1, Class<B> c2, Class<C> c3, TriFunction<A, B, C, $<Evaluator, LispVal>> f) {
        return args -> {
            if (args.arity(3)) {
                LispVal x = args.head();
                LispVal y = args.tail().head();
                LispVal z = args.tail().tail().head();

                if (!c1.isInstance(x))
                    return throwE(new TypeMismatch(c1.getSimpleName(), x));
                if (!c2.isInstance(y))
                    return throwE(new TypeMismatch(c2.getSimpleName(), y));
                if (!c3.isInstance(z))
                    return throwE(new TypeMismatch(c3.getSimpleName(), z));

                return f.apply(c1.cast(x), c2.cast(y), c3.cast(z));
            } else {
                return throwE(new NumArgs(3, args));
            }
        };
    }
}
