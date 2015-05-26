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
import com.cloudway.fp.data.Vector;
import com.cloudway.fp.function.TriFunction;
import com.cloudway.fp.parser.Stream;

import static com.cloudway.fp.control.Conditionals.with;
import static com.cloudway.fp.control.Conditionals.inCaseOf;
import static com.cloudway.fp.control.Syntax.do_;
import static com.cloudway.fp.control.Syntax.loop;
import static com.cloudway.fp.data.Either.left;
import static com.cloudway.fp.data.Either.right;
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
        return Trampoline.run(ReaderT.run(runExcept(exp), env));
    }

    public Either<LispError, LispVal> run($<Evaluator, LispVal> exp) {
        return run(new Env(getPrimitives()), exp);
    }

    public Either<LispError, LispVal> evaluate(String expr) {
        return run(seqR(stdlib, parse(expr)));
    }

    public void runREPL() throws IOException {
        Env env = new Env(getPrimitives());
        run(env, stdlib).getOrThrow(Fn.id());

        BufferedReader input = new BufferedReader(new InputStreamReader(System.in));
        String line;

        // A very simple Read-Eval-Print Loop
        while (true) {
            System.out.print("> ");
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

    public $<Evaluator, LispVal> eval(LispVal exp) {
        return with(exp).<$<Evaluator, LispVal>>get()
            .when(Data   (this::pure))
            .when(Symbol (this::getVar))
            .when(Cons   ((tag, args) -> evalList(exp, tag, args)))
            .orElseGet(() -> throwE(new BadSpecialForm("Unrecognized special form", exp)));
    }

    private <T> $<Evaluator, T> badSyntax(String name, LispVal val) {
        return throwE(new BadSpecialForm(name + ": bad syntax", val));
    }

    private $<Evaluator, LispVal> unbound(Symbol var) {
        return throwE(new UnboundVar("Undefined variable", var.show()));
    }

    private $<Evaluator, LispVal> evalList(LispVal val, LispVal tag, LispVal args) {
        if (tag.isSymbol()) {
            switch (((Symbol)tag).name) {
            case "begin":
                return evalSequence(args);

            case "define":
                return with(args).<$<Evaluator, LispVal>>get()
                  .when(List((first, value) ->
                    first.isSymbol()
                        ? defineVar((Symbol)first, value)
                        : defineFunc(first, Pair.of(value))))
                  .when(Cons(this::defineFunc))
                  .orElseGet(() -> badSyntax("define", val));

            case "defmacro":
                return with(args).<$<Evaluator, LispVal>>get()
                  .when(Cons(this::defineMacro))
                  .orElseGet(() -> badSyntax("defmacro", val));

            case "set!":
                return with(args).<$<Evaluator, LispVal>>get()
                  .when(List((id, value) ->
                    id.isSymbol()
                      ? do_(eval(value), res -> setVar((Symbol)id, res))
                      : throwE(new BadSpecialForm("set! not an identifier", id))))
                  .orElseGet(() -> badSyntax("set!", val));

            case "if":
                return with(args).<$<Evaluator, LispVal>>get()
                  .when(List((pred, conseq) ->
                    do_(eval(pred), result -> isFalse(result) ? pure(Void.VOID) : eval(conseq))))
                  .when(List((pred, conseq, alt) ->
                    do_(eval(pred), result -> isFalse(result) ? eval(alt) : eval(conseq))))
                  .orElseGet(() -> badSyntax("if", val));

            case "cond":
                return evalCond(args);

            case "match":
                return with(args).<$<Evaluator, LispVal>>get()
                  .when(Cons((exp, clauses) ->
                    bind(eval(exp), mval -> evalMatch(mval, clauses))))
                  .orElseGet(() -> badSyntax("match", val));

            case "and":
                return evalAnd(args, Bool.TRUE);

            case "or":
                return evalOr(args, Bool.FALSE);

            case "lambda":
                return analyzeLambda(args);

            case "let":
                return args.isPair() && ((Pair)args).head.isSymbol()
                    ? analyzeLet("let", ((Pair)args).tail, (ps, as, b) ->
                        evalNamedLet((Symbol)((Pair)args).head, ps, as, b))
                    : analyzeLet("let", args, this::evalLet);

            case "let*":
                return analyzeLet("let*", args, this::evalLetStar);

            case "letrec":
                return analyzeLet("letrec", args, this::evalLetrec);

            case "letrec*":
                return analyzeLet("letrec*", args, this::evalLetrecStar);

            case "let-optionals":
                return with(args).<$<Evaluator, LispVal>>get()
                  .when(Cons((first, rest) ->
                    analyzeLet("let-optionals", rest, (ps, as, b) ->
                      do_(eval(first), values ->
                      do_(values.toList(this), vs ->
                      evalLetOptionals(ps, as, vs, b))))))
                  .orElseGet(() -> badSyntax("let-optionals", val));

            case "do":
                return evalDoLoop(args);

            case "quote":
                return with(args).<$<Evaluator, LispVal>>get()
                  .when(List(this::pure))
                  .orElseGet(() -> badSyntax("quote", val));

            case "quasiquote":
                return with(args).<$<Evaluator, LispVal>>get()
                  .when(List(datum -> withQuoteLevel(+1, evalUnquote(datum))))
                  .orElseGet(() -> badSyntax("quasiquote", val));

            case "delay":
                return args.isList()
                    ? withEnv(env -> pure(new Promise(args, env)))
                    : badSyntax("delay", val);
            }
        }

        return do_(eval(tag), func ->
            with(func).<$<Evaluator, LispVal>>get()
              .when(Prim(f ->
                do_(args.mapM(this, this::eval), f)))
              .when(Func(f ->
                do_(args.mapM(this, this::eval), as ->
                do_(applyFunc(f, as)))))
              .when(Macro(m ->
                applyMacro(m, args)))
              .orElseGet(() -> throwE(new NotFunction("Unrecognized function", func.show()))));
    }

    public $<Evaluator, LispVal> apply(LispVal func, LispVal args) {
        return with(func).<$<Evaluator, LispVal>>get()
            .when(Prim(f -> f.apply(args)))
            .when(Func(f -> applyFunc(f, args)))
            .orElseGet(() -> throwE(new NotFunction("Unrecognized function", func.show())));
    }

    private $<Evaluator, LispVal> applyFunc(Func f, LispVal args) {
        return do_(bindVars(f.params, args), bindings ->
               liftReader().local(__ -> f.closure.extend(bindings), evalSequence(f.body)));
    }

    private $<Evaluator, LispVal> applyMacro(Macro m, LispVal args) {
        return match(m.pattern, args, HashPMap.empty()).either(
            this::<LispVal>throwE,
            bindings -> bind(this.<Env>liftReader().local(
                env -> env.extend(bindings),
                evalSequence(m.body)), this::eval));
    }

    private $<Evaluator, LispVal> evalE(Seq<Symbol> params, Seq<LispVal> args, $<Evaluator, LispVal> body) {
        return this.<Env>liftReader().local(env -> env.extend(bindVars(params, args)), body);
    }

    private $<Evaluator, PMap<Symbol, Ref<LispVal>>> bindVars(LispVal params, LispVal args) {
        PMap<Symbol, Ref<LispVal>> bindings = HashPMap.empty();
        int nvars = 0;

        while (params.isPair() && args.isPair()) {
            Pair pp = (Pair)params, pv = (Pair)args;
            Symbol var = (Symbol)pp.head;

            if (bindings.containsKey(var)) {
                return throwE(new LispError("duplicate variable " + var.show()));
            }

            bindings = bindings.put(var, new Ref<>(pv.head));
            params   = pp.tail;
            args     = pv.tail;
            nvars++;
        }

        if (params.isPair()) {
            // less arguments than parameters
            do {
                nvars++;
                params = ((Pair)params).tail;
            } while (params.isSymbol());
            return throwE(new NumArgs(nvars, args));
        }

        if (params.isNil() && !args.isNil()) {
            // more arguments than parameters
            return throwE(new NumArgs(nvars, args));
        }

        if (params.isSymbol()) {
            // varargs parameter
            bindings = bindings.put((Symbol)params, new Ref<>(args));
        }

        return pure(bindings);
    }

    private static PMap<Symbol, Ref<LispVal>> bindVars(Seq<Symbol> params, Seq<LispVal> args) {
        PMap<Symbol, Ref<LispVal>> bindings = HashPMap.empty();
        while (!params.isEmpty()) {
            bindings = bindings.put(params.head(), new Ref<>(args.head()));
            params = params.tail();
            args = args.tail();
        }
        return bindings;
    }

    private $<Evaluator, LispVal> withEnv(Function<Env, $<Evaluator, LispVal>> f) {
        return bind(lift(inner().ask()), f);
    }

    private $<Evaluator, LispVal> defineVar(Symbol var, LispVal expr) {
        return withEnv(env -> do_(eval(expr), value ->
                              do_action(() -> env.put(var, value))));
    }

    @SuppressWarnings("unchecked")
    private $<Evaluator, LispVal> defineFunc(LispVal first, LispVal body) {
        return with(first).<$<Evaluator, LispVal>>get()
            .when(Cons((var, formals) ->
                var.isSymbol() && checkLambda(formals, body)
                  ? withEnv(env -> do_action(() ->
                      env.put((Symbol)var, new Func(formals, body, env))))
                  : badSyntax("define", first)))
            .orElseGet(() -> badSyntax("define", first));
    }

    private $<Evaluator, LispVal> defineMacro(LispVal first, LispVal body) {
        return with(first).<$<Evaluator, LispVal>>get()
            .when(Cons((var, pattern) ->
                var.isSymbol() && checkMacro(pattern, body)
                  ? withEnv(env -> do_action(() ->
                      env.put((Symbol)var, new Macro(pattern, body))))
                  : badSyntax("defmacro", first)))
            .orElseGet(() -> badSyntax("defmacro", first));
    }

    private static boolean checkMacro(LispVal pattern, LispVal body) {
        return isPattern(pattern) && body.isList() && !body.isNil();
    }

    private $<Evaluator, LispVal> getVar(Symbol var) {
        return withEnv(env -> env.lookup(var).either(
            this::pure,
            () -> unbound(var)));
    }

    private $<Evaluator, LispVal> setVar(Symbol var, LispVal value) {
        return withEnv(env -> env.lookupRef(var).either(
            slot -> do_action(() -> slot.set(value)),
            ()   -> unbound(var)));
    }

    // -----------------------------------------------------------------------

    private final Symbol ELSE = getsym("else");
    private final Symbol WHEN = getsym(":when");

    private abstract class Cond {
        final LispVal cond;

        Cond(LispVal cond) {
            this.cond = cond;
        }

        abstract $<Evaluator, LispVal> apply(LispVal tval);
    }

    private class ExpCond extends Cond {
        final LispVal exp;

        ExpCond(LispVal cond, LispVal exp) {
            super(cond);
            this.exp = exp;
        }

        @Override
        $<Evaluator, LispVal> apply(LispVal tval) {
            return exp.isNil() ? pure(tval) : evalSequence(exp);
        }
    }

    private class RecipientCond extends Cond {
        final LispVal recipient;

        RecipientCond(LispVal cond, LispVal recipient) {
            super(cond);
            this.recipient = recipient;
        }

        @Override
        $<Evaluator, LispVal> apply(LispVal tval) {
            return bind(eval(recipient), proc ->
                   Evaluator.this.apply(proc, Pair.of(tval)));
        }
    }

    private class ErrorCond extends Cond {
        ErrorCond(LispVal cond) {
            super(cond);
        }

        @Override
        $<Evaluator, LispVal> apply(LispVal tval) {
            return badSyntax("cond", cond);
        }
    }

    @SuppressWarnings("RedundantTypeArguments")
    private $<Evaluator, LispVal> evalCond(LispVal form) {
        Seq<Cond> clauses = analyzeCondClauses(form);
        return clauses.findFirst(x -> x instanceof ErrorCond).<$<Evaluator, LispVal>>either(
            err -> badSyntax("cond", err.cond), () -> evalCond(clauses)
        );
    }

    private $<Evaluator, LispVal> evalCond(Seq<Cond> clauses) {
        Cond cond = clauses.head();
        return bind(eval(cond.cond), tval -> {
            if (isTrue(tval)) {
                return cond.apply(tval);
            } else if (clauses.tail().isEmpty()) {
                return pure(Void.VOID);
            } else {
                return evalCond(clauses.tail());
            }
        });
    }

    private Seq<Cond> analyzeCondClauses(LispVal args) {
        return with(args).<Seq<Cond>>get()
            .when(List(last ->
              with(last).<Seq<Cond>>get()
                .when(Cons((h, t) ->
                  h.equals(ELSE)
                    ? Seq.of(new ExpCond(Bool.TRUE, t))
                    : Seq.of(analyzeCondClause(last))))
                .orElseGet(() -> Seq.of(new ErrorCond(args)))))

            .when(Cons((first, rest) ->
              Seq.cons(analyzeCondClause(first), () -> analyzeCondClauses(rest))))

            .orElseGet(() -> Seq.of(new ErrorCond(args)));
    }

    private Cond analyzeCondClause(LispVal args) {
        return with(args).<Cond>get()
            .when(List((test, mid, exp) ->
              mid.equals(getsym("=>"))
                ? new RecipientCond(test, exp)
                : new ExpCond(test, Pair.of(mid, exp))))

            .when(Cons((test, exps) ->
               test.equals(ELSE) || !exps.isList()
                ? new ErrorCond(args)
                : new ExpCond(test, exps)))

            .orElseGet(() -> new ErrorCond(args));
    }

    // -----------------------------------------------------------------------

    private static class Match {
        final LispVal pattern;
        final LispVal guard;
        final LispVal body;

        Match(LispVal pattern, LispVal guard, LispVal body) {
            this.pattern = pattern;
            this.guard   = guard;
            this.body    = body;
        }
    }

    private static class ErrorMatch extends Match {
        ErrorMatch(LispVal pattern) {
            super(pattern, Void.VOID, Void.VOID);
        }
    }

    @SuppressWarnings("RedundantTypeArguments")
    private $<Evaluator, LispVal> evalMatch(LispVal exp, LispVal args) {
        Seq<Match> clauses = analyzeMatchClauses(args);
        return clauses.findFirst(x -> x instanceof ErrorMatch).<$<Evaluator, LispVal>>either(
            err -> badSyntax("match", err.pattern),
            () -> evalMatch(exp, clauses)
        );
    }

    private $<Evaluator, LispVal> evalMatch(LispVal exp, Seq<Match> clauses) {
        Match m = clauses.head();
        return match(m.pattern, exp, HashPMap.empty()).<$<Evaluator, LispVal>>either(
            err -> clauses.tail().isEmpty()
                ? throwE(err)
                : evalMatch(exp, clauses.tail()),

            bindings -> this.<Env>liftReader().local(
                env -> env.extend(bindings),
                do_(eval(m.guard), tval ->
                  isTrue(tval)
                    ? evalSequence(m.body) :
                  clauses.tail().isEmpty()
                    ? throwE(new PatternMismatch(m.guard, exp))
                    : evalMatch(exp, clauses.tail()))));
    }

    private Seq<Match> analyzeMatchClauses(LispVal args) {
        return with(args).<Seq<Match>>get()
            .when(List(last ->
              with(last).<Seq<Match>>get()
                .when(Cons((h, t) ->
                  h.equals(ELSE)
                    ? Seq.of(new Match(getsym("_"), Bool.TRUE, t))
                    : Seq.of(analyzeMatchClause(last))))
                .orElseGet(() -> Seq.of(new ErrorMatch(last)))))

            .when(Cons((first, rest) ->
              Seq.cons(analyzeMatchClause(first), () -> analyzeMatchClauses(rest))))

            .orElseGet(() -> Seq.of(new ErrorMatch(args)));
    }

    private Match analyzeMatchClause(LispVal args) {
        return with(args).<Match>get()
            .when(Cons((pat, rest) ->
              pat.equals(ELSE) || !rest.isList()
                ? new ErrorMatch(args)
                : with(rest).<Match>get()
                    .when(Cons((key, guard, exps) ->
                      key.equals(WHEN)
                        ? new Match(pat, guard, exps)
                        : new Match(pat, Bool.TRUE, rest)))
                    .orElseGet(() -> new Match(pat, Bool.TRUE, rest))))
            .orElseGet(() -> new ErrorMatch(args));
    }

    // -----------------------------------------------------------------------

    private $<Evaluator, LispVal> evalAnd(LispVal args, LispVal ret) {
        if (args.isNil()) {
            return pure(ret);
        }

        if (args.isPair()) {
            Pair p = (Pair)args;
            return do_(eval(p.head), result ->
                   isFalse(result) ? pure(result)
                                   : evalAnd(p.tail, result));
        }

        return throwE(new TypeMismatch("pair", args));
    }

    private $<Evaluator, LispVal> evalOr(LispVal args, LispVal ret) {
        if (args.isNil()) {
            return pure(ret);
        }

        if (args.isPair()) {
            Pair p = (Pair)args;
            return do_(eval(p.head), result ->
                   isTrue(result) ? pure(result)
                                  : evalOr(p.tail, result));
        }

        return throwE(new TypeMismatch("pair", args));
    }

    // -----------------------------------------------------------------------

    @SuppressWarnings("unchecked")
    private $<Evaluator, LispVal> analyzeLambda(LispVal form) {
        return with(form).<$<Evaluator, LispVal>>get()
            .when(Cons((formals, body) ->
              checkLambda(formals, body)
                ? makeLambda(formals, body)
                : badSyntax("lambda", form)))
            .orElseGet(() -> badSyntax("lambda", form));
    }

    private static boolean checkLambda(LispVal formals, LispVal body) {
        return formals.allMatch(LispVal::isSymbol) && body.isList() && !body.isNil();
    }

    private $<Evaluator, LispVal> makeLambda(LispVal params, LispVal body) {
        return withEnv(env -> pure(new Func(params, body, env)));
    }

    // -----------------------------------------------------------------------

    private static final class LetParams {
        Seq<Symbol>  vars  = Seq.nil();
        Seq<LispVal> inits = Seq.nil();
    }

    private $<Evaluator, LispVal> analyzeLet(String name, LispVal form,
            TriFunction<Seq<Symbol>, Seq<LispVal>, LispVal, $<Evaluator, LispVal>> trans) {
        return with(form).<$<Evaluator, LispVal>>get()
            .when(Cons((params, body) ->
                do_(analyzeLetParams(name, params), lp ->
                trans.apply(lp.vars, lp.inits, body))))
            .orElseGet(() -> badSyntax(name, form));
    }

    private $<Evaluator, LetParams> analyzeLetParams(String name, LispVal p) {
        LetParams lp = new LetParams();

        for (; p.isPair(); p = ((Pair)p).tail) {
            boolean ok = with(((Pair)p).head).<Boolean>get()
                .when(List((var, init) -> {
                    if (var.isSymbol()) {
                        lp.vars  = Seq.cons((Symbol)var, lp.vars);
                        lp.inits = Seq.cons(init, lp.inits);
                        return true;
                    } else {
                        return false;
                    }
                })).orElse(false);

            if (!ok) {
                return badSyntax(name, ((Pair)p).head);
            }
        }

        if (p.isNil()) {
            lp.vars  = lp.vars.reverse();
            lp.inits = lp.inits.reverse();
            return pure(lp);
        } else {
            return badSyntax(name, p);
        }
    }

    private $<Evaluator, LispVal> evalLet(Seq<Symbol> params, Seq<LispVal> inits, LispVal body) {
        return do_(mapM(inits, this::eval), args ->
               do_(evalE(params, args, evalSequence(body))));
    }

    private $<Evaluator, LispVal>
    evalNamedLet(Symbol tag, Seq<Symbol> params, Seq<LispVal> inits, LispVal body) {
        return do_(mapM(inits, this::eval), args ->
               evalE(Seq.of(tag), Seq.of(Void.VOID),
               do_(makeLambda(Pair.fromList(params), body), f ->
               do_(setVar(tag, f),
               do_(apply(f, Pair.fromList(args)))))));
    }

    private $<Evaluator, LispVal>
    evalLetOptionals(Seq<Symbol> params, Seq<LispVal> defaults, Seq<LispVal> values, LispVal body) {
        int vlen;
        if ((vlen = values.length()) < params.length()) {
            values = values.append(defaults.drop(vlen));
        }
        return evalLet(params, values, body);
    }

    private $<Evaluator, LispVal> evalLetStar(Seq<Symbol> params, Seq<LispVal> args, LispVal body) {
        if (params.isEmpty() || params.tail().isEmpty()) {
            return evalLet(params, args, body);
        } else {
            return do_(eval(args.head()), val ->
                   evalE(Seq.of(params.head()), Seq.of(val),
                   evalLetStar(params.tail(), args.tail(), body)));
        }
    }

    private $<Evaluator, LispVal> evalLetrec(Seq<Symbol> params, Seq<LispVal> args, LispVal body) {
        return evalE(params, params.map(Fn.pure(Void.VOID)),
               do_(mapM(args, this::eval), vals ->
               do_(zipM(params, vals, this::setVar),
               do_(evalSequence(body)))));
    }

    private $<Evaluator, LispVal> evalLetrecStar(Seq<Symbol> params, Seq<LispVal> args, LispVal body) {
        return evalE(params, params.map(Fn.pure(Void.VOID)),
               do_(zipM(params, args, (n, v) -> do_(eval(v), x -> setVar(n, x))),
               do_(evalSequence(body))));
    }

    // -----------------------------------------------------------------------

    private static final class DoParams {
        Seq<Symbol>  vars  = Seq.nil();
        Seq<LispVal> inits = Seq.nil();
        Seq<LispVal> steps = Seq.nil();
    }

    private $<Evaluator, LispVal> evalDoLoop(LispVal form) {
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

        return
          with(form).<$<Evaluator, LispVal>>get()
            .when(Cons((params, exit, body) ->
              with(exit).<$<Evaluator, LispVal>>get()
                .when(Cons((test, finish) ->
                  do_(analyzeDoParams(params), dp ->
                  do_(mapM(dp.inits, this::eval), vals ->
                  evalE(dp.vars, vals, loop(again ->
                    do_(eval(test), tval ->
                    isTrue(tval)
                      ? evalSequence(finish)
                      : do_(evalSequence(body),
                        do_(mapM(dp.steps, this::evalSequence), sval ->
                        do_(zipM(dp.vars, sval, this::setVar),
                        again))))))))))
                .orElseGet(() -> badSyntax("do", exit))))
            .orElseGet(() -> badSyntax("do", form));
    }

    private $<Evaluator, DoParams> analyzeDoParams(LispVal p) {
        DoParams dp = new DoParams();

        for (; p.isPair(); p = ((Pair)p).tail) {
            boolean ok = with(((Pair)p).head).<Boolean>get()
                .when(Cons((var, init, step) -> {
                    if (var.isSymbol()) {
                        dp.vars  = Seq.cons((Symbol)var, dp.vars);
                        dp.inits = Seq.cons(init, dp.inits);
                        dp.steps = Seq.cons(step.isNil() ? Pair.of(var) : step, dp.steps);
                        return true;
                    } else {
                        return false;
                    }
                })).orElse(false);

            if (!ok) {
                return badSyntax("do", ((Pair)p).head);
            }
        }

        if (p.isNil()) {
            dp.vars  = dp.vars.reverse();
            dp.inits = dp.inits.reverse();
            dp.steps = dp.steps.reverse();
            return pure(dp);
        } else {
            return badSyntax("do", p);
        }
    }

    // -----------------------------------------------------------------------

    private $<Evaluator, LispVal> evalSequence(LispVal seq) {
        if (seq.isNil()) {
            return pure(Void.VOID);
        }

        if (seq.isPair()) {
            Pair p = (Pair)seq;
            if (p.tail.isNil()) {
                return eval(p.head);
            } else {
                return seqR(eval(p.head), () -> evalSequence(p.tail));
            }
        }

        return throwE(new BadSpecialForm("Unrecognized sequence", seq));
    }

    private $<Evaluator, LispVal> do_action(Runnable action) {
        return seqR(action(action), pure(Void.VOID));
    }

    private Symbol getsym(String name) {
        return parser.getsym(name);
    }

    // -----------------------------------------------------------------------

    private static boolean isPattern(LispVal val) {
        return with(val).<Boolean>get()
            .when(Text   (__ -> true))
            .when(Num    (__ -> true))
            .when(Bool   (__ -> true))
            .when(Symbol (__ -> true))
            .when(Nil    (() -> true))
            .when(Pair   (lst -> lst.allMatch(Evaluator::isPattern)))
            .orElse(false);
    }

    private static Either<LispError, PMap<Symbol, Ref<LispVal>>>
    match(LispVal pattern, LispVal value, PMap<Symbol, Ref<LispVal>> bindings) {
        return with(pattern).<Either<LispError, PMap<Symbol, Ref<LispVal>>>>get()
            .when(Text   (__ -> matchConst(pattern, value, bindings)))
            .when(Num    (__ -> matchConst(pattern, value, bindings)))
            .when(Bool   (__ -> matchConst(pattern, value, bindings)))
            .when(Nil    (() -> matchConst(pattern, value, bindings)))
            .when(Symbol (var -> matchVariable(var, value, bindings)))
            .when(Quoted (dat -> matchConst(dat, value, bindings)))
            .when(Pair   (lst -> matchPair(lst, value, bindings)))
            .orElseGet(() -> left(new PatternMismatch(pattern, value)));
    }

    private static Either<LispError, PMap<Symbol, Ref<LispVal>>>
    matchConst(LispVal pattern, LispVal value, PMap<Symbol, Ref<LispVal>> bindings) {
        return pattern.equals(value)
            ? right(bindings)
            : left(new PatternMismatch(pattern, value));
    }

    private static Either<LispError, PMap<Symbol, Ref<LispVal>>>
    matchVariable(Symbol var, LispVal value, PMap<Symbol, Ref<LispVal>> bindings) {
        if ("_".equals(var.name)) {
            return right(bindings);
        }

        Maybe<Ref<LispVal>> bound_var = bindings.lookup(var);
        if (bound_var.isPresent()) {
            LispVal bound_val = bound_var.get().get();
            if (bound_val.equals(value)) {
                return right(bindings);
            } else {
                return left(new PatternMismatch(Pair.of(var, bound_val), value));
            }
        }

        return right(bindings.put(var, new Ref<>(value)));
    }

    private static Either<LispError, PMap<Symbol, Ref<LispVal>>>
    matchPair(Pair pattern, LispVal value, PMap<Symbol, Ref<LispVal>> bindings) {
        if (value.isPair()) {
            Pair pv = (Pair)value;
            Either<LispError, PMap<Symbol, Ref<LispVal>>> res
                = match(pattern.head, pv.head, bindings);
            return res.isLeft() ? res : match(pattern.tail, pv.tail, res.right());
        } else {
            return left(new PatternMismatch(pattern, value));
        }
    }

    // -----------------------------------------------------------------------

    private final Symbol Q = getsym("quote");
    private final Symbol QQ = getsym("quasiquote");
    private final Symbol UNQ = getsym("unquote");
    private final Symbol UNQS = getsym("unquote-splicing");

    private $<Evaluator, LispVal> evalUnquote(LispVal exp) {
        return with(exp).<$<Evaluator, LispVal>>get()
            .when(Pair   (this::unquotePair))
            .when(Vector (this::unquoteVector))
            .when(Data   (__ -> pure(exp)))
            .when(Symbol (__ -> pure(exp)))
            .when(Nil    (() -> pure(exp)))
            .orElseGet(() -> throwE(new BadSpecialForm("Unrecognized special form", exp)));
    }

    private $<Evaluator, LispVal> withQuoteLevel(int delta, $<Evaluator, LispVal> action) {
        return this.<Env>liftReader().local(env -> env.updateQL(delta), action);
    }

    private $<Evaluator, LispVal> unquote(LispVal datum, boolean splicing) {
        return withQuoteLevel(-1, withEnv(env ->
            env.getQL() < 0  ? throwE(new BadSpecialForm("unquote: not in quasiquote", datum)) :
            env.getQL() == 0 ? eval(datum)
                             : map(evalUnquote(datum), x ->
                               Pair.of(splicing ? UNQS : UNQ, x))));
    }

    private $<Evaluator, LispVal> unquotePair(LispVal val) {
        return with(val).<$<Evaluator, LispVal>>get()
            .when(TaggedList(QQ, datum -> withQuoteLevel(+1,
                                          map(evalUnquote(datum), x ->
                                          Pair.of(QQ, x)))))

            .when(TaggedList(UNQ,  datum -> unquote(datum, false)))
            .when(TaggedList(UNQS, datum -> unquote(datum, true)))

            .when(Cons(this::unquotePair))

            .orElseGet(() -> evalUnquote(val));
    }

    private $<Evaluator, LispVal> unquotePair(LispVal hd, LispVal tl) {
        return with(hd).<$<Evaluator, LispVal>>get()
            .when(TaggedList(UNQS, datum ->
              do_(unquote(datum, true), xs ->
              do_(unquotePair(tl), ys ->
              append(xs, ys)))))

            .orElseGet(() ->
              do_(evalUnquote(hd), x ->
              do_(unquotePair(tl), ys ->
              pure(new Pair(x, ys)))));
    }

    private $<Evaluator, LispVal> unquoteVector(Vector<LispVal> vec) {
        return map(vec.foldLeft(pure(Vector.empty()), this::vectorSplice), Vec::new);
    }

    private $<Evaluator, Vector<LispVal>>
    vectorSplice($<Evaluator, Vector<LispVal>> res, LispVal val) {
        return with(val).<$<Evaluator, Vector<LispVal>>>get()
            .when(TaggedList(UNQS, datum ->
              do_(res, r ->
              do_(unquote(datum, true), x ->
              do_(x.toList(this), xs ->
              pure(r.append(Vector.fromList(xs))))))))

            .orElseGet(() ->
              do_(res, r ->
              do_(evalUnquote(val), x ->
              pure(r.snoc(x)))));
    }

    private <R> ConditionCase<LispVal, $<Evaluator, R>, RuntimeException>
    TaggedList(Symbol tag, Function<LispVal, $<Evaluator, R>> mapper) {
        return t -> {
            if (t.isPair()) {
                Pair p = (Pair)t;
                if (tag.equals(p.head) && p.tail.isPair()) {
                    Pair pp = (Pair)p.tail;
                    return pp.tail.isNil()
                        ? () -> mapper.apply(pp.head)
                        : () -> badSyntax(tag.name, t);
                }
            }
            return null;
        };
    }

    // =======================================================================

    private final PMap<String, Function<LispVal, $<Evaluator, LispVal>>> primitives =
        HashPMap.<String, Function<LispVal, $<Evaluator, LispVal>>>empty()
            .put("boolean?",    prim(is(Bool.class)))
            .put("symbol?",     prim(is(Symbol.class)))
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
            .put("car",         prim(Pair.class, this::car))
            .put("cdr",         prim(Pair.class, this::cdr))
            .put("set-car!",    prim(Pair.class, LispVal.class, this::set_car))
            .put("set-cdr!",    prim(Pair.class, LispVal.class, this::set_cdr))
            .put("length",      prim(this::length))
            .put("append",      this::append)
            .put("reverse",     prim(this::reverse))
            .put("fold-left",   prim(this::foldl))
            .put("fold-right",  prim(this::foldr))
            .put("map",         prim(this::_map))
            .put("flatmap",     prim(this::flatmap))
            .put("filter",      prim(this::filter))
            .put("for-each",    prim(this::for_each))

            .put("symbol->string", prim(Symbol.class, this::symbolToString))
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
            .put("list->vector",  prim(this::list_to_vector))
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

    private PMap<Symbol, Ref<LispVal>> getPrimitives() {
        return primitives.foldLeftKV(HashPMap.empty(), (map, id, f) ->
            map.put(getsym(id), new Ref<>(new Prim(id, f))));
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
        return pure(Bool.valueOf(obj.isPair()));
    }

    private $<Evaluator, LispVal> isNull(LispVal obj) {
        return pure(Bool.valueOf(obj.isNil()));
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
            .when(Pair(xs -> Pair(ys -> listEquals(xs, ys))))
            .when(Vector(xs -> Vector(ys -> vectorEquals(xs, ys))))
            .orElseGet(() -> x.equals(y));
    }

    private static boolean listEquals(LispVal xs, LispVal ys) {
        while (xs.isPair() && ys.isPair()) {
            Pair px = (Pair)xs, py = (Pair)ys;
            if (!equals(px.head, py.head))
                return false;
            xs = px.tail;
            ys = py.tail;
        }
        return xs.equals(ys);
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

    private <A> Function<LispVal, $<Evaluator, LispVal>>
    boolBinop(Function<LispVal, $<Evaluator, A>> unpacker, BiFunction<A, A, Boolean> op) {
        return prim((x, y) ->
            do_(unpacker.apply(x), left ->
            do_(unpacker.apply(y), right ->
            do_(pure(Bool.valueOf(op.apply(left, right)))))));
    }

    private Function<LispVal, $<Evaluator, LispVal>>
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

    private Function<LispVal, $<Evaluator, LispVal>>
    numericBinop(Maybe<Number> seed, BiFunction<Number, Number, Number> op) {
        return args -> with(args).<$<Evaluator, LispVal>>get()
          .when(Nil(() ->
            seed.isPresent()
              ? pure(new Num(seed.get()))
              : throwE(new NumArgs(2, args))))

          .when(List(x ->
            seed.isPresent()
              ? map(unpackNum(x), y -> new Num(op.apply(seed.get(), y)))
              : throwE(new NumArgs(2, args))))

          .when(Cons((hd, tl) ->
              do_(unpackNum(hd), x ->
              do_(accumulate(x, tl, op), res ->
              pure(new Num(res))))))

          .orElseGet(() -> throwE(new TypeMismatch("list", args)));
    }

    private $<Evaluator, Number> accumulate(Number x, LispVal args, BiFunction<Number, Number, Number> op) {
        return with(args).<$<Evaluator, Number>>get()
            .when(Nil(() -> pure(x)))
            .when(Cons((y, ys) ->
               bind(unpackNum(y), v -> accumulate(op.apply(x, v), ys, op))))
            .orElseGet(() -> throwE(new TypeMismatch("list", args)));
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

    private Function<LispVal, $<Evaluator, LispVal>>
    math_prim(Function<Double, Number> f) {
        return prim(Num.class, x -> pure(new Num(f.apply(x.value.doubleValue()))));
    }

    // ---------------------------------------------------------------------

    private $<Evaluator, LispVal> cons(LispVal x, LispVal y) {
        return pure(new Pair(x, y));
    }

    private $<Evaluator, LispVal> car(Pair pair) {
        return pure(pair.head);
    }

    private $<Evaluator, LispVal> cdr(Pair pair) {
        return pure(pair.tail);
    }

    private $<Evaluator, LispVal> set_car(Pair pair, LispVal val) {
        return do_action(() -> pair.head = val);
    }

    private $<Evaluator, LispVal> set_cdr(Pair pair, LispVal val) {
        return do_action(() -> pair.tail = val);
    }

    private $<Evaluator, LispVal> length(LispVal x) {
        int res = 0;
        while (x.isPair()) {
            res++;
            x = ((Pair)x).tail;
        }
        return x.isNil() ? pure(new Num(res)) : throwE(new TypeMismatch("pair", x));
    }

    private $<Evaluator, LispVal> append(LispVal args) {
        return with(args).<$<Evaluator, LispVal>>get()
            .when(Nil(() -> pure(Nil)))
            .when(List(x -> pure(x)))
            .when(List((x, y) -> append(x, y)))
            .when(Cons((x, y, ys) ->
                do_(append(x, y), hd ->
                do_(append(ys), tl ->
                do_(append(hd, tl))))))
            .get();
    }

    private $<Evaluator, LispVal> append(LispVal x, LispVal y) {
        if (x.isNil()) {
            return pure(y);
        } else if (y.isNil()) {
            return pure(x);
        } else {
            return bind(reverse(x), rev -> reverseCons(rev, y));
        }
    }

    private $<Evaluator, LispVal> reverse(LispVal lst) {
        return reverseCons(lst, Nil);
    }

    private $<Evaluator, LispVal> reverseCons(LispVal hd, LispVal tl) {
        while (hd.isPair()) {
            Pair p = (Pair)hd;
            tl = new Pair(p.head, tl);
            hd = p.tail;
        }
        return hd.isNil() ? pure(tl) : throwE(new TypeMismatch("pair", hd));
    }

    private $<Evaluator, LispVal> foldl(LispVal f, LispVal init, LispVal lst) {
        $<Evaluator, LispVal> res = pure(init);
        while (lst.isPair()) {
            Pair p = (Pair)lst;
            res = bind(res, r -> apply(f, Pair.of(r, p.head)));
            lst = p.tail;
        }
        return lst.isNil() ? res : throwE(new TypeMismatch("pair", lst));
    }

    private $<Evaluator, LispVal> foldr(LispVal f, LispVal init, LispVal lst) {
        if (lst.isNil()) {
            return pure(init);
        }

        if (lst.isPair()) {
            Pair p = (Pair)lst;
            return do_(delay(() -> foldr(f, init, p.tail)), rest ->
                   do_(apply(f, Pair.of(p.head, rest))));
        }

        return throwE(new TypeMismatch("pair", lst));
    }

    private $<Evaluator, LispVal> _map(LispVal f, LispVal lst) {
        if (lst.isNil()) {
            return pure(Nil);
        }

        if (lst.isPair()) {
            Pair p = (Pair)lst;
            return do_(apply(f, Pair.of(p.head)), x ->
                   do_(_map(f, p.tail), xs ->
                   pure(new Pair(x, xs))));
        }

        return throwE(new TypeMismatch("pair", lst));
    }

    private $<Evaluator, LispVal> flatmap(LispVal f, LispVal lst) {
        if (lst.isNil()) {
            return pure(Nil);
        }

        if (lst.isPair()) {
            Pair p = (Pair)lst;
            return do_(apply(f, Pair.of(p.head)), xs ->
                   do_(flatmap(f, p.tail), ys ->
                   append(xs, ys)));
        }

        return throwE(new TypeMismatch("pair", lst));
    }

    private $<Evaluator, LispVal> filter(LispVal pred, LispVal lst) {
        if (lst.isNil()) {
            return pure(Nil);
        }

        if (lst.isPair()) {
            Pair p = (Pair)lst;
            return do_(apply(pred, Pair.of(p.head)), result ->
                   isTrue(result)
                     ? do_(filter(pred, p.tail), rest ->
                       do_(pure(new Pair(p.head, rest))))
                     : filter(pred, p.tail));
        }

        return throwE(new TypeMismatch("pair", lst));
    }

    private $<Evaluator, LispVal> for_each(LispVal proc, LispVal lst) {
        if (lst.isNil()) {
            return pure(Void.VOID);
        }

        if (lst.isPair()) {
            Pair p = (Pair)lst;
            return do_(apply(proc, Pair.of(p.head)), () ->
                   do_(for_each(proc, p.tail)));
        }

        return throwE(new TypeMismatch("pair", lst));
    }

    // ---------------------------------------------------------------------

    private $<Evaluator, LispVal> symbolToString(Symbol symbol) {
        return pure(new Text(symbol.name));
    }

    private $<Evaluator, LispVal> stringToSymbol(Text text) {
        return pure(getsym(text.value));
    }

    private Function<LispVal, $<Evaluator, LispVal>>
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

    private $<Evaluator, LispVal> make_vector(LispVal args) {
        return with(args).<$<Evaluator, LispVal>>get()
            .when(List(k ->
                with_valid_dim(k, n -> make_vector(n, new Num(0L)))))
            .when(List((k, fill) ->
                with_valid_dim(k, n -> make_vector(n, fill))))
            .orElseGet(() -> throwE(new NumArgs(2, args)));
    }

    private $<Evaluator, LispVal> make_vector(int k, LispVal fill) {
        return pure(new Vec(Vector.iterate(k, __ -> fill)));
    }

    private $<Evaluator, LispVal> vector(LispVal args) {
        return map(args.toList(this), xs -> new Vec(Vector.fromList(xs)));
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
        return pure(Pair.fromList(vec.value.asList()));
    }

    private $<Evaluator, LispVal> list_to_vector(LispVal lst) {
        return do_(lst.toList(this), xs -> pure(new Vec(Vector.fromList(xs))));
    }

    private $<Evaluator, LispVal> vector_fill(Vec vec, LispVal fill) {
        return do_action(() -> vec.value = vec.value.map(Fn.pure(fill)));
    }

    // ---------------------------------------------------------------------

    private $<Evaluator, LispVal> force(LispVal t) {
        if (t instanceof Promise) {
            Promise p = (Promise)t;

            if (p.result != null) {
                return p.result;
            }

            return this.<Env>liftReader().local(
                __ -> p.env,
                catchE(err -> p.result = throwE(err),
                       do_(evalSequence(p.body), res ->
                       do_(p.result = pure(res)))));
        } else {
            return pure(t);
        }
    }

    private $<Evaluator, LispVal> error(LispVal args) {
        return throwE(new LispError(args.show()));
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

    private $<Evaluator, LispVal> errNumArgs(int expected, LispVal found) {
        return throwE(new NumArgs(expected, found));
    }

    private $<Evaluator, LispVal> errTypeMismatch(Class<?> type, LispVal found) {
        return throwE(new TypeMismatch(type.getSimpleName(), found));
    }

    private Function<LispVal, $<Evaluator, LispVal>>
    prim(Supplier<$<Evaluator, LispVal>> f) {
        return args -> args.isNil() ? f.get() : errNumArgs(0, args);
    }

    private Function<LispVal, $<Evaluator, LispVal>>
    prim(Function<LispVal, $<Evaluator, LispVal>> f) {
        return args -> inCaseOf(args, List(f), () -> errNumArgs(1, args));
    }

    private <A extends LispVal> Function<LispVal, $<Evaluator, LispVal>>
    prim(Class<A> c, Function<A, $<Evaluator, LispVal>> f) {
        return args -> inCaseOf(args, List((LispVal x) ->
            c.isInstance(x)
                ? f.apply(c.cast(x))
                : errTypeMismatch(c, x)
        ), () -> errNumArgs(1, args));
    }

    private Function<LispVal, $<Evaluator, LispVal>>
    prim(BiFunction<LispVal, LispVal, $<Evaluator, LispVal>> f) {
        return args -> inCaseOf(args, List(f), () -> errNumArgs(2, args));
    }

    private <A extends LispVal, B extends LispVal> Function<LispVal, $<Evaluator, LispVal>>
    prim(Class<A> c1, Class<B> c2, BiFunction<A, B, $<Evaluator, LispVal>> f) {
        return args -> inCaseOf(args, List((x, y) -> {
            if (!c1.isInstance(x))
                return errTypeMismatch(c1, x);
            if (!c2.isInstance(y))
                return errTypeMismatch(c2, y);
            return f.apply(c1.cast(x), c2.cast(y));
        }), () -> errNumArgs(2, args));
    }


    private Function<LispVal, $<Evaluator, LispVal>>
    prim(TriFunction<LispVal, LispVal, LispVal, $<Evaluator, LispVal>> f) {
        return args -> inCaseOf(args, List(f), () -> errNumArgs(3, args));
    }

    private <A extends LispVal, B extends LispVal, C extends LispVal> Function<LispVal, $<Evaluator, LispVal>>
    prim(Class<A> c1, Class<B> c2, Class<C> c3, TriFunction<A, B, C, $<Evaluator, LispVal>> f) {
        return args -> inCaseOf(args, List((x, y, z) -> {
            if (!c1.isInstance(x))
                return errTypeMismatch(c1, x);
            if (!c2.isInstance(y))
                return errTypeMismatch(c2, y);
            if (!c3.isInstance(z))
                return errTypeMismatch(c3, z);
            return f.apply(c1.cast(x), c2.cast(y), c3.cast(z));
        }), () -> errNumArgs(3, args));
    }
}
