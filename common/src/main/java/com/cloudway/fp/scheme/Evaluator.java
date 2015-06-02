/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.scheme;

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
import com.cloudway.fp.control.monad.trans.ContT;
import com.cloudway.fp.control.monad.trans.ExceptTC;
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
import com.cloudway.fp.parser.ParseError;
import com.cloudway.fp.parser.Stream;

import static com.cloudway.fp.control.Conditionals.with;
import static com.cloudway.fp.control.Conditionals.inCaseOf;
import static com.cloudway.fp.control.Syntax.do_;
import static com.cloudway.fp.control.Syntax.let;
import static com.cloudway.fp.control.Syntax.loop;
import static com.cloudway.fp.data.Either.left;
import static com.cloudway.fp.data.Either.right;
import static com.cloudway.fp.scheme.LispError.*;
import static com.cloudway.fp.scheme.LispVal.*;
import static com.cloudway.fp.scheme.LispVal.Void;

// @formatter:off

@SuppressWarnings("Convert2MethodRef")
public class Evaluator extends ExceptTC<Evaluator, LispError, ContT<Trampoline.µ>> {
    private final SchemeParser parser = new SchemeParser();
    private final Either<ParseError, Seq<LispVal>> stdlib;

    public Evaluator() {
        super(ContT.on(Trampoline.tclass));
        stdlib = loadLib("stdlib.scm");
    }

    public Either<ParseError, Seq<LispVal>> parse(String name, Stream<LispVal> input) {
        return parser.parse(name, input);
    }

    public Either<ParseError, Seq<LispVal>> parse(String input) {
        return parse("", parser.getStream(input));
    }

    public Either<ParseError, Seq<LispVal>> parse(String name, Reader input) {
        return parse(name, parser.getStream(input));
    }

    private Either<ParseError, Seq<LispVal>> loadLib(String name) {
        try (InputStream is = getClass().getResourceAsStream(name)) {
            return parse(name, new InputStreamReader(is, StandardCharsets.UTF_8));
        } catch (Exception ex) {
            return left(new ParseError(ex));
        }
    }

    public Env getStandardEnv() {
        Env env = new Env(getPrimitives());
        run(env, stdlib).getOrThrow(Fn.id());
        return env;
    }

    public Either<LispError, LispVal> run(Env env, Either<ParseError, Seq<LispVal>> form) {
        if (form.isLeft()) {
            return left(new LispError.Parser(form.left()));
        } else {
            return runSequence(env, form.right());
        }
    }

    private Either<LispError, LispVal> runSequence(Env env, Seq<LispVal> exps) {
        Either<LispError, LispVal> res = right(Void.VOID);
        while (!exps.isEmpty() && res.isRight()) {
            res = Trampoline.run(ContT.eval(runExcept(eval(env, exps.head()))));
            exps = exps.tail();
        }
        return res;
    }

    public Either<LispError, LispVal> evaluate(String form) {
        return run(getStandardEnv(), parse(form));
    }

    // =======================================================================

    public $<Evaluator, Proc> analyze(Env ctx, LispVal form) {
        return with(form).<$<Evaluator, Proc>>get()
            .when(Datum  (this::analyzeDatum))
            .when(Symbol (this::analyzeVariable))
            .when(Cons   ((tag, args) -> analyzeList(ctx, form, tag, args)))
            .orElseGet(() -> throwE(new BadSpecialForm("Unrecognized special form", form)));
    }

    public $<Evaluator, LispVal> eval(Env env, LispVal exp) {
        return bind(analyze(env, exp), proc -> proc.apply(env));
    }

    private $<Evaluator, Seq<LispVal>> evalM(Env env, Seq<Proc> procs) {
        return mapM(procs, proc -> proc.apply(env));
    }

    public $<Evaluator, LispVal> apply(Env env, LispVal func, LispVal args) {
        return with(func).<$<Evaluator, LispVal>>get()
            .when(Prim(f -> f.apply(env, args)))
            .when(Func(f -> extendEnv(f.closure, f.params, args, f.body)))
            .orElseGet(() -> throwE(new NotFunction("Unrecognized function", func.show())));
    }

    private $<Evaluator, Proc> analyzeList(Env ctx, LispVal form, LispVal tag, LispVal args) {
        if (tag.isSymbol()) {
            switch (((Symbol)tag).name) {
            case "define":
                return with(args).<$<Evaluator, Proc>>get()
                  .when(List((first, value) ->
                    first.isSymbol()
                      ? analyzeVariableDefinition(ctx, (Symbol)first, value)
                      : analyzeFunctionDefinition(ctx, first, Pair.of(value))))
                  .when(Cons((first, rest) -> analyzeFunctionDefinition(ctx, first, rest)))
                  .orElseGet(() -> badSyntax("define", form));

            case "defmacro":
                return with(args).<$<Evaluator, Proc>>get()
                  .when(Cons((first, rest) -> analyzeMacroDefinition(ctx, first, rest)))
                  .orElseGet(() -> badSyntax("defmacro", form));

            case "lambda":
                return with(args).<$<Evaluator, Proc>>get()
                  .when(Cons((first, rest) -> analyzeLambda(ctx, first, rest)))
                  .orElseGet(() -> badSyntax("lambda", form));

            case "set!":
                return with(args).<$<Evaluator, Proc>>get()
                  .when(List((id, exp) ->
                    id.isSymbol() ? analyzeAssignment(ctx, (Symbol)id, exp)
                                  : badSyntax("set!", form)))
                  .orElseGet(() -> badSyntax("set!", form));

            case "if":
                return with(args).<$<Evaluator, Proc>>get()
                  .when(List((pred, conseq)      -> analyzeIf(ctx, pred, conseq, Void.VOID)))
                  .when(List((pred, conseq, alt) -> analyzeIf(ctx, pred, conseq, alt)))
                  .orElseGet(() -> badSyntax("if", form));

            case "cond":
                return analyzeCond(ctx, args);

            case "match":
                return with(args).<$<Evaluator, Proc>>get()
                  .when(Cons((exp, spec) -> analyzeMatch(ctx, exp, spec)))
                  .orElseGet(() -> badSyntax("match", form));

            case "and":
                return analyzeAnd(ctx, args);

            case "or":
                return analyzeOr(ctx, args);

            case "quote":
                return with(args).<$<Evaluator, Proc>>get()
                  .when(List(this::analyzeDatum))
                  .orElseGet(() -> badSyntax("quote", form));

            case "quasiquote":
                return with(args).<$<Evaluator, Proc>>get()
                  .when(List(datum -> pure(env -> evalUnquote(env.incrementQL(), datum))))
                  .orElseGet(() -> badSyntax("quasiquote", form));

            case "begin":
                return analyzeSequence(ctx, args);

            case "delay":
                return map(analyzeSequence(ctx, args), proc ->
                       env -> pure(new Promise(env, proc)));

            case "reset":
                return map(analyzeSequence(ctx, args), proc -> env -> reset(env, proc));

            case "shift":
                return with(args).<$<Evaluator, Proc>>get()
                  .when(Cons((id, body) ->
                    id.isSymbol()
                      ? map(analyzeSequence(ctx, body), proc -> env -> shift(env, (Symbol)id, proc))
                      : badSyntax("shift", form)))
                    .orElseGet(() -> badSyntax("shift", form));

            case "let":
                return args.isPair() && ((Pair)args).head.isSymbol()
                    ? analyzeLet(ctx, "let", ((Pair)args).tail, (ps, as, b) ->
                        translateNamedLet((Symbol)((Pair)args).head, ps, as, b))
                    : analyzeLet(ctx, "let", args, this::translateLet);

            case "let*":
                return analyzeLet(ctx, "let*", args, this::translateLetStar);

            case "letrec":
                return analyzeLet(ctx, "letrec", args, this::translateLetrec);

            case "let-optionals":
                return with(args).<$<Evaluator, Proc>>get()
                  .when(Cons((first, rest) ->
                    do_(analyze(ctx, first), v ->
                    do_(analyzeLet(ctx, "let-optionals", rest, (ps, as, b) ->
                    translateLetOptionals(ps, as, v, b))))))
                  .orElseGet(() -> badSyntax("let-optionals", form));

            case "do":
                return analyzeDo(ctx, args);

            default:
                Maybe<LispVal> mac = ctx.lookup((Symbol)tag);
                if (mac.isPresent() && (mac.get() instanceof Macro)) {
                    return expandMacro(ctx, (Macro)mac.get(), args);
                }
            }
        }

        return analyzeApplication(ctx, tag, args);
    }

    private $<Evaluator, Proc> analyzeDatum(LispVal datum) {
        $<Evaluator, LispVal> res = pure(datum);
        return pure(env -> res);
    }

    private $<Evaluator, Proc> analyzeVariable(Symbol var) {
        return pure(env -> env.lookup(var).either(this::pure, () -> unbound(var)));
    }

    private $<Evaluator, Proc> analyzeAssignment(Env ctx, Symbol var, LispVal exp) {
        return map(analyze(ctx, exp), vproc -> env -> setVar(env, var, vproc));
    }

    @SuppressWarnings("RedundantTypeArguments")
    private $<Evaluator, LispVal> setVar(Env env, Symbol var, Proc vproc) {
        return env.lookupRef(var).<$<Evaluator, LispVal>>either(
            slot -> do_(vproc.apply(env), value ->
                    do_action(() -> slot.set(value))),
            () -> unbound(var));
    }

    private $<Evaluator, Proc> analyzeVariableDefinition(Env ctx, Symbol var, LispVal exp) {
        return map(analyze(ctx, exp), vproc ->
               env -> do_(vproc.apply(env), value ->
                      do_action(() -> env.put(var, value))));
    }

    private $<Evaluator, Proc> analyzeFunctionDefinition(Env ctx, LispVal first, LispVal body) {
        return with(first).<$<Evaluator, Proc>>get()
            .when(Cons((var, formals) ->
              var.isSymbol() && checkLambda(formals, body)
                ? map(analyzeSequence(ctx.extend(), body), bproc ->
                  env -> do_action(() -> env.put((Symbol)var, new Func(formals, bproc, env))))
                : badSyntax("define", first)))
            .orElseGet(() -> badSyntax("define", first));
    }

    private $<Evaluator, Proc> analyzeMacroDefinition(Env ctx, LispVal first, LispVal body) {
        return with(first).<$<Evaluator, Proc>>get()
            .when(Cons((var, pattern) ->
              var.isSymbol() && checkMacro(pattern, body)
                ? do_(analyzeSequence(ctx.extend(), body), bproc ->
                  do_(action(() -> ctx.put((Symbol)var, new Macro(pattern, bproc))),
                  pure(env -> pure(Void.VOID))))
                : badSyntax("defmacro", first)))
            .orElseGet(() -> badSyntax("defmacro", first));
    }

    private $<Evaluator, Proc> analyzeLambda(Env ctx, LispVal formals, LispVal body) {
        if (checkLambda(formals, body)) {
            return map(analyzeSequence(ctx.extend(), body), bproc ->
                   env -> pure(new Func(formals, bproc, env)));
        } else {
            return badSyntax("lambda", formals);
        }
    }

    private static boolean checkLambda(LispVal formals, LispVal body) {
        return formals.allMatch(LispVal::isSymbol) && body.isList() && !body.isNil();
    }

    private static boolean checkMacro(LispVal pattern, LispVal body) {
        return isPattern(pattern) && body.isList() && !body.isNil();
    }

    private $<Evaluator, Proc> analyzeApplication(Env ctx, LispVal operator, LispVal operands) {
        return do_(analyze(ctx, operator), fproc ->
               do_(operands.mapM(this, x -> analyze(ctx, x)), aprocs ->
               pure(env -> do_(fproc.apply(env), op ->
                           do_(aprocs.mapM(this, a -> ((Proc)a).apply(env)), args ->
                           apply(env, op, args))))));
    }

    private $<Evaluator, Proc> expandMacro(Env ctx, Macro macro, LispVal operands) {
        return match(ctx, macro.pattern, operands).<$<Evaluator, Proc>>either(
            err  -> throwE(err),
            eenv -> do_(macro.body.apply(eenv), mexp -> analyze(ctx, mexp)));
    }

    private $<Evaluator, LispVal> extendEnv(Env env, LispVal params, LispVal args, Proc proc) {
        PMap<Symbol, Ref<LispVal>> bindings = env.getBindings();
        int nvars = 0;

        while (params.isPair() && args.isPair()) {
            Pair   pp  = (Pair)params, pv = (Pair)args;
            Symbol var = (Symbol)pp.head;
            bindings   = bindings.put(var, new Ref<>(pv.head));
            params     = pp.tail;
            args       = pv.tail;
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

        return proc.apply(env.extend(bindings));
    }

    private $<Evaluator, Proc> analyzeSequence(Env ctx, LispVal exps) {
        if (exps.isNil()) {
            return pure(env -> pure(Void.VOID));
        }

        if (exps.isPair()) {
            Pair p = (Pair)exps;
            if (p.tail.isNil()) {
                return analyze(ctx, p.head);
            } else {
                return do_(analyze(ctx, p.head), first ->
                       do_(analyzeSequence(ctx, p.tail), rest ->
                       pure(env -> seqR(first.apply(env), () -> rest.apply(env)))));
            }
        }

        return badSyntax("sequence", exps);
    }

    private $<Evaluator, Proc> analyzeIf(Env ctx, LispVal pred, LispVal conseq, LispVal alt) {
        return do_(analyze(ctx, pred),   pproc ->
               do_(analyze(ctx, conseq), cproc ->
               do_(analyze(ctx, alt),    aproc ->
               pure(env -> do_(pproc.apply(env), tval ->
                           isTrue(tval) ? cproc.apply(env)
                                        : aproc.apply(env))))));
    }

    private $<Evaluator, Proc> analyzeAnd(Env ctx, LispVal args) {
        if (args.isNil()) {
            return pure(env -> pure(Bool.TRUE));
        }

        if (args.isPair()) {
            Pair p = (Pair)args;
            if (p.tail.isNil()) {
                return analyze(ctx, p.head);
            } else {
                return do_(analyze(ctx, p.head), first ->
                       do_(analyzeAnd(ctx, p.tail), rest ->
                       pure(env -> do_(first.apply(env), result ->
                                   isFalse(result) ? pure(result)
                                                   : rest.apply(env)))));
            }
        }

        return badSyntax("and", args);
    }

    private $<Evaluator, Proc> analyzeOr(Env ctx, LispVal args) {
        if (args.isNil()) {
            return pure(env -> pure(Bool.FALSE));
        }

        if (args.isPair()) {
            Pair p = (Pair)args;
            if (p.tail.isNil()) {
                return analyze(ctx, p.head);
            } else {
                return do_(analyze(ctx, p.head), first ->
                       do_(analyzeOr(ctx, p.tail), rest ->
                       pure(env -> do_(first.apply(env), result ->
                                   isTrue(result) ? pure(result)
                                                  : rest.apply(env)))));
            }
        }

        return badSyntax("or", args);
    }

    // -----------------------------------------------------------------------

    private final Symbol ELSE = getsym("else");
    private final Symbol WHEN = getsym(":when");
    private final Proc   LAST = env -> pure(Void.VOID);

    private $<Evaluator, Proc> analyzeCond(Env ctx, LispVal form) {
        return with(form).<$<Evaluator, Proc>>get()
            .when(List(last -> analyzeCondClause(ctx, last, LAST)))
            .when(Cons((hd, tl) ->
              bind(delay(() -> analyzeCond(ctx, tl)), rest ->
              analyzeCondClause(ctx, hd, rest))))
            .orElseGet(() -> badSyntax("cond", form));
    }

    private $<Evaluator, Proc> analyzeCondClause(Env ctx, LispVal form, Proc rest) {
        return with(form).<$<Evaluator, Proc>>get()
            .when(List((test, mid, exp) ->
              mid.equals(getsym("=>"))
                ? analyzeRecipientCond(ctx, test, exp, rest)
                : analyzeNormalCond(ctx, test, Pair.of(mid, exp), rest)))
            .when(Cons((test, body) -> analyzeNormalCond(ctx, test, body, rest)))
            .orElseGet(() -> badSyntax("cond", form));
    }

    private $<Evaluator, Proc> analyzeRecipientCond(Env ctx, LispVal test, LispVal exp, Proc rest) {
        return do_(analyze(ctx, test), tproc ->
               do_(analyze(ctx, exp),  pproc ->
               pure(env -> do_(tproc.apply(env), tval ->
                           isTrue(tval)
                             ? bind(pproc.apply(env), proc ->
                               apply(env, proc, Pair.of(tval)))
                             : rest.apply(env)))));
    }

    private $<Evaluator, Proc> analyzeNormalCond(Env ctx, LispVal test, LispVal body, Proc rest) {
        if (test.equals(ELSE)) {
            if (rest == LAST) {
                return analyzeSequence(ctx, body);
            } else {
                return badSyntax("cond", new Pair(test, body));
            }
        }

        if (body.isNil()) {
            return map(analyze(ctx, test), tproc ->
                   env -> do_(tproc.apply(env), tval ->
                          isTrue(tval) ? pure(tval)
                                       : rest.apply(env)));
        } else {
            return bind(analyze(ctx, test), tproc ->
                   map(analyzeSequence(ctx, body), bproc ->
                   env -> do_(tproc.apply(env), tval ->
                          isTrue(tval) ? bproc.apply(env)
                                       : rest.apply(env))));
        }
    }

    // -----------------------------------------------------------------------

    private $<Evaluator, Proc> analyzeMatch(Env ctx, LispVal exp, LispVal spec) {
        return do_(analyze(ctx, exp), vproc ->
               do_(analyzeMatchClauses(ctx, spec), mproc ->
               pure(env -> do_(vproc.apply(env), val ->
                           mproc.apply(env, val)))));
    }

    private $<Evaluator, PProc> analyzeMatchClauses(Env ctx, LispVal form) {
        return with(form).<$<Evaluator, PProc>>get()
            .when(List(last -> analyzeMatchClause(ctx, last, null)))
            .when(Cons((hd, tl) ->
              bind(delay(() -> analyzeMatchClauses(ctx, tl)), rest ->
              analyzeMatchClause(ctx, hd, rest))))
            .orElseGet(() -> badSyntax("match", form));
    }

    private $<Evaluator, PProc> analyzeMatchClause(Env ctx, LispVal form, PProc rest) {
        return with(form).<$<Evaluator, PProc>>get()
            .when(Cons((pat, body) ->
              pat.equals(ELSE)
                ? rest == null
                    ? analyzeSingleMatch(ctx, getsym("_"), body, rest)
                    : badSyntax("match", form)

                : with(body).<$<Evaluator, PProc>>get()
                    .when(Cons((key, guard, exps) ->
                      key.equals(WHEN)
                        ? analyzeGuardedMatch(ctx, pat, guard, exps, rest)
                        : analyzeSingleMatch(ctx, pat, body, rest)))
                    .orElseGet(() ->
                          analyzeSingleMatch(ctx, pat, body, rest))))

            .orElseGet(() -> badSyntax("match", form));
    }

    private $<Evaluator, PProc> analyzeSingleMatch(Env ctx, LispVal pattern, LispVal body, PProc rest) {
        return map(analyzeSequence(ctx, body), bproc -> (env, value) ->
               match(env, pattern, value).<$<Evaluator, LispVal>>either(
                 err  -> rest == null ? throwE(err) : rest.apply(env, value),
                 eenv -> bproc.apply(eenv)));
    }

    private $<Evaluator, PProc> analyzeGuardedMatch(Env ctx, LispVal pattern, LispVal guard, LispVal body, PProc rest) {
        return do_(analyze(ctx, guard), gproc ->
               do_(analyzeSequence(ctx, body), bproc ->
               pure((env, value) ->
                 match(env, pattern, value).<$<Evaluator, LispVal>>either(
                   err  -> rest == null ? throwE(err) : rest.apply(env, value),
                   eenv -> do_(gproc.apply(eenv), tval ->
                           isTrue(tval) ? bproc.apply(eenv) :
                           rest == null ? throwE(new PatternMismatch(guard, value))
                                        : rest.apply(env, value))))));
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

    private static Either<LispError, Env> match(Env env, LispVal pattern, LispVal value) {
        return doMatch(pattern, value, env.getBindings()).map(env::extend);
    }

    private static Either<LispError, PMap<Symbol, Ref<LispVal>>>
    doMatch(LispVal pattern, LispVal value, PMap<Symbol, Ref<LispVal>> bindings) {
        return with(pattern).<Either<LispError, PMap<Symbol, Ref<LispVal>>>>get()
            .when(Text   (__  -> matchConst(pattern, value, bindings)))
            .when(Num    (__  -> matchConst(pattern, value, bindings)))
            .when(Bool   (__  -> matchConst(pattern, value, bindings)))
            .when(Nil    (()  -> matchConst(pattern, value, bindings)))
            .when(Symbol (var -> matchVariable(var, value, bindings)))
            .when(Quoted (dat -> matchConst(dat, value, bindings)))
            .when(Pair   (lst -> matchList(lst, value, bindings)))
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
    matchList(Pair pattern, LispVal value, PMap<Symbol, Ref<LispVal>> bindings) {
        if (value.isPair()) {
            Pair pv = (Pair)value;
            return doMatch(pattern.head, pv.head, bindings).flatMap(b ->
                   doMatch(pattern.tail, pv.tail, b));
        } else {
            return left(new PatternMismatch(pattern, value));
        }
    }

    // -----------------------------------------------------------------------

    private static final class LetParams {
        Seq<Symbol>  vars  = Seq.nil();
        Seq<LispVal> inits = Seq.nil();
    }

    private $<Evaluator, Proc> analyzeLet(Env ctx, String name, LispVal form,
            TriFunction<Seq<Symbol>, Seq<Proc>, Proc, $<Evaluator, Proc>> trans) {
        return with(form).<$<Evaluator, Proc>>get()
            .when(Cons((params, body) ->
              do_(analyzeLetParams(name, params), lp ->
              do_(mapM(lp.inits, x -> analyze(ctx, x)), vprocs ->
              do_(analyzeSequence(ctx.extend(), body), bproc ->
              trans.apply(lp.vars, vprocs, bproc))))))
            .orElseGet(() -> badSyntax(name, form));
    }

    private $<Evaluator, LetParams> analyzeLetParams(String name, LispVal p) {
        LetParams lp = new LetParams();

        for (; p.isPair(); p = ((Pair)p).tail) {
            boolean ok = with(((Pair)p).head).<Boolean>get()
                .when(List((var, init) -> {
                    if (var.isSymbol()) {
                        lp.vars = Seq.cons((Symbol)var, lp.vars);
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

    private static $<Evaluator, LispVal>
    extendEnv(Env env, Seq<Symbol> params, Seq<LispVal> args, Proc body) {
        PMap<Symbol, Ref<LispVal>> bindings = env.getBindings();
        while (!params.isEmpty()) {
            bindings = bindings.put(params.head(), new Ref<>(args.head()));
            params = params.tail();
            args = args.tail();
        }

        return body.apply(env.extend(bindings));
    }

    private $<Evaluator, LispVal> setVariables(Env env, Seq<Symbol> params, Seq<LispVal> args) {
        while (!params.isEmpty()) {
            env.lookupRef(params.head()).get().set(args.head());
            params = params.tail();
            args = args.tail();
        }
        return pure(Void.VOID);
    }

    private $<Evaluator, Proc> translateLet(Seq<Symbol> params, Seq<Proc> inits, Proc body) {
        return pure(env -> do_(evalM(env, inits), args -> extendEnv(env, params, args, body)));
    }

    private $<Evaluator, Proc>
    translateNamedLet(Symbol tag, Seq<Symbol> params, Seq<Proc> inits, Proc body) {
        return pure(env ->
            do_(evalM(env, inits), args ->
            extendEnv(env, Seq.of(tag), Seq.of(Void.VOID),
            eenv -> let(new Func(Pair.fromList(params), body, eenv), f ->
                    do_(setVar(eenv, tag, __ -> pure(f)),
                    do_(apply(eenv, f, Pair.fromList(args))))))));
    }

    private $<Evaluator, Proc>
    translateLetOptionals(Seq<Symbol> params, Seq<Proc> defaults, Proc value, Proc body) {
        return pure(env ->
            do_(value.apply(env), vs ->
            do_(vs.toList(this), args -> {
                int vlen = args.length();
                if (vlen < params.length()) {
                    return do_(evalM(env, defaults.drop(vlen)), dvals ->
                           extendEnv(env, params, args.append(dvals), body));
                } else {
                    return extendEnv(env, params, args, body);
                }
            })));
    }

    private $<Evaluator, Proc> translateLetStar(Seq<Symbol> params, Seq<Proc> inits, Proc body) {
        if (params.isEmpty() || params.tail().isEmpty()) {
            return translateLet(params, inits, body);
        } else {
            return map(translateLetStar(params.tail(), inits.tail(), body), rest ->
                   env -> do_(inits.head().apply(env), val ->
                          extendEnv(env, Seq.of(params.head()), Seq.of(val), rest)));
        }
    }

    private $<Evaluator, Proc> translateLetrec(Seq<Symbol> params, Seq<Proc> inits, Proc body) {
        return pure(env ->
            extendEnv(env, params, params.map(Fn.pure(Void.VOID)),
            eenv -> do_(evalM(eenv, inits), args ->
                    do_(setVariables(eenv, params, args),
                    do_(body.apply(eenv))))));
    }

    // -----------------------------------------------------------------------

    private static final class DoParams {
        Seq<Symbol>  vars  = Seq.nil();
        Seq<LispVal> inits = Seq.nil();
        Seq<LispVal> steps = Seq.nil();
    }

    private $<Evaluator, Proc> analyzeDo(Env ctx, LispVal form) {
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

        return with(form).<$<Evaluator, Proc>>get()
            .when(Cons((params, exit, body) ->
              with(exit).<$<Evaluator, Proc>>get()
                .when(Cons((test, finish) ->
                  do_(analyzeDoParams(params), (DoParams dp) ->
                  do_(mapM(dp.inits, x -> analyze(ctx, x)), (Seq<Proc> vprocs) ->
                  do_(mapM(dp.steps, x -> analyzeSequence(ctx, x)), (Seq<Proc> sprocs) ->
                  do_(analyze(ctx, test), tproc ->
                  do_(analyzeSequence(ctx, finish), fproc ->
                  do_(analyzeSequence(ctx.extend(), body), bproc ->
                  pure(env -> evalDo(env, dp.vars, vprocs, sprocs, tproc, fproc, bproc))))))))))
                .orElseGet(() -> badSyntax("do", exit))))
            .orElseGet(() -> badSyntax("do", form));
    }

    private $<Evaluator, LispVal> evalDo(Env         env,
                                         Seq<Symbol> vars,
                                         Seq<Proc>   inits,
                                         Seq<Proc>   steps,
                                         Proc        test,
                                         Proc        finish,
                                         Proc        body) {
        return do_(evalM(env, inits), vals ->
               extendEnv(env, vars, vals, eenv ->
               loop(again ->
                 do_(test.apply(eenv), tval ->
                 isTrue(tval)
                   ? finish.apply(eenv)
                   : do_(body.apply(eenv),
                     do_(evalM(eenv, steps), svals ->
                     do_(setVariables(eenv, vars, svals),
                     again)))))));
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

    private final Symbol QQ = getsym("quasiquote");
    private final Symbol UNQ = getsym("unquote");
    private final Symbol UNQS = getsym("unquote-splicing");

    private $<Evaluator, LispVal> evalUnquote(Env env, LispVal exp) {
        return with(exp).<$<Evaluator, LispVal>>get()
            .when(Pair   (__ -> unquotePair(env, exp)))
            .when(Vector (v  -> unquoteVector(env, v)))
            .when(Datum  (__ -> pure(exp)))
            .when(Symbol (__ -> pure(exp)))
            .when(Nil    (() -> pure(exp)))
            .orElseGet(() -> throwE(new BadSpecialForm("Unrecognized special form", exp)));
    }

    private $<Evaluator, LispVal> unquote(Env env, LispVal datum, boolean splicing) {
        env = env.decrementQL();
        return env.getQL() < 0  ? throwE(new BadSpecialForm("unquote: not in quasiquote", datum)) :
               env.getQL() == 0 ? eval(env, datum)
                                : map(evalUnquote(env, datum), x ->
                                  Pair.of(splicing ? UNQS : UNQ, x));
    }

    private $<Evaluator, LispVal> unquotePair(Env env, LispVal exp) {
        return with(exp).<$<Evaluator, LispVal>>get()
            .when(TaggedList(QQ, datum -> map(evalUnquote(env.incrementQL(), datum), x ->
                                          Pair.of(QQ, x))))

            .when(TaggedList(UNQ,  datum -> unquote(env, datum, false)))
            .when(TaggedList(UNQS, datum -> unquote(env, datum, true)))

            .when(Cons((hd, tl) -> unquotePair(env, hd, tl)))

            .orElseGet(() -> evalUnquote(env, exp));
    }

    private $<Evaluator, LispVal> unquotePair(Env env, LispVal hd, LispVal tl) {
        return with(hd).<$<Evaluator, LispVal>>get()
            .when(TaggedList(UNQS, datum ->
              do_(unquote(env, datum, true), xs ->
              do_(unquotePair(env, tl), ys ->
              append(xs, ys)))))

            .orElseGet(() ->
              do_(evalUnquote(env, hd), x ->
              do_(unquotePair(env, tl), ys ->
              pure(new Pair(x, ys)))));
    }

    private $<Evaluator, LispVal> unquoteVector(Env env, Vector<LispVal> vec) {
        return map(vec.foldLeft(pure(Vector.empty()),
                      (r, x) -> vectorSplice(env, r, x)),
               Vec::new);
    }

    private $<Evaluator, Vector<LispVal>>
    vectorSplice(Env env, $<Evaluator, Vector<LispVal>> res, LispVal val) {
        return with(val).<$<Evaluator, Vector<LispVal>>>get()
            .when(TaggedList(UNQS, datum ->
              do_(res, r ->
              do_(unquote(env, datum, true), x ->
              do_(x.toList(this), xs ->
              pure(r.append(Vector.fromList(xs))))))))

            .orElseGet(() ->
              do_(res, r ->
              do_(evalUnquote(env, val), x ->
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

    // -----------------------------------------------------------------------

    private <T> $<Evaluator, T> badSyntax(String name, LispVal val) {
        return throwE(new BadSpecialForm(name + ": bad syntax", val));
    }

    private $<Evaluator, LispVal> unbound(Symbol var) {
        return throwE(new UnboundVar("Undefined variable", var.show()));
    }

    private $<Evaluator, LispVal> do_action(Runnable action) {
        return seqR(action(action), pure(Void.VOID));
    }

    private Symbol getsym(String name) {
        return parser.getsym(name);
    }

    // =======================================================================

    private final PMap<String, PProc> primitives = HashPMap.<String, PProc>empty()
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
        .put("map",         primE(this::_map))
        .put("flatmap",     primE(this::flatmap))
        .put("filter",      primE(this::filter))
        .put("for-each",    primE(this::for_each))

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

        .put("eval",        primE(this::eval))
        .put("apply",       primE(this::apply))
        .put("force",       prim(this::force))
        .put("call/cc",     primE(this::callCC))
        .put("error",       this::error)
        .put("not",         prim(this::not))
        .put("gensym",      primE(env -> pure(env.newsym())))
        .put("void",        prim(() -> pure(Void.VOID)))

        .put("load",        primE(Text.class, this::load))
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

    private <A> PProc boolBinop(Function<LispVal, $<Evaluator, A>> unpacker, BiFunction<A, A, Boolean> op) {
        return prim((x, y) ->
            do_(unpacker.apply(x), left ->
            do_(unpacker.apply(y), right ->
            do_(pure(Bool.valueOf(op.apply(left, right)))))));
    }

    private PProc numBoolBinop(BiFunction<Number, Number, Boolean> op) {
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

    private PProc numericBinop(Maybe<Number> seed, BiFunction<Number, Number, Number> op) {
        return (env, args) -> with(args).<$<Evaluator, LispVal>>get()
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

    private PProc math_prim(Function<Double, Number> f) {
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

    private $<Evaluator, LispVal> append(Env env, LispVal args) {
        return with(args).<$<Evaluator, LispVal>>get()
            .when(Nil(() -> pure(Nil)))
            .when(List(x -> pure(x)))
            .when(List((x, y) -> append(x, y)))
            .when(Cons((x, y, ys) ->
                do_(append(x, y), hd ->
                do_(append(env, ys), tl ->
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

    private $<Evaluator, LispVal> _map(Env env, LispVal f, LispVal lst) {
        if (lst.isNil()) {
            return pure(Nil);
        }

        if (lst.isPair()) {
            Pair p = (Pair)lst;
            return do_(apply(env, f, Pair.of(p.head)), x ->
                   do_(_map(env, f, p.tail), xs ->
                   pure(new Pair(x, xs))));
        }

        return throwE(new TypeMismatch("pair", lst));
    }

    private $<Evaluator, LispVal> flatmap(Env env, LispVal f, LispVal lst) {
        if (lst.isNil()) {
            return pure(Nil);
        }

        if (lst.isPair()) {
            Pair p = (Pair)lst;
            return do_(apply(env, f, Pair.of(p.head)), xs ->
                   do_(flatmap(env, f, p.tail), ys ->
                   append(xs, ys)));
        }

        return throwE(new TypeMismatch("pair", lst));
    }

    private $<Evaluator, LispVal> filter(Env env, LispVal pred, LispVal lst) {
        if (lst.isNil()) {
            return pure(Nil);
        }

        if (lst.isPair()) {
            Pair p = (Pair)lst;
            return do_(apply(env, pred, Pair.of(p.head)), result ->
                   isTrue(result)
                     ? do_(filter(env, pred, p.tail), rest ->
                       do_(pure(new Pair(p.head, rest))))
                     : filter(env, pred, p.tail));
        }

        return throwE(new TypeMismatch("pair", lst));
    }

    private $<Evaluator, LispVal> for_each(Env env, LispVal proc, LispVal lst) {
        if (lst.isNil()) {
            return pure(Void.VOID);
        }

        if (lst.isPair()) {
            Pair p = (Pair)lst;
            return do_(apply(env, proc, Pair.of(p.head)), () ->
                   do_(for_each(env, proc, p.tail)));
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

    private PProc stringBoolBinop(BiFunction<String, String, Boolean> op) {
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

    private $<Evaluator, LispVal> make_vector(Env env, LispVal args) {
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

    private $<Evaluator, LispVal> vector(Env env, LispVal args) {
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

            return catchE(err -> p.result = throwE(err),
                          do_(p.body.apply(p.env), res ->
                          do_(p.result = pure(res))));
        } else {
            return pure(t);
        }
    }

    private $<Evaluator, LispVal> callCC(Env env, LispVal proc) {
        Function<Function<LispVal, $<Evaluator, LispVal>>, $<Evaluator, LispVal>> f =
            k -> apply(env, proc, Pair.of(makeContFunc(env, k)));

        return $(inner().<Either<LispError, LispVal>>callCC(c ->
            runExcept(f.apply(a -> $(c.escape(right(a)))))));
    }

    private $<Evaluator, LispVal> reset(Env env, Proc proc) {
        return $(inner().reset(runExcept(proc.apply(env))));
    }

    private $<Evaluator, LispVal> shift(Env env, Symbol id, Proc proc) {
        Function<Function<LispVal, $<Evaluator, LispVal>>, $<Evaluator, LispVal>> f =
            k -> extendEnv(env, Seq.of(id), Seq.of(makeContFunc(env, k)), proc);

        return $(inner().<Either<LispError,LispVal>, Either<LispError,LispVal>>shift(c ->
            runExcept(f.apply(a -> $(inner().lift(c.apply(right(a))))))));
    }

    private static Func makeContFunc(Env env, Function<LispVal, $<Evaluator, LispVal>> k) {
        Symbol cid = env.newsym();
        return new Func(Pair.of(cid), eenv -> k.apply(eenv.get(cid)), env);
    }

    private $<Evaluator, LispVal> error(Env env, LispVal args) {
        return throwE(new LispError(args.show()));
    }

    // ---------------------------------------------------------------------

    private $<Evaluator, LispVal> load(Env env, Text name) {
        try (InputStream is = getInputStream(name.value)) {
            if (is == null) {
                return throwE(new LispError(name.show() + ": not found"));
            } else {
                Reader input = new InputStreamReader(is, StandardCharsets.UTF_8);
                return except(run(env, parse(name.value, input)));
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

    private PProc prim(Supplier<$<Evaluator, LispVal>> f) {
        return (env, args) -> args.isNil() ? f.get() : errNumArgs(0, args);
    }

    private PProc primE(Function<Env, $<Evaluator, LispVal>> f) {
        return (env, args) -> args.isNil() ? f.apply(env) : errNumArgs(1, args);
    }

    private PProc prim(Function<LispVal, $<Evaluator, LispVal>> f) {
        return (env, args) -> inCaseOf(args, List(f), () -> errNumArgs(1, args));
    }

    private <A extends LispVal> PProc prim(Class<A> c, Function<A, $<Evaluator, LispVal>> f) {
        return (env, args) -> inCaseOf(args, List((LispVal x) ->
            c.isInstance(x)
                ? f.apply(c.cast(x))
                : errTypeMismatch(c, x)
        ), () -> errNumArgs(1, args));
    }

    private PProc primE(BiFunction<Env, LispVal, $<Evaluator, LispVal>> f) {
        return (env, args) -> prim(x -> f.apply(env, x)).apply(env, args);
    }

    private <A extends LispVal> PProc primE(Class<A> c, BiFunction<Env, A, $<Evaluator, LispVal>> f) {
        return (env, args) -> prim(c, x -> f.apply(env, x)).apply(env, args);
    }

    private PProc prim(BiFunction<LispVal, LispVal, $<Evaluator, LispVal>> f) {
        return (env, args) -> inCaseOf(args, List(f), () -> errNumArgs(2, args));
    }

    private <A extends LispVal, B extends LispVal> PProc
    prim(Class<A> c1, Class<B> c2, BiFunction<A, B, $<Evaluator, LispVal>> f) {
        return (env, args) -> inCaseOf(args, List((x, y) -> {
            if (!c1.isInstance(x))
                return errTypeMismatch(c1, x);
            if (!c2.isInstance(y))
                return errTypeMismatch(c2, y);
            return f.apply(c1.cast(x), c2.cast(y));
        }), () -> errNumArgs(2, args));
    }

    private PProc primE(TriFunction<Env, LispVal, LispVal, $<Evaluator, LispVal>> f) {
        return (env, args) -> prim((x, y) -> f.apply(env, x, y)).apply(env, args);
    }

    private <A extends LispVal, B extends LispVal, C extends LispVal> PProc
    prim(Class<A> c1, Class<B> c2, Class<C> c3, TriFunction<A, B, C, $<Evaluator, LispVal>> f) {
        return (env, args) -> inCaseOf(args, List((x, y, z) -> {
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
