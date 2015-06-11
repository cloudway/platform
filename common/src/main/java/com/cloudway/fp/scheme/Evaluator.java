/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.scheme;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.function.BiFunction;
import java.util.function.Function;

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
import com.cloudway.fp.data.Ref;
import com.cloudway.fp.data.Seq;
import com.cloudway.fp.data.Tuple;
import com.cloudway.fp.data.Vector;
import com.cloudway.fp.function.TriFunction;

import static com.cloudway.fp.control.Conditionals.with;
import static com.cloudway.fp.control.Syntax.do_;
import static com.cloudway.fp.control.Syntax.let;
import static com.cloudway.fp.data.Either.left;
import static com.cloudway.fp.data.Either.right;
import static com.cloudway.fp.scheme.LispError.*;
import static com.cloudway.fp.scheme.LispVal.*;
import static com.cloudway.fp.scheme.LispVal.Void;

// @formatter:off

@SuppressWarnings("Convert2MethodRef")
public class Evaluator extends ExceptTC<Evaluator, LispError, ContT<Trampoline.µ>> {
    private final SchemeParser parser = new SchemeParser();
    private final Env reportEnv;

    private static final Symbol INTERACTION_ENV = new Symbol("%INTERACTION-ENVIRONMENT%");
    private static final Symbol LOADED_MODULES = new Symbol("%LOADED-MODULES%");
    private static final Symbol STDLIB = new Symbol("stdlib");

    public Evaluator() {
        super(ContT.on(Trampoline.tclass));

        reportEnv = new Env();
        loadPrimitives(reportEnv, Primitives.class);
        loadPrimitives(reportEnv, IOPrimitives.class);
        loadLib(reportEnv, STDLIB).getOrThrow(Fn.id());
    }

    public Either<LispError, LispVal> loadLib(Env env, Symbol module) {
        String name = module.name;
        if (!isModuleLoaded(env, module)) {
            try (InputStream is = getModuleResource(name + ".scm")) {
                if (is == null) {
                    return left(new LispError(name + ": no such module"));
                }

                Reader input = new InputStreamReader(is, StandardCharsets.UTF_8);
                Either<LispError, LispVal> res = run(env, parse(name, input));

                if (res.isLeft()) {
                    return res;
                } else {
                    addLoadedModule(env, module);
                }
            } catch (Exception ex) {
                return left(new LispError(ex));
            }
        }
        return right(Void.VOID);
    }

    private static InputStream getModuleResource(String name) {
        return Primitives.class.getResourceAsStream("/META-INF/scheme/" + name);
    }

    private static boolean isModuleLoaded(Env env, Symbol module) {
        LispVal list = (LispVal)env.getSystem(LOADED_MODULES, Nil).get();
        while (list.isPair()) {
            Pair p = (Pair)list;
            if (module.equals(p.head))
                return true;
            list = p.tail;
        }
        return false;
    }

    private static void addLoadedModule(Env env, Symbol module) {
        Ref<Object> slot = env.getSystem(LOADED_MODULES, Nil);
        slot.set(new Pair(module, (LispVal)slot.get()));
    }

    public Env getNullEnv() {
        return new Env();
    }

    public Env getSchemeReportEnv() {
        Env env = reportEnv.extend();
        env.setSystem(INTERACTION_ENV, env);
        return env;
    }

    public Env getInteractionEnv(Env env) {
        return (Env)env.getSystem(INTERACTION_ENV, env).get();
    }

    public SchemeParser getParser() {
        return parser;
    }

    public Seq<Either<LispError, LispVal>> parse(String input) {
        return parser.parse(input);
    }

    public Seq<Either<LispError, LispVal>> parse(String name, Reader input) {
        return parser.parse(name, input);
    }

    public Either<LispError, LispVal> run(Env env, Seq<Either<LispError, LispVal>> exps) {
        Either<LispError, LispVal> res = right(Void.VOID);
        while (!exps.isEmpty() && res.isRight()) {
            Either<LispError, LispVal> exp = exps.head();
            if (exp.isLeft()) {
                res = exp;
                break;
            } else {
                res = Trampoline.run(ContT.eval(runExcept(eval(env, exp.right()))));
                exps = exps.tail();
            }
        }
        return res;
    }

    public Either<LispError, LispVal> evaluate(String form) {
        return run(getSchemeReportEnv(), parse(form));
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
        return with(func.getValue()).<$<Evaluator, LispVal>>get()
            .when(Prim(f -> f.apply(env, args)))
            .when(Func(f -> extendEnv(f.closure, f.params, args, f.body)))
            .orElseGet(() -> throwE(new NotFunction("Unrecognized function", func.show())));
    }

    private $<Evaluator, Proc> analyzeList(Env ctx, LispVal form, LispVal tag, LispVal args) {
        if (tag.isSymbol()) {
            switch (((Symbol)tag).name) {
            case "define":
                return with(args).<$<Evaluator, Proc>>get()
                  .when(List(id -> id.isSymbol()
                      ? analyzeVariableDefinition(ctx, (Symbol)id, Void.VOID)
                      : badSyntax("define", form)))
                  .when(List((first, exp) ->
                    first.isSymbol()
                      ? analyzeVariableDefinition(ctx, (Symbol)first, exp)
                      : analyzeFunctionDefinition(ctx, first, Pair.of(exp))))
                  .when(Cons((first, rest) ->
                      analyzeFunctionDefinition(ctx, first, rest)))
                  .orElseGet(() -> badSyntax("define", form));

            case "define-values":
                return with(args).<$<Evaluator, Proc>>get()
                  .when(List((vars, exp) ->
                    vars.isList() && vars.allMatch(LispVal::isSymbol)
                      ? analyzeValuesDefinition(ctx, vars, exp)
                      : badSyntax("define-values", form)))
                  .orElseGet(() -> badSyntax("define-values", form));

            case "define-macro":
                return with(args).<$<Evaluator, Proc>>get()
                  .when(Cons((first, rest) -> analyzeMacroDefinition(ctx, first, rest)))
                  .orElseGet(() -> badSyntax("define-macro", form));

            case "lambda":
                return with(args).<$<Evaluator, Proc>>get()
                  .when(Cons((first, rest) -> analyzeLambda(ctx, first, rest)))
                  .orElseGet(() -> badSyntax("lambda", form));

            case "set!":
                return with(args).<$<Evaluator, Proc>>get()
                  .when(List((first, exp) ->
                    with(first).<$<Evaluator, Proc>>get()
                      .when(Symbol(id ->
                        analyzeAssignment(ctx, id, exp)))
                      .when(Cons((proc, pargs) ->
                        analyzeGetterWithSetter(ctx, proc, pargs, exp)))
                      .orElseGet(() -> badSyntax("set!", args))))
                  .orElseGet(() -> badSyntax("set!", args));

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

            case "let":
                return args.isPair() && ((Pair)args).head.isSymbol()
                    ? analyzeLet(ctx, "let", ((Pair)args).tail, (ps, as, b) ->
                        translateNamedLet((Symbol)((Pair)args).head, ps, as, b))
                    : analyzeLet(ctx, "let", args, this::translateLet);

            case "let*":
                return analyzeLet(ctx, "let*", args, this::translateLetStar);

            case "letrec":
                return analyzeLet(ctx, "letrec", args, this::translateLetrec);

            case "letrec*":
                return analyzeLet(ctx, "letrec*", args, this::translateLetrecStar);

            default:
                Maybe<LispVal> mac = ctx.lookupMacro((Symbol)tag);
                if (mac.isPresent()) {
                    if (mac.get() instanceof Macro) {
                        return expandMacro(ctx, (Macro)mac.get(), args);
                    } else if (mac.get() instanceof PrimMacro) {
                        PrimMacro pm = (PrimMacro)mac.get();
                        return pm.proc.apply(ctx, args);
                    }
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
        return pure(env -> env.lookup(var).either(v -> pure(v.get()), () -> unbound(var)));
    }

    private $<Evaluator, Proc> analyzeAssignment(Env ctx, Symbol var, LispVal exp) {
        return map(analyze(ctx, exp), vproc -> env -> setVar(env, var, vproc));
    }

    private $<Evaluator, Proc> analyzeGetterWithSetter(Env ctx, LispVal proc, LispVal args, LispVal exp) {
        LispVal setter = Pair.of(getsym("setter"), proc);
        return do_(Primitives.append(this, args, Pair.of(exp)), sargs ->
               do_(analyze(ctx, new Pair(setter, sargs))));
    }

    @SuppressWarnings("RedundantTypeArguments")
    private $<Evaluator, LispVal> setVar(Env env, Symbol var, Proc vproc) {
        return env.lookup(var).<$<Evaluator, LispVal>>either(
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
                  env -> do_action(() -> env.put((Symbol)var, new Func(var.show(), formals, bproc, env))))
                : badSyntax("define", first)))
            .orElseGet(() -> badSyntax("define", first));
    }

    private $<Evaluator, Proc> analyzeMacroDefinition(Env ctx, LispVal first, LispVal body) {
        return with(first).<$<Evaluator, Proc>>get()
            .when(Cons((var, pattern) ->
              var.isSymbol() && checkMacro(pattern, body)
                ? do_(analyzeSequence(ctx.extend(), body), bproc ->
                  do_(action(() -> ctx.putMacro((Symbol)var, new Macro(var.show(), pattern, bproc))),
                  pure(env -> pure(Void.VOID))))
                : badSyntax("define-macro", first)))
            .orElseGet(() -> badSyntax("define-macro", first));
    }

    private $<Evaluator, Proc> analyzeValuesDefinition(Env ctx, LispVal vars, LispVal exp) {
        return map(analyze(ctx, exp), vproc ->
               env -> do_(vproc.apply(env), result ->
                      result instanceof MultiVal
                        ? defineValues(env, vars, ((MultiVal)result).value)
                        : defineValues(env, vars, Pair.of(result))));
    }

    private $<Evaluator, LispVal> defineValues(Env env, LispVal vars, LispVal vals) {
        int nvars = 0;
        while (vars.isPair() && vals.isPair()) {
            Pair pn = (Pair)vars, pv = (Pair)vals;
            env.put((Symbol)pn.head, pv.head);
            vars = pn.tail;
            vals = pv.tail;
            nvars++;
        }

        if (!vars.isNil()) {
            while (vars.isPair()) {
                nvars++;
                vars = ((Pair)vars).tail;
            }
            return throwE(new NumArgs(nvars, vals));
        }

        if (!vals.isNil()) {
            return throwE(new NumArgs(nvars, vals));
        }

        return pure(Void.VOID);
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
        Env eenv = env.extend();
        int nvars = 0;

        while (params.isPair() && args.isPair()) {
            Pair   pp  = (Pair)params;
            Pair   pv  = (Pair)args;
            Symbol var = (Symbol)pp.head;

            eenv.put(var, pv.head);
            params = pp.tail;
            args   = pv.tail;
            nvars++;
        }

        if (params.isPair()) {
            // less arguments than parameters
            do {
                nvars++;
                params = ((Pair)params).tail;
            } while (params.isPair());
            return throwE(new NumArgs(nvars, args));
        }

        if (params.isNil() && !args.isNil()) {
            // more arguments than parameters
            return throwE(new NumArgs(nvars, args));
        }

        if (params.isSymbol()) {
            // varargs parameter
            eenv.put((Symbol)params, args);
        }

        return proc.apply(eenv);
    }

    public $<Evaluator, Proc> analyzeSequence(Env ctx, LispVal exps) {
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
                           tval.isTrue() ? cproc.apply(env)
                                         : aproc.apply(env))))));
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
                           tval.isTrue()
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
                          tval.isTrue() ? pure(tval)
                                        : rest.apply(env)));
        } else {
            return bind(analyze(ctx, test), tproc ->
                   map(analyzeSequence(ctx, body), bproc ->
                   env -> do_(tproc.apply(env), tval ->
                          tval.isTrue() ? bproc.apply(env)
                                        : rest.apply(env))));
        }
    }

    // -----------------------------------------------------------------------

    private $<Evaluator, Proc> analyzeMatch(Env ctx, LispVal exp, LispVal spec) {
        return do_(analyze(ctx, exp), vproc ->
               do_(analyzeMatchClauses(ctx, spec), mproc ->
               pure(env -> do_(vproc.apply(env), val ->
                           mproc.apply(env, val.getValue())))));
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
                           tval.isTrue() ? bproc.apply(eenv) :
                           rest == null  ? throwE(new PatternMismatch(guard, value))
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

    public static Either<LispError, Env> match(Env env, LispVal pattern, LispVal value) {
        return doMatch(pattern, value, HashPMap.empty()).map(bindings ->
               env.extend(bindings));
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

        boolean add(LispVal var, LispVal init) {
            if (var.isSymbol()) {
                vars  = Seq.cons((Symbol)var, vars);
                inits = Seq.cons(init, inits);
                return true;
            } else {
                return false;
            }
        }
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
                .when(List((var      ) -> lp.add(var, Void.VOID)))
                .when(List((var, init) -> lp.add(var, init)))
                .orElse(false);
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
        Env eenv = env.extend();
        while (!params.isEmpty()) {
            eenv.put(params.head(), args.head());
            params = params.tail();
            args = args.tail();
        }
        return body.apply(eenv);
    }

    private $<Evaluator, LispVal> setVariables(Env env, Seq<Symbol> params, Seq<LispVal> args) {
        while (!params.isEmpty()) {
            env.lookup(params.head()).get().set(args.head());
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

    private $<Evaluator, Proc> translateLetStar(Seq<Symbol> params, Seq<Proc> inits, Proc body) {
        if (params.isEmpty()) {
            return pure(env -> body.apply(env.extend()));
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

    private $<Evaluator, Proc> translateLetrecStar(Seq<Symbol> params, Seq<Proc> inits, Proc body) {
        return pure(env ->
            extendEnv(env, params, params.map(Fn.pure(Void.VOID)),
            eenv -> do_(zipM_(params, inits, (n, v) -> setVar(eenv, n, v)),
                    do_(() -> body.apply(eenv.extend())))));
    }

    // -----------------------------------------------------------------------

    private final Symbol QQ = getsym("quasiquote");
    private final Symbol UNQ = getsym("unquote");
    private final Symbol UNQS = getsym("unquote-splicing");

    public $<Evaluator, LispVal> evalUnquote(Env env, LispVal exp) {
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
              Primitives.append(this, xs, ys)))))

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

    public $<Evaluator, LispVal> callCC(Env env, LispVal proc) {
        Function<Function<LispVal, $<Evaluator, LispVal>>, $<Evaluator, LispVal>> f =
            k -> apply(env, proc, Pair.of(makeContProc(env, k)));

        return $(inner().<Either<LispError, LispVal>>callCC(c ->
            runExcept(f.apply(a -> $(c.escape(right(a)))))));
    }

    private static Func makeContProc(Env env, Function<LispVal, $<Evaluator, LispVal>> k) {
        Symbol cid = env.newsym();
        return new Func(cid, eenv -> k.apply(makeMultiVal(eenv.get(cid))), env);
    }

    public $<Evaluator, LispVal> values(LispVal args) {
        return pure(makeMultiVal(args));
    }

    public $<Evaluator, LispVal> callWithValues(Env env, LispVal producer, LispVal consumer) {
        return do_(apply(env, producer, Nil), result ->
               result instanceof MultiVal
                 ? apply(env, consumer, ((MultiVal)result).value)
                 : apply(env, consumer, Pair.of(result)));
    }

    private static LispVal makeMultiVal(LispVal args) {
        if (args.isPair() && ((Pair)args).tail.isNil()) {
            return ((Pair)args).head;
        } else {
            return new MultiVal(args);
        }
    }

    public $<Evaluator, LispVal> reset(Env env, Proc proc) {
        return $(inner().reset(runExcept(proc.apply(env))));
    }

    public $<Evaluator, LispVal> shift(Env env, Symbol id, Proc proc) {
        Function<Function<LispVal, $<Evaluator, LispVal>>, $<Evaluator, LispVal>> f =
            k -> extendEnv(env, Seq.of(id), Seq.of(makeShiftProc(env, k)), proc);

        return $(inner().<Either<LispError,LispVal>, Either<LispError,LispVal>>shift(c ->
            runExcept(f.apply(a -> $(inner().lift(c.apply(right(a))))))));
    }

    private static Func makeShiftProc(Env env, Function<LispVal, $<Evaluator, LispVal>> k) {
        Symbol cid = env.newsym();
        return new Func(Pair.of(cid), eenv -> k.apply(eenv.get(cid)), env);
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

    public Symbol getsym(String name) {
        return parser.getsym(name);
    }

    public KeySym getkeysym(String name) {
        return parser.getkeysym(name);
    }

    // =======================================================================

    public void loadPrimitives(Env env, Class<?> primLib) {
        for (Method method : primLib.getMethods()) {
            int mods = method.getModifiers();
            if (Modifier.isPublic(mods) && Modifier.isStatic(mods)) {
                String name = getPrimName(method);
                LispVal prim = loadPrimitive(name, method);
                if (prim instanceof PrimMacro) {
                    env.putMacro(getsym(name), prim);
                } else {
                    env.put(getsym(name), prim);
                }
            }
        }
    }

    @SuppressWarnings("unchecked")
    public LispVal loadPrimitive(String name, Method method) {
        int mods = method.getModifiers();
        if (!(Modifier.isPublic(mods) && Modifier.isStatic(mods))) {
            throw new LispError("The primitive method must be public and static: " + method);
        }

        if (method.isAnnotationPresent(Syntax.class)) {
            if (!checkMacroMethod(method)) {
                throw new LispError("Invalid syntax method: " + method);
            }

            return new PrimMacro(name, (env, args) -> {
                try {
                    return ($<Evaluator, Proc>)method.invoke(null, this, env, args);
                } catch (InvocationTargetException ex) {
                    return throwE(new LispError(ex.getTargetException()));
                } catch (Exception ex) {
                    return throwE(new LispError(ex));
                }
            });
        } else {
            if (!checkPrimMethod(method)) {
                throw new LispError("Invalid primitive method: " + method);
            }

            BiFunction<Env, LispVal, $<Evaluator, Object[]>>
                unpacker = unpackArgs(method);
            Packer packer = packRetval(method.getReturnType());

            return new Prim(name, (env, args) ->
                do_(unpacker.apply(env, args), jargs ->
                do_(invoke(method, jargs), result ->
                result == null
                  ? pure(Void.VOID)
                  : packer.apply(result))));
        }
    }

    private static String getPrimName(Method method) {
        Name nameTag = method.getAnnotation(Name.class);
        return nameTag != null ? nameTag.value() : method.getName().replace('_', '-');
    }

    private static boolean isVarArgs(Method method) {
        return method.isAnnotationPresent(VarArgs.class);
    }

    private static boolean checkPrimMethod(Method method) {
        if (method.getReturnType() == $.class) {
            return checkGenericReturnType(method, $.class, Evaluator.class, LispVal.class);
        } else if (method.getReturnType() == Either.class) {
            return checkGenericReturnType(method, Either.class, LispError.class, LispVal.class);
        } else {
            return true;
        }
    }

    private static boolean checkMacroMethod(Method method) {
        Class<?>[] params = method.getParameterTypes();
        return params.length == 3
            && params[0] == Evaluator.class
            && params[1] == Env.class
            && params[2] == LispVal.class
            && checkGenericReturnType(method, $.class, Evaluator.class, Proc.class);
    }

    private static boolean checkGenericReturnType(Method method, Class<?> rawType, Class<?>... args) {
        Type rt = method.getGenericReturnType();
        if (!(rt instanceof ParameterizedType))
            return false;

        ParameterizedType ret = (ParameterizedType)rt;
        Type[] targs = ret.getActualTypeArguments();
        return ret.getRawType() == rawType && Arrays.equals(targs, args);
    }

    private BiFunction<Env, LispVal, $<Evaluator, Object[]>> unpackArgs(Method method) {
        Class<?>[] params  = method.getParameterTypes();
        boolean    varargs = isVarArgs(method);
        boolean    passMe  = false;
        boolean    passEnv = false;
        int        nparams = params.length;
        Unpacker[] required, optional;
        int        i       = 0;

        if (nparams > 0 && params[i] == Evaluator.class) {
            passMe = true;
            nparams--;
            i++;
        }

        if (nparams > 0 && params[i] == Env.class) {
            passEnv = true;
            nparams--;
            i++;
        }

        if (varargs) {
            if (nparams == 0 || params[params.length - 1] != LispVal.class)
                throw new LispError("The last argument of a VarArgs method must be a LispVal");
            nparams--;
        }

        optional = getOptionalArgs(method, varargs);
        nparams -= optional.length;

        required = new Unpacker[nparams];
        for (int j = 0; j < nparams; i++, j++) {
            Maybe<Unpacker> unpacker = getUnpacker(params[i]);
            if (unpacker.isAbsent())
                throw new LispError("Unrecognized java type: " + params[i].getName());
            required[j] = unpacker.get();
        }

        return unpackArgs(params.length, passMe, passEnv, required, optional, varargs);
    }

    private Unpacker[] getOptionalArgs(Method method, boolean varargs) {
        Type[]      params   = method.getGenericParameterTypes();
        int         nparams  = params.length - (varargs ? 1 : 0);
        int         iopt     = -1;
        Unpacker[]  optional = new Unpacker[0];

        for (int i = 0; i < nparams; i++) {
            Unpacker unpacker = getOptionalUnpacker(params[i]);
            if (unpacker != null) {
                if (iopt == -1) {
                    optional = new Unpacker[nparams - i];
                    iopt = 0;
                }
                optional[iopt++] = unpacker;
            } else {
                if (iopt != -1) {
                    throw new LispError("Optional arguments must be contiguous: " + method);
                }
            }
        }

        return optional;
    }

    private Unpacker getOptionalUnpacker(Type param) {
        if (param instanceof ParameterizedType) {
            ParameterizedType t = (ParameterizedType)param;
            Type[] as = t.getActualTypeArguments();

            if (t.getRawType() == Maybe.class && as.length == 1 && as[0] instanceof Class) {
                Maybe<Unpacker> v = getUnpacker((Class)as[0]);
                if (v.isAbsent())
                    throw new LispError("Unrecognized java type: " + as[0].getTypeName());
                return v.get();
            }
        }

        return null;
    }

    private BiFunction<Env, LispVal, $<Evaluator, Object[]>>
    unpackArgs(int nargs, boolean passMe, boolean passEnv,
               Unpacker[] required, Unpacker[] optional, boolean varargs) {
        return (env, args) -> {
            Object[] res = new Object[nargs];
            int nreq = required.length;
            int nopt = optional.length;
            int i    = 0;
            int j;

            if (passMe)
                res[i++] = this;
            if (passEnv)
                res[i++] = env;

            for (j = 0; j < nreq && args.isPair(); i++, j++) {
                Pair p = (Pair)args;
                Either<LispError, Object> v = required[j].apply(p.head.getValue());
                if (v.isLeft())
                    return throwE(v.left());
                res[i] = v.right();
                args = p.tail;
            }

            if (j < nreq)
                return throwE(new NumArgs(nreq, args));

            for (j = 0; j < nopt; i++, j++) {
                if (args.isNil()) {
                    res[i] = Maybe.empty();
                } else if (args.isPair()) {
                    Pair p = (Pair)args;
                    Either<LispError,Object> v = optional[j].apply(p.head.getValue());
                    if (v.isLeft())
                        return throwE(v.left());
                    res[i] = Maybe.of(v.right());
                    args = p.tail;
                } else {
                    return throwE(new TypeMismatch("pair", args));
                }
            }

            if (varargs) {
                if (!args.allMatch(x -> !(x instanceof MultiVal)))
                    args = args.map(LispVal::getValue);
                res[i] = args;
            } else if (!args.isNil()) {
                return throwE(new NumArgs(nreq + nopt, args));
            }

            return pure(res);
        };
    }

    private Packer packRetval(Class<?> type) {
        Maybe<Packer> v = getPacker(type);
        if (v.isPresent()) {
            return v.get();
        } else {
            throw new LispError("Unrecognized java type: " + type.getName());
        }
    }

    private $<Evaluator, Object> invoke(Method method, Object[] args) {
        try {
            return pure(method.invoke(null, args));
        } catch (InvocationTargetException ex) {
            Throwable cause = ex.getTargetException();
            if (cause instanceof LispError)
                return throwE((LispError)cause);
            return throwE(new LispError(cause));
        } catch (Exception ex) {
            return throwE(new LispError(ex));
        }
    }

    // ---------------------------------------------------------------------

    @FunctionalInterface
    private interface Unpacker {
        Either<LispError, Object> apply(LispVal val);
    }

    @FunctionalInterface
    private interface Packer {
        $<Evaluator, LispVal> apply(Object obj);
    }

    private final Seq<Tuple<Class<?>, Unpacker>> unpackers = Seq.of(
        Tuple.of(Boolean.class,   Evaluator::unpackBoolean),
        Tuple.of(Boolean.TYPE,    Evaluator::unpackBoolean),
        Tuple.of(Character.class, Evaluator::unpackChar),
        Tuple.of(Character.TYPE,  Evaluator::unpackChar),
        Tuple.of(String.class,    Evaluator::unpackString),
        Tuple.of(Double.class,    Evaluator::unpackDouble),
        Tuple.of(Double.TYPE,     Evaluator::unpackDouble),
        Tuple.of(Integer.class,   Evaluator::unpackInt),
        Tuple.of(Integer.TYPE,    Evaluator::unpackInt),
        Tuple.of(Long.class,      Evaluator::unpackLong),
        Tuple.of(Long.TYPE,       Evaluator::unpackLong),
        Tuple.of(Number.class,    Evaluator::unpackNumber),
        Tuple.of(Symbol.class,    Evaluator::unpackSymbol),
        Tuple.of(Text.class,      Evaluator::unpackText),
        Tuple.of(Pair.class,      Evaluator::unpackPair),
        Tuple.of(Vec.class,       Evaluator::unpackVector),
        Tuple.of(LispVal.class,   val -> right(val))
    );

    @SuppressWarnings("unchecked")
    private final Seq<Tuple<Class<?>, Packer>> packers = Seq.of(
        Tuple.of(Either.class,   obj -> except((Either<LispError, LispVal>)obj)),
        Tuple.of($.class,        obj -> ($<Evaluator, LispVal>)obj),
        Tuple.of(java.lang.Void.TYPE , obj -> pure(Void.VOID)),
        Tuple.of(Boolean.TYPE,   obj -> pure(Bool.valueOf((Boolean)obj))),
        Tuple.of(Character.TYPE, obj -> pure(new Char((Character)obj))),
        Tuple.of(String.class,   obj -> pure(new Text((String)obj))),
        Tuple.of(Double.TYPE,    obj -> pure(new Num((Double)obj))),
        Tuple.of(Integer.TYPE,   obj -> pure(new Num((long)(Integer)obj))),
        Tuple.of(Long.TYPE,      obj -> pure(new Num((Long)obj))),
        Tuple.of(Number.class,   obj -> pure(new Num((Number)obj))),
        Tuple.of(LispVal.class,  obj -> pure((LispVal)obj))
    );

    private Maybe<Unpacker> getUnpacker(Class<?> type) {
        return unpackers.findFirst(t -> t.first().isAssignableFrom(type)).map(Tuple::second);
    }

    private Maybe<Packer> getPacker(Class<?> type) {
        return packers.findFirst(t -> t.first().isAssignableFrom(type)).map(Tuple::second);
    }

    private static Either<LispError, Object> unpackBoolean(LispVal val) {
        return val instanceof Bool
            ? right(((Bool)val).value)
            : left(new TypeMismatch("boolean", val));
    }

    private static Either<LispError, Object> unpackChar(LispVal val) {
        return val instanceof Char
            ? right(((Char)val).value)
            : left(new TypeMismatch("char", val));
    }

    private static Either<LispError, Object> unpackString(LispVal val) {
        return val instanceof Text
            ? right(((Text)val).value)
            : left(new TypeMismatch("string", val));
    }

    private static Either<LispError, Object> unpackDouble(LispVal val) {
        return val instanceof Num
            ? right(((Num)val).value.doubleValue())
            : left(new TypeMismatch("real", val));
    }

    private static Either<LispError, Object> unpackInt(LispVal val) {
        return val instanceof Num
            ? right(((Num)val).value.intValue())
            : left(new TypeMismatch("integer", val));
    }

    private static Either<LispError, Object> unpackLong(LispVal val) {
        return val instanceof Num
            ? right(((Num)val).value.longValue())
            : left(new TypeMismatch("long", val));
    }

    private static Either<LispError, Object> unpackNumber(LispVal val) {
        return val instanceof Num
            ? right(((Num)val).value)
            : left(new TypeMismatch("number", val));
    }

    private static Either<LispError, Object> unpackSymbol(LispVal val) {
        return val instanceof Symbol
            ? right(val)
            : left(new TypeMismatch("symbol", val));
    }

    private static Either<LispError, Object> unpackText(LispVal val) {
        return val instanceof Text
            ? right(val)
            : left(new TypeMismatch("string", val));
    }

    private static Either<LispError, Object> unpackPair(LispVal val) {
        return val instanceof Pair
            ? right(val)
            : left(new TypeMismatch("pair", val));
    }

    private static Either<LispError, Object> unpackVector(LispVal val) {
        return val instanceof Vec
            ? right(val)
            : left(new TypeMismatch("vector", val));
    }
}
