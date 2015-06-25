/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.scheme;

import javax.script.AbstractScriptEngine;
import javax.script.Bindings;
import javax.script.Invocable;
import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineFactory;
import javax.script.ScriptException;
import javax.script.SimpleBindings;

import java.io.Reader;

import com.cloudway.fp.data.Either;
import com.cloudway.fp.data.Fn;
import com.cloudway.fp.data.Maybe;
import com.cloudway.fp.data.Ref;
import com.cloudway.fp.scheme.LispVal.Pair;
import com.cloudway.fp.scheme.LispVal.Symbol;
import static javax.script.ScriptContext.ENGINE_SCOPE;

class SchemeScriptEngine extends AbstractScriptEngine
    implements Invocable
{
    private final SchemeScriptEngineFactory factory;
    private final Evaluator evaluator;
    private final Env env;
    
    SchemeScriptEngine(SchemeScriptEngineFactory factory) {
        this.factory = factory;
        this.evaluator = new Evaluator();
        this.env = evaluator.getSchemeReportEnv();
    }
    
    @Override
    public Object eval(String script, ScriptContext ctx) throws ScriptException {
        try {
            return evaluator.run(extendEnv(ctx), evaluator.parse(script))
                            .getOrThrow(Fn.id())
                            .getObject();
        } catch (LispError ex) {
            throw new ScriptException(ex);
        }
    }

    @Override
    public Object eval(Reader reader, ScriptContext ctx) throws ScriptException {
        try {
            String filename = (String)get(ScriptEngine.FILENAME);
            return evaluator.run(extendEnv(ctx), evaluator.parse(filename, reader))
                            .getOrThrow(Fn.id())
                            .getObject();
        } catch (LispError ex) {
            throw new ScriptException(ex);
        }
    }

    private Env extendEnv(ScriptContext ctx) {
        for (String var : ctx.getBindings(ENGINE_SCOPE).keySet()) {
            Symbol sym = evaluator.getsym(var);

            Maybe<Ref<LispVal>> ref = env.lookup(sym);
            if (ref.isPresent() &&
                ref.get() instanceof DynamicRef &&
                ((DynamicRef)ref.get()).ctx == ctx) {
                continue;
            }

            env.putRef(sym, new DynamicRef(ctx, var));
        }

        return env;
    }

    private static class DynamicRef extends Ref<LispVal> {
        private static final long serialVersionUID = 1431808158706186279L;

        final ScriptContext ctx;
        final String var;

        DynamicRef(ScriptContext ctx, String var) {
            this.ctx = ctx;
            this.var = var;
        }

        @Override
        public LispVal get() {
            return Packer.pack(ctx.getAttribute(var, ENGINE_SCOPE));
        }

        @Override
        public LispVal set(LispVal newValue) {
            ctx.setAttribute(var, newValue.getObject(), ENGINE_SCOPE);
            return newValue;
        }
    }

    @Override
    public Bindings createBindings() {
        return new SimpleBindings();
    }

    @Override
    public ScriptEngineFactory getFactory() {
        return factory;
    }

    @Override
    public Object invokeFunction(String name, Object... args)
        throws ScriptException, NoSuchMethodException {
        Maybe<Ref<LispVal>> func = env.lookup(evaluator.getsym(name));

        if (func.isAbsent())
            throw new NoSuchMethodException(name);

        Either<LispError, LispVal>
            result = evaluator.run(evaluator.apply(env, func.get().get(), packArgs(args)));

        if (result.isLeft()) {
            throw new ScriptException(result.left());
        } else {
            return result.right().getObject();
        }
    }

    private static LispVal packArgs(Object[] args) {
        LispVal res = LispVal.Nil;
        for (int i = args.length; --i >= 0; ) {
            res = Pair.cons(Packer.pack(args[i]), res);
        }
        return res;
    }

    @Override
    public Object invokeMethod(Object thiz, String name, Object... args)
        throws ScriptException, NoSuchMethodException {
        throw new NoSuchMethodException(name);
    }

    @Override
    public <T> T getInterface(Class<T> clasz) {
        return null;
    }

    @Override
    public <T> T getInterface(Object thiz, Class<T> clasz) {
        return null;
    }
}
