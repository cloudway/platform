/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.scheme;

import java.lang.invoke.MethodHandles;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

import com.cloudway.fp.$;
import com.cloudway.fp.data.Fn;
import static com.cloudway.fp.scheme.LispVal.Nil;
import static com.cloudway.fp.scheme.LispVal.VOID;
import static com.cloudway.fp.control.Syntax.do_;

class JLambda {
    private final Packer[] packers;
    private final Unpacker unpacker;
    private final Class<?> ret;
    private final LispVal  lambda;

    private JLambda(Packer[] packers, Unpacker unpacker, Class<?> ret, LispVal lambda) {
        this.packers  = packers;
        this.unpacker = unpacker;
        this.ret      = ret;
        this.lambda   = lambda;
    }

    static Method findInterfaceMethod(Class<?> cls) {
        Method interface_method = null;
        if (cls.isInterface()) {
            for (Method method : cls.getMethods()) {
                if (Modifier.isAbstract(method.getModifiers())) {
                    if (interface_method != null)
                        return null;
                    interface_method = method;
                }
            }
        }
        return interface_method;
    }

    static boolean isFunctionalInterface(Class<?> cls) {
        return findInterfaceMethod(cls) != null;
    }

    static JLambda make(Method method, LispVal lambda) {
        Class<?>[] params = method.getParameterTypes();
        Packer[] packers = new Packer[params.length];
        for (int i = 0; i < params.length; i++) {
            packers[i] = Packer.get(params[i]);
        }

        Class<?> ret = method.getReturnType();
        Unpacker unpacker = Unpacker.get(ret);

        return new JLambda(packers, unpacker, ret, lambda);
    }

    Object invoke(Env env, Object[] args) throws Throwable {
        Evaluator me = env.getEvaluator();
        $<Evaluator, Object> result =
            do_(me.apply(env, lambda, packArguments(args, packers)), r ->
            do_(me.except(env, unpacker.apply(env, ret, r.normalize()))));
        return me.run(result).getOrThrow(Fn.id());
    }

    private static LispVal packArguments(Object[] args, Packer[] packers) {
        assert args.length == packers.length;
        LispVal res = Nil;

        for (int i = args.length; --i >= 0; ) {
            LispVal x;
            if (args[i] == null) {
                x = VOID;
            } else if (args[i] instanceof LispVal) {
                x = (LispVal)args[i];
            } else {
                x = packers[i].apply(args[i]);
            }
            res = LispVal.Pair.cons(x, res);
        }
        return res;
    }

    private static final Constructor<MethodHandles.Lookup> forLookup;

    static {
        try {
            forLookup = MethodHandles.Lookup.class.getDeclaredConstructor(Class.class, int.class);
            if (!forLookup.isAccessible()) {
                forLookup.setAccessible(true);
            }
        } catch (Exception ex) {
            throw new InternalError(ex);
        }
    }

    static Object invokeDefault(Object proxy, Method method, Object[] args) throws Throwable {
        return forLookup
            .newInstance(method.getDeclaringClass(), MethodHandles.Lookup.PRIVATE)
            .unreflectSpecial(method, method.getDeclaringClass())
            .bindTo(proxy)
            .invokeWithArguments(args);
    }
}
