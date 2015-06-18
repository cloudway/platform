/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.scheme;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Comparator;
import java.util.function.BiFunction;

import com.cloudway.fp.$;
import com.cloudway.fp.data.HashPMap;
import com.cloudway.fp.data.Maybe;
import com.cloudway.fp.data.PMap;
import com.cloudway.fp.data.Seq;
import com.cloudway.fp.function.TriFunction;
import com.cloudway.fp.scheme.LispVal.Pair;

final class JClassDecl {
    static class Dispatcher {
        final Class<?>[] params;
        final boolean isVarArgs;
        final boolean isStatic;

        Dispatcher(Class<?>[] params, boolean isVarArgs, boolean isStatic) {
            this.params     = params;
            this.isVarArgs  = isVarArgs;
            this.isStatic   = isStatic;
        }
    }

    static class MethodDispatcher extends Dispatcher {
        final TriFunction<Env, Object, LispVal, $<Evaluator, LispVal>> dispatcher;

        MethodDispatcher(Method method,
        TriFunction<Env, Object, LispVal, $<Evaluator, LispVal>> dispatcher) {
            super(method.getParameterTypes(),
                  method.isVarArgs(),
                  Modifier.isStatic(method.getModifiers()));
            this.dispatcher = dispatcher;
        }
    }

    static class ConstructorDispatcher extends Dispatcher {
        final BiFunction<Env, LispVal, $<Evaluator, LispVal>> dispatcher;

        ConstructorDispatcher(Constructor<?> cons,
        BiFunction<Env, LispVal, $<Evaluator, LispVal>> dispatcher) {
            super(cons.getParameterTypes(), cons.isVarArgs(), false);
            this.dispatcher = dispatcher;
        }
    }

    static class Builder {
        private Seq<ConstructorDispatcher> constructors = Seq.nil();
        private PMap<String, Seq<MethodDispatcher>> methods = HashPMap.empty();

        public void addConstructor(Constructor<?> cons,
        BiFunction<Env, LispVal, $<Evaluator, LispVal>> dispatcher) {
            ConstructorDispatcher cd = new ConstructorDispatcher(cons, dispatcher);
            constructors = Seq.cons(cd, constructors);
        }

        public void addMethod(String name, Method method,
        TriFunction<Env, Object, LispVal, $<Evaluator, LispVal>> dispatcher) {
            MethodDispatcher md = new MethodDispatcher(method, dispatcher);
            methods = methods.merge(name, Seq.of(md), Seq::append);
        }

        public JClassDecl build() {
            constructors = optimize(constructors);
            methods = methods.map(Builder::optimize);
            return new JClassDecl(constructors, methods);
        }

        private static <A extends Dispatcher> Seq<A> optimize(Seq<A> dispatchers) {
            return dispatchers.sorted(new ParameterComparator());
        }

        private static class ParameterComparator implements Comparator<Dispatcher> {
            @Override
            public int compare(Dispatcher a, Dispatcher b) {
                if (a.params.length != b.params.length)
                    return Integer.compare(a.params.length, b.params.length);

                for (int i = 0; i < a.params.length; i++) {
                    int c = compareType(a.params[i], b.params[i]);
                    if (c != 0)
                        return c;
                }

                return 0;
            }

            private static int compareType(Class<?> t1, Class<?> t2) {
                if (t1 == t2)
                    return 0;

                if (t1.isAssignableFrom(t2))
                    return 1;
                if (t2.isAssignableFrom(t1))
                    return -1;

                int o1 = getNumberOrder(t1);
                int o2 = getNumberOrder(t2);
                if (o1 != -1 && o2 != -1)
                    return o1 - o2;

                // order is not significant for unrelated types but
                // we put primitive types before object types
                return o1 != -1 ? -1 : o2 != -1 ? 1 : 0;
            }
        }
    }

    private final Seq<ConstructorDispatcher> constructors;
    private final PMap<String, Seq<MethodDispatcher>> methods;

    private JClassDecl(Seq<ConstructorDispatcher> constructors,
                       PMap<String, Seq<MethodDispatcher>> methods) {
        this.constructors = constructors;
        this.methods = methods;
    }

    public Maybe<BiFunction<Env, LispVal, $<Evaluator, LispVal>>>
    getConstructorDispatcher(LispVal args) {
        return findBestMatch(constructors, args, false).map(cd -> cd.dispatcher);
    }

    public Maybe<TriFunction<Env, Object, LispVal, $<Evaluator, LispVal>>>
    getMethodDispatcher(boolean _static, String name, LispVal args) {
        return methods.lookup(name)
                      .flatMap(mms -> findBestMatch(mms, args, _static))
                      .map(md -> md.dispatcher);
    }

    private static <A extends Dispatcher>
    Maybe<A> findBestMatch(Seq<A> mms, LispVal args, boolean _static) {
        if (mms.tail().isEmpty()) {
            return mms.peek();
        }

        int nargs = count(args);
        Maybe<A> candidate;

        candidate = mms.findFirst(md ->
            md.isStatic == _static && !md.isVarArgs &&
            nargs == md.params.length &&
            typesMatched(md.params, nargs, args));
        if (candidate.isPresent())
            return candidate;

        candidate = mms.findFirst(md ->
            md.isStatic == _static && md.isVarArgs &&
            nargs >= md.params.length - 1 &&
            typesMatched(md.params, md.params.length - 1, args));
        if (candidate.isPresent())
            return candidate;

        return Maybe.empty();
    }

    private static boolean typesMatched(Class<?>[] params, int nargs, LispVal args) {
        for (int i = 0; i < nargs; i++) {
            Pair p = (Pair)args;
            args = p.tail;

            Class<?> type = LispVal.class.isAssignableFrom(params[i])
                ? p.head.getClass() : p.head.getObjectType();

            if (!typeMatches(params[i], type)) {
                return false;
            }
        }

        return true;
    }

    private static boolean typeMatches(Class<?> t, Class<?> v) {
        if (getBoxedType(t).isAssignableFrom(v))
            return true;

        int o1 = getNumberOrder(t);
        int o2 = getNumberOrder(v);
        return o1 != -1 && o2 != -1 && o1 >= o2;
    }

    private static int count(LispVal args) {
        int nargs = 0;
        for (LispVal p = args; p.isPair(); p = ((Pair)p).tail)
            nargs++;
        return nargs;
    }

    private static Class<?> getBoxedType(Class<?> t) {
        if (t.isPrimitive()) {
            if (t == Integer.TYPE)
                return Integer.class;
            if (t == Long.TYPE)
                return Long.class;
            if (t == Short.TYPE)
                return Short.class;
            if (t == Byte.TYPE)
                return Byte.class;
            if (t == Boolean.TYPE)
                return Boolean.class;
            if (t == Character.TYPE)
                return Character.class;
            if (t == Double.TYPE)
                return Double.class;
            if (t == Float.TYPE)
                return Float.class;
        }
        return t;
    }

    private static int getNumberOrder(Class<?> t) {
        if (t == Byte.class || t == Byte.TYPE)
            return 1;
        if (t == Short.class || t == Short.TYPE)
            return 2;
        if (t == Integer.class || t == Integer.TYPE)
            return 3;
        if (t == Long.class || t == Long.TYPE)
            return 4;
        if (t == Float.class || t == Float.TYPE)
            return 5;
        if (t == Double.class || t == Double.TYPE)
            return 6;
        return -1;
    }
}
