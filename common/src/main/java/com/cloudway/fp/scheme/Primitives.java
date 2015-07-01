/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.scheme;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.function.BiPredicate;

import com.cloudway.fp.$;
import com.cloudway.fp.data.Either;
import com.cloudway.fp.data.Fn;
import com.cloudway.fp.data.Maybe;
import com.cloudway.fp.data.Tuple;
import com.cloudway.fp.scheme.numsys.Num;

import com.google.common.reflect.AbstractInvocationHandler;
import com.google.common.reflect.Reflection;

import static com.cloudway.fp.control.Conditionals.with;
import static com.cloudway.fp.control.Syntax.do_;
import static com.cloudway.fp.scheme.LispError.*;
import static com.cloudway.fp.scheme.LispVal.*;
import static com.cloudway.fp.scheme.Evaluator.match;
import static java.lang.Character.toLowerCase;

@SuppressWarnings("unused")
public final class Primitives {
    private Primitives() {}

    @Name("boolean?")
    public static boolean isBoolean(LispVal val) {
        return val instanceof Bool;
    }

    @Name("symbol?")
    public static boolean isSymbol(LispVal val) {
        return val.isSymbol();
    }

    @Name("keyword?")
    public static boolean isKeyword(LispVal val) {
        return val instanceof KeySym;
    }

    @Name("char?")
    public static boolean isChar(LispVal val) {
        return val instanceof Char;
    }

    @Name("string?")
    public static boolean isString(LispVal val) {
        return val instanceof Text;
    }

    @Name("vector?")
    public static boolean isVector(LispVal val) {
        return val instanceof Vec;
    }

    @Name("promise?")
    public static boolean isPromise(LispVal val) {
        return val instanceof Promise;
    }

    @Name("list?")
    public static boolean isList(LispVal val) {
        return val.isList();
    }

    @Name("pair?")
    public static boolean isPair(LispVal val) {
        return val instanceof Pair;
    }

    @Name("null?")
    public static boolean isNull(LispVal val) {
        return val.isNil();
    }

    @Name("procedure?")
    public static boolean isProcedure(LispVal val) {
        return val.isProcedure();
    }

    // ---------------------------------------------------------------------

    @Name("eq?")
    public static boolean eq(LispVal x, LispVal y) {
        return x.eqv(y);
    }

    @Name("eqv?")
    public static boolean eqv(LispVal x, LispVal y) {
        return x.eqv(y);
    }

    @Name("equal?")
    public static boolean equal(LispVal x, LispVal y) {
        return x.equals(y);
    }

    // ---------------------------------------------------------------------

    public static LispVal cons(LispVal x, LispVal y) {
        return Pair.cons(x, y);
    }

    public static LispVal car(Pair pair) {
        return pair.head;
    }

    public static LispVal cdr(Pair pair) {
        return pair.tail;
    }

    @Name("set-car!")
    public static void set_car(Pair pair, LispVal val) {
        pair.head = val;
    }

    @Name("set-cdr!")
    public static void set_cdr(Pair pair, LispVal val) {
        pair.tail = val;
    }

    @VarArgs
    public static LispVal list(LispVal args) {
        return args;
    }

    public static long length(LispVal lst) {
        long len = 0;
        while (lst.isPair()) {
            len++;
            lst = ((Pair)lst).tail;
        }

        if (lst.isNil()) {
            return len;
        } else {
            throw new TypeMismatch("pair", lst);
        }
    }

    public static LispVal list_ref(LispVal lst, int k) {
        while (k != 0 && lst.isPair()) {
            k--;
            lst = ((Pair)lst).tail;
        }

        if (k == 0 && lst.isPair()) {
            return ((Pair)lst).head;
        } else {
            throw new TypeMismatch("pair", lst);
        }
    }

    public static LispVal list_tail(LispVal lst, int k) {
        while (k != 0 && lst.isPair()) {
            k--;
            lst = ((Pair)lst).tail;
        }

        if (k == 0) {
            return lst;
        } else {
            throw new TypeMismatch("pair", lst);
        }
    }

    private static LispVal mem_helper(LispVal item, LispVal lst, BiPredicate<LispVal, LispVal> pred) {
        while (lst.isPair()) {
            Pair p = (Pair)lst;
            if (pred.test(item, p.head))
                return lst;
            lst = p.tail;
        }

        if (lst.isNil()) {
            return Bool.FALSE;
        } else {
            throw new TypeMismatch("pair", lst);
        }
    }

    public static LispVal memq(LispVal item, LispVal lst) {
        return mem_helper(item, lst, Primitives::eq);
    }

    public static LispVal memv(LispVal item, LispVal lst) {
        return mem_helper(item, lst, Primitives::eqv);
    }

    public static LispVal member(LispVal item, LispVal lst) {
        return mem_helper(item, lst, Primitives::equal);
    }

    private static LispVal ass_helper(LispVal obj, LispVal alist, BiPredicate<LispVal, LispVal> pred) {
        while (alist.isPair()) {
            Pair p = (Pair)alist;
            if (p.head.isPair()) {
                if (pred.test(obj, ((Pair)p.head).head))
                    return p.head;
            } else {
                throw new TypeMismatch("pair", p.head);
            }
            alist = p.tail;
        }

        if (alist.isNil()) {
            return Bool.FALSE;
        } else {
            throw new TypeMismatch("pair", alist);
        }
    }

    public static LispVal assq(LispVal obj, LispVal alist) {
        return ass_helper(obj, alist, Primitives::eq);
    }

    public static LispVal assv(LispVal obj, LispVal alist) {
        return ass_helper(obj, alist, Primitives::eqv);
    }

    public static LispVal assoc(LispVal obj, LispVal alist) {
        return ass_helper(obj, alist, Primitives::equal);
    }

    @VarArgs
    public static $<Evaluator, LispVal> append(Evaluator me, Env env, LispVal lists) {
        return with(lists).<$<Evaluator, LispVal>>get()
            .when(Nil(() -> me.pure(Nil)))
            .when(List(x -> me.pure(x)))
            .when(List((x, y) -> append(me, env, x, y)))
            .when(Cons((x, y, ys) ->
                do_(append(me, env, x, y), hd ->
                do_(append(me, env, ys), tl ->
                do_(append(me, env, hd, tl))))))
            .orElseGet(() -> me.throwE(env, new TypeMismatch("pair", lists)));
    }

    private static $<Evaluator, LispVal> append(Evaluator me, Env env, LispVal xs, LispVal ys) {
        return me.except(env, Pair.append(xs, ys));
    }

    public static $<Evaluator, LispVal> reverse(Evaluator me, Env env, LispVal lst) {
        return me.except(env, Pair.reverse(lst));
    }

    @VarArgs
    public static $<Evaluator, LispVal>
    map(Evaluator me, Env env, LispVal f, LispVal lst, LispVal more) {
        if (lst.isNil()) {
            return me.pure(LispVal.Nil);
        }

        if (lst.isPair()) {
            Pair p = (Pair)lst;
            if (more.isNil()) {
                return do_(me.apply(env, f, Pair.list(p.head)), x ->
                       do_(map(me, env, f, p.tail, Nil), xs ->
                       me.pure(cons(x, xs))));
            } else {
                return do_(cars_cdrs(me, env, more), (as, ds) ->
                       do_(me.apply(env, f, cons(p.head, as)), x ->
                       do_(map(me, env, f, p.tail, ds), xs ->
                       me.pure(cons(x, xs)))));
            }
        }

        return me.throwE(env, new TypeMismatch("pair", lst));
    }

    @VarArgs
    public static $<Evaluator, LispVal>
    flatmap(Evaluator me, Env env, LispVal f, LispVal lst, LispVal more) {
        if (lst.isNil()) {
            return me.pure(LispVal.Nil);
        }

        if (lst.isPair()) {
            Pair p = (Pair)lst;
            if (more.isNil()) {
                return do_(me.apply(env, f, Pair.list(p.head)), xs ->
                       do_(flatmap(me, env, f, p.tail, Nil), ys ->
                       append(me, env, xs, ys)));
            } else {
                return do_(cars_cdrs(me, env, more), (as, ds) ->
                       do_(me.apply(env, f, cons(p.head, as)), xs ->
                       do_(flatmap(me, env, f, p.tail, ds), ys ->
                       append(me, env, xs, ys))));
            }
        }

        return me.throwE(env, new TypeMismatch("pair", lst));
    }

    @VarArgs
    public static $<Evaluator, LispVal>
    filter(Evaluator me, Env env, LispVal pred, LispVal lst, LispVal more) {
        if (lst.isNil()) {
            return me.pure(LispVal.Nil);
        }

        if (lst.isPair()) {
            Pair p = (Pair)lst;
            if (more.isNil()) {
                return do_(me.apply(env, pred, Pair.list(p.head)), result ->
                       result.isTrue()
                         ? do_(filter(me, env, pred, p.tail, Nil), rest ->
                           do_(me.pure(cons(p.head, rest))))
                         : filter(me, env, pred, p.tail, Nil));
            } else {
                return do_(cars_cdrs(me, env, more), (as, ds) ->
                       do_(me.apply(env, pred, cons(p.head, as)), result ->
                       result.isTrue()
                         ? do_(filter(me, env, pred, p.tail, ds), rest ->
                           do_(me.pure(cons(p.head, rest))))
                         : filter(me, env, pred, p.tail, ds)));
            }
        }

        return me.throwE(env, new TypeMismatch("pair", lst));
    }

    @VarArgs
    public static $<Evaluator, LispVal>
    for_each(Evaluator me, Env env, LispVal proc, LispVal lst, LispVal more) {
        if (lst.isNil()) {
            return me.pure(VOID);
        }

        if (lst.isPair()) {
            Pair p = (Pair)lst;
            if (more.isNil()) {
                return do_(me.apply(env, proc, Pair.list(p.head)), () ->
                       do_(for_each(me, env, proc, p.tail, Nil)));
            } else {
                return do_(cars_cdrs(me, env, more), (as, ds) ->
                       do_(me.apply(env, proc, cons(p.head, as)), () ->
                       do_(for_each(me, env, proc, p.tail, ds))));
            }
        }

        return me.throwE(env, new TypeMismatch("pair", lst));
    }

    public static $<Evaluator, LispVal>
    fold_right(Evaluator me, Env env, LispVal op, LispVal init, LispVal lst) {
        if (lst.isNil()) {
            return me.pure(init);
        }

        if (lst.isPair()) {
            Pair p = (Pair)lst;
            return do_(me.delay(() -> fold_right(me, env, op, init, p.tail)), rest ->
                   do_(me.apply(env, op, Pair.list(p.head, rest))));
        }

        return me.throwE(env, new TypeMismatch("pair", lst));
    }

    public static $<Evaluator, LispVal>
    fold_left(Evaluator me, Env env, LispVal op, LispVal init, LispVal lst) {
        $<Evaluator, LispVal> res = me.pure(init);
        while (lst.isPair()) {
            Pair p = (Pair)lst;
            res = me.bind(res, r -> me.apply(env, op, Pair.list(r, p.head)));
            lst = p.tail;
        }
        return lst.isNil() ? res : me.throwE(env, new TypeMismatch("pair", lst));
    }

    private static $<Evaluator, Tuple<LispVal, LispVal>>
    cars_cdrs(Evaluator me, Env env, LispVal lists) {
        LispVal cars = Nil, cdrs = Nil;

        while (lists.isPair()) {
            Pair p = (Pair)lists;
            if (p.head.isNil()) {
                break;
            }

            if (p.head.isPair()) {
                Pair sub = (Pair)p.head;
                cars = cons(sub.head, cars);
                cdrs = cons(sub.tail, cdrs);
            } else {
                return me.throwE(env, new TypeMismatch("pair", p.head));
            }

            lists = p.tail;
        }

        if (lists.isNil()) {
            return me.liftM2(Tuple::of, reverse(me, env, cars), reverse(me, env, cdrs));
        } else {
            return me.throwE(env, new TypeMismatch("pair", lists));
        }
    }

    // ---------------------------------------------------------------------

    @Name("symbol->string")
    public static String symbol2string(Symbol sym) {
        return sym.name;
    }

    @Name("string->symbol")
    public static Symbol string2symbol(Evaluator me, String str) {
        return me.getsym(str);
    }

    @Name("keyword->string")
    public static String keyword2string(KeySym sym) {
        return sym.name;
    }

    @Name("string->keyword")
    public static KeySym string2keyword(Evaluator me, String str) {
        return me.getkeysym(str);
    }

    // ---------------------------------------------------------------------

    @Name("char=?")
    public static boolean char_eq(char c1, char c2) {
        return c1 == c2;
    }

    @Name("char<?")
    public static boolean char_lt(char c1, char c2) {
        return c1 < c2;
    }

    @Name("char>?")
    public static boolean char_gt(char c1, char c2) {
        return c1 > c2;
    }

    @Name("char<=?")
    public static boolean char_le(char c1, char c2) {
        return c1 <= c2;
    }

    @Name("char>=?")
    public static boolean char_ge(char c1, char c2) {
        return c1 >= c2;
    }

    @Name("char-ci=?")
    public static boolean char_ci_eq(char c1, char c2) {
        return toLowerCase(c1) == toLowerCase(c2);
    }

    @Name("char-ci<?")
    public static boolean char_ci_lt(char c1, char c2) {
        return toLowerCase(c1) < toLowerCase(c2);
    }

    @Name("char-ci>?")
    public static boolean char_ci_gt(char c1, char c2) {
        return toLowerCase(c1) > toLowerCase(c2);
    }

    @Name("char-ci<=?")
    public static boolean char_ci_le(char c1, char c2) {
        return toLowerCase(c1) <= toLowerCase(c2);
    }

    @Name("char-ci>=?")
    public static boolean char_ci_ge(char c1, char c2) {
        return toLowerCase(c1) >= toLowerCase(c2);
    }

    @Name("char-alphabetic?")
    public static boolean char_alphabetic(char c) {
        return Character.isAlphabetic(c);
    }

    @Name("char-numeric?")
    public static boolean char_numeric(char c) {
        return Character.isDigit(c);
    }

    @Name("char-whitespace?")
    public static boolean char_whitespace(char c) {
        return Character.isWhitespace(c);
    }

    @Name("char-upper-case?")
    public static boolean char_upper_case(char c) {
        return Character.isUpperCase(c);
    }

    @Name("char-lower-case?")
    public static boolean char_lower_case(char c) {
        return Character.isLowerCase(c);
    }

    @Name("char->integer")
    public static int char2integer(char c) {
        return c;
    }

    @Name("integer->char")
    public static char integer2char(int n) {
        return (char)n;
    }

    public static char char_upcase(char c) {
        return Character.toUpperCase(c);
    }

    public static char char_downcase(char c) {
        return Character.toLowerCase(c);
    }

    // ---------------------------------------------------------------------

    public static Text make_string(int k, Maybe<Character> c) {
        if (k < 0) throw new TypeMismatch("nonnegative-integer", Num.make(k));

        char[] chars = new char[k];
        Arrays.fill(chars, c.orElse(' '));
        return new MText(chars);
    }

    @VarArgs
    public static Text string(LispVal args) {
        StringBuilder buf = new StringBuilder();
        while (!args.isNil()) {
            Pair p = (Pair)args;
            if (!(p.head instanceof Char))
                throw new TypeMismatch("char", p.head);
            buf.append(((Char)p.head).value);
            args = p.tail;
        }

        if (args.isNil()) {
            return new MText(buf.toString());
        } else {
            throw new TypeMismatch("pair", args);
        }
    }

    public static int string_length(Text str) {
        return str.length();
    }

    public static char string_ref(Text str, int k) {
        return str.get(k);
    }

    @Name("string-set!")
    public static void string_set(Text t, int k, char c) {
        t.set(k, c);
    }

    @Name("string-fill!")
    public static void string_fill(Text t, char c) {
        t.fill(c);
    }

    public static Text substring(Text str, int start, int end) {
        return str.substring(start, end);
    }

    @VarArgs
    public static Text string_append(LispVal args) {
        StringBuilder buf = new StringBuilder();
        while (args.isPair()) {
            Pair p = (Pair)args;
            if (!(p.head instanceof Text))
                throw new TypeMismatch("string", p.head);
            ((Text)p.head).append(buf);
            args = p.tail;
        }

        if (!args.isNil()) {
            throw new TypeMismatch("pair", args);
        } else {
            return new MText(buf.toString());
        }
    }

    @Name("string->list")
    public static LispVal string2list(Text str) {
        return str.toList();
    }

    @Name("list->string")
    public static Text list2string(LispVal lst) {
        return string(lst);
    }

    public static Text string_copy(Text str) {
        return str.copy();
    }

    @Name("string=?")
    public static boolean string_eq(String x, String y) {
        return x.equals(y);
    }

    @Name("string<?")
    public static boolean string_lt(String x, String y) {
        return x.compareTo(y) < 0;
    }

    @Name("string<=?")
    public static boolean string_le(String x, String y) {
        return x.compareTo(y) <= 0;
    }

    @Name("string>?")
    public static boolean string_gt(String x, String y) {
        return x.compareTo(y) > 0;
    }

    @Name("string>=?")
    public static boolean string_ge(String x, String y) {
        return x.compareTo(y) >= 0;
    }

    @Name("string-ci=?")
    public static boolean string_ci_eq(String x, String y) {
        return x.compareToIgnoreCase(y) == 0;
    }

    @Name("string-ci<?")
    public static boolean string_ci_lt(String x, String y) {
        return x.compareToIgnoreCase(y) < 0;
    }

    @Name("string-ci<=?")
    public static boolean string_ci_le(String x, String y) {
        return x.compareToIgnoreCase(y) <= 0;
    }

    @Name("string-ci>?")
    public static boolean string_ci_gt(String x, String y) {
        return x.compareToIgnoreCase(y) > 0;
    }

    @Name("string-ci>=?")
    public static boolean string_ci_ge(String x, String y) {
        return x.compareToIgnoreCase(y) >= 0;
    }

    // ---------------------------------------------------------------------

    public static Vec make_vector(int k, Maybe<LispVal> fill) {
        LispVal[] vec = new LispVal[k];
        Arrays.fill(vec, fill.orElse(VOID));
        return new Vec(vec);
    }

    @VarArgs
    public static LispVal vector(LispVal args) {
        ArrayList<LispVal> lst = new ArrayList<>();
        while (args.isPair()) {
            Pair p = (Pair)args;
            lst.add(p.head);
            args = p.tail;
        }

        if (args.isNil()) {
            return new Vec(lst.toArray(new LispVal[lst.size()]));
        } else {
            throw new TypeMismatch("pair", args);
        }
    }

    public static int vector_length(Vec vec) {
        return vec.value.length;
    }

    public static LispVal vector_ref(Vec vec, int k) {
        return vec.value[k];
    }

    @Name("vector-set!")
    public static void vector_set(Vec vec, int k, LispVal obj) {
        vec.value[k] = obj;
    }

    @Name("vector->list")
    public static LispVal vector2list(Vec vec) {
        LispVal res = Nil;
        LispVal[] els = vec.value;
        for (int i = els.length; --i >= 0; ) {
            res = Pair.cons(els[i], res);
        }
        return res;
    }

    @Name("list->vector")
    public static LispVal list2vector(LispVal lst) {
        return vector(lst);
    }

    @Name("vector-fill!")
    public static void vector_fill(Vec vec, LispVal fill) {
        Arrays.fill(vec.value, fill);
    }

    // ---------------------------------------------------------------------

    public static LispVal box(LispVal value) {
        return new Box(value);
    }

    @Name("box?")
    public static boolean isBox(LispVal val) {
        return val instanceof Box;
    }

    public static LispVal unbox(LispVal val) {
        return (val instanceof Box) ? ((Box)val).value : val;
    }

    @Name("set-box!")
    public static void set_box(LispVal box, LispVal value) {
        if (box instanceof Box) {
            ((Box)box).value = value;
        } else {
            throw new TypeMismatch("box", box);
        }
    }

    // ---------------------------------------------------------------------

    public static Env scheme_report_environment(Evaluator me, int version) {
        if (version != 5)
            throw new TypeMismatch("<5>", Num.make(version));
        return me.getSchemeReportEnv();
    }

    public static Env null_environment(Evaluator me, int version) {
        if (version != 5)
            throw new TypeMismatch("<5>", Num.make(version));
        return me.getNullEnv();
    }

    public static Env interaction_environment(Evaluator me, Env env) {
        return me.getInteractionEnv(env);
    }

    public static $<Evaluator, LispVal>
    eval(Evaluator me, Env env, LispVal exp, Maybe<Env> env_spec) {
        return me.eval(env_spec.orElse(me.getInteractionEnv(env)), exp);
    }

    @VarArgs
    public static $<Evaluator, LispVal>
    apply(Evaluator me, Env env, LispVal func, LispVal args, LispVal rest) {
        if (rest.isNil()) {
            return me.apply(env, func, args);
        }

        args = Pair.list(args);
        while (rest.isPair()) {
            Pair p = (Pair)rest;
            if (p.tail.isNil()) {
                rest = p.head;
                break;
            } else {
                args = Pair.cons(p.head, args);
                rest = p.tail;
            }
        }

        LispVal last = rest;
        return do_(reverse(me, env, args), rev ->
               do_(append(me, env, rev, last), app ->
               me.apply(env, func, app)));
    }

    @Syntax
    public static $<Evaluator, Proc> delay(Evaluator me, Env ctx, LispVal args) {
        return me.map(me.analyzeSequence(ctx, args), proc ->
               env -> me.pure(new Promise(env, proc)));
    }

    public static $<Evaluator, LispVal> force(Evaluator me, LispVal t) {
        if (t instanceof Promise) {
            Promise p = (Promise)t;

            if (p.result != null) {
                return p.result;
            }

            return me.catchE(err -> p.result = me.throwE(err),
                             p.result = p.body.apply(p.env));
        } else {
            return me.pure(t);
        }
    }

    public static $<Evaluator, LispVal>
    call_with_current_continuation(Evaluator me, Env env, LispVal proc) {
        return me.callCC(env, proc);
    }

    @VarArgs
    public static $<Evaluator, LispVal> values(Evaluator me, LispVal args) {
        return me.values(args);
    }

    public static $<Evaluator, LispVal>
    call_with_values(Evaluator me, Env env, LispVal producer, LispVal consumer) {
        return me.callWithValues(env, producer, consumer);
    }

    public static $<Evaluator, LispVal>
    dynamic_wind(Evaluator me, Env env, LispVal before, LispVal thunk, LispVal after) {
        return me.dynamicWind(env, before, thunk, after);
    }

    @Syntax
    public static $<Evaluator, Proc> reset(Evaluator me, Env ctx, LispVal args) {
        return me.map(me.analyzeSequence(ctx, args), proc -> env -> me.reset(env, proc));
    }

    @Syntax
    public static $<Evaluator, Proc> shift(Evaluator me, Env ctx, LispVal args) {
        return with(args).<$<Evaluator, Proc>>get()
            .when(Cons((id, body) ->
              id.isSymbol()
                ? me.map(me.analyzeSequence(ctx, body), proc ->
                  env -> me.shift(env, id.getSymbol(), proc))
                : badSyntax(me, ctx, "shift", args)))
            .orElseGet(() -> badSyntax(me, ctx, "shift", args));
    }

    @VarArgs
    public static $<Evaluator, LispVal> error(Evaluator me, Env env, LispVal args) {
        LispVal caller = null;
        String  message;

        if (args.isPair() && ((Pair)args).head.isSymbol()) {
            caller = ((Pair)args).head;
            args = ((Pair)args).tail;
        }

        if (args.isPair()) {
            Pair p = (Pair)args;
            if (p.head instanceof Text) {
                message = ((Text)p.head).value();
                args = p.tail;
            } else {
                return me.throwE(env, new TypeMismatch("string", p.head));
            }
        } else {
            return me.throwE(env, new NumArgs(1, args));
        }

        Printer pr = new Printer();
        if (caller != null) {
            pr.add(caller);
            pr.add(": ");
        }
        pr.add(message);
        while (args.isPair()) {
            Pair p = (Pair)args;
            pr.add(" ");
            pr.add(p.head);
            args = p.tail;
        }

        return args.isNil()
            ? pr.checkError().either(
                err -> me.throwE(env, err),
                ()  -> me.throwE(env, new LispError(pr.toString())))
            : me.throwE(env, new TypeMismatch("pair", args));
    }

    public static Either<LispError, LispVal> raise(LispVal obj) {
        if ((obj instanceof JObject) && ((JObject)obj).value instanceof Throwable) {
            Throwable cause = (Throwable)((JObject)obj).value;
            return Either.left(new LispError(cause));
        } else {
            return Either.left(new Condition(obj));
        }
    }

    public static $<Evaluator, LispVal>
    with_exception_handler(Evaluator me, Env env, LispVal handler, LispVal thunk) {
        return me.catchE(e -> e instanceof Condition
                             ? me.apply(env, handler, Pair.list(((Condition)e).value))
                             : me.throwE(e),
                         me.apply(env, thunk, Nil));
    }

    @Syntax
    public static $<Evaluator, Proc> and(Evaluator me, Env ctx, LispVal args) {
        if (args.isNil()) {
            return me.pure(env -> me.pure(Bool.TRUE));
        }

        if (args.isPair()) {
            Pair p = (Pair)args;
            if (p.tail.isNil()) {
                return me.analyze(ctx, p.head);
            } else {
                return do_(me.analyze(ctx, p.head), first ->
                       do_(and(me, ctx, p.tail), rest ->
                       me.pure(env -> do_(first.apply(env), result ->
                                      result.isFalse() ? me.pure(result)
                                                       : rest.apply(env)))));
            }
        }

        return badSyntax(me, ctx, "and", args);
    }

    @Syntax
    public static $<Evaluator, Proc> or(Evaluator me, Env ctx, LispVal args) {
        if (args.isNil()) {
            return me.pure(env -> me.pure(Bool.FALSE));
        }

        if (args.isPair()) {
            Pair p = (Pair)args;
            if (p.tail.isNil()) {
                return me.analyze(ctx, p.head);
            } else {
                return do_(me.analyze(ctx, p.head), first ->
                       do_(or(me, ctx, p.tail), rest ->
                       me.pure(env -> do_(first.apply(env), result ->
                                      result.isTrue() ? me.pure(result)
                                                      : rest.apply(env)))));
            }
        }

        return badSyntax(me, ctx, "or", args);
    }

    public static boolean not(LispVal val) {
        return val == Bool.FALSE;
    }

    @Name("void") @VarArgs
    public static void void_(LispVal args) {
        // args ignored
    }

    public static Symbol gensym(Env env, Maybe<LispVal> prefix) {
        if (prefix.isAbsent()) {
            return env.newsym();
        }

        LispVal x = prefix.get();
        if (x.isSymbol()) {
            return env.newsym(x.getSymbolName());
        } else if (x instanceof Text) {
            return env.newsym(((Text)x).value());
        } else {
            throw new TypeMismatch("string or symbol", x);
        }
    }

    private static <A> $<Evaluator, A> badSyntax(Evaluator me, Env ctx, String name, LispVal val) {
        return me.throwE(ctx, new BadSpecialForm(name + ": bad syntax", val));
    }

    // ---------------------------------------------------------------------

    public static $<Evaluator, LispVal> macroexpand(Evaluator me, Env ctx, LispVal form) {
        return expand(me, ctx, form, false);
    }

    public static $<Evaluator, LispVal> macroexpand_1(Evaluator me, Env ctx, LispVal form) {
        return expand(me, ctx, form, true);
    }

    private static $<Evaluator, LispVal>
    expand(Evaluator me, Env ctx, LispVal form, boolean single) {
        return with(form).<$<Evaluator, LispVal>>get()
            .when(Datum  (__  -> me.pure(form)))
            .when(Scoped (var -> me.pure(ctx.getRenamed(var))))
            .when(Symbol (__  -> me.pure(form)))
            .when(Nil    (()  -> me.pure(form)))
            .when(Cons   ((first, rest) -> do_(expand(me, ctx, first, single), tag ->
                                           do_(expandList(me, ctx, tag, rest, single)))))
            .orElseGet(() -> me.throwE(ctx, new BadSpecialForm("Unrecognized special form", form)));
    }

    private static $<Evaluator, LispVal>
    expandList(Evaluator me, Env ctx, LispVal tag, LispVal rest, boolean single) {
        if (tag.isSymbol()) {
            switch(tag.getSymbolName()) {
            case "sys:define":
            case "define-macro":
                return expandForm(me, renameDefinedVars(ctx, rest), tag, rest, single);

            case "sys:lambda":
                return expandForm(me, renameLambdaVars(ctx, rest), tag, rest, single);

            case "sys:let":
            case "sys:letrec":
                return expandForm(me, renameLetVars(ctx, rest), tag, rest, single);

            case "quote":
                return me.pure(Pair.cons(tag, rest));

            case "quasiquote":
                return with(rest).<$<Evaluator, LispVal>>get()
                  .when(List(datum ->
                    do_(me.evalUnquote(ctx.incrementQL(), datum), qexp ->
                    expand(me, ctx, qexp, single))))
                  .orElseGet(() -> badSyntax(me, ctx, "quasiquote", rest));

            default:
                Maybe<LispVal> var = ctx.lookupMacro(tag.getSymbol());
                if (var.isPresent() && var.get() instanceof Macro) {
                    Macro mac = (Macro)var.get();
                    return match(mac.closure.macroExtend(ctx, mac), mac.pattern, rest).<$<Evaluator, LispVal>>either(
                        err  -> me.throwE(ctx, err),
                        eenv -> single ? mac.body.apply(eenv)
                                       : do_(mac.body.apply(eenv), mexp ->
                                         expand(me, ctx, mexp, false)));
                }
            }
        }

        return expandForm(me, ctx, tag, rest, single);
    }

    private static $<Evaluator, LispVal>
    expandForm(Evaluator me, Env ctx, LispVal tag, LispVal form, boolean single) {
        return do_(form.mapM(me, x -> expand(me, ctx, x, single)), args ->
               do_(me.pure(Pair.cons(tag, args))));
    }

    private static void renamePattern(Env ctx, LispVal pattern) {
        while (pattern.isPair()) {
            Pair p = (Pair)pattern;
            renamePattern(ctx, p.head);
            pattern = p.tail;
        }

        if (pattern instanceof Scoped) {
            ctx.rename(pattern);
        }
    }

    private static Env renameDefinedVars(Env ctx, LispVal form) {
        LispVal var, formals;

        if (form.isPair()) {
            LispVal def = ((Pair)form).head;
            if (def.isSymbol()) {
                ctx.rename(def);
                return ctx;
            } else if (def.isPair()) {
                var = ((Pair)def).head;
                formals = ((Pair)def).tail;
            } else {
                return ctx;
            }
        } else {
            return ctx;
        }

        if (var instanceof Scoped) {
            ctx.rename(var);
        }

        Env lctx = ctx.lexicalExtend();
        renamePattern(lctx, formals);
        return lctx;
    }

    private static Env renameLambdaVars(Env ctx, LispVal form) {
        if (form.isPair()) {
            Env lctx = ctx.lexicalExtend();
            renamePattern(lctx, ((Pair)form).head);
            return lctx;
        } else {
            return ctx;
        }
    }

    private static Env renameLetVars(Env ctx, LispVal form) {
        LispVal tag = null, bindings;

        if (form.isPair() && ((Pair)form).head.isSymbol()) {
            tag = ((Pair)form).head;
            form = ((Pair)form).tail;
        }

        if (form.isPair()) {
            bindings = ((Pair)form).head;
        } else {
            return ctx;
        }

        Env lctx = ctx.lexicalExtend();

        if (tag instanceof Scoped) {
            lctx.rename(tag);
        }

        while (bindings.isPair()) {
            Pair p = (Pair)bindings;
            if (p.head.isPair() && (((Pair)p.head).head instanceof Scoped))
                lctx.rename(((Pair)p.head).head);
            bindings = p.tail;
        }

        return lctx;
    }

    public static String dump(LispVal val) {
        return val.toString();
    }

    // ---------------------------------------------------------------------

    @Syntax
    public static $<Evaluator, Proc> require(Evaluator me, Env ctx, LispVal args) {
        if (args.isPair()) {
            Pair p = (Pair)args;
            if (p.head.isSymbol() && p.tail.isNil()) {
                Symbol name = p.head.getSymbol();
                return me.pure(env -> me.except(me.loadLib(env, name)));
            }
        }
        return me.throwE(ctx, new BadSpecialForm("require: bad syntax", args));
    }

    public static $<Evaluator, LispVal> load(Evaluator me, Env env, String name) {
        try (InputStream is = Files.newInputStream(Paths.get(name))) {
            Reader input = new InputStreamReader(is, StandardCharsets.UTF_8);
            return me.except(me.run(env.getOuter(), me.parse(name, input)));
        } catch (Exception ex) {
            return me.throwE(env, new LispError(ex));
        }
    }

    @Syntax
    public static $<Evaluator, Proc> import_library(Evaluator me, Env ctx, LispVal args) {
        if (args.isPair()) {
            Pair p = (Pair)args;
            if (p.head.isSymbol() && p.tail.isNil()) {
                String cls = p.head.getSymbolName();
                return me.pure(env -> {
                    try {
                        me.loadPrimitives(env, Class.forName(cls));
                        return me.pure(VOID);
                    } catch (LispError ex) {
                        return me.throwE(ctx, ex);
                    } catch (Exception ex) {
                        return me.throwE(ctx, new LispError(ex));
                    }
                });
            }
        }
        return me.throwE(ctx, new BadSpecialForm("load-library: bad-syntax", args));
    }

    @Name("jclass?")
    public static boolean isJClass(LispVal val) {
        return val instanceof JClass;
    }

    @Name("jobject?")
    public static boolean isJObject(LispVal val) {
        return val instanceof JObject;
    }

    public static JClass jclass(Symbol cls) throws ClassNotFoundException {
        String name = cls.getSymbolName();
        if (name.indexOf('.') == -1)
            name = "java.lang." + name;
        return new JClass(Class.forName(name));
    }

    public static JClass jobject_class(JObject obj) {
        return new JClass(obj.value.getClass());
    }

    @Name("instance-of?")
    public static boolean instance_of(Object obj, JClass cls) {
        return cls.value.isInstance(obj);
    }

    public static LispVal
    get_field(LispVal obj, Symbol name) throws Exception {
        if (obj instanceof JClass) {
            return getField(((JClass)obj).value, null, name.name);
        } else if (obj instanceof JObject) {
            Object jobj = ((JObject)obj).value;
            return getField(jobj.getClass(), jobj, name.name);
        } else {
            throw new TypeMismatch("jclass or jobject", obj);
        }
    }

    private static LispVal
    getField(Class<?> cls, Object obj, String name)
        throws NoSuchFieldException, IllegalAccessException
    {
        if (obj == null && "class".equals(name))
            return new JObject(cls);

        Field field = cls.getField(name);
        if (Modifier.isStatic(field.getModifiers()) != (obj == null))
            throw new NoSuchFieldException(name);
        return Packer.pack(field.get(obj));
    }

    @Name("set-field!")
    public static void
    set_field(Env env, LispVal obj, Symbol name, LispVal value) throws Exception {
        if (obj instanceof JClass) {
            setField(env, ((JClass)obj).value, null, name.name, value);
        } else if (obj instanceof JObject) {
            Object jobj = ((JObject)obj).value;
            setField(env, jobj.getClass(), jobj, name.name, value);
        } else {
            throw new TypeMismatch("jclass or jobject", obj);
        }
    }

    private static void
    setField(Env env, Class<?> cls, Object obj, String name, LispVal value)
        throws NoSuchFieldException, IllegalAccessException
    {
        Field field = cls.getField(name);
        if (Modifier.isStatic(field.getModifiers()) != (obj == null))
            throw new NoSuchFieldException(name);

        field.set(obj, Unpacker.unpack(env, field.getType(), value.normalize()).getOrThrow(Fn.id()));
    }

    public static Object make_proxy(Env env, Class<?> cls, LispVal fns) {
        Map<Method, JLambda> dispatch = createProxyDispatch(cls, fns);

        return Reflection.newProxy(cls, new AbstractInvocationHandler() {
            @Override
            protected Object handleInvocation(Object proxy, Method method, Object[] args) throws Throwable {
                JLambda jlambda = dispatch.get(method);
                if (jlambda != null) {
                    return jlambda.invoke(env, args);
                }

                if (method.isDefault()) {
                    return JLambda.invokeDefault(proxy, method, args);
                }

                throw new LispError(method.getName() + ": method is not implemented");
            }
        });
    }

    private static Map<Method, JLambda> createProxyDispatch(Class<?> cls, LispVal fns) {
        if (!cls.isInterface())
            throw new LispError(cls.getName() + ": not a interface");

        Map<Method, JLambda> dispatch = new HashMap<>();
        for (Method method : cls.getMethods()) {
            LispVal fn = searchAlist(method.getName(), fns);
            if (fn != null) {
                dispatch.put(method, JLambda.make(method, fn));
            }
        }
        return dispatch;
    }

    private static LispVal searchAlist(String name, LispVal alist) {
        while (alist.isPair()) {
            Pair p = (Pair)alist;
            if (p.head.isPair()) {
                Pair a = (Pair)p.head;
                if (a.head.isSymbol() && name.equals(a.head.getSymbolName()))
                    return a.tail;
            }
            alist = p.tail;
        }
        return null;
    }
}
