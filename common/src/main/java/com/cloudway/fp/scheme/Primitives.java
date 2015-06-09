/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.scheme;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.lang.reflect.Method;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Iterator;
import java.util.function.BiFunction;
import java.util.function.BiPredicate;

import com.cloudway.fp.$;
import com.cloudway.fp.data.Fn;
import com.cloudway.fp.data.Maybe;
import com.cloudway.fp.data.Rational;
import com.cloudway.fp.data.Seq;
import com.cloudway.fp.data.Tuple;
import com.cloudway.fp.data.Vector;

import static com.cloudway.fp.control.Conditionals.with;
import static com.cloudway.fp.control.Syntax.do_;
import static com.cloudway.fp.scheme.LispError.*;
import static com.cloudway.fp.scheme.LispVal.*;
import static com.cloudway.fp.scheme.LispVal.Void;
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

    @Name("number?")
    public static boolean isNumber(LispVal val) {
        return val instanceof Num;
    }

    @Name("integer?")
    public static boolean isInteger(LispVal val) {
        if (val instanceof Num) {
            Number n = ((Num)val).value;
            return (n instanceof Long) || (n instanceof BigInteger);
        } else {
            return false;
        }
    }

    @Name("real?")
    public static boolean isReal(LispVal val) {
        return (val instanceof Num) && (((Num)val).value instanceof Double);
    }

    @Name("rational?")
    public static boolean isRational(LispVal val) {
        return (val instanceof Num) && (((Num)val).value instanceof Rational);
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
        return (val instanceof Prim) || (val instanceof Func);
    }

    // ---------------------------------------------------------------------

    @Name("exact?")
    public static boolean isExact(Number x) {
        return !isInexact(x);
    }

    @Name("inexact?")
    public static boolean isInexact(Number x) {
        return x instanceof Double || x instanceof Float;
    }

    @Name("eq?")
    public static boolean eq(LispVal x, LispVal y) {
        return x.equals(y);
    }

    @Name("eqv?")
    public static boolean eqv(LispVal x, LispVal y) {
        return x.equals(y);
    }

    @Name("equal?")
    public static boolean equal(LispVal x, LispVal y) {
        if (x == y) {
            return true;
        }

        return with(x, y).<Boolean>get()
            .when(Pair(xs -> Pair(ys -> listEqual(xs, ys))))
            .when(Vector(xs -> Vector(ys -> vectorEqual(xs, ys))))
            .orElseGet(() -> x.equals(y));
    }

    private static boolean listEqual(LispVal xs, LispVal ys) {
        while (xs.isPair() && ys.isPair()) {
            Pair px = (Pair)xs, py = (Pair)ys;
            if (!equal(px.head, py.head))
                return false;
            xs = px.tail;
            ys = py.tail;
        }
        return xs.equals(ys);
    }

    private static boolean vectorEqual(Vector<LispVal> x, Vector<LispVal> y) {
        if (x.size() != y.size())
            return false;

        Iterator<LispVal> i1 = x.iterator();
        Iterator<LispVal> i2 = y.iterator();
        while (i1.hasNext() && i2.hasNext()) {
            if (!equal(i1.next(), i2.next()))
                return false;
        }
        return true;
    }

    // ---------------------------------------------------------------------

    private static final int LT = 0x01;
    private static final int GT = 0x02;
    private static final int EQ = 0x04;
    private static final int LE = LT | EQ;
    private static final int GE = GT | EQ;

    private static int compare(Number x, Number y) {
        int cmp;
        if (x instanceof Double || y instanceof Double) {
            cmp = Double.compare(x.doubleValue(), y.doubleValue());
        } else if (x instanceof Rational || y instanceof Rational) {
            cmp = Rational.valueOf(x).compareTo(Rational.valueOf(y));
        } else if (x instanceof BigInteger || y instanceof BigInteger) {
            cmp = toBigInteger(x).compareTo(toBigInteger(y));
        } else {
            cmp = Long.compare(x.longValue(), y.longValue());
        }
        return cmp < 0 ? LT : cmp > 0 ? GT : EQ;
    }

    private static boolean compare(int ord, Number x, Number y, LispVal ys) {
        if ((compare(x, y) & ord) == 0) {
            return false;
        }

        while (ys.isPair()) {
            Pair p = (Pair)ys;
            Number z = unpackNum(p.head);
            if ((compare(y, z) & ord) == 0)
                return false;
            y = z;
            ys = p.tail;
        }

        return true;
    }

    @Name("=") @VarArgs
    public static boolean num_eq(Number x, Number y, LispVal ys) {
        return compare(EQ, x, y, ys);
    }

    @Name("<") @VarArgs
    public static boolean num_lt(Number x, Number y, LispVal ys) {
        return compare(LT, x, y, ys);
    }

    @Name("<=") @VarArgs
    public static boolean num_le(Number x, Number y, LispVal ys) {
        return compare(LE, x, y, ys);
    }

    @Name(">") @VarArgs
    public static boolean num_gt(Number x, Number y, LispVal ys) {
        return compare(GT, x, y, ys);
    }

    @Name(">=") @VarArgs
    public static boolean num_ge(Number x, Number y, LispVal ys) {
        return compare(GE, x, y, ys);
    }

    // ---------------------------------------------------------------------

    @Name("+") @VarArgs
    public static Number plus(LispVal args) {
        return accumulate(Primitives::plus, 0L, args);
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

    @Name("-") @VarArgs
    public static Number minus(Number first, LispVal rest) {
        if (rest.isNil())
            return minus(0L, first);
        return accumulate(Primitives::minus, first, rest);
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

    @Name("*") @VarArgs
    public static Number times(LispVal args) {
        return accumulate(Primitives::times, 1L, args);
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

    @Name("/") @VarArgs
    public static Number divide(Number first, LispVal rest) {
        if (rest.isNil())
            return divide(1L, first);
        return accumulate(Primitives::divide, first, rest);
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

    public static Number quotient(Number x, Number y) {
        if (x instanceof Double || y instanceof Double) {
            return Math.round(x.doubleValue() / y.doubleValue());
        } else if (x instanceof Rational || y instanceof Rational) {
            return reduce(Rational.valueOf(x).divide(Rational.valueOf(y)).numerator());
        } else if (x instanceof BigInteger || y instanceof BigInteger) {
            return reduce(toBigInteger(x).divide(toBigInteger(y)));
        } else {
            return x.longValue() / y.longValue();
        }
    }

    public static Number modulo(Number x, Number y) {
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

    public static Number remainder(Number x, Number y) {
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

    private static Number accumulate(BiFunction<Number, Number, Number> op, Number z, LispVal args) {
        while (args.isPair()) {
            Pair p = (Pair)args;
            z = op.apply(z, unpackNum(p.head));
            args = p.tail;
        }

        if (args.isNil()) {
            return z;
        } else {
            throw new TypeMismatch("list", args);
        }
    }

    private static Number unpackNum(LispVal val) {
        if (val instanceof Num) {
            return ((Num)val).value;
        } else {
            throw new TypeMismatch("number", val);
        }
    }

    private static BigInteger toBigInteger(Number n) {
        return (n instanceof BigInteger) ? (BigInteger)n : BigInteger.valueOf(n.longValue());
    }

    private static Number reduce(BigInteger n) {
        return n.bitLength() < 64 ? n.longValue() : n;
    }

    private static Number reduce(Rational rat) {
        BigInteger n = rat.numerator(), d = rat.denominator();
        if (n.signum() == 0) {
            return 0L;
        } else if (n.bitLength() < 64 && d.equals(BigInteger.ONE)) {
            return n.longValue();
        } else {
            return rat;
        }
    }

    // ---------------------------------------------------------------------

    public static double floor(double x) {
        return Math.floor(x);
    }

    public static double ceiling(double x) {
        return Math.ceil(x);
    }

    public static double truncate(double x) {
        return Math.rint(x);
    }

    public static long round(double x) {
        return Math.round(x);
    }

    public static Number numerator(Number x) {
        if (x instanceof Rational) {
            return reduce(((Rational)x).numerator());
        } else if (x instanceof Double) {
            return Rational.valueOf(x).numerator().doubleValue();
        } else {
            return x;
        }
    }

    public static Number denominator(Number x) {
        if (x instanceof Rational) {
            return reduce(((Rational)x).denominator());
        } else if (x instanceof Double) {
            return Rational.valueOf(x).denominator().doubleValue();
        } else {
            return 1L;
        }
    }

    public static double exp(double x) {
        return Math.exp(x);
    }

    public static double log(double x) {
        return Math.log(x);
    }

    public static double sin(double x) {
        return Math.sin(x);
    }

    public static double cos(double x) {
        return Math.cos(x);
    }

    public static double tan(double x) {
        return Math.tan(x);
    }

    public static double asin(double x) {
        return Math.asin(x);
    }

    public static double acos(double x) {
        return Math.acos(x);
    }

    public static double atan(double x, Maybe<Double> y) {
        return y.isAbsent() ? Math.atan(x) : Math.atan2(x, y.get());
    }

    public static double sqrt(double x) {
        return Math.sqrt(x);
    }

    public static Number expt(Number x, Number y) {
        if (!(x instanceof Double) &&
              ((y instanceof Integer) ||
               (y instanceof Long && y.intValue() == y.longValue()))) {
            int n = y.intValue();
            if (x instanceof Rational) {
                return reduce(((Rational)x).pow(n));
            } else {
                return reduce(toBigInteger(x).pow(n));
            }
        } else {
            return Math.pow(x.doubleValue(), y.doubleValue());
        }
    }

    // ---------------------------------------------------------------------

    @Name("exact->inexact")
    public static double exact2inexact(Number x) {
        return x.doubleValue();
    }

    @Name("inexact->exact")
    public static Number inexact2exact(Number x) {
        return reduce(Rational.valueOf(x));
    }

    @Name("number->string")
    public static String number2string(Num z, Maybe<Integer> r) {
        int radix = r.orElse(10);

        if (radix == 10)
            return z.show();

        if (z.value instanceof Double)
            throw new LispError("inexact numbers can only be represented in base 10");

        if (z.value instanceof Rational) {
            Rational rat = (Rational)z.value;
            return rat.numerator().toString(radix) +
                   "/" +
                   rat.denominator().toString(radix);
        }

        if (z.value instanceof BigInteger)
            return ((BigInteger)z.value).toString(radix);

        return Long.toString(z.value.longValue(), radix);
    }

    @Name("string->number")
    public static long string2number(String str, Maybe<Integer> r) {
        int radix = r.orElse(10);
        return Long.parseLong(str, radix); // FIXME
    }

    // ---------------------------------------------------------------------

    public static LispVal cons(LispVal x, LispVal y) {
        return new Pair(x, y);
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
    public static $<Evaluator, LispVal> append(Evaluator me, LispVal lists) {
        return with(lists).<$<Evaluator, LispVal>>get()
            .when(Nil(() -> me.pure(Nil)))
            .when(List(x -> me.pure(x)))
            .when(List((x, y) -> append(me, x, y)))
            .when(Cons((x, y, ys) ->
                do_(append(me, x, y), hd ->
                do_(append(me, ys), tl ->
                do_(append(me, hd, tl))))))
            .orElseGet(() -> me.throwE(new TypeMismatch("pair", lists)));
    }

    static $<Evaluator, LispVal> append(Evaluator me, LispVal x, LispVal y) {
        if (x.isNil()) {
            return me.pure(y);
        } else if (y.isNil()) {
            return me.pure(x);
        } else {
            return do_(reverse(me, x), rev -> reverseCons(me, rev, y));
        }
    }

    public static $<Evaluator, LispVal> reverse(Evaluator me, LispVal lst) {
        return reverseCons(me, lst, Nil);
    }

    private static $<Evaluator, LispVal> reverseCons(Evaluator me, LispVal hd, LispVal tl) {
        while (hd.isPair()) {
            Pair p = (Pair)hd;
            tl = new Pair(p.head, tl);
            hd = p.tail;
        }
        return hd.isNil() ? me.pure(tl) : me.throwE(new TypeMismatch("pair", hd));
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
                return do_(me.apply(env, f, Pair.of(p.head)), x ->
                       do_(map(me, env, f, p.tail, Nil), xs ->
                       me.pure(cons(x, xs))));
            } else {
                return do_(cars_cdrs(me, more), (as, ds) ->
                       do_(me.apply(env, f, cons(p.head, as)), x ->
                       do_(map(me, env, f, p.tail, ds), xs ->
                       me.pure(cons(x, xs)))));
            }
        }

        return me.throwE(new TypeMismatch("pair", lst));
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
                return do_(me.apply(env, f, Pair.of(p.head)), xs ->
                       do_(flatmap(me, env, f, p.tail, Nil), ys ->
                       append(me, xs, ys)));
            } else {
                return do_(cars_cdrs(me, more), (as, ds) ->
                       do_(me.apply(env, f, cons(p.head, as)), xs ->
                       do_(flatmap(me, env, f, p.tail, ds), ys ->
                       append(me, xs, ys))));
            }
        }

        return me.throwE(new TypeMismatch("pair", lst));
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
                return do_(me.apply(env, pred, Pair.of(p.head)), result ->
                       result.isTrue()
                         ? do_(filter(me, env, pred, p.tail, Nil), rest ->
                           do_(me.pure(cons(p.head, rest))))
                         : filter(me, env, pred, p.tail, Nil));
            } else {
                return do_(cars_cdrs(me, more), (as, ds) ->
                       do_(me.apply(env, pred, cons(p.head, as)), result ->
                       result.isTrue()
                         ? do_(filter(me, env, pred, p.tail, ds), rest ->
                           do_(me.pure(cons(p.head, rest))))
                         : filter(me, env, pred, p.tail, ds)));
            }
        }

        return me.throwE(new TypeMismatch("pair", lst));
    }

    @VarArgs
    public static $<Evaluator, LispVal>
    for_each(Evaluator me, Env env, LispVal proc, LispVal lst, LispVal more) {
        if (lst.isNil()) {
            return me.pure(Void.VOID);
        }

        if (lst.isPair()) {
            Pair p = (Pair)lst;
            if (more.isNil()) {
                return do_(me.apply(env, proc, Pair.of(p.head)), () ->
                       do_(for_each(me, env, proc, p.tail, Nil)));
            } else {
                return do_(cars_cdrs(me, more), (as, ds) ->
                       do_(me.apply(env, proc, cons(p.head, as)), () ->
                       do_(for_each(me, env, proc, p.tail, ds))));
            }
        }

        return me.throwE(new TypeMismatch("pair", lst));
    }

    public static $<Evaluator, LispVal>
    fold_right(Evaluator me, Env env, LispVal op, LispVal init, LispVal lst) {
        if (lst.isNil()) {
            return me.pure(init);
        }

        if (lst.isPair()) {
            Pair p = (Pair)lst;
            return do_(me.delay(() -> fold_right(me, env, op, init, p.tail)), rest ->
                   do_(me.apply(env, op, Pair.of(p.head, rest))));
        }

        return me.throwE(new TypeMismatch("pair", lst));
    }

    public static $<Evaluator, LispVal>
    fold_left(Evaluator me, Env env, LispVal op, LispVal init, LispVal lst) {
        $<Evaluator, LispVal> res = me.pure(init);
        while (lst.isPair()) {
            Pair p = (Pair)lst;
            res = me.bind(res, r -> me.apply(env, op, Pair.of(r, p.head)));
            lst = p.tail;
        }
        return lst.isNil() ? res : me.throwE(new TypeMismatch("pair", lst));
    }

    private static $<Evaluator, Tuple<LispVal, LispVal>> cars_cdrs(Evaluator me, LispVal lists) {
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
                return me.throwE(new TypeMismatch("pair", p.head));
            }

            lists = p.tail;
        }

        if (lists.isNil()) {
            return me.liftM2(Tuple::of, reverse(me, cars), reverse(me, cdrs));
        } else {
            return me.throwE(new TypeMismatch("pair", lists));
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

    public static String make_string(int k, Maybe<Character> c) {
        if (k < 0) throw new TypeMismatch("nonnegative-integer", new Num((long)k));

        char[] chars = new char[k];
        Arrays.fill(chars, c.orElse('\0'));
        return new String(chars);
    }

    @VarArgs
    public static String string(LispVal args) {
        StringBuilder buf = new StringBuilder();
        while (!args.isNil()) {
            Pair p = (Pair)args;
            if (!(p.head instanceof Char))
                throw new TypeMismatch("char", p.head);
            buf.append(((Char)p.head).value);
            args = p.tail;
        }

        if (args.isNil()) {
            return buf.toString();
        } else {
            throw new TypeMismatch("pair", args);
        }
    }

    public static int string_length(String str) {
        return str.length();
    }

    public static char string_ref(String str, int k) {
        return str.charAt(k);
    }

    @Name("string-set!")
    public static void string_set(Text t, int k, char c) {
        String str = t.value;
        int len = str.length();
        char[] chars = new char[len];
        str.getChars(0, len, chars, 0);
        chars[k] = c;
        t.value = new String(chars);
    }

    @Name("string-fill!")
    public static void string_fill(Text t, char c) {
        char[] chars = new char[t.value.length()];
        Arrays.fill(chars, c);
        t.value = new String(chars);
    }

    public static String substring(String str, int start, int end) {
        return str.substring(start, end);
    }

    @VarArgs
    public static String string_append(LispVal args) {
        StringBuilder buf = new StringBuilder();
        while (args.isPair()) {
            Pair p = (Pair)args;
            if (!(p.head instanceof Text))
                throw new TypeMismatch("string", p.head);
            buf.append(((Text)p.head).value);
            args = p.tail;
        }

        if (!args.isNil()) {
            throw new TypeMismatch("pair", args);
        } else {
            return buf.toString();
        }
    }

    @Name("string->list")
    public static LispVal string2list(String str) {
        return Pair.fromList(Seq.wrap(str).map(Char::new));
    }

    @Name("list->string")
    public static String list2string(LispVal lst) {
        return string(lst);
    }

    public static String string_copy(String str) {
        return str;
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
        return new Vec(Vector.iterate(k, __ -> fill.orElse(new Num(0L))));
    }

    @VarArgs
    public static $<Evaluator, LispVal> vector(Evaluator me, LispVal args) {
        return me.map(args.toList(me), xs -> new Vec(Vector.fromList(xs)));
    }

    public static int vector_length(Vec vec) {
        return vec.value.size();
    }

    public static LispVal vector_ref(Vec vec, int k) {
        return vec.value.at(k);
    }

    @Name("vector-set!")
    public static void vector_set(Vec vec, int k, LispVal obj) {
        vec.value = vec.value.update(k, obj);
    }

    @Name("vector->list")
    public static LispVal vector2list(Vec vec) {
        return Pair.fromList(vec.value.asList());
    }

    @Name("list->vector")
    public static $<Evaluator, LispVal> list2vector(Evaluator me, LispVal lst) {
        return me.map(lst.toList(me), xs -> new Vec(Vector.fromList(xs)));
    }

    @Name("vector-fill!")
    public static void vector_fill(Vec vec, LispVal fill) {
        vec.value = vec.value.map(Fn.pure(fill));
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
            throw new TypeMismatch("<5>", new Num(version));
        return me.getSchemeReportEnv();
    }

    public static Env null_environment(Evaluator me, int version) {
        if (version != 5)
            throw new TypeMismatch("<5>", new Num(version));
        return me.getNullEnv();
    }

    public static Env interaction_environment(Evaluator me, Env env) {
        return me.getInteractionEnv(env);
    }

    public static $<Evaluator, LispVal> eval(Evaluator me, LispVal exp, Env env) {
        return me.eval(env, exp);
    }

    @VarArgs
    public static $<Evaluator, LispVal>
    apply(Evaluator me, Env env, LispVal func, LispVal args, LispVal rest) {
        if (rest.isNil()) {
            return me.apply(env, func, args);
        }

        args = Pair.of(args);
        while (rest.isPair()) {
            Pair p = (Pair)rest;
            if (p.tail.isNil()) {
                rest = p.head;
                break;
            } else {
                args = new Pair(p.head, args);
                rest = p.tail;
            }
        }

        LispVal last = rest;
        return do_(reverse(me, args), rev ->
               do_(append(me, rev, last), app ->
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
                             do_(p.body.apply(p.env), res ->
                             do_(p.result = me.pure(res))));
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
                   env -> me.shift(env, (Symbol)id, proc))
                 : badSyntax(me, "shift", args)))
            .orElseGet(() -> badSyntax(me, "shift", args));
    }

    @VarArgs
    public static $<Evaluator, LispVal> error(Evaluator me, LispVal args) {
        LispVal caller = null;
        String  message;

        if (args.isPair() && ((Pair)args).head.isSymbol()) {
            caller = ((Pair)args).head;
            args = ((Pair)args).tail;
        }

        if (args.isPair()) {
            Pair p = (Pair)args;
            if (p.head instanceof Text) {
                message = ((Text)p.head).value;
                args = p.tail;
            } else {
                return me.throwE(new TypeMismatch("string", p.head));
            }
        } else {
            return me.throwE(new NumArgs(1, args));
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
            ? me.throwE(new LispError(pr.toString()))
            : me.throwE(new TypeMismatch("pair", args));
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

        return badSyntax(me, "and", args);
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

        return badSyntax(me, "or", args);
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
        if (x instanceof Symbol) {
            return env.newsym(((Symbol)x).name);
        } else if (x instanceof Text) {
            return env.newsym(((Text)x).value);
        } else {
            throw new TypeMismatch("string or symbol", x);
        }
    }

    private static <A> $<Evaluator, A> badSyntax(Evaluator me, String name, LispVal val) {
        return me.throwE(new BadSpecialForm(name + ": bad syntax", val));
    }

    // ---------------------------------------------------------------------

    public static $<Evaluator, LispVal> macroexpand(Evaluator me, Env ctx, LispVal form) {
        return with(form).<$<Evaluator, LispVal>>get()
            .when(Datum  (__ -> me.pure(form)))
            .when(Symbol (__ -> me.pure(form)))
            .when(Nil    (() -> me.pure(form)))
            .when(Cons   ((first, rest) -> do_(macroexpand(me, ctx, first), tag ->
                                           do_(expandList(me, ctx, tag, rest)))))
            .orElseGet(() -> me.throwE(new BadSpecialForm("Unrecognized special form", form)));
    }

    private static $<Evaluator, LispVal> expandList(Evaluator me, Env ctx, LispVal tag, LispVal rest) {
        if (tag.isSymbol()) {
            switch(((Symbol)tag).name) {
            case "quote":
                return me.pure(new Pair(tag, rest));

            case "quasiquote":
                return with(rest).<$<Evaluator, LispVal>>get()
                  .when(List(datum ->
                    do_(me.evalUnquote(ctx.incrementQL(), datum), qexp ->
                    macroexpand(me, ctx, qexp))))
                  .orElseGet(() -> badSyntax(me, "quasiquote", rest));

            default:
                Maybe<LispVal> var = ctx.lookupMacro((Symbol)tag);
                if (var.isPresent() && var.get() instanceof Macro) {
                    Macro mac = (Macro)var.get();
                    return Evaluator.match(ctx, mac.pattern, rest).<$<Evaluator, LispVal>>either(
                        err -> me.throwE(err),
                        eenv -> do_(mac.body.apply(eenv), mexp -> macroexpand(me, ctx, mexp)));
                }
            }
        }

        return do_(rest.mapM(me, x -> macroexpand(me, ctx, x)), els ->
               do_(me.pure(new Pair(tag, els))));
    }

    // ---------------------------------------------------------------------

    @Syntax
    public static $<Evaluator, Proc> require(Evaluator me, Env ctx, LispVal args) {
        if (args.isPair()) {
            Pair p = (Pair)args;
            if (p.head.isSymbol() && p.tail.isNil()) {
                return me.pure(env -> me.except(me.loadLib(env, (Symbol)p.head)));
            }
        }
        return me.throwE(new BadSpecialForm("require: bad syntax", args));
    }

    public static $<Evaluator, LispVal> load(Evaluator me, Env env, String name) {
        try (InputStream is = Files.newInputStream(Paths.get(name))) {
            Reader input = new InputStreamReader(is, StandardCharsets.UTF_8);
            return me.except(me.run(env, me.parse(name, input)));
        } catch (Exception ex) {
            return me.throwE(new LispError(ex));
        }
    }

    public static void import_library(Evaluator me, Env env, String cls) {
        try {
            me.loadPrimitives(env, Class.forName(cls));
        } catch (LispError ex) {
            throw ex;
        } catch (Exception ex) {
            throw new LispError(ex);
        }
    }

    public static LispVal import_method(Evaluator me, String cls, String mth) {
        try {
            Method method = findMethod(Class.forName(cls), mth);
            return me.loadPrimitive(getPrimName(method), method);
        } catch (LispError ex) {
            throw ex;
        } catch (Exception ex) {
            throw new LispError(ex);
        }
    }

    private static Method findMethod(Class<?> c, String name) {
        for (Method method : c.getMethods())
            if (name.equals(method.getName()))
                return method;
        throw new LispError(name + ": method not found");
    }

    private static String getPrimName(Method method) {
        Name nameTag = method.getAnnotation(Name.class);
        return nameTag != null ? nameTag.value() : method.getName().replace('_', '-');
    }
}
