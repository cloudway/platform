/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.scheme.numsys;

import com.cloudway.fp.data.Maybe;
import com.cloudway.fp.function.TriFunction;
import com.cloudway.fp.scheme.LispError.TypeMismatch;
import com.cloudway.fp.scheme.LispVal;
import com.cloudway.fp.scheme.LispVal.Pair;
import com.cloudway.fp.scheme.Name;
import com.cloudway.fp.scheme.SchemeParser;
import com.cloudway.fp.scheme.VarArgs;

@SuppressWarnings("unused")
public final class NumberPrimitives {
    private NumberPrimitives() {}

    @Name("number?")
    public static boolean isNumber(LispVal val) {
        return val instanceof Num;
    }

    @Name("integer?")
    public static boolean isInteger(LispVal val) {
        return (val instanceof Num) && Field.implies(((Num)val).lower().toExact(), BigInt.TAG);
    }

    @Name("real?")
    public static boolean isReal(LispVal val) {
        return (val instanceof Num) && Field.implies(((Num)val).lower(), Real.TAG);
    }

    @Name("rational?")
    public static boolean isRational(LispVal val) {
        if (val instanceof Real) {
            double r = ((Real)val).value;
            return !Double.isInfinite(r) && !Double.isNaN(r);
        } else {
            return isReal(val);
        }
    }

    @Name("complex?")
    public static boolean isComplex(LispVal val) {
        return (val instanceof Num) && Field.implies((Num)val, Complex.TAG);
    }

    @Name("exact?")
    public static boolean isExact(Num val) {
        return val.isExact();
    }

    @Name("inexact?")
    public static boolean isInexact(Num val) {
        return !val.isExact();
    }

    @Name("exact->inexact")
    public static Num exact2inexact(Num val) {
        return val.toInexact();
    }

    @Name("inexact->exact")
    public static Num inexact2exact(Num val) {
        return val.toExact();
    }

    @Name("number->string")
    public static String number2string(Num z, Maybe<Integer> r) {
        return z.show(r.orElse(10));
    }

    @Name("string->number")
    public static LispVal string2number(String str, Maybe<Integer> r) {
        try {
            return SchemeParser.parseNumber(str, r.orElse(10));
        } catch (Exception ex) {
            return LispVal.Bool.FALSE;
        }
    }

    // ----------------------------------------------------------------------

    private static Num unpackNum(LispVal val) {
        if (val instanceof Num) {
            return (Num)val;
        } else {
            throw new TypeMismatch("number", val);
        }
    }

    private static final int LT = 0x01;
    private static final int GT = 0x02;
    private static final int EQ = 0x04;
    private static final int LE = LT | EQ;
    private static final int GE = GT | EQ;

    private static int compare(Num x, Num y) {
        int cmp = x.compareTo(y);
        return cmp < 0 ? LT : cmp > 0 ? GT : EQ;
    }

    private static boolean compare(int ord, Num x, Num y, LispVal ys) {
        if ((compare(x, y) & ord) == 0) {
            return false;
        }

        while (ys.isPair()) {
            Pair p = (Pair)ys;
            Num z = unpackNum(p.head);
            if ((compare(y, z) & ord) == 0)
                return false;
            y = z;
            ys = p.tail;
        }

        return true;
    }

    @Name("=") @VarArgs
    public static boolean eq(Num x, Num y, LispVal ys) {
        try {
            return compare(EQ, x, y, ys);
        } catch (Exception ex) {
            return false;
        }
    }

    @Name("<") @VarArgs
    public static boolean lt(Num x, Num y, LispVal ys) {
        return compare(LT, x, y, ys);
    }

    @Name("<=") @VarArgs
    public static boolean le(Num x, Num y, LispVal ys) {
        return compare(LE, x, y, ys);
    }

    @Name(">") @VarArgs
    public static boolean gt(Num x, Num y, LispVal ys) {
        return compare(GT, x, y, ys);
    }

    @Name(">=") @VarArgs
    public static boolean ge(Num x, Num y, LispVal ys) {
        return compare(GE, x, y, ys);
    }

    @Name("zero?")
    public static boolean isZero(Num x) {
        return x.isZero();
    }

    @Name("negative?")
    public static boolean isNegative(Num x) {
        return x.isNegative();
    }

    @Name("positive?")
    public static boolean isPositive(Num x) {
        return !(x.isZero() || x.isNegative());
    }

    // ----------------------------------------------------------------------

    private static Num accumulate(TriFunction<Field<Num>, Num, Num, Num> op, Num z, LispVal args) {
        while (args.isPair()) {
            Pair p = (Pair)args;
            z = Field.op(op, z, unpackNum(p.head));
            args = p.tail;
        }

        if (args.isNil()) {
            return z;
        } else {
            throw new TypeMismatch("list", args);
        }
    }

    @Name("+") @VarArgs
    public static Num add(LispVal args) {
        return accumulate(Field::add, Num.ZERO, args);
    }

    @Name("-") @VarArgs
    public static Num sub(Num first, LispVal rest) {
        if (rest.isNil())
            return first.negate();
        return accumulate(Field::sub, first, rest);
    }

    @Name("*") @VarArgs
    public static Num mul(LispVal args) {
        return accumulate(Field::mul, Num.ONE, args);
    }

    @Name("/") @VarArgs
    public static Num div(Num first, LispVal rest) {
        if (rest.isNil())
            return Field.op(Field::div, Num.ONE, first);
        return accumulate(Field::div, first, rest);
    }

    public static Num abs(Num x) {
        return x.abs();
    }

    public static Num quotient(Num x, Num y) {
        return Field.op(Field::quotient, x, y);
    }

    public static Num modulo(Num x, Num y) {
        return Field.op(Field::modulo, x, y);
    }

    public static Num remainder(Num x, Num y) {
        return Field.op(Field::remainder, x, y);
    }

    public static Num numerator(Num x) {
        return x.numerator();
    }

    public static Num denominator(Num x) {
        return x.denominator();
    }

    public static Num expt(Num x, Num y) {
        return Field.expt(x, y);
    }

    // ----------------------------------------------------------------------

    public static Num floor(Num x) {
        return x.floor();
    }

    public static Num ceiling(Num x) {
        return x.ceiling();
    }

    public static Num truncate(Num x) {
        return x.truncate();
    }

    public static Num round(Num x) {
        return x.round();
    }

    public static Num exp(Num x) {
        return x.exp();
    }

    public static Num log(Num x) {
        return x.log();
    }

    public static Num sin(Num x) {
        return x.sin();
    }

    public static Num cos(Num x) {
        return x.cos();
    }

    public static Num tan(Num x) {
        return x.tan();
    }

    public static Num asin(Num x) {
        return x.asin();
    }

    public static Num acos(Num x) {
        return x.acos();
    }

    public static Num atan(Num x, Maybe<Num> y) {
        return y.isAbsent() ? x.atan() : x.atan2(y.get());
    }

    public static Num sqrt(Num x) {
        return x.sqrt();
    }

    // ----------------------------------------------------------------------

    public static Num make_rectangular(Num x, Num y) {
        if (x instanceof Complex)
            throw new TypeMismatch("real", x);
        if (y instanceof Complex)
            throw new TypeMismatch("real", y);
        return new Complex(x, y);
    }

    public static Num make_polar(double m, double a) {
        return new Complex(m * Math.cos(a), m * Math.sin(a));
    }

    public static Num real_part(Num z) {
        if (z instanceof Complex) {
            return ((Complex)z).real;
        } else {
            return z;
        }
    }

    public static Num imag_part(Num z) {
        if (z instanceof Complex) {
            return ((Complex)z).imag;
        } else {
            return Num.ZERO;
        }
    }

    public static Num magnitude(Num z) {
        if (z instanceof Complex) {
            return Num.make(((Complex)z).magnitude());
        } else {
            return z;
        }
    }

    public static Num angle(Num z) {
        if (z instanceof Complex) {
            return Num.make(((Complex)z).argument());
        } else if (z.isNegative()) {
            return new Real(Math.PI);
        } else {
            return Num.ZERO;
        }
    }
}
