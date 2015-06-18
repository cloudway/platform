/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.scheme.numsys;

import com.cloudway.fp.data.Rational;
import com.cloudway.fp.scheme.LispError;

@SuppressWarnings("FloatingPointEquality")
public class Real extends Num {
    public static final Field<Real> TAG = new Tag();

    static {
        Field.install(Int32.TAG, TAG, x -> new Real(x.value));
        Field.install(Int64.TAG, TAG, x -> new Real(x.value));
        Field.install(BigInt.TAG, TAG, x -> new Real(x.value.doubleValue()));
        Field.install(Ratio.TAG, TAG, x -> new Real(x.value.doubleValue()));
    }

    public final double value;

    public Real(double value) {
        this.value = value;
    }

    @Override
    public Field<Real> tag() {
        return TAG;
    }

    @Override
    public boolean isZero() {
        return value == 0;
    }

    @Override
    public boolean isNegative() {
        return value < 0;
    }

    @Override
    public Num lower() {
        return this;
    }

    @Override
    public Object getObject() {
        return value;
    }

    @Override
    public Class<?> getObjectType() {
        return Double.class;
    }

    @Override
    public boolean isExact() {
        return false;
    }

    @Override
    public double toReal() {
        return value;
    }

    @Override
    public Num toExact() {
        long r = (long)Math.rint(value);
        if (r == value) {
            if ((int)r == r) {
                return new Int32((int)r);
            } else {
                return new Int64(r);
            }
        } else {
            return Num.make(Rational.valueOf(value));
        }
    }

    @Override
    public Num negate() {
        return new Real(-value);
    }

    @Override
    public Num numerator() {
        Rational rat = Rational.valueOf(value);
        return new BigInt(rat.numerator()).lower();
    }

    @Override
    public Num denominator() {
        Rational rat = Rational.valueOf(value);
        return new BigInt(rat.denominator()).lower();
    }

    @Override
    public Num floor() {
        return new Real(Math.floor(value));
    }

    @Override
    public Num ceiling() {
        return new Real(Math.ceil(value));
    }

    @Override
    public Num truncate() {
        return value >= 0 ? floor() : ceiling();
    }

    @Override
    public Num round() {
        return new Real(Math.round(value));
    }

    @Override
    public Num sqrt() {
        return new Real(Math.sqrt(value));
    }

    @Override
    public String show() {
        if (value == Double.POSITIVE_INFINITY)
            return "+inf.0";
        if (value == Double.NEGATIVE_INFINITY)
            return "-inf.0";
        if (Double.isNaN(value))
            return "+nan.0";
        return Double.toString(value);
    }

    @Override
    public String show(int radix) {
        if (radix != 10)
            throw new LispError("inexact numbers can only be represented in base 10");
        return show();
    }

    @Override
    public boolean eqv(Object obj) {
        if (Double.isNaN(value))
            return false;
        if (obj == this)
            return true;
        if (obj instanceof Real)
            return value == ((Real)obj).value;
        return false;
    }

    @Override
    public boolean equals(Object obj) {
        return eqv(obj);
    }

    @Override
    public int hashCode() {
        return Double.hashCode(value);
    }

    public String toString() {
        return "#Real(" + value + ")";
    }

    public static class Tag extends Field<Real> {
        @Override
        public int compare(Real x, Real y) {
            return Double.compare(x.value, y.value);
        }

        @Override
        public Num add(Real x, Real y) {
            return new Real(x.value + y.value);
        }

        @Override
        public Num sub(Real x, Real y) {
            return new Real(x.value - y.value);
        }

        @Override
        public Num mul(Real x, Real y) {
            return new Real(x.value * y.value);
        }

        @Override
        public Num div(Real x, Real y) {
            return new Real(x.value / y.value);
        }

        @Override
        public Num quotient(Real x, Real y) {
            double a = x.value, b = y.value;
            if (Math.rint(a) != a)
                throw new LispError.TypeMismatch("integer", x);
            if (Math.rint(b) != b) {
                throw new LispError.TypeMismatch("integer", y);
            }

            double r = a / b;
            return new Real(r >= 0 ? Math.floor(r) : Math.ceil(r));
        }

        @Override
        public Num modulo(Real x, Real y) {
            double a = x.value, b = y.value;
            if (Math.rint(a) != a)
                throw new LispError.TypeMismatch("integer", x);
            if (Math.rint(b) != b) {
                throw new LispError.TypeMismatch("integer", y);
            }

            double r = a % b;
            if (r > 0 && b < 0 || r < 0 && b > 0)
                r += b;
            return new Real(r >= 0 ? Math.floor(r) : Math.ceil(r));
        }

        @Override
        public Num remainder(Real x, Real y) {
            double a = x.value, b = y.value;
            if (Math.rint(a) != a)
                throw new LispError.TypeMismatch("integer", x);
            if (Math.rint(b) != b) {
                throw new LispError.TypeMismatch("integer", y);
            }

            double r = a % b;
            return new Real(r >= 0 ? Math.floor(r) : Math.ceil(r));
        }

        @Override
        public Num pow(Real x, int n) {
            return new Real(Math.pow(x.value, n));
        }
    }
}
