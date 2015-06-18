/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.scheme.numsys;

import java.math.BigInteger;

import com.cloudway.fp.data.Rational;
import com.cloudway.fp.scheme.LispError;

public class Ratio extends Num {
    public static final Field<Ratio> TAG = new Tag();

    static {
        Field.install(Int32.TAG, TAG, x -> new Ratio(Rational.valueOf(x.value)));
        Field.install(Int64.TAG, TAG, x -> new Ratio(Rational.valueOf(x.value)));
        Field.install(BigInt.TAG, TAG, x -> new Ratio(Rational.valueOf(x.value)));
    }

    public final Rational value;

    public Ratio(Rational value) {
        this.value = value;
    }

    @Override
    public Field<Ratio> tag() {
        return TAG;
    }

    @Override
    public boolean isZero() {
        return value.signum() == 0;
    }

    @Override
    public boolean isNegative() {
        return value.signum() < 0;
    }

    @Override
    public Num lower() {
        if (value.denominator().equals(BigInteger.ONE)) {
            return new BigInt(value.numerator()).lower();
        } else {
            return this;
        }
    }

    @Override
    public Object getObject() {
        return value;
    }

    @Override
    public Class<?> getObjectType() {
        return Rational.class;
    }

    @Override
    public double toReal() {
        return value.doubleValue();
    }

    @Override
    public Num negate() {
        return new Ratio(value.negate()).lower();
    }

    @Override
    public Num numerator() {
        return new BigInt(value.numerator()).lower();
    }

    @Override
    public Num denominator() {
        return new BigInt(value.denominator()).lower();
    }

    @Override
    public Num floor() {
        BigInteger r = value.numerator().divide(value.denominator());
        if (value.signum() < 0)
            r = r.subtract(BigInteger.ONE);
        return new BigInt(r).lower();
    }

    @Override
    public Num ceiling() {
        BigInteger r = value.numerator().divide(value.denominator());
        if (value.signum() > 0)
            r = r.add(BigInteger.ONE);
        return new BigInt(r).lower();
    }

    @Override
    public Num truncate() {
        BigInteger r = value.numerator().divide(value.denominator());
        return new BigInt(r).lower();
    }

    @Override
    public Num round() {
        BigInteger qr[] = value.numerator().abs().divideAndRemainder(value.denominator());
        BigInteger q = qr[0], r = qr[1];

        if (value.signum() < 0) {
            q = q.negate();
            if (r.add(r).compareTo(value.denominator()) > 0)
                q = q.subtract(BigInteger.ONE);
        } else {
            if (r.add(r).compareTo(value.denominator()) > 0)
                q = q.add(BigInteger.ONE);
        }

        return new BigInt(q).lower();
    }

    @Override
    public Num sqrt() {
        if (value.signum() == 0)
            return Num.ZERO;

        if (value.signum() < 0)
            return new Complex(Num.ZERO, sqrt(value.negate()));

        return sqrt(value);
    }

    public static Num sqrt(Rational x) {
        Num numer = BigInt.sqrt(x.numerator());
        Num denom = BigInt.sqrt(x.denominator());

        BigInt n = Field.raise(numer, BigInt.TAG);
        BigInt d = Field.raise(denom, BigInt.TAG);

        if (n != null && d != null) {
            return new Ratio(Rational.make(n.value, d.value));
        } else {
            return Field.op(Field::div, numer, denom);
        }
    }

    @Override
    public String show() {
        return show(10);
    }

    @Override
    public String show(int radix) {
        if (value.denominator().signum() == 0) {
            int sign = value.numerator().signum();
            return sign < 0 ? "-inf.0" : sign > 0 ? "+inf.0" : "+nan.0";
        } else if (value.denominator().equals(BigInteger.ONE)) {
            return value.numerator().toString(radix);
        } else {
            return value.numerator().toString(radix) +
                   "/" +
                   value.denominator().toString(radix);
        }
    }

    @Override
    public boolean eqv(Object obj) {
        if (obj == this)
            return true;
        return (obj instanceof Ratio) && value.equals(((Ratio)obj).value);
    }

    @Override
    public boolean equals(Object obj) {
        return eqv(obj);
    }

    @Override
    public int hashCode() {
        return value.hashCode();
    }

    public String toString() {
        return "#Ratio(" + value + ")";
    }

    private static class Tag extends Field<Ratio> {
        @Override
        public int compare(Ratio x, Ratio y) {
            return x.value.compareTo(y.value);
        }

        @Override
        public Num add(Ratio x, Ratio y) {
            return new Ratio(x.value.add(y.value)).lower();
        }

        @Override
        public Num sub(Ratio x, Ratio y) {
            return new Ratio(x.value.subtract(y.value)).lower();
        }

        @Override
        public Num mul(Ratio x, Ratio y) {
            return new Ratio(x.value.multiply(y.value)).lower();
        }

        @Override
        public Num div(Ratio x, Ratio y) {
            return new Ratio(x.value.divide(y.value)).lower();
        }

        @Override
        public Num quotient(Ratio x, Ratio y) {
            Rational a = x.value, b = y.value;

            if (!a.denominator().equals(BigInteger.ONE))
                throw new LispError.TypeMismatch("integer", x);
            if (!b.denominator().equals(BigInteger.ONE))
                throw new LispError.TypeMismatch("integer", y);

            return new BigInt(a.numerator().divide(b.numerator())).lower();
        }

        @Override
        public Num modulo(Ratio x, Ratio y) {
            Rational a = x.value, b = y.value;

            if (!a.denominator().equals(BigInteger.ONE))
                throw new LispError.TypeMismatch("integer", x);
            if (!b.denominator().equals(BigInteger.ONE))
                throw new LispError.TypeMismatch("integer", y);

            return new BigInt(a.numerator().mod(b.numerator())).lower();
        }

        @Override
        public Num remainder(Ratio x, Ratio y) {
            Rational a = x.value, b = y.value;

            if (!a.denominator().equals(BigInteger.ONE))
                throw new LispError.TypeMismatch("integer", x);
            if (!b.denominator().equals(BigInteger.ONE))
                throw new LispError.TypeMismatch("integer", y);

            return new BigInt(a.numerator().remainder(b.numerator())).lower();
        }

        @Override
        public Num pow(Ratio x, int n) {
            return new Ratio(x.value.pow(n)).lower();
        }
    }
}
