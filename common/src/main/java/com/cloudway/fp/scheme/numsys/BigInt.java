/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.scheme.numsys;

import java.math.BigInteger;

import com.cloudway.fp.data.Rational;

public class BigInt extends Num {
    public static final Field<BigInt> TAG = new Tag();

    static {
        Field.install(Int32.TAG, TAG, x -> new BigInt(BigInteger.valueOf(x.value)));
        Field.install(Int64.TAG, TAG, x -> new BigInt(BigInteger.valueOf(x.value)));
    }

    public final BigInteger value;

    public BigInt(BigInteger value) {
        this.value = value;
    }

    @Override
    public Field<BigInt> tag() {
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
        BigInteger r = value;
        int sz = r.bitLength();
        if (sz < 16)
            return new Int16(r.shortValue());
        if (sz < 32)
            return new Int32(r.intValue());
        if (sz < 64)
            return new Int64(r.longValue());
        return this;
    }

    @Override
    public Object getObject() {
        return value;
    }

    @Override
    public Class<?> getObjectType() {
        int sz = value.bitLength();
        if (sz < 16)
            return Short.class;
        if (sz < 32)
            return Integer.class;
        if (sz < 64)
            return Long.class;
        return BigInteger.class;
    }

    @Override
    public double toReal() {
        return value.doubleValue();
    }

    @Override
    public Num negate() {
        return new BigInt(value.negate()).lower();
    }

    @Override
    public Num sqrt() {
        if (value.signum() == 0)
            return Num.ZERO;

        if (value.signum() < 0)
            return new Complex(Num.ZERO, sqrt(value.negate()));

        return sqrt(value);
    }

    public static Num sqrt(final BigInteger x) {
        BigInteger a, b;

        // Compute a = (a + x/a)/2 repeatedly
        a = BigInteger.ZERO.setBit(x.bitLength() / 2);
        b = a.add(x.divide(a)).shiftRight(1);
        if (!a.equals(b)) {
            do {
                a = b;
                b = a.add(x.divide(a)).shiftRight(1);
            } while (b.compareTo(a) < 0);
        }

        if (a.pow(2).equals(x)) {
            return new BigInt(a).lower(); // exact
        } else {
            return new Real(Math.sqrt(x.doubleValue())); // inexact
        }
    }

    @Override
    public String show() {
        return value.toString();
    }

    @Override
    public String show(int radix) {
        return value.toString(radix);
    }

    @Override
    public boolean eqv(Object obj) {
        if (obj == this)
            return true;
        if (!(obj instanceof Num))
            return false;

        BigInt that = Field.raise((Num)obj, TAG);
        return that != null && this.value.equals(that.value);
    }

    @Override
    public boolean equals(long value) {
        return this.value.equals(BigInteger.valueOf(value));
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
        return "#BigInt(" + value + ")";
    }

    private static class Tag extends Field<BigInt> {
        @Override
        public int compare(BigInt x, BigInt y) {
            return x.value.compareTo(y.value);
        }

        @Override
        public Num add(BigInt x, BigInt y) {
            return new BigInt(x.value.add(y.value)).lower();
        }

        @Override
        public Num sub(BigInt x, BigInt y) {
            return new BigInt(x.value.subtract(y.value)).lower();
        }

        @Override
        public Num mul(BigInt x, BigInt y) {
            return new BigInt(x.value.multiply(y.value)).lower();
        }

        @Override
        public Num div(BigInt x, BigInt y) {
            return new Ratio(Rational.valueOf(x.value, y.value)).lower();
        }

        @Override
        public Num quotient(BigInt x, BigInt y) {
            return new BigInt(x.value.divide(y.value)).lower();
        }

        @Override
        public Num modulo(BigInt x, BigInt y) {
            return new BigInt(x.value.mod(y.value)).lower();
        }

        @Override
        public Num remainder(BigInt x, BigInt y) {
            return new BigInt(x.value.remainder(y.value)).lower();
        }
    }
}
