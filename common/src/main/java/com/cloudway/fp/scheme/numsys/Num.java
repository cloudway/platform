/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.scheme.numsys;

import java.math.BigInteger;

import com.cloudway.fp.data.Rational;
import com.cloudway.fp.scheme.LispVal;

public abstract class Num implements LispVal, Comparable<Num> {
    public static final Num ZERO = make(0);
    public static final Num ONE  = make(1);

    public static Num make(short value) {
        return new Int16(value);
    }

    public static Num make(int value) {
        return new Int32(value);
    }

    public static Num make(long value) {
        return new Int64(value);
    }

    public static Num make(BigInteger value) {
        return new BigInt(value);
    }

    public static Num make(Rational value) {
        return new Ratio(value);
    }

    public static Num make(double value) {
        return new Real(value);
    }

    public abstract Field<? extends Num> tag();

    public abstract boolean isZero();
    public abstract boolean isNegative();

    public abstract Num lower();

    public boolean isExact() {
        return true;
    }

    public abstract double toReal();

    public Num toExact() {
        return this;
    }

    public Num toInexact() {
        return new Real(toReal());
    }

    public abstract Num negate();

    public Num numerator() {
        return this;
    }

    public Num denominator() {
        return ONE;
    }

    @Override
    public boolean isSelfEvaluating() {
        return true;
    }

    public abstract String show(int radix);

    @Override
    public int compareTo(Num that) {
        return Field.op(Field::compare, this, that);
    }

    public boolean equals(long value) {
        return false;
    }

    public abstract boolean equals(Object obj);
    public abstract int hashCode();

    public Num abs() {
        return isNegative() ? negate() : this;
    }

    public Num floor() {
        return this;
    }

    public Num ceiling() {
        return this;
    }

    public Num truncate() {
        return this;
    }

    public Num round() {
        return this;
    }

    public Num exp() {
        return make(Math.exp(toReal()));
    }

    public Num log() {
        return make(Math.log(toReal()));
    }

    public Num sin() {
        return make(Math.sin(toReal()));
    }

    public Num cos() {
        return make(Math.cos(toReal()));
    }

    public Num tan() {
        return make(Math.tan(toReal()));
    }

    public Num asin() {
        return make(Math.asin(toReal()));
    }

    public Num acos() {
        return make(Math.acos(toReal()));
    }

    public Num atan() {
        return make(Math.atan(toReal()));
    }

    public Num atan2(Num y) {
        return make(Math.atan2(toReal(), y.toReal()));
    }

    @SuppressWarnings("FloatingPointEquality")
    public Num sqrt() {
        double x = toReal();

        if (x < 0) {
            return new Complex(0, Math.sqrt(-x));
        }

        double y = Math.sqrt(x);
        long   a = (long)Math.rint(y);

        if (a == y) {
            return (int)a == a ? new Int32((int)a) : new Int64(a);
        } else {
            return new Real(y);
        }
    }
}
