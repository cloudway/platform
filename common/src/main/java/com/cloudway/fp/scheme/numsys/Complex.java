/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.scheme.numsys;

import com.cloudway.fp.scheme.LispError.TypeMismatch;

public class Complex extends Num {
    public static final Field<Complex> TAG = new Tag();

    static {
        Field.install(Int32.TAG, TAG, x -> new Complex(x, Num.ZERO));
        Field.install(Int64.TAG, TAG, x -> new Complex(x, Num.ZERO));
        Field.install(BigInt.TAG, TAG, x -> new Complex(x, Num.ZERO));
        Field.install(Ratio.TAG, TAG, x -> new Complex(x, Num.ZERO));
        Field.install(Real.TAG, TAG, x -> new Complex(x.value, 0));
    }

    public static final Complex ZERO = new Complex(Num.ZERO, Num.ZERO);
    public static final Complex ONE = new Complex(Num.ONE, Num.ZERO);

    public final Num real, imag;

    public Complex(Num real, Num imag) {
        this.real = real;
        this.imag = imag;
    }

    public Complex(double real, double imag) {
        this.real = make(real);
        this.imag = make(imag);
    }

    @Override
    public Field<Complex> tag() {
        return TAG;
    }

    @Override
    public boolean isZero() {
        return real.isZero() && imag.isZero();
    }

    @Override
    public boolean isNegative() {
        if (imag.isZero()) {
            return real.isNegative();
        } else {
            throw new TypeMismatch("real", this);
        }
    }

    @Override
    public Num lower() {
        if (imag.isZero()) {
            return real.lower();
        } else {
            return this;
        }
    }

    @Override
    public boolean isExact() {
        return real.isExact() && imag.isExact();
    }

    @Override
    public double toReal() {
        if (imag.isZero()) {
            return real.toReal();
        } else {
            throw new TypeMismatch("real", this);
        }
    }

    @Override
    public Num toExact() {
        return new Complex(real.toExact(), imag.toExact());
    }

    @Override
    public Num toInexact() {
        return new Complex(real.toInexact(), imag.toInexact());
    }

    @Override
    public Num negate() {
        return new Complex(real.negate(), imag.negate());
    }

    @Override
    public String show() {
        return show(10);
    }

    @Override
    public String show(int radix) {
        StringBuilder buf = new StringBuilder();
        buf.append(real.show(radix));
        if (imag.isNegative())
            buf.append(imag.show(radix));
        else
            buf.append("+").append(imag.show(radix));
        buf.append("i");
        return buf.toString();
    }

    @Override
    public boolean eqv(Object obj) {
        if (this == obj)
            return true;
        if (!(obj instanceof Complex))
            return false;
        Complex that = (Complex)obj;
        return real.equals(that.real) && imag.equals(that.imag);
    }

    @Override
    public boolean equals(Object obj) {
        return eqv(obj);
    }

    @Override
    public int hashCode() {
        return 31 * real.hashCode() + imag.hashCode();
    }

    @Override
    public String toString() {
        return "#Complex(" + real + ", " + imag + ")";
    }

    public double magnitude() {
        return Math.hypot(real.toReal(), imag.toReal());
    }

    public double argument() {
        return Math.atan2(imag.toReal(), real.toReal());
    }

    public Complex inverse() {
        Num a = real, b = imag;
        Num t = Field.op(Field::add, Field.op(Field::mul, a, a), Field.op(Field::mul, b, b));
        return new Complex(Field.op(Field::div, a, t), Field.op(Field::div, b.negate(), t));
    }

    public Complex add(Complex that) {
        Num a = this.real, b = this.imag;
        Num c = that.real, d = that.imag;
        return new Complex(Field.op(Field::add, a, c), Field.op(Field::add, b, d));
    }

    public Complex sub(Complex that) {
        Num a = this.real, b = this.imag;
        Num c = that.real, d = that.imag;
        return new Complex(Field.op(Field::sub, a, c), Field.op(Field::sub, b, d));
    }

    public Complex mul(Complex that) {
        Num a = this.real, b = this.imag;
        Num c = that.real, d = that.imag;
        Num r = Field.op(Field::sub, Field.op(Field::mul, a, c), Field.op(Field::mul, b, d));
        Num i = Field.op(Field::add, Field.op(Field::mul, b, c), Field.op(Field::mul, a, d));
        return new Complex(r, i);
    }

    public Complex div(Complex that) {
        Num a = this.real, b = this.imag;
        Num c = that.real, d = that.imag;

        Num t = Field.op(Field::add, Field.op(Field::mul, c, c), Field.op(Field::mul, d, d));
        Num r = Field.op(Field::add, Field.op(Field::mul, a, c), Field.op(Field::mul, b, d));
        Num i = Field.op(Field::sub, Field.op(Field::mul, b, c), Field.op(Field::mul, a, d));
        return new Complex(Field.op(Field::div, r, t), Field.op(Field::div, i, t));
    }

    public Complex pow(int n) {
        if (n == 0)
            return ONE;
        if (n == 1)
            return this;
        if (n == -1)
            return inverse();

        Complex z = this;
        if (n < 0) {
            z = inverse();
            n = -n;
        }

        Num r = z.real;
        Num i = z.imag;
        while (--n != 0) {
            while (n % 2 == 0) {
                // r = r*r - i*i
                Num r1 = Field.op(Field::sub, Field.op(Field::mul, r, r),
                                              Field.op(Field::mul, i, i));

                // i = 2*r*i
                Num i1 = Field.op(Field::mul, r, i);
                i1 = Field.op(Field::add, i1, i1);

                r = r1;
                i = i1;
                n >>= 1;
            }
            z = z.mul(new Complex(r, i));
        }
        return z;
    }

    public Complex expt(Complex that) {
        double r = that.real.toReal();
        double i = that.imag.toReal();

        double r1 = Math.log(this.magnitude());
        double i1 = this.argument();
        double r2 = (r1 * r) - (i1 * i);
        double i2 = (r1 * i) + (i1 * r);
        double m  = Math.exp(r2);
        return new Complex(m * Math.cos(i2), m * Math.sin(i2));
    }

    @Override
    public Real abs() {
        return new Real(magnitude());
    }

    @Override
    public Num floor() {
        if (imag.isZero()) {
            return real.floor();
        } else {
            throw new TypeMismatch("real", this);
        }
    }

    @Override
    public Num ceiling() {
        if (imag.isZero()) {
            return real.ceiling();
        } else {
            throw new TypeMismatch("real", this);
        }
    }

    @Override
    public Num truncate() {
        if (imag.isZero()) {
            return real.truncate();
        } else {
            throw new TypeMismatch("real", this);
        }
    }

    @Override
    public Num round() {
        if (imag.isZero()) {
            return real.round();
        } else {
            throw new TypeMismatch("real", this);
        }
    }

    @Override
    public Complex exp() {
        double m = Math.exp(real.toReal());
        double a = imag.toReal();
        return new Complex(m * Math.cos(a), m * Math.sin(a));
    }

    @Override
    public Complex log() {
        return new Complex(Math.log(magnitude()), argument());
    }

    @Override
    public Complex sin() {
        double a = real.toReal(), b = imag.toReal();
        return new Complex(Math.sin(a) * Math.cosh(b), Math.cos(a) * Math.sinh(b));
    }

    @Override
    public Complex cos() {
        double a = real.toReal(), b = imag.toReal();
        return new Complex(Math.cos(a) * Math.cosh(b), -Math.sin(a) * Math.sinh(b));
    }

    @Override
    public Complex tan() {
        return sin().div(cos());
    }

    private static Complex rotl(Complex z) {
        return new Complex(z.imag.negate(), z.real);
    }

    private static Complex rotr(Complex z) {
        return new Complex(z.imag, z.real.negate());
    }

    @Override
    public Complex asin() {
        // -i log(ix + sqrt(1 - x^2))
        Complex x = this;
        return rotr(rotl(x).add(ONE.sub(x.mul(x)).sqrt()).log());
    }

    @Override
    public Complex acos() {
        // -i log(x + i sqrt(1 - x^2))
        Complex x = this;
        return rotr(x.add(rotl(ONE.sub(x.mul(x)).sqrt())).log());
    }

    @Override
    public Complex atan() {
        // -i log((1+ix) / sqrt(1+x^2))
        Complex x = this;
        return rotr(ONE.add(rotl(x)).div(ONE.add(x.mul(x)).sqrt()).log());
    }

    @Override
    public Complex sqrt() {
        double m = Math.sqrt(magnitude());
        double a = argument() / 2;
        return new Complex(m * Math.cos(a), m * Math.sin(a));
    }

    private static final class Tag extends Field<Complex> {
        private static Num ensureReal(Complex x) {
            if (!x.imag.isZero())
                throw new TypeMismatch("real", x);
            return x.real;
        }

        @Override
        public int compare(Complex x, Complex y) {
            if (x.real.equals(y.real) && x.imag.equals(y.imag))
                return 0;

            return op(Field::compare, ensureReal(x), ensureReal(y));
        }

        @Override
        public Num add(Complex x, Complex y) {
            return x.add(y).lower();
        }

        @Override
        public Num sub(Complex x, Complex y) {
            return x.sub(y).lower();
        }

        @Override
        public Num mul(Complex x, Complex y) {
            return x.mul(y).lower();
        }

        @Override
        public Num div(Complex x, Complex y) {
            return x.div(y).lower();
        }

        @Override
        public Num quotient(Complex x, Complex y) {
            return op(Field::quotient, ensureReal(x), ensureReal(y));
        }

        @Override
        public Num modulo(Complex x, Complex y) {
            return op(Field::modulo, ensureReal(x), ensureReal(y));
        }

        @Override
        public Num remainder(Complex x, Complex y) {
            return op(Field::remainder, ensureReal(x), ensureReal(y));
        }

        @Override
        public Num pow(Complex x, int n) {
            return x.pow(n).lower();
        }
    }
}
