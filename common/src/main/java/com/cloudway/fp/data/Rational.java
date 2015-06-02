/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.data;

import java.math.BigInteger;
import java.math.BigDecimal;
import java.math.MathContext;
import java.math.RoundingMode;

/**
 * This class represents the ratio of two integer numbers.
 */
public final class Rational extends Number implements Comparable<Rational> {
    private static final long serialVersionUID = 7222266596477798641L;

    /**
     * The rational number represents ZERO (0/1).
     */
    public static final Rational ZERO = new Rational(BigInteger.ZERO, BigInteger.ONE);

    /**
     * The rational number represents ONE (1/1).
     */
    public static final Rational ONE = new Rational(BigInteger.ONE, BigInteger.ONE);

    /**
     * The rational number represents positive infinity (1/0).
     */
    public static final Rational POSITIVE_INFINITY = new Rational(BigInteger.ONE, BigInteger.ZERO);

    /**
     * The rational number represents negative infinity (-1/0).
     */
    public static final Rational NEGATIVE_INFINITY = new Rational(BigInteger.ONE.negate(), BigInteger.ZERO);

    // the numerator and denominator components of this rational number.
    private BigInteger numer, denom;

    /**
     * Construct a rational number.
     */
    private Rational(BigInteger numer, BigInteger denom) {
        this.numer = numer;
        this.denom = denom;
    }

    /**
     * Creates a rational number having the specified numerator and denominator
     * components.
     *
     * @param numer the numerator component of this rational number.
     * @param denom the denominator component of this rational number.
     * @return the rational number.
     */
    public static Rational valueOf(Number numer, Number denom) {
        if (numer instanceof BigInteger) {
            if (denom instanceof BigInteger) {
                return make((BigInteger)numer, (BigInteger)denom);
            } else {
                return make((BigInteger)numer, BigInteger.valueOf(denom.longValue()));
            }
        } else if (denom instanceof BigInteger) {
            return make(BigInteger.valueOf(numer.longValue()), (BigInteger)denom);
        } else {
            return make(numer.longValue(), denom.longValue());
        }
    }

    /**
     * Create a rational number from the specified numeric value. This method
     * is used for converting other numeric value type into rational number.
     *
     * @param val the numeric value
     * @return the rational number
     */
    public static Rational valueOf(Number val) {
        if (val instanceof BigDecimal) {
            return valueOf((BigDecimal)val);
        } else if (val instanceof BigInteger) {
            return new Rational((BigInteger)val, BigInteger.ONE);
        } else if (val instanceof Rational) {
            return (Rational)val;
        } else if (val instanceof Double || val instanceof Float) {
            return valueOf(val.doubleValue());
        } else {
            return make(val.longValue(), 1);
        }
    }

    /**
     * Create a rational number from String representation.
     *
     * @param val the String representation of the rational number.
     * @return the rational number.
     */
    public static Rational valueOf(String val) {
        return valueOf(val, 10);
    }

    /**
     * Create a rational number from String representation.
     *
     * @param val the String representation of the rational number.
     * @param radix radix to be used in interpreting {@code val}
     * @return the rational number.
     */
    public static Rational valueOf(String val, int radix) {
        int sep = val.indexOf('/');
        if (sep >= 0) {
            BigInteger numer = new BigInteger(val.substring(0, sep), radix);
            BigInteger denom = new BigInteger(val.substring(sep+1), radix);
            return new Rational(numer, denom).normalize();
        } else {
            return new Rational(new BigInteger(val, radix), BigInteger.ONE);
        }
    }

    /**
     * Create a rational number from specified numerator and denominator
     * in BigInteger type. Performs necessary normalization.
     */
    public static Rational make(BigInteger numer, BigInteger denom) {
        return new Rational(numer, denom).normalize();
    }

    /**
     * Create a rational number from specified numerator and denominator
     * in long type. Performs necessary normalization.
     */
    public static Rational make(long numer, long denom) {
        if (denom == 0)
            return (numer >= 0) ? POSITIVE_INFINITY : NEGATIVE_INFINITY;
        if (numer == 0)
            return ZERO;
        if (numer == denom)
            return ONE;

        if (denom < 0) {
            numer = -numer;
            denom = -denom;
        }

        long g = gcd(numer, denom);
        if (g != 1) {
            numer /= g;
            denom /= g;
        }

        return new Rational(BigInteger.valueOf(numer), BigInteger.valueOf(denom));
    }

    private static long gcd(long m, long n) {
        if (m < 0) m = -m;
        if (n < 0) n = -n;
        
        long r;
        while (n > 0) {
            r = m % n;
            m = n;
            n = r;
        }
        return m;
    }

    private static final int CHUNK = 28;

    /**
     * Convert double precision inexact floating number into rational
     * number exactly, without rounding.
     */
    private static Rational valueOf(double x) {
        if (x == 0.0)
            return ZERO;
        if (x == Double.POSITIVE_INFINITY)
            return POSITIVE_INFINITY;
        if (x == Double.NEGATIVE_INFINITY)
            return NEGATIVE_INFINITY;

        boolean neg = false;
        if (x < 0) {
            x = -x;
            neg = true;
        }

        double[] t = frexp(x);
        double f = t[0];
        int e = (int)t[1];

        BigInteger top = BigInteger.ZERO;
        BigInteger bot = BigInteger.ONE;

        // Suck up CHUNK bits at a time; 28 is enough so that we suck
        // up all bits in 2 iterations for all known binary double-
        // precision formats, and small enough to fit in an int.
        while (f != 0.0) {
            f = ldexp(f, CHUNK);
            int digit = (int)f;
            top = top.shiftLeft(CHUNK).add(BigInteger.valueOf(digit));
            f -= digit;
            e -= CHUNK;
        }

        if (e > 0) {
            top = top.shiftLeft(e);
        } else {
            bot = bot.shiftLeft(-e);
        }
        if (neg) {
            top = top.negate();
        }

        return make(top, bot);
    }

    private static final long D_MASK  = 0x7ffL;
    private static final long D_BIAS  = 0x3FEL;
    private static final int  D_SHIFT = (64-11-1);

    private static double[] frexp(double d) {
        if (d == 0.0) {
            return new double[]{0.0, 0.0};
        }

        long x = Double.doubleToLongBits(d);
        int e = (int)(((x >>> D_SHIFT) & D_MASK) - D_BIAS);
        x &= ~(D_MASK << D_SHIFT);
        x |= D_BIAS << D_SHIFT;
        d = Double.longBitsToDouble(x);
        return new double[]{d, e};
    }

    private static double ldexp(double d, int e) {
        if (d == 0.0) {
            return 0.0;
        }

        long x = Double.doubleToLongBits(d);
        e += (int)((x >>> D_SHIFT) & D_MASK);
        if (e <= 0)
            return 0.0;   // underflow
        if (e >= D_MASK)  // overflow
            return (d < 0) ? Double.NEGATIVE_INFINITY : Double.POSITIVE_INFINITY;
        x &= ~(D_MASK << D_SHIFT);
        x |= (long)e << D_SHIFT;
        return Double.longBitsToDouble(x);
    }

    private static Rational valueOf(BigDecimal val) {
        int scale = val.scale();
        BigInteger n, d;

        if (scale == 0) {
            n = val.unscaledValue();
            d = BigInteger.ONE;
        } else if (scale > 0) {
            n = val.unscaledValue();
            d = BigInteger.TEN.pow(scale);
        } else {
            n = val.unscaledValue().multiply(BigInteger.TEN.pow(-scale));
            d = BigInteger.ONE;
        }

        return make(n, d);
    }

    /**
     * Returns the numerator of this rational number.
     *
     * @return the numerator of this rational number.
     */
    public BigInteger numerator() {
        return numer;
    }

    /**
     * Returns the denominator of this rational number.
     *
     * @return the denominator of this rational number.
     */
    public BigInteger denominator() {
        return denom;
    }

    /**
     * Returns the sum of this rational number with the one specified.
     *
     * @param that the rational number to be added.
     * @return {@code this + that}
     */
    public Rational add(Rational that) {
        return make(this.numer.multiply(that.denom).add(this.denom.multiply(that.numer)),
                    this.denom.multiply(that.denom));
    }

    /**
     * Returns the difference between this rational number and the one specified.
     *
     * @param that the rational number to be subtracted.
     * @return {@code this - that}.
     */
    public Rational subtract(Rational that) {
        return make(this.numer.multiply(that.denom).subtract(this.denom.multiply(that.numer)),
                    this.denom.multiply(that.denom));
    }

    /**
     * Returns the product of this rational number with the one specified.
     *
     * @param that the rational multiplier.
     * @return {@code this * that}.
     */
    public Rational multiply(Rational that) {
        return make(this.numer.multiply(that.numer),
                    this.denom.multiply(that.denom));
    }

    /**
     * Returns this rational number divided by the one specified.
     *
     * @param that the rational divisor.
     * @return {@code this / that}.
     */
    public Rational divide(Rational that) {
        return make(this.numer.multiply(that.denom),
                    this.denom.multiply(that.numer));
    }

    /**
     * Returns remainder of this rational number divided by the one specified.
     *
     * @param that the rational divisor.
     * @return {@code this % that}.
     */
    public Rational remainder(Rational that) {
        Rational[] divrem = divideAndRemainder(that);
        return divrem[1];
    }

    /**
     * Returns tuple of quotient and remainder of this rational number divided
     * by the one specified.
     *
     * @param that the rational divisor.
     * @return {@code (this / that, this % that)}.
     */
    public Rational[] divideAndRemainder(Rational that) {
        // use the identity  x = i * y + r to determine r
        BigInteger n = that.numer, d = that.denom;
        BigInteger i = this.numer.multiply(d).divide(this.denom.multiply(n)); // i=x/y

        Rational[] result = new Rational[2];
        result[0] = make(i, BigInteger.ONE);
        result[1] = this.subtract(make(that.numer.multiply(i), that.denom)); // r=x-i*y
        return result;
    }

    /**
     * Returns Greatest Common Divisor of this rational number and the one specified.
     *
     * @param that another rational to compute gcd
     * @return {@code gcd(this, that)}.
     */
    public Rational gcd(Rational that) {
        if (that.signum() == 0)
            return this.abs();
        if (this.signum() == 0)
            return that.abs();

        Rational a = this.abs();
        Rational b = that.abs();
        Rational r;
        while (b.signum() != 0) {
            r = a.remainder(b); a = b; b = r;
        }
        return a;
    }

    /**
     * Returns this rational number raised to the specified power.
     *
     * @param n the exponent.
     * @return {@code this^n}.
     */
    public Rational pow(int n) {
        if (n < 0) {
            return make(denom.pow(-n), numer.pow(-n));
        } else {
            return make(numer.pow(n), denom.pow(n));
        }
    }

    /**
     * Returns the negation of this rational number.
     *
     * @return the negation of this rational number.
     */
    public Rational negate() {
        return new Rational(numer.negate(), denom);
    }

    /**
     * Returns the signum of this rational number.
     *
     * @return -1, 0, or 1 as the value of this rational number is negative,
     *         zero or positive.
     */
    public int signum() {
        return numer.signum();
    }

    /**
     * Returns the absolute value of this rational number.
     *
     * @return {@code abs(this)}
     */
    public Rational abs() {
        return numer.signum() >= 0 ? this : new Rational(numer.negate(), denom);
    }

    /**
     * Converts this rational number to a BigInteger.
     *
     * @return this rational number converted to a BigInteger.
     */
    public BigInteger toBigInteger() {
        return numer.divide(denom);
    }

    /**
     * Convert this rational number to a {@code BigDecimal}. This conversion
     * can lose information about the precision of the rational value.
     *
     * @return this rational number converted to a {@code BigDecimal}.
     */
    public BigDecimal toBigDecimal() {
        BigDecimal n = new BigDecimal(numer);
        BigDecimal d = new BigDecimal(denom);
        MathContext mc = new MathContext((int)Math.min(n.precision() +
                                                       (long)Math.ceil(10.0*d.precision()/3.0),
                                                       Integer.MAX_VALUE),
                                         RoundingMode.HALF_EVEN);
        return n.divide(d, mc);
    }

    /**
     * Convert this rational number to a {@code BigDecimal}. This conversion
     * can lose information about the precision of the rational value.
     *
     * @param mc the MathContext used for division.
     * @return this rational number converted to a {@code BigDecimal}.
     */
    public BigDecimal toBigDecimal(MathContext mc) {
        return new BigDecimal(numer).divide(new BigDecimal(denom), mc);
    }

    /**
     * Converts this rational number to a {@code long}. The fraction in the
     * rational number is truncated and if this rational number is too big to fit
     * in a {@code long}, only the low-order 64 bits are returned.
     *
     * @return this rational number converted to a {@code long}.
     */
    @Override
    public long longValue() {
        return numer.divide(denom).longValue();
    }

    /**
     * Converts this rational number to an {@code int}. The fraction in the
     * rational number is truncated and if this rational number is too big to fit
     * in an {@code int}, only the low-order 32 bits are returned.
     *
     * @return this rational number converted to an {@code int}.
     */
    @Override
    public int intValue() {
        return (int)longValue();
    }

    /**
     * Converts this rational number to a {@code short}. The fraction in the
     * rational number is truncated and if this rational number is too big to fit
     * in a {@code short}, only the low-order 16 bits are returned.
     *
     * @return this rational number converted to a {@code short}.
     */
    @Override
    public short shortValue() {
        return (short)longValue();
    }

    /**
     * Converts this rational number to a {@code byte}. The fraction in the
     * rational number is truncated and if this rational number is too big to fit
     * in a {@code byte}, only the low-order 8 bits are returned.
     *
     * @return this rational number converted to a {@code byte}.
     */
    @Override
    public byte byteValue() {
        return (byte)longValue();
    }

    /**
     * Converts this rational number to a {@code double}. This conversion
     * can lose information about the precision of the rational value.
     *
     * @return this rational number converted to a {@code double}.
     */
    @Override
    public double doubleValue() {
        if (numer.signum() < 0) {
            return -this.abs().doubleValue();
        }

        // Normalize to 63 bit
        int numerBitLength = numer.bitLength();
        int denomBitLength = denom.bitLength();
        if (numerBitLength > denomBitLength) {
            int shift = denomBitLength - 63;
            long divisor = denom.shiftRight(shift).longValue();
            BigInteger dividend = numer.shiftRight(shift);
            return dividend.doubleValue() / divisor;
        } else {
            int shift = numerBitLength - 63;
            long dividend = numer.shiftRight(shift).longValue();
            BigInteger divisor = denom.shiftRight(shift);
            return dividend / divisor.doubleValue();
        }
    }

    /**
     * Converts this rational number to a {@code float}. This conversion
     * can lose information about the precision of the rational value.
     *
     * @return this rational number converted to a {@code float}.
     */
    @Override
    public float floatValue() {
        return (float)doubleValue();
    }

    /**
     * Compares this rational number with the specified Object for equality.
     *
     * @param obj Object to which this rational number is to be compared.
     * @return {@code true} if and only if the specified Object is a
     *         Rational whose value is numerically equal to this Rational.
     */
    public boolean equals(Object obj) {
        if (obj instanceof Rational) {
            Rational that = (Rational)obj;
            return this.numer.equals(that.numer) && this.denom.equals(that.denom);
        } else {
            return false;
        }
    }

    /**
     * Returns the hash code for this rational number.
     *
     * @return hash code for this rational number.
     */
    public int hashCode() {
        return numer.hashCode() ^ denom.hashCode();
    }

    /**
     * Returns the String representation of this rational number.
     *
     * @return String representation of this rational number.
     */
    public String toString() {
        if (denom.signum() == 0) {
            return numer.signum() >= 0 ? "Infinity" : "-Infinity";
        } else if (denom.equals(BigInteger.ONE)) {
            return numer.toString();
        } else {
            return numer.toString().concat("/").concat(denom.toString());
        }
    }

    /**
     * Compares this rational number with the specified one.
     *
     * @param that Rational to which this Rational is to be compared.
     * @return -1, 0 or 1 as this Rational is numerically less than, equal
     *         to, or greater than.
     */
    @Override
    public int compareTo(Rational that) {
        return this.numer.multiply(that.denom).compareTo(
               that.numer.multiply(this.denom));
    }

    private Rational normalize() {
        if (denom.signum() == 0) {
            return numer.signum() >= 0 ? POSITIVE_INFINITY : NEGATIVE_INFINITY;
        }

        if (denom.signum() < 0) {
            numer = numer.negate();
            denom = denom.negate();
        }

        BigInteger gcd = numer.gcd(denom);
        if (!gcd.equals(BigInteger.ONE)) {
            numer = numer.divide(gcd);
            denom = denom.divide(gcd);
        }

        return this;
    }
}
