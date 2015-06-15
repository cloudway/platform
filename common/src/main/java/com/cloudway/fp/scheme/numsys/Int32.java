/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.scheme.numsys;

import com.cloudway.fp.data.Rational;

public class Int32 extends Num {
    public static final Field<Int32> TAG = new Tag();

    static {
        Field.install(Int16.TAG, TAG, x -> new Int32(x.value));
    }

    public final int value;

    public Int32(int value) {
        this.value = value;
    }

    @Override
    public Field<Int32> tag() {
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
    public double toReal() {
        return value;
    }

    @Override
    public Num lower() {
        return this;
    }

    @Override
    public Num negate() {
        if (value == Integer.MIN_VALUE) {
            return new Int64((long)Integer.MAX_VALUE + 1);
        } else {
            return new Int32(-value);
        }
    }

    @Override
    public String show() {
        return Integer.toString(value);
    }

    @Override
    public String show(int radix) {
        return Integer.toString(value, radix);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this)
            return true;
        return (obj instanceof Num) && ((Num)obj).equals(value);
    }

    @Override
    public boolean equals(long value) {
        return value == this.value;
    }

    @Override
    public int hashCode() {
        return Integer.hashCode(value);
    }

    public String toString() {
        return "#Int32(" + value + ")";
    }

    private static class Tag extends Field<Int32> {
        @Override
        public int compare(Int32 x, Int32 y) {
            return Integer.compare(x.value, y.value);
        }

        @Override
        public Num add(Int32 x, Int32 y) {
            int a = x.value, b = y.value;
            int r = a + b;
            if (((a ^ r) & (b ^ r)) < 0) {
                return new Int64((long)a + (long)b);
            } else {
                return new Int32(r);
            }
        }

        @Override
        public Num sub(Int32 x, Int32 y) {
            int a = x.value, b = y.value;
            int r = a - b;
            if (((a ^ b) & (a ^ r)) < 0) {
                return new Int64((long)a - (long)b);
            } else {
                return new Int32(r);
            }
        }

        @Override
        public Num mul(Int32 x, Int32 y) {
            long r = (long)x.value * (long)y.value;
            if ((int)r == r) {
                return new Int32((int)r);
            } else {
                return new Int64(r);
            }
        }

        @Override
        public Num div(Int32 x, Int32 y) {
            if (y.value == 1) {
                return x;
            } else {
                return new Ratio(Rational.valueOf(x.value, y.value)).lower();
            }
        }

        @Override
        public Num quotient(Int32 x, Int32 y) {
            return new Int32(x.value / y.value);
        }

        @Override
        public Num modulo(Int32 x, Int32 y) {
            int m = y.value;
            int r = x.value % m;
            if (r > 0 && m < 0 || r < 0 && m > 0)
                r += m;
            return new Int32(r);
        }

        @Override
        public Num remainder(Int32 x, Int32 y) {
            return new Int32(x.value % y.value);
        }
    }
}
