/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.scheme.numsys;

import com.cloudway.fp.data.Rational;

public class Int16 extends Num {
    public static final Field<Int16> TAG = new Tag();

    public final short value;

    public Int16(short value) {
        this.value = value;
    }

    @Override
    public Field<Int16> tag() {
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
    public double toReal() {
        return value;
    }

    @Override
    public Num negate() {
        if (value == Short.MIN_VALUE) {
            return new Int32(Short.MAX_VALUE + 1);
        } else {
            return new Int16((short)-value);
        }
    }

    @Override
    public String show() {
        return Short.toString(value);
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
        return Short.hashCode(value);
    }

    public String toString() {
        return "#Int16(" + value + ")";
    }

    private static class Tag extends Field<Int16> {
        @Override
        public int compare(Int16 x, Int16 y) {
            return Short.compare(x.value, y.value);
        }

        @Override
        public Num add(Int16 x, Int16 y) {
            int r = x.value + y.value;
            if ((short)r == r) {
                return new Int16((short)r);
            } else {
                return new Int32(r);
            }
        }

        @Override
        public Num sub(Int16 x, Int16 y) {
            int r = x.value - y.value;
            if ((short)r == r) {
                return new Int16((short)r);
            } else {
                return new Int32(r);
            }
        }

        @Override
        public Num mul(Int16 x, Int16 y) {
            int r = x.value * y.value;
            if ((short)r == r) {
                return new Int16((short)r);
            } else {
                return new Int32(r);
            }
        }

        @Override
        public Num div(Int16 x, Int16 y) {
            if (y.value == 1) {
                return x;
            } else {
                return new Ratio(Rational.valueOf(x.value, y.value)).lower();
            }
        }

        @Override
        public Num quotient(Int16 x, Int16 y) {
            return new Int16((short)(x.value / y.value));
        }

        @Override
        public Num modulo(Int16 x, Int16 y) {
            int m = y.value;
            int r = x.value % m;
            if (r > 0 && m < 0 || r < 0 && m > 0)
                r += m;
            return new Int32(r);
        }

        @Override
        public Num remainder(Int16 x, Int16 y) {
            return new Int16((short)(x.value % y.value));
        }
    }
}
