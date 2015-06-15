/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.scheme.numsys;

import java.util.function.Function;

import com.cloudway.fp.function.TriFunction;
import com.cloudway.fp.scheme.LispError;
import com.google.common.collect.HashBasedTable;
import com.google.common.collect.Table;

public abstract class Field<N extends Num> {
    private static final Table<Field<?>, Field<?>, Function<Num, Num>>
        tower = HashBasedTable.create();

    /**
     * Install a number converter function.
     */
    @SuppressWarnings("unchecked")
    public static <A extends Num, B extends Num>
    void install(Field<A> from, Field<B> to, Function<A, B> cvt) {
        tower.put(from, to, (Function)cvt);

        // given:
        //   A B A->B
        //   B C B->C
        // implicitly add:
        //   A C (compose B->C A->B)

        tower.column(from).forEach((a, f) -> {
            if (!tower.contains(a, to)) {
                tower.put(a, to, cvt.compose((Function)f));
            }
        });
    }

    /**
     * Raise the given number to another number system.
     */
    @SuppressWarnings("unchecked")
    public static <N extends Num> N raise(Num x, Field<N> b) {
        if (x.tag() == b) {
            return (N)x;
        } else {
            Function<Num, Num> cvt = tower.get(x.tag(), b);
            return cvt != null ? (N)cvt.apply(x) : null;
        }
    }

    public static <N extends Num> N raise_e(Num x, Field<N> b) {
        N n = raise(x, b);
        if (n == null)
            throw new LispError("conversion error");
        return n;
    }

    public static boolean implies(Num x, Field<?> b) {
        return x.tag() == b || tower.contains(x.tag(), b);
    }

    /**
     * Compute a generic operation.
     */
    @SuppressWarnings("unchecked")
    public static <A> A op(TriFunction<Field<Num>, Num, Num, A> op, Num x, Num y) {
        Field<Num> tx = (Field<Num>)x.tag();
        Field<Num> ty = (Field<Num>)y.tag();

        if (tx == ty)
            return op.apply(tx, x, y);

        Num x1 = raise(x, ty);
        if (x1 != null)
            return op.apply(ty, x1, y);

        Num y1 = raise(y, tx);
        if (y1 != null)
            return op.apply(tx, x, y1);

        throw new LispError("conversion error");
    }

    public abstract int compare(N x, N y);
    public abstract Num add(N x, N y);
    public abstract Num sub(N x, N y);
    public abstract Num mul(N x, N y);
    public abstract Num div(N x, N y);
    public abstract Num quotient(N x, N y);
    public abstract Num modulo(N x, N y);
    public abstract Num remainder(N x, N y);

    public Num pow(N x, int n) {
        BigInt a = raise_e(x, BigInt.TAG);
        return new BigInt(a.value.pow(n)).lower();
    }

    @SuppressWarnings({"rawtypes", "unchecked"})
    public static Num expt(Num x, Num y) {
        if (y.isExact()) {
            Num n = y.lower();
            if (n instanceof Int32) {
                Field xt = x.tag();
                return xt.pow(x, ((Int32)n).value);
            }
        }

        if (x instanceof Complex || y instanceof Complex) {
            Complex a = raise_e(x, Complex.TAG);
            Complex b = raise_e(y, Complex.TAG);
            return a.expt(b);
        } else {
            return new Real(Math.pow(x.toReal(), y.toReal()));
        }
    }
}
