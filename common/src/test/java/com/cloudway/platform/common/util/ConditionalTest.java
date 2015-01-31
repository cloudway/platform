/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util;

import org.junit.Test;
import static org.junit.Assert.*;

import java.io.IOException;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;
import com.google.common.collect.ImmutableMap;

import static com.cloudway.platform.common.util.Conditionals.*;
import static com.cloudway.platform.common.util.Optionals.*;
import static com.cloudway.platform.common.util.Predicates.*;

public class ConditionalTest
{
    private static class Cookie {
        String value = "no";

        void check() {
            check("yes");
        }

        void check(String expect) {
            assertEquals(expect, value);
            value = "no";
        }
    }

    private static ExceptionAction<RuntimeException> noop(Cookie cookie) {
        return () -> cookie.value = "yes";
    }

    private static ExceptionAction<RuntimeException> trueBranchFailed() {
        return () -> fail("true branch executed");
    }

    private static ExceptionAction<RuntimeException> falseBranchFailed() {
        return () -> fail("false branch executed");
    }

    private static ExceptionAction<RuntimeException> otherwiseBranchFailed() {
        return () -> fail("otherwise branch executed");
    }

    private static void throwing() throws IOException {
        throw new IOException();
    }

    private static void nothrow() throws IOException {
    }

    @Test
    public void testBooleanAction1() {
        Cookie cookie = new Cookie();

        with().when(true, noop(cookie))
              .when(false, falseBranchFailed());
        cookie.check();

        with().when(false, falseBranchFailed())
              .when(true, noop(cookie));
        cookie.check();

        with().when(true, noop(cookie))
              .when(true, trueBranchFailed());
        cookie.check();

        with().when(false, noop(cookie))
              .when(false, falseBranchFailed());
        cookie.check("no");

        with().when(true, noop(cookie))
              .otherwise(otherwiseBranchFailed());
        cookie.check();

        with().when(false, falseBranchFailed())
              .otherwise(noop(cookie));
        cookie.check();

        with().otherwise(noop(cookie));
        cookie.check();
    }

    @Test
    public void testBooleanAction2() {
        Cookie cookie = new Cookie();

        with().when(() -> true, noop(cookie))
              .when(() -> false, falseBranchFailed());
        cookie.check();

        with().when(() -> false, falseBranchFailed())
              .when(() -> true, noop(cookie));
        cookie.check();

        with().when(() -> true, noop(cookie))
              .when(() -> true, trueBranchFailed());
        cookie.check();

        with().when(() -> false, noop(cookie))
              .when(() -> false, falseBranchFailed());
        cookie.check("no");

        with().when(() -> true, noop(cookie))
               .otherwise(otherwiseBranchFailed());
        cookie.check();

        with().when(() -> false, falseBranchFailed())
              .otherwise(noop(cookie));
        cookie.check();
    }

    @Test
    public void testOrElseThrowAction1() {
        Cookie cookie = new Cookie();
        with().when(true, noop(cookie)).orElseThrow(IllegalStateException::new);
        cookie.check();
    }

    @Test(expected = IllegalStateException.class)
    public void testOrElseThrowAction2() {
        with().when(false, falseBranchFailed()).orElseThrow(IllegalStateException::new);
    }

    @Test(expected = IllegalStateException.class)
    public void testOrElseThrowAction3() {
        with().orElseThrow(IllegalStateException::new);
    }

    @Test(expected = IOException.class)
    public void testThrowingAction1() throws IOException {
        with().<IOException>throwing().when(true, () -> throwing());
    }

    @Test
    public void testThrowingAction2() throws IOException {
        with().<IOException>throwing().when(false, () -> throwing());
    }

    @Test(expected = IOException.class)
    public void testThrowingAction3() throws IOException {
        with().<IOException>throwing().when(true, () -> throwing()).when(false, () -> throwing());
    }

    @Test(expected = IOException.class)
    public void testThrowingAction4() throws IOException {
        with().<IOException>throwing().when(false, () -> throwing()).when(true, () -> throwing());
    }

    @Test
    public void testThrowingAction5() throws IOException {
        with().<IOException>throwing().when(true, () -> nothrow()).otherwise(() -> throwing());
    }

    @Test(expected = IOException.class)
    public void testThrowingAction6() throws IOException {
        with().<IOException>throwing().when(false, () -> throwing()).otherwise(() -> throwing());
    }

    // ----------------------------------------------------------------------

    static class Tuple<T> {
        final T x, y;

        Tuple(T x, T y) {
            this.x = x;
            this.y = y;
        }

        static <T> Tuple<T> of(T x, T y) {
            return new Tuple<>(x, y);
        }
    }

    static <T, R, X extends Throwable> ConditionCase<Tuple<T>, R, X>
    Tuple(BiFunction<? super T, ? super T, ? extends R> mapper) {
        return t -> () -> mapper.apply(t.x, t.y);
    }

    @Test
    public void matchOptionalsTest() {
        assertEquals("Just hello", matchOptionals(Optional.of(Optional.of("hello"))));
        assertEquals("Nothing", matchOptionals(Optional.of(Optional.empty())));
        assertEquals("Empty", matchOptionals(Optional.empty()));
    }

    private static String matchOptionals(Optional<Optional<String>> thing) {
        return with(thing).<String>get()
            .when(JustIn(Just(x -> "Just " + x)))
            .when(JustIn(Nothing(() -> "Nothing")))
            .when(Nothing(() -> "Empty"))
            .get();
    }

    @Test
    public void matchOptionalTupleTest() {
        assertEquals("xy", matchOptionalTuple(Optional.of(Tuple.of("x", "y"))));
        assertEquals("Nothing", matchOptionalTuple(Optional.empty()));
    }

    private static String matchOptionalTuple(Optional<Tuple<String>> thing) {
        return with(thing).<String>get()
            .when(JustIn(Tuple((x, y) -> x + y)))
            .orElse("Nothing");
    }

    @Test
    public void optionalActionTest() {
        assertEquals("Just hello", optionalAction(Optional.of("hello")));
        assertEquals("Nothing", optionalAction(Optional.empty()));
        assertEquals("Just hello", optionalDoing(Optional.of("hello")));
        assertEquals("Nothing", optionalDoing(Optional.empty()));
    }

    private static String optionalAction(Optional<String> thing) {
        StringBuilder buf = new StringBuilder();
        with(thing)
            .when(Just(x -> buf.append("Just ").append(x)))
            .when(Nothing(() -> buf.append("Nothing")));
        return buf.toString();
    }

    private static String optionalDoing(Optional<String> thing) {
        StringBuilder buf = new StringBuilder();
        with(thing)
            .when(Just(doing(x -> buf.append("Just ").append(x))))
            .when(Nothing(doing(() -> buf.append("Nothing"))));
        return buf.toString();
    }

    // ---------------------------------------------------------------------

    @FunctionalInterface
    interface TriFunction<T, U, V, R> {
        R apply(T t, U u, V v);
    }

    interface Expr {}

    static class Const implements Expr {
        final int val;

        Const(int val) {
            this.val = val;
        }

        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (!(obj instanceof Const))
                return false;
            Const other = (Const)obj;
            return val == other.val;
        }

        public int hashCode() {
            return 31 + val;
        }

        public String toString() {
            return "Const(" + val + ")";
        }
    }

    static class Var implements Expr {
        final String name;

        Var(String name) {
            this.name = name;
        }

        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (!(obj instanceof Var))
                return false;
            Var other = (Var)obj;
            return name.equals(other.name);
        }

        public int hashCode() {
            return name.hashCode();
        }

        public String toString() {
            return "Var(" + name + ")";
        }
    }

    static class BinOp implements Expr {
        final String opname;
        final Expr left;
        final Expr right;

        public BinOp(String opname, Expr left, Expr right) {
            this.opname = opname;
            this.left = left;
            this.right = right;
        }

        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (!(obj instanceof BinOp))
                return false;
            BinOp other = (BinOp)obj;
            return opname.equals(other.opname)
                && left.equals(other.left)
                && right.equals(other.right);
        }

        public int hashCode() {
            return Objects.hash(opname, left, right);
        }

        public String toString() {
            return "BinOp(" + opname + ", " + left + ", " + right + ")";
        }
    }

    static Expr cst(int val) { return new Const(val); }
    static Expr var(String name) { return new Var(name); }
    static Expr op(String op, Expr l, Expr r) { return new BinOp(op, l, r); }

    static <R> ConditionCase<Expr, R, RuntimeException>
    BinOp(TriFunction<String, Expr, Expr, ? extends R> mapper) {
        return e -> {
            if (e instanceof BinOp) {
                BinOp b = (BinOp)e;
                return () -> mapper.apply(b.opname, b.left, b.right);
            }
            return null;
        };
    }

    static <R> ConditionCase<Expr, R, RuntimeException>
    BinOp(Function<Expr, Expr> operator, String opname, BiFunction<Expr, Expr, ? extends R> mapper) {
        return e -> ((e instanceof BinOp) && opname.equals(((BinOp)e).opname))
            ? () -> mapper.apply(operator.apply(((BinOp)e).left), operator.apply(((BinOp)e).right))
            : null;
    }

    static <R> ConditionCase<Expr, R, RuntimeException>
    BinOp(String opname, BiFunction<Expr, Expr,? extends R> mapper) {
        return BinOp(Function.identity(), opname, mapper);
    }

    static <R> ConditionCase<Expr, R, RuntimeException>
    Const(Function<Integer, ? extends R> mapper) {
        return e -> (e instanceof Const)
            ? () -> mapper.apply(((Const)e).val)
            : null;
    }

    static Predicate<Expr> Const(int val) {
        return e -> (e instanceof Const) && (((Const)e).val == val);
    }

    static <R> ConditionCase<Expr, R, RuntimeException>
    Var(Function<String, ? extends R> mapper) {
        return e -> (e instanceof Var)
            ? () -> mapper.apply(((Var)e).name)
            : null;
    }

    static Expr simplify(Expr expr) {
        return with(expr).<Expr>get()
            .when(BinOp(ConditionalTest::simplify, "+", (l, r) ->
              with().<Expr>get()
                .when(l, is(Const(0)), r)
                .when(r, is(Const(0)), l)
                .orElseGet(() -> {
                    if (l instanceof Const && r instanceof Const)
                        return new Const(((Const)l).val + ((Const)r).val);
                    return new BinOp("+", l, r);
                })
            ))
            .when(BinOp(ConditionalTest::simplify, "*", (l, r) ->
              with().<Expr>get()
                .when(l, is(Const(0)), l)
                .when(r, is(Const(0)), r)
                .when(l, is(Const(1)), r)
                .when(r, is(Const(1)), l)
                .orElseGet(() -> {
                    if (l instanceof Const && r instanceof Const)
                        return new Const(((Const)l).val * ((Const)r).val);
                    return new BinOp("*", l, r);
                })
            ))
            .orElse(expr);
    }

    static String format(Expr expr) {
        return with(expr).<String>get()
            .when(BinOp((op, l, r) -> "(" + format(l) + op + format(r) + ")"))
            .when(Const(String::valueOf))
            .when(Var(Function.identity()))
            .get();
    }

    static int evaluate(Expr expr, Map<String,Integer> bindings) {
        return with(expr).<Integer>get()
            .when(Const(Function.identity()))
            .when(Var  (x -> bindings.getOrDefault(x, 0)))
            .when(BinOp("+", (l, r) -> evaluate(l, bindings) + evaluate(r, bindings)))
            .when(BinOp("*", (l, r) -> evaluate(l, bindings) * evaluate(r, bindings)))
            .get();
    }

    @Test
    public void simplifyTest() {
        Expr expr;

        expr = op("+", cst(5), cst(0));
        assertEquals(cst(5), simplify(expr));

        expr = op("*", cst(1), var("x"));
        assertEquals(var("x"), simplify(expr));

        expr = op("*", var("x"), cst(0));
        assertEquals(cst(0), simplify(expr));

        expr = op("+", op("*", cst(3), cst(2)), cst(5));
        assertEquals(cst(11), simplify(expr));

        expr = op("+", op("*", var("x"), cst(2)), cst(5));
        assertEquals(expr, simplify(expr));

        expr = op("+", var("x"), op("*", cst(3), cst(2)));
        assertEquals(op("+", var("x"), cst(6)), simplify(expr));

        expr = op("*", cst(0), op("+", var("x"), cst(2)));
        assertEquals(cst(0), simplify(expr));
    }

    @Test
    public void formatTest() {
        Expr expr = op("*", op("+", var("x"), var("y")), op("+", var("z"), cst(3)));
        assertEquals("((x+y)*(z+3))", format(expr));
    }

    @Test
    public void evaluateTest() {
        Expr expr = op("*", op("+", var("x"), var("y")), op("+", var("z"), cst(3)));
        ImmutableMap<String,Integer> bindings = ImmutableMap.of("x", 2, "y", 7, "z", 8);
        assertEquals((2+7)*(8+3), evaluate(expr, bindings));
    }
}
