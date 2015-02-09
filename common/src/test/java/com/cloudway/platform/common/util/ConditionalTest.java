/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util;

import org.junit.Test;
import static org.junit.Assert.*;

import java.io.IOException;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Random;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import com.google.common.collect.ImmutableMap;

import com.cloudway.platform.common.util.function.TriFunction;
import com.cloudway.platform.common.util.function.ExceptionAction;
import com.cloudway.platform.common.util.function.ExceptionBiFunction;
import com.cloudway.platform.common.util.function.ExceptionFunction;
import com.cloudway.platform.common.util.function.ExceptionSupplier;

import static com.cloudway.platform.common.util.Conditionals.*;
import static com.cloudway.platform.common.util.ListComprehension.*;
import static com.cloudway.platform.common.util.Optionals.*;
import static com.cloudway.platform.common.util.Tuple.Tuple;
import static com.cloudway.platform.common.util.Seq.Seq;
import static java.util.stream.Collectors.*;

// @formatter:off
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

    @SuppressWarnings("RedundantThrowsDeclaration")
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

    @Test
    public void matchOptionalTest() {
        assertEquals("Just hello", matchOptional(Optional.of(Optional.of("hello"))));
        assertEquals("Nothing", matchOptional(Optional.of(Optional.empty())));
        assertEquals("Empty", matchOptional(Optional.empty()));
    }

    private static String matchOptional(Optional<Optional<String>> thing) {
        return with(thing).<String>get()
            .when(in(Just(Just(x -> "Just " + x))))
            .when(in(Just(Nothing(() -> "Nothing"))))
            .when(Nothing(() -> "Empty"))
            .get();
    }

    @Test
    public void matchOptionalPairTest() {
        Optional<String> a = Optional.of("hello");
        Optional<String> b = Optional.of("world");
        Optional<String> empty = Optional.empty();

        assertEquals("hello, world", matchOptionalPair(a, b));
        assertEquals("hello", matchOptionalPair(a, empty));
        assertEquals("world", matchOptionalPair(empty, b));
        assertEquals("", matchOptionalPair(empty, empty));

        assertEquals("hello, world", matchAnyPair(a, b));
        assertEquals("hello", matchAnyPair(a, empty));
        assertEquals("world", matchAnyPair(empty, b));
        assertEquals("", matchAnyPair(empty, empty));
    }

    private static String matchOptionalPair(Optional<String> a, Optional<String> b) {
        return with(a, b).<String>get()
            .when(Just(x -> Just(y -> x + ", " + y)))
            .when(Just(x -> Nothing(() -> x)))
            .when(Nothing(() -> Just(y -> y)))
            .when(Nothing(() -> Nothing(() -> "")))
            .get();
    }

    private static String matchAnyPair(Optional<String> a, Optional<String> b) {
        return with(a, b).<String>get()
            .when(Just(x -> Just(y -> x + ", " + y)))
            .when(Just(x -> Any(() -> x)))
            .when(Any(() -> Just(y -> y)))
            .when(Any(() -> Any(() -> "")))
            .get();
    }

    @Test
    public void matchOptionalTupleTest() {
        assertEquals("xy", matchOptionalTuple(Optional.of(Tuple.of("x", "y"))));
        assertEquals("Nothing", matchOptionalTuple(Optional.empty()));
    }

    private static String matchOptionalTuple(Optional<Tuple<String,String>> thing) {
        return with(thing).<String>get()
            .when(in(Just(Tuple((x, y) -> x + y))))
            .orElse("Nothing");
    }

    @Test
    public void matchTuplePairTest() {
        Random rnd = new Random();
        int a = rnd.nextInt(100);
        int b = rnd.nextInt(100);
        int c = rnd.nextInt(100);
        int d = rnd.nextInt(100);

        int res = matchTuplePair(Tuple.of(a, b), Tuple.of(c, d));
        assertEquals((a+b) * (c+d), res);
    }

    private static int matchTuplePair(Tuple<Integer,Integer> x, Tuple<Integer,Integer> y) {
        return with(x, y).<Integer>get()
            .when(Tuple((a, b) -> Tuple((c, d) -> (a+b) * (c+d))))
            .get();
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

    @Test
    public void comparingTupleTest1() {
        Comparator<Tuple<Integer, Integer>> c1 = Tuple.comparator();
        Comparator<Tuple<Integer, Integer>> c2 = Tuple.comparator(Comparator.comparingInt(i -> i));

        Tuple<Integer,Integer> t1 = Tuple.of(0, 1);
        Tuple<Integer,Integer> t2 = Tuple.of(0, 1);
        Tuple<Integer,Integer> t3 = Tuple.of(1, 1);
        Tuple<Integer,Integer> t4 = Tuple.of(0, 2);

        assertTrue(c1.compare(t1, t2) == 0);
        assertTrue(c1.compare(t1, t3) < 0);
        assertTrue(c1.compare(t3, t1) > 0);
        assertTrue(c1.compare(t1, t4) < 0);
        assertTrue(c1.compare(t4, t1) > 0);
        assertTrue(c1.compare(t3, t4) > 0);
        assertTrue(c1.compare(t4, t3) < 0);

        assertTrue(c2.compare(t1, t2) == 0);
        assertTrue(c2.compare(t1, t3) < 0);
        assertTrue(c2.compare(t3, t1) > 0);
        assertTrue(c2.compare(t1, t4) < 0);
        assertTrue(c2.compare(t4, t1) > 0);
        assertTrue(c2.compare(t3, t4) > 0);
        assertTrue(c2.compare(t4, t3) < 0);
    }

    @Test
    public void comparingTupleTest2() {
        Comparator<Tuple<Integer, String>> c1 = Tuple.comparator();
        Comparator<Tuple<Integer, String>> c2 = Tuple.comparator(
            Comparator.<Integer>naturalOrder(), Comparator.<String>naturalOrder());

        Tuple<Integer, String> t1 = Tuple.of(0, "a");
        Tuple<Integer, String> t2 = Tuple.of(0, "a");
        Tuple<Integer, String> t3 = Tuple.of(1, "a");
        Tuple<Integer, String> t4 = Tuple.of(0, "b");

        assertTrue(c1.compare(t1, t2) == 0);
        assertTrue(c1.compare(t1, t3) < 0);
        assertTrue(c1.compare(t3, t1) > 0);
        assertTrue(c1.compare(t1, t4) < 0);
        assertTrue(c1.compare(t4, t1) > 0);
        assertTrue(c1.compare(t3, t4) > 0);
        assertTrue(c1.compare(t4, t3) < 0);

        assertTrue(c2.compare(t1, t2) == 0);
        assertTrue(c2.compare(t1, t3) < 0);
        assertTrue(c2.compare(t3, t1) > 0);
        assertTrue(c2.compare(t1, t4) < 0);
        assertTrue(c2.compare(t4, t1) > 0);
        assertTrue(c2.compare(t3, t4) > 0);
        assertTrue(c2.compare(t4, t3) < 0);
    }


    // ----------------------------------------------------------------------

    @Test
    public void listComprehensionPatternMatchingOptionals() {
        @SuppressWarnings("unchecked") Optional<String>[] source = new Optional[] {
            Optional.of("hello"), Optional.empty(), Optional.of("world"), Optional.of("")
        };
        String result;

        result = From(Stream.of(source), With(Just(x -> Where(!x.isEmpty(), Yield(x))))).build().collect(joining(", "));
        assertEquals("hello, world", result);

        result = From(Seq.of(source), With(Just(x -> Where(!x.isEmpty(), Yield(x))))).build().show(", ", "", "");
        assertEquals("hello, world", result);

        result = Stream.of(source).filter(with(Just(x -> !x.isEmpty()))).map(Optional::get).collect(joining(", "));
        assertEquals("hello, world", result);
    }

    @Test
    public void listComprehensionPatternMatchingTuples() {
        @SuppressWarnings("unchecked") Tuple<String, Integer>[] source = new Tuple[] {
            Tuple.of("a", 1), Tuple.of("b", 2), Tuple.of("c", 3)
        };
        String result;

        result = From(Stream.of(source), With(Tuple((a, b) -> Yield(a + ":" + b)))).build().collect(joining(","));
        assertEquals("a:1,b:2,c:3", result);

        result = From(Seq.of(source), With(Tuple((a, b) -> Yield(a + ":" + b)))).build().show(",", "", "");
        assertEquals("a:1,b:2,c:3", result);
    }

    @Test
    public void listComprehensionPatternMatchingOptionalTuples() {
        @SuppressWarnings("unchecked") Optional<Tuple<String,Integer>>[] source = new Optional[] {
            Optional.of(Tuple.of("a", 1)), Optional.empty(), Optional.of(Tuple.of("c", 3))
        };
        String result;

        result = From(Stream.of(source), With(in(Just(Tuple((a,b) -> Yield(a + ":" + b)))))).build().collect(joining(","));
        assertEquals("a:1,c:3", result);

        result = From(Seq.of(source), With(in(Just(Tuple((a,b) -> Yield(a + ":" + b)))))).build().show(",", "", "");
        assertEquals("a:1,c:3", result);
    }

    // ---------------------------------------------------------------------

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
            return val;
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
            .when(BinOp(ConditionalTest::simplify, "+", (left, right) ->
                with(left, right).<Expr>get()
                    .when(Const(0), _, () -> right)
                    .when(_, Const(0), () -> left)
                    .when(Const(l -> Const(r -> new Const(l + r))))
                    .orElseGet(() -> new BinOp("+", left, right))
            ))
            .when(BinOp(ConditionalTest::simplify, "*", (left, right) ->
                with(left, right).<Expr>get()
                    .when(Const(0), _, () -> new Const(0))
                    .when(_, Const(0), () -> new Const(0))
                    .when(Const(1), _, () -> right)
                    .when(_, Const(1), () -> left)
                    .when(Const(l -> Const(r -> new Const(l * r))))
                    .orElseGet(() -> new BinOp("*", left, right))
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

        expr = op("+", cst(0), cst(5));
        assertEquals(cst(5), simplify(expr));

        expr = op("+", var("x"), cst(0));
        assertEquals(var("x"), simplify(expr));

        expr = op("+", cst(0), var("x"));
        assertEquals(var("x"), simplify(expr));

        expr = op("*", cst(5), cst(1));
        assertEquals(cst(5), simplify(expr));

        expr = op("*", cst(1), cst(5));
        assertEquals(cst(5), simplify(expr));

        expr = op("*", var("x"), cst(1));
        assertEquals(var("x"), simplify(expr));

        expr = op("*", cst(1), var("x"));
        assertEquals(var("x"), simplify(expr));

        expr = op("*", cst(5), cst(0));
        assertEquals(cst(0), simplify(expr));

        expr = op("*", cst(0), cst(5));
        assertEquals(cst(0), simplify(expr));

        expr = op("*", var("x"), cst(0));
        assertEquals(cst(0), simplify(expr));

        expr = op("*", cst(0), var("x"));
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
        assertEquals((2 + 7) * (8 + 3), evaluate(expr, bindings));
    }

    // ---------------------------------------------------------------------

    static class Temperature extends Number implements Comparable<Temperature> {
        private static final long serialVersionUID = -3153088642370013706L;

        @FunctionalInterface
        private interface Conv {
            Temperature convert(Number value);
        }

        private final String unit;
        private final Conv conv;
        private final double value;

        private Temperature(String unit, Conv conv, double value) {
            this.unit = unit;
            this.conv = conv;
            this.value = value;
        }

        public String getUnit() {
            return unit;
        }

        public double getValue() {
            return value;
        }

        private double convert(Number other) {
            return conv.convert(other).value;
        }

        public Temperature plus(Number other) {
            return new Temperature(unit, conv, value + convert(other));
        }

        public Temperature minus(Number other) {
            return new Temperature(unit, conv, value - convert(other));
        }

        public static Temperature C(Number t) {
            return with(t).<Temperature>get()
                .when(Temperature("C", x -> (Temperature)t))
                .when(Temperature("F", x -> new Temperature("C", Temperature::C, (5 * x - 160) / 9)))
                .when(Number.class,    x -> new Temperature("C", Temperature::C, x.doubleValue()))
                .get();
        }

        public static Temperature F(Number t) {
            return with(t).<Temperature>get()
                .when(Temperature("F", x -> (Temperature)t))
                .when(Temperature("C", x -> new Temperature("F", Temperature::F, 9 * x / 5 + 32)))
                .when(Number.class, x -> new Temperature("F", Temperature::F, x.doubleValue()))
                .get();
        }

        public Temperature toC() {
            return C(this);
        }

        public Temperature toF() {
            return F(this);
        }

        @SuppressWarnings("MethodNameSameAsClassName")
        public static <T, R, X extends Throwable> ConditionCase<T, R, X>
        Temperature(String unit, ExceptionFunction<Double, ? extends R, X> mapper) {
            return cast(Temperature.class, t -> unit.equals(t.unit)
                ? (ExceptionSupplier<R,X>)() -> mapper.evaluate(t.value)
                : null);
        }

        @Override
        public int compareTo(Temperature other) {
            return Double.compare(value, convert(other));
        }

        @Override
        public boolean equals(Object obj) {
            if (obj == this)
                return true;
            if (!(obj instanceof Number))
                return false;
            return Double.compare(value, convert((Number)obj)) == 0;
        }

        @Override
        public int hashCode() {
            return Objects.hash(unit, value);
        }

        @Override
        public String toString() {
            return value + "." + unit;
        }

        @Override
        public int intValue() {
            return (int)value;
        }

        @Override
        public long longValue() {
            return (long)value;
        }

        @Override
        public float floatValue() {
            return (float)value;
        }

        @Override
        public double doubleValue() {
            return value;
        }
    }

    @Test
    public void temperatureTest() {
        Temperature c = Temperature.C(25);
        Temperature f = Temperature.F(212);

        assertEquals(Temperature.F(77), c.toF());
        assertEquals(Temperature.C(100), f.toC());
        assertEquals(Temperature.C(125), c.plus(f));
        assertEquals(Temperature.F(135), f.minus(c));

        assertTrue(c.compareTo(f) < 0);
        assertTrue(f.compareTo(c) > 0);
    }

    // ---------------------------------------------------------------------

    static class Complex {
        private final double real;
        private final double imag;

        public Complex(double real, double imag) {
            this.real = real;
            this.imag = imag;
        }

        @SuppressWarnings("MethodNameSameAsClassName")
        public static <R, X extends Throwable> ConditionCase<Complex, ? extends R, X>
        Complex(ExceptionBiFunction<Double, Double, ? extends R, X> mapper) {
            return t -> () -> mapper.evaluate(t.real, t.imag);
        }

        public Complex plus(Complex that) {
            return with(this, that).<Complex>get()
                .when(Complex((a, b) -> Complex((c, d) -> new Complex(a + c, b + d))))
                .get();
        }

        public Complex minus(Complex that) {
            return with(this, that).<Complex>get()
                .when(Complex((a, b) -> Complex((c, d) -> new Complex(a - c, b - d))))
                .get();
        }

        public Complex multiply(Complex that) {
            return with(this, that).<Complex>get()
                .when(Complex((a, b) -> Complex((c, d) -> new Complex(a*c - b*d, b*c + a*d))))
                .get();
        }

        public Complex divide(Complex that) {
            return with(this, that).<Complex>get()
                .when(Complex((a, b) -> Complex((c, d) -> {
                    double t = c*c + d*d;
                    return new Complex((a*c + b*d)/t, (b*c - a*d)/t);
                })))
                .get();
        }

        public boolean equals(Object obj) {
            if (obj == this)
                return true;
            if (!(obj instanceof Complex))
                return false;

            return with(this, (Complex)obj).<Boolean>get()
                .when(Complex((a, b) -> Complex((c, d) ->
                    Double.compare(a, c) == 0 && Double.compare(b, d) == 0)))
                .get();
        }

        public int hashCode() {
            return 31 * (31 + Double.hashCode(real) + Double.hashCode(imag));
        }

        public String toString() {
            return with(real, imag).<String>get()
                .when(_, 0.0, () -> String.valueOf(real))
                .when(0.0, _, () -> imag + "i")
                .orElseGet(() -> real + (imag > 0 ? "+" : "") + imag + "i");
        }
    }

    @Test
    public void complexTest() {
        double a = Math.random();
        double b = Math.random();
        double c = Math.random();
        double d = Math.random();
        double t = c*c + d*d;

        Complex x = new Complex(a, b);
        Complex y = new Complex(c, d);

        assertEquals(new Complex(a+c, b+d), x.plus(y));
        assertEquals(new Complex(a-c, b-d), x.minus(y));
        assertEquals(new Complex(a*c-b*d, b*c+a*d), x.multiply(y));
        assertEquals(new Complex((a*c + b*d)/t, (b*c - a*d)/t), x.divide(y));

        assertEquals("1.0+2.0i", new Complex(1, 2).toString());
        assertEquals("1.0-2.0i", new Complex(1, -2).toString());
        assertEquals("1.0", new Complex(1, 0).toString());
        assertEquals("-2.0i", new Complex(0, -2).toString());
    }

    // ---------------------------------------------------------------------

    static class TreeSet<T extends Comparable<T>> {
        private enum Color { R, B }
        private static final Color R = Color.R;
        private static final Color B = Color.B;

        // Algebra data type

        private interface Tree<T> {
            Color color();
        }

        private static class Empty<T> implements Tree<T> {
            @Override
            public Color color() {
                return B;
            }

            public String toString() {
                return "()";
            }
        }

        private static class Node<T> implements Tree<T> {
            final Color color;
            final T value;
            final Tree<T> left, right;

            Node(Color color, Tree<T> left, T value, Tree<T> right) {
                this.color = color;
                this.value = value;
                this.left = left;
                this.right = right;
            }

            @Override
            public Color color() {
                return color;
            }

            public String toString() {
                return "(" + value + " " + left + " " + right + ")";
            }
        }

        // Constructors

        @SuppressWarnings("rawtypes")
        private static final Empty EMPTY = new Empty();

        @SuppressWarnings("unchecked")
        private static <T> Tree<T> Empty() {
            return (Tree<T>)EMPTY;
        }

        private static <T> Tree<T> Node(Color color, Tree<T> left, T value, Tree<T> right) {
            return new Node<>(color, left, value, right);
        }

        // Deconstructions

        static <T, R, X extends Throwable> ConditionCase<Tree<T>, R, X>
        Empty(ExceptionSupplier<R, X> action) {
            return t -> (t instanceof Empty) ? action : null;
        }

        static <T, R, X extends Throwable> ConditionCase<Tree<T>, R, X>
        Node(Function<Color, TriFunction<? super Tree<T>, ? super T, ? super Tree<T>, ? extends R>> mapper) {
            return t -> {
                if (t instanceof Node) {
                    @SuppressWarnings("unchecked") Node<T> n = (Node)t;
                    return () -> mapper.apply(n.color).apply(n.left, n.value, n.right);
                }
                return null;
            };
        }

        static <T, R, X extends Throwable> ConditionCase<Tree<T>, R, X>
        Node(TriFunction<? super Tree<T>, ? super T, ? super Tree<T>, ? extends R> mapper) {
            return t -> {
                if (t instanceof Node) {
                    @SuppressWarnings("unchecked") Node<T> n = (Node)t;
                    return () -> mapper.apply(n.left, n.value, n.right);
                }
                return null;
            };
        }

        static <T, R, X extends Throwable> ConditionCase<Tree<T>, R, X>
        Node(Color color, TriFunction<? super Tree<T>, ? super T, ? super Tree<T>, ? extends R> mapper) {
            return t -> {
                if (t instanceof Node && color == ((Node)t).color) {
                    @SuppressWarnings("unchecked") Node<T> n = (Node)t;
                    return () -> mapper.apply(n.left, n.value, n.right);
                }
                return null;
            };
        }

        static <T, R, X extends Throwable> ConditionCase<Tree<T>, R, X>
        Node(Color color, ConditionCase<Tree<T>, BiFunction<? super T, Tree<T>, ? extends R>, X> mapper) {
            return t -> {
                if (t instanceof Node && color == ((Node)t).color) {
                    @SuppressWarnings("unchecked") Node<T> n = (Node)t;
                    ExceptionSupplier<BiFunction<? super T, Tree<T>, ? extends R>, X> sup = mapper.evaluate(n.left);
                    return sup != null ? () -> sup.produce().apply(n.value, n.right) : null;
                }
                return null;
            };
        }

        static <T, R, X extends Throwable> ConditionCase<Tree<T>, R, X>
        Node(Color color, BiFunction<Tree<T>, ? super T, ConditionCase<Tree<T>, R, X>> mapper) {
            return  t -> {
                if (t instanceof Node && color == ((Node)t).color) {
                    @SuppressWarnings("unchecked") Node<T> n = (Node)t;
                    return mapper.apply(n.left, n.value).evaluate(n.right);
                }
                return null;
            };
        }

        // Operations

        private static <T> Tree<T> make_black(Tree<T> t) {
            return when(t, Node(R, (a, x, b) -> Node(B, a, x, b)), otherwise(t));
        }

        private static <T extends Comparable<T>> Tree<T> insert(Tree<T> t, T x) {
            return with(t).<Tree<T>>get()
                .when(Empty(() -> Node(R, t, x, t)))
                .when(Node(c -> (a, y, b) -> {
                    int cmp = x.compareTo(y);
                    return cmp < 0 ? balance(c, insert(a, x), y, b) :
                           cmp > 0 ? balance(c, a, y, insert(b, x))
                                   : t;
                })).get();
        }

        /**
         * The implementation of level-two pattern matching is not supported well,
         * so there are some tedious code in following. See the original code that
         * is "more" readable.
         *
         * <p><pre>
         *     balance :: Color -> (Tree t) -> t -> (Tree t) -> (Tree t)
         *     balance B (Node R (Node R a x b) y c) z d =
         *         Node R (Node B a x b) y (Node B c z d)
         *     balance B (Node R a x (Node R b y c)) z d =
         *         Node R (Node B a x b) y (Node B c z d)
         *     balance B a x (Node R (Node R b y c) z d) =
         *         Node R (Node B a x b) y (Node B c z d))
         *     balance B a x (Node R b y (Node R c z d)) =
         *         Node R (Node B a x b) y (Node B c z d))
         *     balance = Node
         * </pre></p>
         */
        private static <T> Tree<T> balance(Color color, Tree<T> left, T value, Tree<T> right) {
            if (color == B) {
                return with(left, right).<Tree<T>>get()
                    .when(Node(R, Node(R, (a, x, b) -> (y, c) -> Any(d ->
                          Node(R, Node(B, a, x, b), y, Node(B, c, value, d))))))
                    .when(Node(R, (a, x) -> Node(R, (b, y, c) -> Any(d ->
                          Node(R, Node(B, a, x, b), y, Node(B, c, value, d))))))
                    .when(Any(a -> Node(R, Node(R, (b, y, c) -> (z, d) ->
                          Node(R, Node(B, a, value, b), y, Node(B, c, z, d))))))
                    .when(Any(a -> Node(R, (b, y) -> Node(R, (c, z, d) ->
                          Node(R, Node(B, a, value, b), y, Node(B, c, z, d))))))
                    .orElseGet(() -> Node(B, left, value, right));
            } else {
                return Node(color, left, value, right);
            }
        }

        private static <T extends Comparable<T>> boolean member(Tree<T> t, T x) {
            return when(t, Node((a, y, b) -> {
                int cmp = x.compareTo(y);
                return cmp < 0 ? member(a, x) :
                       cmp > 0 ? member(b, x)
                               : true;
            }), otherwise(false));
        }

        private static <T> int card(Tree<T> t) {
            return when(t, Node((a, x, b) -> 1 + card(a) + card(b)), otherwise(0));
        }

        // Traversal

        public enum Traverser {
            PRE_ORDER {
                @Override
                public <T> void walk(Tree<T> tree, Consumer<T> action) {
                    do__(tree, Node((a, x, b) -> {
                        walk(a, action);
                        action.accept(x);
                        walk(b, action);
                        return null;
                    }));
                }
            },

            POST_ORDER {
                @Override
                public <T> void walk(Tree<T> tree, Consumer<T> action) {
                    do__(tree, Node((a, x, b) -> {
                        walk(b, action);
                        action.accept(x);
                        walk(a, action);
                        return null;
                    }));
                }
            },

            BREADTH_FIRST {
                @Override
                public <T> void walk(Tree<T> tree, Consumer<T> action) {
                    walk(Seq.of(tree), action);
                }

                // this is non-pure implementation
                private <T> void walk(Seq<Tree<T>> queue, Consumer<T> action) {
                    while (!queue.isEmpty()) {
                        queue = when(queue, Seq((tree, remaining) ->
                            when(tree, Node((a, x, b) -> {
                                action.accept(x);
                                return remaining.append(Seq.of(a, b));
                            }), otherwise(remaining))));
                    }
                }
            };

            public abstract <T> void walk(Tree<T> tree, Consumer<T> action);
        }

        // TreeSet implementation

        private Tree<T> root = Empty();

        public void add(T x) {
            root = make_black(insert(root, x));
        }

        public boolean contains(T x) {
            return member(root, x);
        }

        public int size() {
            return card(root);
        }

        public void forEach(Traverser traverser, Consumer<T> action) {
            traverser.walk(root, action);
        }

        public void forEach(Consumer<T> action) {
            forEach(Traverser.PRE_ORDER, action);
        }

        public <U> U reduce(Traverser traverser, U seed, BiFunction<U, ? super T, U> acc) {
            class Reducer implements Consumer<T> {
                U result = seed;
                @Override public void accept(T x) {
                    result = acc.apply(result, x);
                }
            };

            Reducer reducer = new Reducer();
            traverser.walk(root, reducer);
            return reducer.result;
        }

        public <U> U reduce(U seed, BiFunction<U, ? super T, U> acc) {
            return reduce(Traverser.PRE_ORDER, seed, acc);
        }

        public <A, R> R collect(Traverser traverser, Collector<T, A, R> collector) {
            A container = collector.supplier().get();
            BiConsumer<A, T> accumulator = collector.accumulator();
            traverser.walk(root, x -> accumulator.accept(container, x));
            return collector.finisher().apply(container);
        }

        public <R> R collect(Collector<T, ?, R> collector) {
            return collect(Traverser.PRE_ORDER, collector);
        }

        public List<T> toList() {
            return collect(Collectors.toList());
        }

        public String toString() {
            return root.toString();
        }

        void check() {
            // Verify the red-black tree.
            //
            // 1. The root node must be black
            // 2. No red node can have a red child
            // 3. Every path from root to an empty leaf node must contain the same
            //    number of black nodes - the black height of the tree.
            assertEquals(B, root.color());
            check(root);
        }

        private int check(Tree<T> node) {
            return when(node, Node(c -> (a, x, b) -> {
                int lht = check(a);
                int rht = check(b);
                assertEquals(lht, rht);
                assertFalse(c == R && (a.color() == R || b.color() == R));
                return c == B ? lht + 1 : lht;
            }), otherwise(0));
        }
    }

    @Test
    public void randomTreeTest() {
        Random rnd = new Random();
        testTree(IntStream.generate(() -> rnd.nextInt(1000)).limit(100).toArray());
    }

    @Test
    public void ascendingSequenceTreeTest() {
        testTree(IntStream.rangeClosed(1, 100).toArray());
    }

    @Test
    public void descendingSequenceTreeTest() {
        testTree(IntStream.rangeClosed(1, 100).map(i -> 101 - i).toArray());
    }

    private static void testTree(int[] source) {
        TreeSet<Integer> set = buildTree(IntStream.of(source));
        assertTrue(IntStream.of(source).allMatch(set::contains));
        assertTrue(isSorted(set.collect(TreeSet.Traverser.PRE_ORDER, toList()), Comparator.naturalOrder()));
        assertTrue(isSorted(set.collect(TreeSet.Traverser.POST_ORDER, toList()), Comparator.reverseOrder()));
        assertEquals(distinct(source).count(), set.size());
        assertEquals(distinct(source).sum(), (int)set.reduce(0, Integer::sum));
        assertEquals(distinct(source).sum(), (int)set.collect(summingInt(i->i)));
        assertEquals(set.toList(), buildTree(reverse(source)).toList());
        assertEquals(set.toList(), buildTree(shuffle(source)).toList());
    }

    private static TreeSet<Integer> buildTree(IntStream stream) {
        TreeSet<Integer> set = new TreeSet<>();
        stream.forEach(x -> { set.add(x); set.check(); });
        return set;
    }

    private static <T extends Comparable<T>> boolean isSorted(List<T> list, Comparator<T> cmp) {
        if (!list.isEmpty()) {
            T x = list.get(0);
            for (int i = 1; i < list.size(); i++) {
                T y = list.get(i);
                if (cmp.compare(x, y) > 0)
                    return false;
                x = y;
            }
        }
        return true;
    }

    private static IntStream reverse(int[] source) {
        int len = source.length;
        return IntStream.range(0, len).map(i -> source[len - i - 1]);
    }

    private static IntStream shuffle(int[] source) {
        int len = source.length;
        Random rnd = new Random();
        return IntStream.generate(() -> rnd.nextInt(len)).distinct().limit(len).map(i -> source[i]);
    }

    private static IntStream distinct(int[] source) {
        return IntStream.of(source).distinct();
    }
}
