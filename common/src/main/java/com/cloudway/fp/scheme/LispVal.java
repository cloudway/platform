/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.scheme;

import java.util.StringJoiner;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.regex.Pattern;

import com.cloudway.fp.$;
import com.cloudway.fp.control.ConditionCase;
import com.cloudway.fp.data.Seq;
import com.cloudway.fp.data.Traversable;
import com.cloudway.fp.data.Vector;
import com.cloudway.fp.function.ExceptionSupplier;
import com.cloudway.fp.function.TriFunction;
import com.cloudway.fp.scheme.LispError.TypeMismatch;

/**
 * Represents a Lisp value.
 */
@SuppressWarnings("EqualsAndHashcode")
public interface LispVal {
    /**
     * Returns a string representation of the value.
     */
    String show();

    /**
     * Returns true if this Lisp value represents an atom.
     */
    default boolean isSymbol() {
        return this instanceof Symbol;
    }

    /**
     * Returns true if this Lisp value represents a Pair.
     */
    default boolean isPair() {
        return this instanceof Pair;
    }

    /**
     * Returns true if this Lisp value represents a list.
     */
    default boolean isList() {
        return false;
    }

    /**
     * Returns true if this Lisp value represents a empty list.
     */
    default boolean isNil() {
        return this == Nil;
    }

    /**
     * Returns true if this Lisp value represents a false value.
     */
    default boolean isFalse() {
        return this == Bool.FALSE;
    }

    /**
     * Returns true if this Lisp value represents a true value.
     */
    default boolean isTrue() {
        return this != Bool.FALSE;
    }

    /**
     * Returns true if this Lisp value evaluating to itself.
     */
    default boolean isSelfEvaluating() {
        return false;
    }

    default LispVal getValue() {
        return this;
    }

    default LispVal map(Function<LispVal, LispVal> f) {
        LispVal rev = Nil;
        LispVal vals = this;
        while (vals.isPair()) {
            Pair p = (Pair)vals;
            rev = new Pair(f.apply(p.head), rev);
            vals = p.tail;
        }

        LispVal res = vals.isNil() ? Nil : f.apply(vals);
        while (rev.isPair()) {
            Pair p = (Pair)rev;
            res = new Pair(p.head, res);
            rev = p.tail;
        }

        return res;
    }

    @SuppressWarnings("unchecked")
    default $<Evaluator, LispVal>
    mapM(Evaluator m, Function<LispVal, $<Evaluator, ? extends LispVal>> f) {
        return ($<Evaluator, LispVal>)f.apply(this);
    }

    default boolean allMatch(Predicate<LispVal> p) {
        return p.test(this);
    }

    default $<Evaluator, Seq<LispVal>> toList(Evaluator m) {
        return m.throwE(new TypeMismatch("pair", this));
    }

    // -----------------------------------------------------------------------
    // Constructors

    class Symbol implements LispVal {
        private static final Pattern INVALID_SYMCHAR =
            Pattern.compile("[^a-zA-Z0-9!$%&*+\\-/:.<=>?@^_~]");

        public final String name;

        public Symbol(String name) {
            this.name = name;
        }

        @Override
        public String show() {
            if (name.isEmpty() || INVALID_SYMCHAR.matcher(name).find()) {
                return '|' + name + '|';
            } else {
                return name;
            }
        }

        public String toString() {
            return "#Symbol('" + name + "')";
        }

        public boolean equals(Object obj) {
            if (obj == this)
                return true;
            if (obj.getClass() == this.getClass())
                return name.equals(((Symbol)obj).name);
            return false;
        }

        public int hashCode() {
            return name.hashCode();
        }
    }

    final class KeySym extends Symbol {
        public KeySym(String name) {
            super(name);
        }

        @Override
        public boolean isSymbol() {
            return false;
        }

        @Override
        public boolean isSelfEvaluating() {
            return true;
        }

        @Override
        public String show() {
            return super.show() + ":";
        }
    }

    final class Text implements LispVal {
        public String value;

        public Text(String value) {
            this.value = value;
        }

        @Override
        public boolean isSelfEvaluating() {
            return true;
        }

        @Override
        public String show() {
            StringBuilder buf = new StringBuilder(value.length() + 2);
            buf.append('"');
            for (int i = 0, len = value.length(); i < len; i++) {
                char c = value.charAt(i);
                switch (c) {
                case '\b': buf.append("\\b"); break;
                case '\t': buf.append("\\t"); break;
                case '\f': buf.append("\\f"); break;
                case '\r': buf.append("\\r"); break;
                case '\n': buf.append("\\n"); break;
                case '\\': buf.append("\\\\"); break;
                case '"':  buf.append("\\\""); break;
                default:   buf.append(c);
                }
            }
            buf.append('"');
            return buf.toString();
        }

        public String toString() {
            return "#Text(\"" + value + "\")";
        }

        public boolean equals(Object obj) {
            if (obj == this)
                return true;
            if (obj instanceof Text)
                return value.equals(((Text)obj).value);
            return false;
        }
    }

    final class Num implements LispVal {
        public final Number value;

        public Num(Number value) {
            this.value = value;
        }

        @Override
        public boolean isSelfEvaluating() {
            return true;
        }

        @Override
        public String show() {
            return value.toString();
        }

        public String toString() {
            return "#Num(" + value.getClass().getSimpleName() + ", " + value + ")";
        }

        public boolean equals(Object obj) {
            if (obj == this)
                return true;
            if (obj instanceof Num)
                return value.equals(((Num)obj).value);
            return false;
        }
    }

    final class Bool implements LispVal {
        public static final Bool TRUE  = new Bool(true);
        public static final Bool FALSE = new Bool(false);

        public static Bool valueOf(boolean value) {
            return value ? TRUE : FALSE;
        }

        public final boolean value;

        private Bool(boolean value) {
            this.value = value;
        }

        @Override
        public boolean isSelfEvaluating() {
            return true;
        }

        @Override
        public String show() {
            return value ? "#t" : "#f";
        }

        public String toString() {
            return "#Bool(" + value + ")";
        }

        public boolean equals(Object obj) {
            if (obj == this)
                return true;
            if (obj instanceof Bool)
                return value == ((Bool)obj).value;
            return false;
        }
    }

    final class Char implements LispVal {
        public final char value;

        public Char(char value) {
            this.value = value;
        }

        @Override
        public boolean isSelfEvaluating() {
            return true;
        }

        @Override
        public String show() {
            switch (value) {
            case '\0':      return "#\\nul";
            case '\u0007':  return "#\\alarm";
            case '\u0008':  return "#\\backspace";
            case '\u0009':  return "#\\tab";
            case '\n':      return "#\\newline";
            case '\u000b':  return "#\\vtab";
            case '\u000c':  return "#\\page";
            case '\r':      return "#\\return";
            case '\u001b':  return "#\\esc";
            case ' ':       return "#\\space";
            case '\u007f':  return "#\\delete";
            default:        return "#\\" + value;
            }
        }

        public String toString() {
            return "#Char(" + value + ")";
        }

        public boolean equals(Object obj) {
            if (obj == this)
                return true;
            if (obj instanceof Char)
                return value == ((Char)obj).value;
            return false;
        }
    }

    LispVal Nil = new LispVal() {
        @Override
        public boolean isList() {
            return true;
        }

        @Override
        public $<Evaluator, LispVal>
        mapM(Evaluator m, Function<LispVal, $<Evaluator, ? extends LispVal>> f) {
            return m.pure(this);
        }

        @Override
        public boolean allMatch(Predicate<LispVal> p) {
            return true;
        }

        @Override
        public $<Evaluator, Seq<LispVal>> toList(Evaluator m) {
            return m.pure(Seq.nil());
        }

        @Override
        public String show() {
            return "()";
        }

        public String toString() {
            return "#Nil";
        }
    };

    final class Pair implements LispVal {
        public LispVal head, tail;

        public static LispVal of(LispVal x) {
            return new Pair(x, Nil);
        }

        public static LispVal of(LispVal x, LispVal y) {
            return new Pair(x, new Pair(y, Nil));
        }

        public static LispVal fromList(Seq<? extends LispVal> vals) {
            return vals.foldRight_(Nil, Pair::new);
        }

        public Pair(LispVal head, LispVal tail) {
            this.head = head;
            this.tail = tail;
        }

        @Override
        public boolean isList() {
            LispVal t = this;
            while (t.isPair()) {
                t = ((Pair)t).tail;
            }
            return t.isNil();
        }

        @Override
        public $<Evaluator, Seq<LispVal>> toList(Evaluator m) {
            Seq<LispVal> res = Seq.nil();

            LispVal t = this;
            while (t.isPair()) {
                Pair p = (Pair)t;
                res = Seq.cons(p.head, res);
                t = p.tail;
            }

            if (t.isNil()) {
                return m.pure(res.reverse());
            } else {
                return m.throwE(new TypeMismatch("pair", t));
            }
        }

        @Override
        public $<Evaluator, LispVal>
        mapM(Evaluator m, Function<LispVal, $<Evaluator, ? extends LispVal>> f) {
            return m.bind(f.apply(head), x ->
                   m.map(tail.mapM(m, f), y ->
                   new Pair(x, y)));
        }

        @Override
        public boolean allMatch(Predicate<LispVal> pred) {
            LispVal t = this;
            while (t.isPair()) {
                Pair p = (Pair)t;
                if (!pred.test(p.head))
                    return false;
                t = p.tail;
            }
            return t.isNil() || pred.test(t);
        }

        @Override
        public String show() {
            String abbrev = abbreviation();
            if (abbrev != null && tail.isPair()) {
                Pair second = (Pair)tail;
                if (second.tail.isNil()) {
                    return abbrev + second.head.show();
                }
            }

            return showList();
        }

        private String abbreviation() {
            if (head.isSymbol()) {
                switch (((Symbol)head).name) {
                case "quote":
                    return "'";
                case "quasiquote":
                    return "`";
                case "unquote":
                    return ",";
                case "unquote-splicing":
                    return ",@";
                }
            }
            return null;
        }

        private String showList() {
            StringJoiner sj = new StringJoiner(" ", "(", ")");

            LispVal t = this;
            while (t.isPair()) {
                Pair p = (Pair)t;
                sj.add(p.head.show());
                t = p.tail;
            }

            if (!t.isNil()) {
                sj.add(".");
                sj.add(t.show());
            }

            return sj.toString();
        }

        public String toString() {
            return "#Pair(" + head + " " + tail + ")";
        }
    }

    final class Vec implements LispVal {
        public Vector<LispVal> value;

        public Vec(Traversable<Vector.µ, LispVal> value) {
            this.value = (Vector<LispVal>)value;
        }

        @Override
        public boolean isSelfEvaluating() {
            return true;
        }

        @Override
        public String show() {
            return value.foldLeft(new StringJoiner(" ", "#(", ")"),
                (sj, x) -> sj.add(x.show())).toString();
        }

        public String toString() {
            return value.show(" ", "#Vector(", ")");
        }
    }

    final class Promise implements LispVal {
        public final Env  env;
        public final Proc body;
        public $<Evaluator, LispVal> result;

        public Promise(Env env, Proc body) {
            this.body = body;
            this.env = env;
        }

        @Override
        public boolean isSelfEvaluating() {
            return true;
        }

        @Override
        public String show() {
            return "#<promise>";
        }

        @Override
        public String toString() {
            return "#Promise(" + body.show() + ")";
        }
    }

    final class MultiVal implements LispVal {
        public final LispVal value;

        public MultiVal(LispVal value) {
            this.value = value;
        }

        @Override
        public LispVal getValue() {
            return value.isPair() ? ((Pair)value).head : Void.VOID;
        }

        @Override
        public boolean isTrue() {
            return getValue().isTrue();
        }

        @Override
        public boolean isFalse() {
            return getValue().isFalse();
        }

        @Override
        public String show() {
            StringJoiner sj = new StringJoiner("\n", "", "");
            LispVal args = value;
            while (args.isPair()) {
                Pair p = (Pair)args;
                sj.add(p.head.show());
                args = p.tail;
            }
            return sj.toString();
        }

        public String toString() {
            return "#MultiVal(" + value + ")";
        }
    }

    final class Box implements LispVal {
        public LispVal value;

        public Box(LispVal value) {
            this.value = value;
        }

        @Override
        public boolean isSelfEvaluating() {
            return true;
        }

        @Override
        public String show() {
            return "#&" + value.show();
        }

        public String toString() {
            return "#Box(" + value + ")";
        }
    }

    final class Prim implements LispVal {
        public final String name;
        public final PProc  proc;

        public Prim(String name, PProc proc) {
            this.name = name;
            this.proc = proc;
        }

        @Override
        public String show() {
            return "#<primitive:" + name + ">";
        }

        public String toString() {
            return "#Primitive:" + name;
        }
    }

    final class Func implements LispVal {
        public final String  name;
        public final LispVal params;
        public final Env     closure;
        public final Proc    body;

        public Func(LispVal params, Proc body, Env closure) {
            this("", params, body, closure);
        }

        public Func(String name, LispVal params, Proc body, Env closure) {
            this.name    = name;
            this.params  = params;
            this.body    = body;
            this.closure = closure;
        }

        @Override
        public String show() {
            return "#<procedure:" + name + " (lambda " + params.show() + " ...)>";
        }

        public String toString() {
            return "#Lambda(" + params.show() + " " + body.show() + ")";
        }
    }

    final class Macro implements LispVal {
        public final String name;
        public final LispVal pattern;
        public final Proc body;

        public Macro(String name, LispVal pattern, Proc body) {
            this.name    = name;
            this.pattern = pattern;
            this.body    = body;
        }

        @Override
        public String show() {
            return "#<macro:" + name + ">";
        }

        public String toString() {
            return "#Macro(" + pattern.show() + " " + body.show() + ")";
        }
    }

    final class PrimMacro implements LispVal {
        public final String name;
        public final BiFunction<Env, LispVal, $<Evaluator, Proc>> proc;

        public PrimMacro(String name, BiFunction<Env, LispVal, $<Evaluator, Proc>> proc) {
            this.name = name;
            this.proc = proc;
        }

        @Override
        public String show() {
            return "<macro:" + name + ">";
        }

        public String toString() {
            return "#PrimMacro:" + name;
        }
    }

    final class Void implements LispVal {
        public static final Void VOID = new Void();

        private Void() {}

        @Override
        public boolean isSelfEvaluating() {
            return true;
        }

        @Override
        public String show() {
            return "#<void>";
        }

        public String toString() {
            return "#Void";
        }
    }

    // -----------------------------------------------------------------------
    // Runtime procedure

    @FunctionalInterface
    interface Proc extends LispVal, Function<Env, $<Evaluator, LispVal>> {
        @Override
        default String show() {
            return "#<procedure>";
        }
    }

    @FunctionalInterface
    interface PProc extends BiFunction<Env, LispVal, $<Evaluator, LispVal>> {}

    // -----------------------------------------------------------------------
    // Deconstructions

    static <R> ConditionCase<LispVal, R, RuntimeException>
    Datum(Function<LispVal, ? extends R> mapper) {
        return t -> t.isSelfEvaluating()
            ? () -> mapper.apply(t)
            : null;
    }

    static <R> ConditionCase<LispVal, R, RuntimeException>
    Symbol(Function<Symbol, ? extends R> mapper) {
        return t -> t instanceof Symbol
            ? () -> mapper.apply((Symbol)t)
            : null;
    }

    static <R> ConditionCase<LispVal, R, RuntimeException>
    Text(Function<String, ? extends R> mapper) {
        return t -> t instanceof Text
            ? () -> mapper.apply(((Text)t).value)
            : null;
    }

    static <R> ConditionCase<LispVal, R, RuntimeException>
    Num(Function<Number, ? extends R> mapper) {
        return t -> t instanceof Num
            ? () -> mapper.apply(((Num)t).value)
            : null;
    }

    static <R> ConditionCase<LispVal, R, RuntimeException>
    Bool(Function<Boolean, ? extends R> mapper) {
        return t -> t instanceof Bool
            ? () -> mapper.apply(((Bool)t).value)
            : null;
    }

    static <R> ConditionCase<LispVal, R, RuntimeException>
    Quoted(Function<LispVal, ? extends R> mapper) {
        return t -> {
            Pair p, pp;
            if ((t.isPair()) &&
                (p = (Pair)t).head.isSymbol() &&
                "quote".equals(((Symbol)p.head).name) &&
                p.tail.isPair() &&
                (pp = (Pair)p.tail).tail.isNil()) {
                return () -> mapper.apply(pp.head);
            }
            return null;
        };
    }

    static <R, X extends Throwable> ConditionCase<LispVal, R, X>
    Nil(ExceptionSupplier<R, X> mapper) {
        return t -> t.isNil() ? mapper : null;
    }

    static <R> ConditionCase<LispVal, R, RuntimeException>
    Pair(Function<Pair, ? extends R> mapper) {
        return t -> t instanceof Pair
            ? () -> mapper.apply((Pair)t)
            : null;
    }

    static <R> ConditionCase<LispVal, R, RuntimeException>
    Cons(BiFunction<LispVal, LispVal, ? extends R> mapper) {
        return t -> {
            if (t.isPair()) {
                Pair p = (Pair)t;
                return () -> mapper.apply(p.head, p.tail);
            }
            return null;
        };
    }

    static <R> ConditionCase<LispVal, R, RuntimeException>
    Cons(TriFunction<LispVal, LispVal, LispVal, ? extends R> mapper) {
        return t -> {
            Pair p, pp;
            if (t.isPair() && ((p = (Pair)t).tail.isPair())) {
                pp = (Pair)p.tail;
                return () -> mapper.apply(p.head, pp.head, pp.tail);
            }
            return null;
        };
    }

    static <R> ConditionCase<LispVal, R, RuntimeException>
    List(Function<LispVal, ? extends R> mapper) {
        return t -> {
            Pair p;
            if (t.isPair() && ((p = (Pair)t).tail.isNil())) {
                return () -> mapper.apply(p.head);
            }
            return null;
        };
    }

    static <R> ConditionCase<LispVal, R, RuntimeException>
    List(BiFunction<LispVal, LispVal, ? extends R> mapper) {
        return t -> {
            Pair p, pp;
            if (t.isPair() &&
                ((p = (Pair)t).tail.isPair()) &&
                ((pp = (Pair)p.tail).tail.isNil())) {
                return () -> mapper.apply(p.head, pp.head);
            }
            return null;
        };
    }

    static <R> ConditionCase<LispVal, R, RuntimeException>
    List(TriFunction<LispVal, LispVal, LispVal, ? extends R> mapper) {
        return t -> {
            Pair p, pp, ppp;
            if (t.isPair() &&
                (p = (Pair)t).tail.isPair() &&
                (pp = (Pair)p.tail).tail.isPair() &&
                (ppp = (Pair)pp.tail).tail.isNil()) {
                return () -> mapper.apply(p.head, pp.head, ppp.head);
            }
            return null;
        };
    }

    static <R> ConditionCase<LispVal, R, RuntimeException>
    Vector(Function<Vector<LispVal>, ? extends R> mapper) {
        return t -> t instanceof Vec
            ? () -> mapper.apply(((Vec)t).value)
            : null;
    }

    static <R> ConditionCase<LispVal, R, RuntimeException>
    Prim(Function<PProc, ? extends R> mapper) {
        return t -> t instanceof Prim
            ? () -> mapper.apply(((Prim)t).proc)
            : null;
    }

    static <R> ConditionCase<LispVal, R, RuntimeException>
    Func(Function<Func, ? extends R> mapper) {
        return t -> t instanceof Func
            ? () -> mapper.apply(((Func)t))
            : null;
    }

    static <R> ConditionCase<LispVal, R, RuntimeException>
    Macro(Function<Macro, ? extends R> mapper) {
        return t -> t instanceof Macro
            ? () -> mapper.apply(((Macro)t))
            : null;
    }

    static <R, X extends Throwable> ConditionCase<LispVal, R, X>
    Void(ExceptionSupplier<R, X> mapper) {
        return t -> (t instanceof Void) ? mapper : null;
    }
}
