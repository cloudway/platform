/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.scheme;

import java.util.StringJoiner;
import java.util.function.Function;
import java.util.regex.Pattern;

import com.cloudway.fp.$;
import com.cloudway.fp.control.ConditionCase;
import com.cloudway.fp.data.Maybe;
import com.cloudway.fp.data.Seq;
import com.cloudway.fp.data.Traversable;
import com.cloudway.fp.data.Vector;
import com.cloudway.fp.function.ExceptionBiFunction;
import com.cloudway.fp.function.ExceptionFunction;
import com.cloudway.fp.function.ExceptionSupplier;

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
    default boolean isAtom() {
        return false;
    }

    /**
     * Returns true if this Lisp value represents a list.
     */
    default boolean isList() {
        return false;
    }

    final class Atom implements LispVal {
        private static final Pattern INVALID_SYMCHAR =
            Pattern.compile("[^a-z0-9!$%&*+\\-/:.<=>?@^_~]");

        public final String name;

        public Atom(String name) {
            this.name = name;
        }

        @Override
        public boolean isAtom() {
            return true;
        }

        @Override
        public String show() {
            if (INVALID_SYMCHAR.matcher(name).find()) {
                return '|' + name + '|';
            } else {
                return name;
            }
        }

        public String toString() {
            return "#Atom('" + name + "')";
        }

        public boolean equals(Object obj) {
            if (obj == this)
                return true;
            if (obj instanceof Atom)
                return name.equals(((Atom)obj).name);
            return false;
        }

        public int hashCode() {
            return name.hashCode();
        }
    }

    final class Text implements LispVal {
        public final String value;

        public Text(String value) {
            this.value = value;
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
        public String show() {
            return value.toString();
        }

        public String toString() {
            return "#Numeric(" + value.getClass().getSimpleName() + ", " + value + ")";
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

    final class List implements LispVal {
        public static final List NIL = new List(Seq.nil());

        public final Seq<LispVal> value;

        public List(Seq<LispVal> value) {
            this.value = value;
        }

        @Override
        public boolean isList() {
            return true;
        }

        @Override
        public String show() {
            if (!value.isEmpty() && value.head().isAtom()) {
                String abbrev = null;
                switch (((Atom)value.head()).name) {
                case "quote":
                    abbrev = "'";
                    break;
                case "quasiquote":
                    abbrev = "`";
                    break;
                case "unquote":
                    abbrev = ",";
                    break;
                case "unquote-splicing":
                    abbrev = ",@";
                    break;
                }

                if (abbrev != null && value.count() == 2) {
                    return abbrev + value.tail().head().show();
                }
            }

            return value.map(LispVal::show).show(" ", "(", ")");
        }

        public String toString() {
            return "#List(" + value.force() + ")";
        }
    }

    final class DottedList implements LispVal {
        public final Seq<LispVal> head;
        public final LispVal tail;

        public DottedList(Seq<LispVal> head, LispVal tail) {
            this.head = head;
            this.tail = tail;
        }

        @Override
        public String show() {
            return head.map(LispVal::show).show(" ", "(", "")
                + " . " + tail.show() + ")";
        }

        public String toString() {
            return "#DottedList(" + head.force() + " " + tail.show() + ")";
        }
    }

    final class Vec implements LispVal {
        public Vector<LispVal> value;

        public Vec(Traversable<Vector.µ, LispVal> value) {
            this.value = (Vector<LispVal>)value;
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
        public final LispVal exp;
        public final Env     env;
        public final Atom    tag;

        public Promise(LispVal exp, Env env, Atom tag) {
            this.exp = exp;
            this.env = env;
            this.tag = tag;
        }

        @Override
        public String show() {
            return "#<promise>";
        }

        @Override
        public String toString() {
            return "#Promise(" + exp.show() + ")";
        }
    }

    final class Prim implements LispVal {
        public final Function<Seq<LispVal>, $<Evaluator, LispVal>> func;

        public Prim(Function<Seq<LispVal>, $<Evaluator, LispVal>> func) {
            this.func = func;
        }

        @Override
        public String show() {
            return "<primitive>";
        }

        public String toString() {
            return "#Primitive";
        }
    }

    final class Func implements LispVal {
        public final Seq<Atom>    params;
        public final Maybe<Atom>  vararg;
        public final Seq<LispVal> body;
        public final Env          closure;

        public Func(Seq<Atom> params, Maybe<Atom> vararg, Seq<LispVal> body, Env closure) {
            this.params  = params;
            this.vararg  = vararg;
            this.body    = body;
            this.closure = closure;
        }

        @Override
        public String show() {
            return "(lambda (" + params.map(Atom::show).show(" ", "", "") +
                   vararg.map(t -> " . " + t.show()).orElse("") +
                   ") ...)";
        }

        public String toString() {
            return "#Lambda(" + params.show(" ", "(", "") +
                   vararg.map(t -> ". " + t).orElse("") +
                   ") " + body.show() + ")";
        }
    }

    final class Macro implements LispVal {
        public final Seq<Atom>    params;
        public final Maybe<Atom>  vararg;
        public final Seq<LispVal> body;

        public Macro(Seq<Atom> params, Maybe<Atom> vararg, Seq<LispVal> body) {
            this.params = params;
            this.vararg = vararg;
            this.body   = body;
        }

        @Override
        public String show() {
            return "(macro (" + params.map(Atom::show).show(" ", "", "") +
                   vararg.map(t -> " . " + t.show()).orElse("") +
                   ") ...)";
        }

        public String toString() {
            return "#Macro(" + show() + ")";
        }
    }

    final class Void implements LispVal {
        public static final Void VOID = new Void();

        private Void() {}

        @Override
        public String show() {
            return "";
        }

        public String toString() {
            return "#Void";
        }
    }

    // Deconstructions

    static <R, X extends Throwable> ConditionCase<LispVal, R, X>
    Atom(ExceptionFunction<Atom, ? extends R, X> mapper) {
        return t -> t.isAtom()
            ? () -> mapper.evaluate((Atom)t)
            : null;
    }

    static <R, X extends Throwable> ConditionCase<LispVal, R, X>
    Text(ExceptionFunction<String, ? extends R, X> mapper) {
        return t -> t instanceof Text
            ? () -> mapper.evaluate(((Text)t).value)
            : null;
    }

    static <R, X extends Throwable> ConditionCase<LispVal, R, X>
    Num(ExceptionFunction<Number, ? extends R, X> mapper) {
        return t -> t instanceof Num
            ? () -> mapper.evaluate(((Num)t).value)
            : null;
    }

    static <R, X extends Throwable> ConditionCase<LispVal, R, X>
    Bool(ExceptionFunction<Boolean, ? extends R, X> mapper) {
        return t -> t instanceof Bool
            ? () -> mapper.evaluate(((Bool)t).value)
            : null;
    }

    static <R, X extends Throwable> ConditionCase<LispVal, R, X>
    List(ExceptionFunction<Seq<LispVal>, ? extends R, X> mapper) {
        return t -> t.isList() ? () -> mapper.evaluate(((List)t).value) : null;
    }

    static <R, X extends Throwable> ConditionCase<LispVal, R, X>
    Nil(ExceptionSupplier<R, X> mapper) {
        return t -> t.isList() && ((List)t).value.isEmpty() ? mapper : null;
    }

    static <R, X extends Throwable> ConditionCase<LispVal, R, X>
    Cons(ExceptionBiFunction<LispVal, Seq<LispVal>, ? extends R, X> mapper) {
        return t -> {
            if (t instanceof List && !((List)t).value.isEmpty()) {
                List lst = (List)t;
                return () -> mapper.evaluate(lst.value.head(), lst.value.tail());
            }
            return null;
        };
    }

    static <R, X extends Throwable> ConditionCase<LispVal, R, X>
    DottedList(ExceptionBiFunction<Seq<LispVal>, LispVal, ? extends R, X> mapper) {
        return t -> {
            if (t instanceof DottedList) {
                DottedList dl = (DottedList)t;
                return () -> mapper.evaluate(dl.head, dl.tail);
            }
            return null;
        };
    }

    static <R, X extends Throwable> ConditionCase<LispVal, R, X>
    Vector(ExceptionFunction<Vector<LispVal>, ? extends R, X> mapper) {
        return t -> t instanceof Vec
            ? () -> mapper.evaluate(((Vec)t).value)
            : null;
    }

    static <R, X extends Throwable> ConditionCase<LispVal, R, X>
    Promise(ExceptionFunction<Promise, ? extends R, X> mapper) {
        return t -> (t instanceof Promise)
            ? () -> mapper.evaluate((Promise)t)
            : null;
    }

    static <R, X extends Throwable> ConditionCase<LispVal, R, X>
    Prim(ExceptionFunction<Function<Seq<LispVal>, $<Evaluator, LispVal>>, ? extends R, X> mapper) {
        return t -> (t instanceof Prim)
            ? () -> mapper.evaluate(((Prim)t).func)
            : null;
    }

    static <R, X extends Throwable> ConditionCase<LispVal, R, X>
    Func(ExceptionFunction<Func, ? extends R, X> mapper) {
        return t -> (t instanceof Func)
            ? () -> mapper.evaluate(((Func)t))
            : null;
    }

    static <R, X extends Throwable> ConditionCase<LispVal, R, X>
    Macro(ExceptionFunction<Macro, ? extends R, X> mapper) {
        return t -> (t instanceof Macro)
            ? () -> mapper.evaluate(((Macro)t))
            : null;
    }

    static <R, X extends Throwable> ConditionCase<LispVal, R, X>
    Void(ExceptionSupplier<R, X> mapper) {
        return t -> (t instanceof Void) ? mapper : null;
    }
}
