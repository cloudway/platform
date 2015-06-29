/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.scheme;

import com.cloudway.fp.data.Seq;
import static com.cloudway.fp.scheme.LispVal.*;

@SuppressWarnings("serial")
public class LispError extends RuntimeException {
    private Seq<LispVal> trace = Seq.nil();

    public LispError(String message) {
        super(message);
    }

    protected LispError() {
    }

    protected LispError(Throwable cause) {
        super(cause);
    }

    public Seq<LispVal> getCallTrace() {
        return trace;
    }

    public LispError setCallTrace(Seq<LispVal> trace) {
        this.trace = trace;
        return this;
    }

    @Override
    public String getMessage() {
        StringBuilder buf = new StringBuilder();
        buf.append("Error: ");
        buf.append(getRawMessage());

        if (!trace.isEmpty()) {
            buf.append("\n\nCall history:\n");

            Seq<LispVal> xs = trace;
            int count = 0;

            while (!xs.isEmpty() && count < 1000) {
                LispVal x = xs.head();
                xs = xs.tail();
                count++;

                if (x instanceof Prim) {
                    buf.append("\t#<primitive:");
                    buf.append(((Prim)x).name);
                    buf.append(">\n");
                } else if (x instanceof Macro) {
                    buf.append("\t#<syntax:");
                    buf.append(((Macro)x).name);
                    buf.append(">\n");
                } else if (x instanceof PrimMacro) {
                    buf.append("\t#<syntax:");
                    buf.append(((PrimMacro)x).name);
                    buf.append(">\n");
                } else if (x instanceof Func) {
                    Func fn = (Func)x;
                    buf.append("\t(");
                    buf.append(fn.name.isEmpty() ? "lambda" : fn.name);
                    if (!fn.params.isNil()) {
                        buf.append(" ");
                        buf.append(fn.params.show());
                    }
                    buf.append(")\n");
                }
            }

            if (!xs.isEmpty()) {
                buf.append("\t...\n");
            }
        }

        return buf.toString();
    }

    protected String getRawMessage() {
        return super.getMessage();
    }

    public static class NumArgs extends LispError {
        public final int expected;
        public final LispVal found;

        public NumArgs(int expected, LispVal found) {
            this.expected = expected;
            this.found = found;
        }

        @Override
        public String getRawMessage() {
            return "expected " + expected + " args; found values " + found.show();
        }
    }

    public static class TypeMismatch extends LispError {
        public final String expected;
        public final LispVal found;

        public TypeMismatch(String expected, LispVal found) {
            this.expected = expected;
            this.found = found;
        }

        @Override
        public String getRawMessage() {
            return "invalid type: expected " + expected + ", found " + found.show();
        }
    }

    public static class PatternMismatch extends LispError {
        public final LispVal pattern;
        public final LispVal found;

        public PatternMismatch(LispVal pattern, LispVal found) {
            this.pattern = pattern;
            this.found = found;
        }

        @Override
        public String getRawMessage() {
            return "pattern mismatch: pattern " + pattern.show() + ", found " + found.show();
        }
    }

    public static class Parser extends LispError {
        public final String filename;
        public final int line, column;

        public Parser(String filename, int line, int column, String message) {
            super(message);
            this.filename = filename;
            this.line = line;
            this.column = column;
        }

        @Override
        public String getRawMessage() {
            StringBuilder sb = new StringBuilder();
            if (filename != null && !filename.isEmpty()) {
                sb.append('"').append(filename).append("\" ");
            }
            if (line != -1 && column != -1) {
                sb.append("(line ").append(line);
                sb.append(", column ").append(column);
                sb.append(")");
            }
            sb.append("\n");
            sb.append(super.getRawMessage());
            return sb.toString();
        }
    }

    public static class BadSpecialForm extends LispError {
        public final String message;
        public final LispVal form;

        public BadSpecialForm(String message, LispVal form) {
            this.message = message;
            this.form = form;
        }

        @Override
        public String getRawMessage() {
            return message + ": " + form.show();
        }
    }

    public static class NotFunction extends LispError {
        public final String message;
        public final String func;

        public NotFunction(String message, String func) {
            this.message = message;
            this.func = func;
        }

        @Override
        public String getRawMessage() {
            return message + ": " + func;
        }
    }

    public static class UnboundVar extends LispError {
        public final String varname;

        public UnboundVar(String varname) {
            this.varname = varname;
        }

        @Override
        public String getRawMessage() {
            return "undefined variable: " + varname;
        }
    }

    public static class Condition extends LispError {
        public final LispVal value;

        public Condition(LispVal value) {
            this.value = value;
        }

        @Override
        public String getRawMessage() {
            return "uncaught exception: " + value.show();
        }
    }
}
