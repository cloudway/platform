/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.scheme;

import com.cloudway.fp.data.Seq;
import com.cloudway.fp.parser.ParseError;

@SuppressWarnings("serial")
public class LispError extends RuntimeException {
    public LispError(String message) {
        super(message);
    }

    protected LispError() {
    }

    protected LispError(Throwable cause) {
        super(cause);
    }

    public static class NumArgs extends LispError {
        private final int expected;
        private final Seq<LispVal> found;

        public NumArgs(int expected, Seq<LispVal> found) {
            this.expected = expected;
            this.found = found;
        }

        @Override
        public String getMessage() {
            return "Expected " + expected + " args; found values " +
                   found.map(LispVal::show).show(" ", "(", ")");
        }
    }

    public static class TypeMismatch extends LispError {
        private final String expected;
        private final LispVal found;

        public TypeMismatch(String expected, LispVal found) {
            this.expected = expected;
            this.found = found;
        }

        @Override
        public String getMessage() {
            return "Invalid type: expected " + expected + ", found " + found.show();
        }
    }

    public static class Parser extends LispError {
        public Parser(ParseError err) {
            super(err);
        }

        @Override
        public ParseError getCause() {
            return (ParseError)super.getCause();
        }
    }

    public static class BadSpecialForm extends LispError {
        private final LispVal form;

        public BadSpecialForm(String message, LispVal form) {
            super(message);
            this.form = form;
        }

        @Override
        public String getMessage() {
            return super.getMessage() + ": " + form.show();
        }
    }

    public static class NotFunction extends LispError {
        private final String func;

        public NotFunction(String message, String func) {
            super(message);
            this.func = func;
        }

        @Override
        public String getMessage() {
            return super.getMessage() + ": " + func;
        }
    }

    public static class UnboundVar extends LispError {
        private final String varname;

        public UnboundVar(String message, String varname) {
            super(message);
            this.varname = varname;
        }

        @Override
        public String getMessage() {
            return super.getMessage() + ": " + varname;
        }
    }
}
