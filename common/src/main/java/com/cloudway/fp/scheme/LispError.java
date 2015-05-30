/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.scheme;

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
        public final int expected;
        public final LispVal found;

        public NumArgs(int expected, LispVal found) {
            this.expected = expected;
            this.found = found;
        }

        @Override
        public String getMessage() {
            return "Expected " + expected + " args; found values " + found.show();
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
        public String getMessage() {
            return "Invalid type: expected " + expected + ", found " + found.show();
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
        public String getMessage() {
            return "Pattern mismatch: pattern " + pattern.show() + ", found " + found.show();
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
        public final LispVal form;

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
        public final String func;

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
        public final String varname;

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
