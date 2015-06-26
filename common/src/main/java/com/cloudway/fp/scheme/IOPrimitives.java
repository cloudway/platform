/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.scheme;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.util.concurrent.atomic.AtomicReference;

import com.cloudway.fp.$;
import static com.cloudway.fp.control.Syntax.let;
import com.cloudway.fp.data.Either;
import com.cloudway.fp.data.Maybe;
import com.cloudway.fp.data.Seq;
import com.cloudway.fp.io.IOConsumer;

import static com.cloudway.fp.control.Syntax.do_;
import com.cloudway.fp.scheme.LispError.TypeMismatch;
import static com.cloudway.fp.scheme.LispVal.*;

// @formatter:off

@SuppressWarnings("unused")
public final class IOPrimitives {
    private IOPrimitives() {}

    public static final class InputPort implements LispVal {
        public final String name;
        public final Reader input;
        public Seq<Either<LispError, LispVal>> stream;

        public InputPort(String name, Reader input) {
            this.name   = name;
            this.input  = input;
        }

        public void close() {
            try {
                input.close();
            } catch (IOException ex) {
                throw new LispError(ex);
            }
        }

        @Override
        public String show() {
            return "#<input port \"" + name + "\">";
        }
    }

    public static final class OutputPort implements LispVal {
        public final String  name;
        public final Writer  output;
        public final boolean autoFlush;

        public OutputPort(String name, Writer output, boolean autoFlush) {
            this.name      = name;
            this.output    = output;
            this.autoFlush = autoFlush;
        }

        public void close() {
            try {
                output.close();
            } catch (IOException ex) {
                throw new LispError(ex);
            }
        }

        @Override
        public String show() {
            return "#<output port \"" + name + "\">";
        }
    }

    public static final LispVal EOF = new LispVal() {
        @Override
        public String show() {
            return "#!eof";
        }
    };

    private static final Symbol CURRENT_INPUT  = new Symbol("CURRENT-INPUT");
    private static final Symbol CURRENT_OUTPUT = new Symbol("CURRENT-OUTPUT");

    @Name("input-port?")
    public static boolean isInputPort(LispVal obj) {
        return obj instanceof InputPort;
    }

    @Name("output-port?")
    public static boolean isOutputPort(LispVal obj) {
        return obj instanceof OutputPort;
    }

    @Name("eof-object?")
    public static boolean isEOF(LispVal obj) {
        return obj == EOF;
    }

    public static $<Evaluator, LispVal> open_input_file(Evaluator me, String filename) {
        return me.delay(() -> {
            try {
                InputStream is = new FileInputStream(filename);
                Reader input = new BufferedReader(new InputStreamReader(is));
                return me.pure(new InputPort(filename, input));
            } catch (IOException ex) {
                return me.throwE(new LispError(ex));
            }
        });
    }

    public static $<Evaluator, LispVal> open_output_file(Evaluator me, String filename) {
        return me.delay(() -> {
            try {
                OutputStream os = new FileOutputStream(filename);
                Writer output = new BufferedWriter(new OutputStreamWriter(os));
                return me.pure(new OutputPort(filename, output, false));
            } catch (IOException ex) {
                return me.throwE(new LispError(ex));
            }
        });
    }

    public static $<Evaluator, LispVal> open_input_string(Evaluator me, String str) {
        return me.pure(new InputPort("(string)", new StringReader(str)));
    }

    public static $<Evaluator, LispVal> open_output_string(Evaluator me) {
        return me.pure(new OutputPort("(string)", new StringWriter(), false));
    }

    public static $<Evaluator, LispVal> get_output_string(Evaluator me, LispVal arg) {
        if (arg instanceof OutputPort) {
            Writer out = ((OutputPort)arg).output;
            if (out instanceof StringWriter) {
                return me.pure(new MText(out.toString()));
            }
        }
        return me.throwE(new TypeMismatch("output string port", arg));
    }

    public static $<Evaluator, LispVal> close_input_port(Evaluator me, LispVal arg) {
        if (arg instanceof InputPort) {
            return do_(me.action(((InputPort)arg)::close),
                   do_(me.pure(VOID)));
        } else {
            return me.throwE(new TypeMismatch("input port", arg));
        }
    }

    public static $<Evaluator, LispVal> close_output_port(Evaluator me, LispVal arg) {
        if (arg instanceof OutputPort) {
            return do_(me.action(((OutputPort)arg)::close),
                   do_(me.pure(VOID)));
        } else {
            return me.throwE(new TypeMismatch("output port", arg));
        }
    }

    public static InputPort current_input_port(Evaluator me, Env env) {
        return env.getSystem(CURRENT_INPUT, () -> getStandardInput(me)).get();
    }

    public static OutputPort current_output_port(Env env) {
        return env.getSystem(CURRENT_OUTPUT, () -> getStandardOutput()).get();
    }

    private static InputPort getStandardInput(Evaluator me) {
        return new InputPort("(stdin)", new InputStreamReader(System.in));
    }

    private static OutputPort getStandardOutput() {
        return new OutputPort("(stdout)", new PrintWriter(System.out), true);
    }

    public static $<Evaluator, LispVal>
    call_with_input_file(Evaluator me, Env env, String filename, LispVal proc) {
        return do_(open_input_file(me, filename), port ->
               me.guard(close_input_port(me, port),
               me.apply(env, proc, Pair.list(port))));
    }

    public static $<Evaluator, LispVal>
    call_with_output_file(Evaluator me, Env env, String filename, LispVal proc) {
        return do_(open_output_file(me, filename), port ->
               me.guard(close_output_port(me, port),
               me.apply(env, proc, Pair.list(port))));
    }

    public static $<Evaluator, LispVal>
    with_input_from_file(Evaluator me, Env env, String filename, LispVal thunk) {
        AtomicReference<LispVal> ref = env.getSystem(CURRENT_INPUT, (LispVal)null);
        return do_(open_input_file(me, filename), port ->
               let(ref.getAndSet(port), current ->
               me.guard(do_(me.action(() -> ref.set(current)), close_input_port(me, port)),
               me.apply(env, thunk, LispVal.Nil))));
    }

    public static $<Evaluator, LispVal>
    with_output_to_file(Evaluator me, Env env, String filename, LispVal thunk) {
        AtomicReference<LispVal> ref = env.getSystem(CURRENT_OUTPUT, (LispVal)null);
        return do_(open_output_file(me, filename), port ->
               let(ref.getAndSet(port), current ->
               me.guard(do_(me.action(() -> ref.set(current)), close_output_port(me, port)),
               me.apply(env, thunk, LispVal.Nil))));
    }

    public static $<Evaluator, LispVal>
    call_with_input_string(Evaluator me, Env env, String input, LispVal proc) {
        return do_(open_input_string(me, input), port ->
               me.apply(env, proc, Pair.list(port)));
    }

    public static $<Evaluator, LispVal>
    call_with_output_string(Evaluator me, Env env, LispVal proc) {
        return do_(open_output_string(me), port ->
               do_(me.apply(env, proc, Pair.list(port)),
               do_(() -> get_output_string(me, port))));
    }

    public static $<Evaluator, LispVal>
    with_input_from_string(Evaluator me, Env env, String input, LispVal thunk) {
        AtomicReference<LispVal> ref = env.getSystem(CURRENT_INPUT, (LispVal)null);
        return do_(open_input_string(me, input), port ->
               let(ref.getAndSet(port), current ->
               me.guard(me.action(() -> ref.set(current)),
               me.apply(env, thunk, LispVal.Nil))));
    }

    public static $<Evaluator, LispVal>
    with_output_to_string(Evaluator me, Env env, LispVal thunk) {
        AtomicReference<LispVal> ref = env.getSystem(CURRENT_OUTPUT, (LispVal)null);
        return do_(open_output_string(me), port ->
               let(ref.getAndSet(port), current ->
               me.guard(me.action(() -> ref.set(current)),
               do_(me.apply(env, thunk, LispVal.Nil),
               do_(() -> get_output_string(me, port))))));
    }

    public static $<Evaluator, LispVal> read(Evaluator me, Env env, Maybe<LispVal> maybe_port) {
        LispVal port_val = maybe_port.orElseGet(() -> current_input_port(me, env));
        if (!(port_val instanceof InputPort)) {
            return me.throwE(new TypeMismatch("input port", port_val));
        }

        InputPort port = (InputPort)port_val;
        if (port.stream == null) {
            port.stream = me.getParser().parse(port.name, port.input);
        } else if (!port.stream.isEmpty()) {
            port.stream = port.stream.tail();
        }

        return port.stream.isEmpty()
            ? me.pure(EOF)
            : me.except(port.stream.head());
    }

    private static void forOutputPort(Env env, Maybe<LispVal> arg, IOConsumer<OutputPort> proc)
        throws IOException
    {
        LispVal port = arg.orElseGet(() -> current_output_port(env));
        if (port instanceof OutputPort) {
            proc.consume((OutputPort)port);
        } else {
            throw new TypeMismatch("output port", port);
        }
    }

    public static void write(Env env, LispVal val, Maybe<LispVal> port_arg)
        throws IOException
    {
        forOutputPort(env, port_arg, port -> {
            Printer pr = new Printer();
            pr.add(val);
            pr.print(port.output::write);
            if (port.autoFlush)
                port.output.flush();
        });
    }

    public static void display(Env env, LispVal val, Maybe<LispVal> port_arg)
        throws IOException
    {
        forOutputPort(env, port_arg, port -> print(val, port, false));
    }

    public static void print(Env env, LispVal val, Maybe<LispVal> port_arg)
        throws IOException
    {
        forOutputPort(env, port_arg, port -> print(val, port, true));
    }

    private static void print(LispVal val, OutputPort port, boolean newline)
        throws IOException
    {
        if (val instanceof CText) {
            port.output.write(((CText)val).value);
        } else if (val instanceof MText) {
            port.output.write(((MText)val).value);
        } else if (val instanceof Char) {
            port.output.write(((Char)val).value);
        } else {
            writeValue(port.output, val);
        }

        if (newline)
            port.output.write(System.lineSeparator());
        if (port.autoFlush)
            port.output.flush();
    }

    private static void writeValue(Writer out, LispVal val) throws IOException {
        Printer pr = new Printer() {
            @Override
            public void add(LispVal v) {
                if (v instanceof Text) {
                    super.add(((Text)v).value());
                } else if (v instanceof Char) {
                    super.add(String.valueOf(((Char)v).value));
                } else {
                    super.add(v);
                }
            }
        };

        pr.add(val);
        pr.print(out::write);
    }

    public static void newline(Env env, Maybe<LispVal> port_arg)
        throws IOException
    {
        forOutputPort(env, port_arg, port -> {
            port.output.write(System.lineSeparator());
            if (port.autoFlush)
                port.output.flush();
        });
    }

    public static void write_char(Env env, Char c, Maybe<LispVal> port_arg)
        throws IOException
    {
        forOutputPort(env, port_arg, port -> {
            port.output.write(c.value);
        });
    }
}


