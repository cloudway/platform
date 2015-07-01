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

    private static final class ReaderPort implements InputPort {
        private final String name;
        private final Reader reader;
        private Seq<Either<LispError, LispVal>> stream;

        public ReaderPort(String name, Reader reader) {
            this.name = name;
            this.reader = reader;
        }

        @Override
        public $<Evaluator, LispVal> read(Evaluator me, Env env) {
            if (stream == null) {
                stream = me.getParser().parse(name, reader);
            } else if (!stream.isEmpty()) {
                stream = stream.tail();
            }

            return stream.isEmpty()
                ? me.pure(EOF)
                : me.except(stream.head());
        }

        @Override
        public void close() {
            try {
                reader.close();
            } catch (IOException ex) {
                throw new LispError(ex);
            }
        }

        @Override
        public String show() {
            return "#<input port \"" + name + "\">";
        }
    }

    private static final class WriterPort implements OutputPort {
        private final String name;
        private final Writer writer;
        private final boolean autoFlush;

        public WriterPort(String name, Writer writer, boolean autoFlush) {
            this.name = name;
            this.writer = writer;
            this.autoFlush = autoFlush;
        }

        @Override
        public void write(int c) throws IOException {
            writer.write(c);
        }

        @Override
        public void write(char[] cbuf) throws IOException {
            writer.write(cbuf);
        }

        @Override
        public void write(String str) throws IOException {
            writer.write(str);
        }

        @Override
        public void flush() throws IOException {
            writer.flush();
        }

        @Override
        public boolean autoFlush() {
            return autoFlush;
        }

        @Override
        public void close() {
            try {
                writer.close();
            } catch (IOException ex) {
                throw new LispError(ex);
            }
        }

        @Override
        public String show() {
            return "#<output port \"" + name + "\">";
        }
    }

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
        return obj == InputPort.EOF;
    }

    public static $<Evaluator, LispVal> open_input_file(Evaluator me, Env env, String filename) {
        return me.delay(() -> {
            try {
                InputStream is = new FileInputStream(filename);
                Reader input = new BufferedReader(new InputStreamReader(is));
                return me.pure(new ReaderPort(filename, input));
            } catch (IOException ex) {
                return me.throwE(env, new LispError(ex));
            }
        });
    }

    public static $<Evaluator, LispVal> open_output_file(Evaluator me, Env env, String filename) {
        return me.delay(() -> {
            try {
                OutputStream os = new FileOutputStream(filename);
                Writer output = new BufferedWriter(new OutputStreamWriter(os));
                return me.pure(new WriterPort(filename, output, false));
            } catch (IOException ex) {
                return me.throwE(env, new LispError(ex));
            }
        });
    }

    public static $<Evaluator, LispVal> open_input_string(Evaluator me, String str) {
        return me.pure(new ReaderPort("(string)", new StringReader(str)));
    }

    public static $<Evaluator, LispVal> open_output_string(Evaluator me) {
        return me.pure(new WriterPort("(string)", new StringWriter(), false));
    }

    public static $<Evaluator, LispVal> get_output_string(Evaluator me, Env env, LispVal arg) {
        if (arg instanceof WriterPort) {
            Writer out = ((WriterPort)arg).writer;
            if (out instanceof StringWriter) {
                return me.pure(new MText(out.toString()));
            }
        }
        return me.throwE(env, new TypeMismatch("output string port", arg));
    }

    public static $<Evaluator, LispVal> close_input_port(Evaluator me, Env env, LispVal arg) {
        if (arg instanceof InputPort) {
            return do_(me.action(((InputPort)arg)::close),
                   do_(me.pure(VOID)));
        } else {
            return me.throwE(env, new TypeMismatch("input port", arg));
        }
    }

    public static $<Evaluator, LispVal> close_output_port(Evaluator me, Env env, LispVal arg) {
        if (arg instanceof OutputPort) {
            return do_(me.action(((OutputPort)arg)::close),
                   do_(me.pure(VOID)));
        } else {
            return me.throwE(env, new TypeMismatch("output port", arg));
        }
    }

    public static InputPort current_input_port(Evaluator me, Env env) {
        return env.getSystem(CURRENT_INPUT, () -> getStandardInput(me)).get();
    }

    public static OutputPort current_output_port(Env env) {
        return env.getSystem(CURRENT_OUTPUT, () -> getStandardOutput()).get();
    }

    private static InputPort getStandardInput(Evaluator me) {
        return new ReaderPort("(stdin)", new InputStreamReader(System.in));
    }

    private static OutputPort getStandardOutput() {
        return new WriterPort("(stdout)", new PrintWriter(System.out), true);
    }

    public static $<Evaluator, LispVal>
    call_with_input_file(Evaluator me, Env env, String filename, LispVal proc) {
        return do_(open_input_file(me, env, filename), port ->
               me.guard(close_input_port(me, env, port),
               me.apply(env, proc, Pair.list(port))));
    }

    public static $<Evaluator, LispVal>
    call_with_output_file(Evaluator me, Env env, String filename, LispVal proc) {
        return do_(open_output_file(me, env, filename), port ->
               me.guard(close_output_port(me, env, port),
               me.apply(env, proc, Pair.list(port))));
    }

    public static $<Evaluator, LispVal>
    with_input_from_file(Evaluator me, Env env, String filename, LispVal thunk) {
        AtomicReference<LispVal> ref = env.getSystem(CURRENT_INPUT, (LispVal)null);
        return do_(open_input_file(me, env, filename), port ->
               let(ref.getAndSet(port), current ->
               me.guard(do_(me.action(() -> ref.set(current)), close_input_port(me, env, port)),
               me.apply(env, thunk, LispVal.Nil))));
    }

    public static $<Evaluator, LispVal>
    with_output_to_file(Evaluator me, Env env, String filename, LispVal thunk) {
        AtomicReference<LispVal> ref = env.getSystem(CURRENT_OUTPUT, (LispVal)null);
        return do_(open_output_file(me, env, filename), port ->
               let(ref.getAndSet(port), current ->
               me.guard(do_(me.action(() -> ref.set(current)), close_output_port(me, env, port)),
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
               do_(() -> get_output_string(me, env, port))));
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
               do_(() -> get_output_string(me, env, port))))));
    }

    public static $<Evaluator, LispVal> read(Evaluator me, Env env, Maybe<LispVal> maybe_port) {
        LispVal port = maybe_port.orElseGet(() -> current_input_port(me, env));
        if (port instanceof InputPort) {
            return ((InputPort)port).read(me, env);
        } else {
            return me.throwE(env, new TypeMismatch("input port", port));
        }
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
        forOutputPort(env, port_arg, port -> write(val, port, false, false));
    }

    public static void display(Env env, LispVal val, Maybe<LispVal> port_arg)
        throws IOException
    {
        forOutputPort(env, port_arg, port -> write(val, port, true, false));
    }

    public static void print(Env env, LispVal val, Maybe<LispVal> port_arg)
        throws IOException
    {
        forOutputPort(env, port_arg, port -> write(val, port, true, true));
    }

    private static void write(LispVal val, OutputPort port, boolean display, boolean newline)
        throws IOException
    {
        if (port instanceof Printer) {
            Printer pr = (Printer)port;
            boolean wasDisplay = pr.isDisplay();
            pr.setDisplay(display);
            pr.add(val);
            pr.setDisplay(wasDisplay);
        } else {
            Printer pr = new Printer();
            pr.setDisplay(display);
            pr.add(val);

            pr.checkError().ifPresent(err -> { throw err; });
            pr.print(port::write);
        }

        if (newline)
            port.write(System.lineSeparator());
        if (port.autoFlush())
            port.flush();
    }

    public static void newline(Env env, Maybe<LispVal> port_arg)
        throws IOException
    {
        forOutputPort(env, port_arg, port -> {
            port.write(System.lineSeparator());
            if (port.autoFlush())
                port.flush();
        });
    }

    public static void write_char(Env env, Char c, Maybe<LispVal> port_arg)
        throws IOException
    {
        forOutputPort(env, port_arg, port -> {
            port.write(c.value);
        });
    }
}


