/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util;

import java.io.ByteArrayOutputStream;
import java.io.Closeable;
import java.io.File;
import java.io.Flushable;
import java.io.IOException;
import java.io.InputStream;
import java.io.InterruptedIOException;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import com.google.common.io.ByteStreams;
import static java.lang.ProcessBuilder.Redirect;

@SuppressWarnings("unused")
public final class Exec
{
    private final ProcessBuilder builder;

    private InputStream in;
    private OutputStream out;
    private OutputStream err;

    private static final File devNull =
        Files.exists(Paths.get("/dev/null")) ? new File("/dev/null") : null;

    /**
     * Wraps a ProcessBuilder.
     */
    public Exec(ProcessBuilder builder) {
        this.builder = builder;
        builder.inheritIO();
    }

    /**
     * Create an Exec object from command arguments as String array.
     */
    public static Exec args(String... command) {
        return new Exec(new ProcessBuilder(command));
    }

    /**
     * Create an Exec object from command arguments as Object array.
     * The array elements will be converted to strings.
     */
    public static Exec args(Object... args) {
        String[] command = new String[args.length];
        for (int i = 0; i < args.length; i++) {
            command[i] = String.valueOf(args[i]);
        }
        return new Exec(new ProcessBuilder(command));
    }

    /**
     * Create an Exec object from command arguments as String List.
     */
    public static Exec args(List<String> command) {
        return new Exec(new ProcessBuilder(command));
    }

    /**
     * Create an Exec object from command line.
     */
    public static Exec line(String command) {
        return args(command.split(" ")); // FIXME
    }

    /**
     * Sets the program and arguments.
     *
     * @see ProcessBuilder#command(java.util.List)
     */
    public Exec command(List<String> command) {
        builder.command(command);
        return this;
    }

    /**
     * Sets the program and arguments.
     *
     * @see ProcessBuilder#command(String...)
     */
    public Exec command(String... command) {
        builder.command(command);
        return this;
    }

    /**
     * @see ProcessBuilder#command()
     */
    public List<String> command() {
        return builder.command();
    }

    /**
     * @see ProcessBuilder#environment()
     */
    public Map<String,String> environment() {
        return builder.environment();
    }

    /**
     * @see ProcessBuilder#environment()
     */
    public Exec environment(Map<String,String> env, boolean override) {
        if (override)
            builder.environment().clear();
        builder.environment().putAll(env);
        return this;
    }

    /**
     * @see ProcessBuilder#environment()
     */
    public Exec environment(Map<String,String> env) {
        return environment(env, false);
    }

    /**
     * @see ProcessBuilder#directory()
     */
    public File directory() {
        return builder.directory();
    }

    /**
     * @see ProcessBuilder#directory(java.io.File)
     */
    public Exec directory(File directory) {
        builder.directory(directory);
        return this;
    }

    /**
     * @see ProcessBuilder#directory(java.io.File)
     */
    public Exec directory(Path path) {
        builder.directory(path.toFile());
        return this;
    }

    /**
     * @see ProcessBuilder#redirectInput(ProcessBuilder.Redirect)
     */
    public Exec redirectInput(Redirect source) {
        builder.redirectInput(source);
        this.in = null;
        return this;
    }

    /**
     * @see ProcessBuilder#redirectOutput(ProcessBuilder.Redirect)
     */
    public Exec redirectOutput(Redirect destination) {
        builder.redirectOutput(destination);
        this.out = null;
        return this;
    }

    /**
     * @see ProcessBuilder#redirectError(ProcessBuilder.Redirect)
     */
    public Exec redirectError(Redirect destination) {
        builder.redirectError(destination);
        this.err = null;
        return this;
    }

    /**
     * @see ProcessBuilder#redirectInput(java.io.File)
     */
    public Exec redirectInput(File file) {
        if (file == null && devNull == null) {
            redirectInputStream(NullInputStream.INSTANCE); // workaround
        } else {
            builder.redirectInput(file != null ? file : devNull);
            this.in = null;
        }
        return this;
    }

    public Exec redirectInputStream(InputStream input) {
        Objects.requireNonNull(input);
        builder.redirectInput(Redirect.PIPE);
        this.in = input;
        return this;
    }

    /**
     * @see ProcessBuilder#redirectOutput(java.io.File)
     */
    public Exec redirectOutput(File file) {
        if (file == null && devNull == null) {
            redirectOutputStream(NullOutputStream.INSTANCE); // workaround
        } else {
            builder.redirectOutput(file != null ? file : devNull);
            this.out = null;
        }
        return this;
    }

    public Exec redirectOutputStream(OutputStream output) {
        Objects.requireNonNull(output);
        builder.redirectOutput(Redirect.PIPE);
        this.out = output;
        return this;
    }

    /**
     * @see ProcessBuilder#redirectError(java.io.File)
     */
    public Exec redirectError(File file) {
        if (file == null && devNull == null) {
            redirectErrorStream(NullOutputStream.INSTANCE); // workaround
        } else {
            builder.redirectError(file != null ? file : devNull);
            this.err = null;
        }
        return this;
    }

    public Exec redirectErrorStream(OutputStream error) {
        Objects.requireNonNull(error);
        builder.redirectError(Redirect.PIPE);
        this.err = error;
        return this;
    }

    public Exec checkError() {
        return redirectErrorStream(new ErrorOutputStream());
    }

    /**
     * @see ProcessBuilder#redirectInput()
     */
    public Redirect redirectInput() {
        return builder.redirectInput();
    }

    /**
     * @see ProcessBuilder#redirectOutput()
     */
    public Redirect redirectOutput() {
        return builder.redirectOutput();
    }

    /**
     * @see ProcessBuilder#redirectError()
     */
    public Redirect redirectError() {
        return builder.redirectError();
    }

    /**
     * @see ProcessBuilder#inheritIO()
     */
    public Exec inheritIO() {
        builder.inheritIO();
        this.in = null;
        this.out = null;
        this.err = null;
        return this;
    }

    public Exec silentIO() {
        redirectInput((File)null);
        redirectOutput((File)null);
        redirectError((File)null);
        return this;
    }

    /**
     * @see ProcessBuilder#redirectErrorStream()
     */
    public boolean redirectErrorStream() {
        return builder.redirectErrorStream();
    }

    /**
     * @see ProcessBuilder#redirectErrorStream(boolean)
     */
    public Exec redirectErrorStream(boolean redirectErrorStream) {
        builder.redirectErrorStream(redirectErrorStream);
        return this;
    }

    /**
     * @see ProcessBuilder#start()
     */
    public Process start() throws IOException {
        Process p = builder.start();
        handleStreams(p, in, out, err);
        return p;
    }

    /**
     * @see ProcessBuilder#start()
     */
    public int run() throws IOException {
        Process p = start();

        try {
            int rc = p.waitFor();

            if (rc != 0 && err instanceof ErrorOutputStream) {
                ByteArrayOutputStream s = (ByteArrayOutputStream)err;
                throw new IOException(s.toString().trim());
            }

            return rc;
        } catch (InterruptedException ie) {
            throw new InterruptedIOException("*interrupted*");
        }
    }

    /**
     * Performs the expansion by executing command and return the contents
     * as the standard output of the command, with any trailing newlines
     * deleted.
     *
     * The output string is decoded with the given Charset parameter.
     */
    public String subst(Charset cs) throws IOException {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        redirectOutputStream(out);
        run();
        return MoreFiles.chomp(out.toByteArray(), cs);
    }

     /**
     * Performs the expansion by executing command and return the contents
     * as the standard output of the command, with any trailing newlines
     * deleted.
     *
     * The output string is decoded with the default platform charset.
     */
    public String subst() throws IOException {
         return subst(Charset.defaultCharset());
    }

    static class NullInputStream extends InputStream {
        static final NullInputStream INSTANCE = new NullInputStream();
        private NullInputStream() {}
        public int read()      { return -1; }
        public int available() { return 0; }
    }

    static class NullOutputStream extends OutputStream {
        static final NullOutputStream INSTANCE = new NullOutputStream();
        private NullOutputStream() {}
        public void write(int b) {}
        public void write(byte b[], int off, int len) {}
    }

    static class ErrorOutputStream extends ByteArrayOutputStream {
    }

    private static class StreamPumper extends Thread {
        private final InputStream in;
        private final OutputStream out;
        private final boolean doneClose;

        StreamPumper(InputStream in, OutputStream out, boolean doneClose) {
            this.in = in;
            this.out = out;
            this.doneClose = doneClose;
        }

        @Override
        public void run() {
            try {
                ByteStreams.copy(in, out);
            } catch (Exception ex) {
                // log and ignore
            } finally {
                if (doneClose) {
                    // We need to close the out, since some
                    // processes would just wait for the stream
                    // to be closed before they process its content,
                    // and produce the output.
                    closeIt(out);
                }
            }
        }
    }

    private static void handleStreams(Process p, InputStream in, OutputStream out, OutputStream err) {
        InputStream pOut = null;
        InputStream pErr = null;
        OutputStream pIn = null;

        StreamPumper tIn = null, tOut = null, tErr = null;

        if (in != null) {
            pIn = p.getOutputStream();
            tIn = new StreamPumper(in, pIn, true);
            tIn.start();
        }

        if (out != null) {
            pOut = p.getInputStream();
            tOut = new StreamPumper(pOut, out, false);
            tOut.start();
        }

        if (err != null) {
            pErr = p.getErrorStream();
            tErr = new StreamPumper(pErr, err, false);
            tErr.start();
        }

        if (out != null) {
            joinIt(tOut);
            flushIt(out);
            closeIt(pOut);
        }

        if (err != null) {
            joinIt(tErr);
            flushIt(err);
            closeIt(pErr);
        }

        if (in != null) {
            joinIt(tIn);
            closeIt(pIn);
        }
    }

    private static void closeIt(Closeable s) {
        try { s.close(); } catch (IOException ex) {}
    }

    private static void flushIt(Flushable s) {
        try { s.flush(); } catch (IOException ex) {}
    }

    private static void joinIt(Thread t) {
        try { t.join(); } catch (InterruptedException ex) {}
    }
}
