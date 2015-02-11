/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container;

import java.io.IOException;
import java.nio.charset.Charset;

import com.cloudway.platform.common.os.Exec;

public class FakeExec extends Exec {
    private int exitCode;
    private String output = "";
    private String error;
    private boolean checkError;

    public FakeExec(Exec wrapped) {
        super(new ProcessBuilder(wrapped.command()));
    }

    public FakeExec withExitCode(int exitCode) {
        this.exitCode = exitCode;
        return this;
    }

    public FakeExec withOutput(String output) {
        this.output = output;
        return this;
    }

    public FakeExec withError(String error) {
        this.error = error;
        return this;
    }

    @Override
    public Exec checkError() {
        checkError = true;
        return super.checkError();
    }

    @Override
    public Process start() throws IOException {
        throw new UnsupportedOperationException();
    }

    @Override
    public int run() throws IOException {
        if (checkError && error != null)
            throw new IOException(error);
        return exitCode;
    }

    @Override
    public String subst(Charset cs) throws IOException {
        if (checkError && error != null)
            throw new IOException(error);
        return output;
    }

    @Override
    public String subst() throws IOException {
        return subst(Charset.defaultCharset());
    }
}
