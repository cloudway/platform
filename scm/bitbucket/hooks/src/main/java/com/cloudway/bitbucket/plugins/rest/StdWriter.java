/**
 * Cloudway Platform
 * Copyright (c) 2012-2016 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.bitbucket.plugins.rest;

import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;

public class StdWriter extends FilterOutputStream {
    // Stdin represents standard input stream type.
    public static final byte Stdin = 0;
    // Stdout represents standard output stream type.
    public static final byte Stdout = 1;
    // Stderr represents standard error stream type.
    public static final byte Stderr = 2;
    // Data represents application data stream type.
    public static final byte Data = 3;

    private static final int stdWriterPrefixLen = 8;
    private static final int stdWriterFdIndex = 0;
    private static final int stdWriterSizeIndex = 4;

    private final byte prefix;

    public StdWriter(OutputStream out, byte prefix) {
        super(out);
        this.prefix = prefix;
    }

    @Override
    public void write(int b) throws IOException {
        byte[] buf = new byte[1];
        buf[0] = (byte)(b & 0xff);
        write(buf, 0, 1);
    }

    @Override
    public void write(byte[] b, int off, int len) throws IOException {
        if ((off | len | (b.length - (len + off)) | (off + len)) < 0)
            throw new IndexOutOfBoundsException();
        if (len == 0)
            return;

        ByteBuffer header = ByteBuffer.allocate(stdWriterPrefixLen);
        header.order(ByteOrder.BIG_ENDIAN);
        header.put(stdWriterFdIndex, prefix);
        header.putInt(stdWriterSizeIndex, len);

        synchronized (out) {
            out.write(header.array());
            out.write(b, off, len);
            out.flush();
        }
    }
}
