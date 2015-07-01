/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.scheme;

import java.io.IOException;

/**
 * The interface for scheme output port. Similar to java.io.Writer.
 */
public interface OutputPort extends LispVal {
    /**
     * Write a single character. The character to be written is contained
     * in the 16 low-order bits of the given integer value; the 16 high-order
     * bits are ignored.
     *
     * @param c specifying a character to be written
     * @throws IOException if an I/O error occurs
     */
    void write(int c) throws IOException;

    /**
     * Writes an array of characters.
     *
     * @param cbuf Array of characters to be written
     * @throws IOException if an I/O error occurs
     */
    void write(char[] cbuf) throws IOException;

    /**
     * Writes a string.
     *
     * @param s String to be written
     * @throws IOException if an I/O error occurs
     */
    void write(String s) throws IOException;

    /**
     * Flushes the output port.
     *
     * @throws IOException if an I/O error occurs
     */
    void flush() throws IOException;

    /**
     * Returns true if the output port supports auto flushing.
     *
     * @return true if the output port supports auto flushing.
     */
    default boolean autoFlush() {
        return false;
    }

    /**
     * Closes the output port.
     */
    void close();
}
