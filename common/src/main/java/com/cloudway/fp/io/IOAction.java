/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.io;

import java.io.IOException;
import java.io.UncheckedIOException;

import com.cloudway.fp.function.ExceptionAction;

/**
 * Represents an action that perform I/O operation.
 */
@FunctionalInterface
public interface IOAction extends Runnable, ExceptionAction<IOException>
{
    /**
     * Performs the I/O operation by wrapping {@link IOException} to
     * an {@link UncheckedIOException}.
     *
     * @throws UncheckedIOException when an I/O error occurs
     */
    @Override
    default void run() {
        try {
            perform();
        } catch (IOException ex) {
            throw new UncheckedIOException(ex);
        }
    }

    /**
     * Wraps the I/O action into a regular action. If the I/O action throws
     * {@link IOException}, then the wrapped action wraps the IOException to
     * an {@link UncheckedIOException} and thrown.
     *
     * @param action the I/O action
     * @return the regular action
     */
    static Runnable wrap(IOAction action) {
        return action;
    }
}
