/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util;

import java.io.IOException;
import java.io.UncheckedIOException;

/**
 * Represents an action that perform I/O operation.
 */
@FunctionalInterface
public interface IOAction
{
    /**
     * Performs the I/O operation
     *
     * @throws IOException if a I/O error occurs
     */
    void perform() throws IOException;

    /**
     * Wraps the I/O action into a regular action. If the I/O action throws
     * {@link IOException}, then the wrapped action wraps the IOException to
     * an {@link UncheckedIOException} and thrown.
     *
     * @param other the I/O action
     * @return the regular action
     */
    static Runnable wrap(IOAction other) {
        return () -> {
            try {
                other.perform();
            } catch (IOException ex) {
                throw new UncheckedIOException(ex);
            }
        };
    }
}
