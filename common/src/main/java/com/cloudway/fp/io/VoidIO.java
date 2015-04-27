/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.io;

import java.io.IOException;
import com.cloudway.fp.data.Unit;

/**
 * A special I/O action that has no result generated.
 */
@FunctionalInterface
public interface VoidIO extends IO<Unit> {
    /**
     * Perform the I/O action.
     *
     * @throws IOException if I/O error occurred
     */
    void runVoidIO() throws IOException;

    /**
     * Performs the I/O action and return unit value.
     *
     * @return the unit value
     * @throws IOException if I/O error occurred
     */
    @Override
    default Unit runIO() throws IOException {
        runVoidIO();
        return Unit.U;
    }
}
