/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util;

import java.io.IOException;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;

/**
 * Wraps an IOException to a RuntimeException that can be thrown
 * from a lambda expression.
 */
public class RuntimeIOException extends RuntimeException
{
    private static final long serialVersionUID = 5708208325646880795L;

    public RuntimeIOException(IOException ex) {
        super(ex);
    }

    public IOException getCause() {
        return (IOException)super.getCause();
    }
}
