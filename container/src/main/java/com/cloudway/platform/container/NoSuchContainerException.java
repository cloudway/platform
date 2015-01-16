/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container;

public class NoSuchContainerException extends RuntimeException
{
    private static final long serialVersionUID = -4964332477801076211L;

    private final String id;

    public NoSuchContainerException(String id, String message) {
        super(message);
        this.id = id;
    }

    public NoSuchContainerException(String id, String message, Throwable cause) {
        super(message, cause);
        this.id = id;
    }

    public String getContainerId() {
        return id;
    }
}
