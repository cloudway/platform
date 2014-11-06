/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container.proxy;

public class ProxyMapping
{
    private final String frontend;
    private final String backend;
    private final String protocol;

    public ProxyMapping(String frontend, String backend, String protocol) {
        this.frontend = frontend;
        this.backend = backend;
        this.protocol = protocol;
    }

    public String getFrontend() {
        return frontend;
    }

    public String getBackend() {
        return backend;
    }

    public String getProtocol() {
        return protocol;
    }
}
