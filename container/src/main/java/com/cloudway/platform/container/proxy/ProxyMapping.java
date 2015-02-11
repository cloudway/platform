/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container.proxy;

import java.util.Objects;
import com.google.common.base.MoreObjects;

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

    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!(obj instanceof ProxyMapping))
            return false;
        ProxyMapping other = (ProxyMapping)obj;
        return Objects.equals(frontend, other.frontend)
            && Objects.equals(backend, other.backend)
            && Objects.equals(protocol, other.protocol);
    }

    public int hashCode() {
        return Objects.hash(frontend, backend, protocol);
    }

    public String toString() {
        return MoreObjects.toStringHelper(this)
            .add("frontend", frontend)
            .add("backend",  backend)
            .add("protocol", protocol)
            .toString();
    }
}
