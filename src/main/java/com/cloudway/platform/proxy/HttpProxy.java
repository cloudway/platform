/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.proxy;

import java.io.IOException;
import java.util.Collection;
import java.util.Map;

import com.cloudway.platform.proxy.apache.ApacheProxy;

@SuppressWarnings("unused")
public interface HttpProxy
{
    static HttpProxy getInstance() {
        return ApacheProxy.INSTANCE; // FIXME
    }

    void addMapping(String path, String uri)
        throws IOException;

    void addMappings(Map<String, String> mappings)
        throws IOException;

    void removeMapping(String path)
        throws IOException;

    void removeMappings(Collection<String> paths)
        throws IOException;

    void addAlias(String name, String fqdn)
        throws IOException;

    void removeAlias(String name)
        throws IOException;

    void purge(String fqdn)
        throws IOException;
}
