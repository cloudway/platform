/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container.proxy;

import java.io.IOException;
import java.util.Collection;
import java.util.Map;

import com.cloudway.platform.container.ApplicationContainer;
import com.cloudway.platform.container.proxy.apache.ApacheProxy;

public interface HttpProxy
{
    static HttpProxy getInstance() {
        return ApacheProxy.INSTANCE; // FIXME
    }

    void addMappings(ApplicationContainer container, Collection<ProxyMapping> mappings)
        throws IOException;

    void removeMappings(ApplicationContainer container, Collection<ProxyMapping> mappings)
        throws IOException;

    void addAlias(String name, String fqdn)
        throws IOException;

    void removeAlias(String name)
        throws IOException;

    void idle(ApplicationContainer container)
        throws IOException;

    boolean unidle(ApplicationContainer container)
        throws IOException;

    boolean isIdle(ApplicationContainer container);

    void purge(ApplicationContainer container)
        throws IOException;
}
