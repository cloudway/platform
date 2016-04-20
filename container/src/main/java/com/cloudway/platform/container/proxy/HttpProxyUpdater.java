/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container.proxy;

import java.io.IOException;
import java.util.Collection;

import com.cloudway.platform.container.Container;

/**
 * The adapter interface for concrete proxy implementation.
 */
public interface HttpProxyUpdater
{
    void addMappings(Container container, Collection<ProxyMapping> mappings)
        throws IOException;

    void removeMappings(Container container, Collection<ProxyMapping> mappings)
        throws IOException;

    void addAlias(String name, String fqdn)
        throws IOException;

    void removeAlias(String name)
        throws IOException;

    void idle(Container container)
        throws IOException;

    boolean unidle(Container container)
        throws IOException;

    boolean isIdle(Container container);

    void purge(Container container)
        throws IOException;
}
