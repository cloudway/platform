/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container.proxy;

import java.io.IOException;
import java.util.Collection;
import java.util.function.Supplier;

import com.cloudway.platform.container.ApplicationContainer;
import com.cloudway.platform.container.proxy.apache.ApacheProxyUpdater;

/**
 * The public interface for proxy mapping.
 */
public final class HttpProxy
{
    private static final Supplier<HttpProxyUpdater> DEFAULT_PROVIDER
        = () -> ApacheProxyUpdater.INSTANCE;
    private static Supplier<HttpProxyUpdater> _provider = DEFAULT_PROVIDER;

    public static void setProvider(Supplier<HttpProxyUpdater> provider) {
        _provider = provider != null ? provider : DEFAULT_PROVIDER;
    }

    private static HttpProxyUpdater provider() {
        return _provider.get();
    }

    private HttpProxy() {}

    /**
     * Add mappings from frontend URI to backend URI.
     */
    public static void addMappings(ApplicationContainer container, Collection<ProxyMapping> mappings)
        throws IOException {
        provider().addMappings(container, mappings);
    }

    /**
     * Remove mappings from frontend URI to backend URI.
     */
    public static void removeMappings(ApplicationContainer container, Collection<ProxyMapping> mappings)
        throws IOException {
        provider().removeMappings(container, mappings);
    }

    /**
     * Add an alias for the given name to a fully qualified domain name.
     */
    public static void addAlias(String name, String fqdn) throws IOException {
        provider().addAlias(name, fqdn);
    }

    /**
     * Remove the alias of given name.
     */
    public static void removeAlias(String name) throws IOException {
        provider().removeAlias(name);
    }

    /**
     * Make the application idle.  Activate the application when it's
     * accessed from outside world.
     */
    public static void idle(ApplicationContainer container) throws IOException {
        provider().idle(container);
    }

    /**
     * Explicitly unidle an application.
     */
    public static boolean unidle(ApplicationContainer container) throws IOException {
        return provider().unidle(container);
    }

    /**
     * Check to see if the application is idle.
     */
    public static boolean isIdle(ApplicationContainer container) {
        return provider().isIdle(container);
    }

    /**
     * Remove all proxy information for the container.
     */
    public static void purge(ApplicationContainer container) throws IOException {
        provider().purge(container);
    }
}
