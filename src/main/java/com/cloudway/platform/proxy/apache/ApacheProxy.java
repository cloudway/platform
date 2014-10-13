/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.proxy.apache;

import java.io.IOException;
import java.util.Collection;
import java.util.Map;

import com.cloudway.platform.proxy.HttpProxy;

public class ApacheProxy implements HttpProxy
{
    public static final ApacheProxy INSTANCE = new ApacheProxy();

    private ApacheDB mappings = new ApacheDB("mappings");
    private ApacheDB aliases  = new ApacheDB("aliases");

    public void addMapping(String path, String uri)
        throws IOException
    {
        mappings.writting(d -> d.put(path, uri));
    }

    public void addMappings(Map<String, String> m)
        throws IOException
    {
        mappings.writting(d -> d.putAll(m));
    }

    public void removeMapping(String path)
        throws IOException
    {
        mappings.writting(d -> d.remove(path));
    }

    public void removeMappings(Collection<String> paths)
        throws IOException
    {
        mappings.writting(d -> d.keySet().removeAll(paths));
    }

    public void addAlias(String name, String fqdn)
        throws IOException
    {
        aliases.writting(d -> d.put(name, fqdn));
    }

    public void removeAlias(String name)
        throws IOException
    {
        aliases.writting(d -> d.remove(name));
    }

    public void purge(String fqdn) throws IOException {
        mappings.writting(d -> d.keySet().removeIf(k -> {
            int i = k.indexOf('/');
            if (i != -1)
                k = k.substring(0, i);
            return fqdn.equals(k);
        }));

        aliases.writting(d -> d.values().remove(fqdn));
    }
}
