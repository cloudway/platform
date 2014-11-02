/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import com.cloudway.platform.common.util.FileUtils;

public class Addon
{
    private final ApplicationContainer container;
    private Path path;

    private String          shortName;
    private String          displayName;
    private String          version;
    private AddonType       type;
    private List<Endpoint>  endpoints = new ArrayList<>();

    private Exception failure;

    public static boolean isAddonDirectory(Path dir) {
        try {
            return !Files.isHidden(dir) && Files.exists(FileUtils.join(dir, "metadata", "addon.xml"));
        } catch (IOException ex) {
            return false;
        }
    }

    public static Addon load(ApplicationContainer container, Path path) {
        Addon addon = new Addon(container, path);
        Map<String, String> env = Environ.loadAll(container);
        Path file = FileUtils.join(path, "metadata", "addon.xml");

        try {
            MetaData.Addon metadata = MetaData.load(file);

            addon.shortName   = metadata.name;
            addon.displayName = metadata.displayName;
            addon.version     = metadata.version;
            addon.type        = metadata.category;

            metadata.endpoints.stream().forEach(ep -> {
                Endpoint endpoint = addon.new Endpoint(
                    ("CLOUDWAY_" + addon.shortName + "_" + ep.privateHostName).toUpperCase(),
                    ("CLOUDWAY_" + addon.shortName + "_" + ep.privatePortName).toUpperCase()
                );
                endpoint.setPrivateHost(env.get(endpoint.getPrivateHostName()));
                endpoint.setPrivatePort(ep.privatePort);
                ep.proxyMappings.stream().forEach(map ->
                    endpoint.addProxyMapping(map.frontend, map.backend));
                addon.endpoints.add(endpoint);
            });
        } catch (Exception ex) {
            addon.failure = ex;
        }

        return addon;
    }

    private Addon(ApplicationContainer container, Path path) {
        this.container = container;
        this.path = path;
    }

    public Path getPath() {
        return path;
    }

    public void setPath(Path path) {
        this.path = path;
    }

    public String getName() {
        return shortName;
    }

    public String getDisplayName() {
        return displayName;
    }

    public String getVersion() {
        return version;
    }

    public AddonType getType() {
        return type;
    }

    public boolean isValid() {
        return failure == null;
    }

    public void validate() {
        if (failure != null) {
            throw new IllegalStateException(failure.getMessage(), failure);
        }
    }

    public List<Endpoint> getEndpoints() {
        return Collections.unmodifiableList(endpoints);
    }

    public Map<String, String> getProxyMappings() {
        Map<String, String> mappings = new LinkedHashMap<>();
        endpoints.forEach(ep ->
            ep.getProxyMappings().forEach((f,b) ->
                mappings.put(getFrontendUri(f), getBackendUri(ep, b))));
        return mappings;
    }

    private String getFrontendUri(String uri) {
        if (uri.endsWith("/"))
            uri = uri.substring(0, uri.length()-1);
        if (!uri.isEmpty() && !uri.startsWith("/"))
            uri = "/" + uri;
        return container.getDomainName() + uri;
    }

    private String getBackendUri(Endpoint ep, String uri) {
        if (uri.startsWith("/") && uri.endsWith("/")) {
            uri = uri.substring(0, uri.length()-1);
        }
        if (uri.isEmpty() || uri.startsWith("/")) {
            return ep.getPrivateHost() + ":" + ep.getPrivatePort() + uri;
        } else {
            return uri; // GONE, FORBIDDEN, REDIRECT:/url, etc
        }
    }

    public class Endpoint {
        private String privateHostName;
        private String privatePortName;
        private String privateHost;
        private int    privatePort;
        private Map<String,String> mappings;

        Endpoint(String privateHostName, String privatePortName) {
            this.privateHostName = privateHostName;
            this.privatePortName = privatePortName;
        }

        public String getPrivateHostName() {
            return privateHostName;
        }

        public String getPrivatePortName() {
            return privatePortName;
        }

        public String getPrivateHost() {
            return privateHost;
        }

        public void setPrivateHost(String host) {
            this.privateHost = host;
        }

        public int getPrivatePort() {
            return privatePort;
        }

        public void setPrivatePort(int port) {
            this.privatePort = port;
        }

        void addProxyMapping(String frontend, String backend) {
            if (mappings == null)
                mappings = new LinkedHashMap<>();
            mappings.put(frontend, backend);
        }

        Map<String, String> getProxyMappings() {
            return mappings == null ? Collections.emptyMap() : mappings;
        }

        public String getInfo() {
            String prefix = "CLOUDWAY_" + Addon.this.getName() + "_";
            return Addon.this.getDisplayName() + ", " +
                getPrivateHostName().substring(prefix.length()) + ":" +
                getPrivatePortName().substring(prefix.length());
        }
    }
}
