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
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.cloudway.platform.common.util.FileUtils;
import com.cloudway.platform.container.proxy.ProxyMapping;

public class Addon
{
    private Path            path;
    private String          shortName;
    private String          displayName;
    private String          version;
    private AddonType       type;
    private List<Endpoint>  endpoints = new ArrayList<>();
    private Exception       failure;

    public static boolean isAddonDirectory(Path dir) {
        try {
            return !Files.isHidden(dir) && Files.exists(FileUtils.join(dir, "metadata", "addon.xml"));
        } catch (IOException ex) {
            return false;
        }
    }

    public static Addon load(ApplicationContainer container, Path path) {
        return new Addon(container, path);
    }

    private Addon(ApplicationContainer container, Path path) {
        this.path = path;

        Map<String, String> env = Environ.loadAll(container);
        Path file = FileUtils.join(path, "metadata", "addon.xml");

        try {
            MetaData.Addon metadata = MetaData.load(file);

            this.shortName   = metadata.name;
            this.displayName = metadata.displayName;
            this.version     = metadata.version;
            this.type        = metadata.category;

            metadata.endpoints.stream().forEach(ep -> {
                Endpoint endpoint = new Endpoint(
                    ("CLOUDWAY_" + this.shortName + "_" + ep.privateHostName).toUpperCase(),
                    ("CLOUDWAY_" + this.shortName + "_" + ep.privatePortName).toUpperCase()
                );

                endpoint.setPrivateHost(env.get(endpoint.getPrivateHostName()));
                endpoint.setPrivatePort(ep.privatePort);

                ep.proxyMappings.forEach(pm -> {
                    if (pm.protocols == null || pm.protocols.isEmpty()) {
                        endpoint.addProxyMapping(pm.frontend, pm.backend, "http");
                    } else {
                        Stream.of(pm.protocols.split(",")).forEach(protocol ->
                            endpoint.addProxyMapping(pm.frontend, pm.backend, protocol));
                    }
                });

                this.endpoints.add(endpoint);
            });
        } catch (Exception ex) {
            this.failure = ex;
        }
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

    public List<ProxyMapping> getProxyMappings() {
        return endpoints.stream().flatMap(ep ->
            ep.getProxyMappings().stream().map((ProxyMapping pm) ->
                new ProxyMapping(getFrontendUri(pm), getBackendUri(ep, pm), pm.getProtocol())
            )).collect(Collectors.toList());
    }

    static String getFrontendUri(ProxyMapping pm) {
        String uri = pm.getFrontend();
        if (uri.endsWith("/"))
            uri = uri.substring(0, uri.length()-1);
        if (!uri.isEmpty() && !uri.startsWith("/"))
            uri = "/" + uri;
        return uri;
    }

    static String getBackendUri(Endpoint ep, ProxyMapping pm) {
        String uri = pm.getBackend();
        if (uri.startsWith("/") && uri.endsWith("/")) {
            uri = uri.substring(0, uri.length()-1);
        }
        if (uri.isEmpty() || uri.startsWith("/")) {
            return pm.getProtocol() + "://" + ep.getPrivateHost() + ":" + ep.getPrivatePort() + uri;
        } else {
            return uri; // GONE, FORBIDDEN, REDIRECT:/url, etc
        }
    }

    public class Endpoint {
        private String privateHostName;
        private String privatePortName;
        private String privateHost;
        private int    privatePort;
        private List<ProxyMapping> mappings;

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

        void addProxyMapping(String frontend, String backend, String protocol) {
            if (mappings == null)
                mappings = new ArrayList<>();
            mappings.add(new ProxyMapping(frontend, backend, protocol));
        }

        List<ProxyMapping> getProxyMappings() {
            return mappings == null ? Collections.emptyList() : mappings;
        }

        public String getInfo() {
            String prefix = "CLOUDWAY_" + Addon.this.getName() + "_";
            return Addon.this.getDisplayName() + ", " +
                getPrivateHostName().substring(prefix.length()) + ":" +
                getPrivatePortName().substring(prefix.length());
        }
    }
}
