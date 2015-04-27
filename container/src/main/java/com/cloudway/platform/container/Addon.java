/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import com.cloudway.platform.common.util.MoreFiles;
import com.cloudway.platform.container.proxy.ProxyMapping;
import com.google.common.base.MoreObjects;
import com.google.common.collect.ImmutableList;

import static com.cloudway.fp.control.Syntax.*;
import static com.cloudway.platform.common.util.MoreCollectors.*;
import static com.google.common.base.Strings.isNullOrEmpty;

public class Addon
{
    private final Path           path;
    private final String         shortName;
    private final String         displayName;
    private final String         version;
    private final AddonType      type;
    private final List<Endpoint> endpoints;
    private final Exception      failure;

    private static Path metadataFile(Path dir) {
        return MoreFiles.join(dir, "metadata", "addon.xml");
    }

    public static boolean isAddonDirectory(Path dir) {
        try {
            return !Files.isHidden(dir) && Files.exists(metadataFile(dir));
        } catch (IOException ex) {
            return false;
        }
    }

    public static Addon load(Path path, Map<String,String> env) {
        MetaData.Addon metadata = null;
        Exception failure = null;
        try {
            metadata = MetaData.load(metadataFile(path));
        } catch (Exception ex) {
            failure = ex;
        }
        return new Addon(path, metadata, env, failure);
    }

    private Addon(Path path, MetaData.Addon metadata, Map<String,String> env, Exception failure) {
        this.path = path;
        this.failure = failure;

        if (metadata != null) {
            this.shortName   = metadata.name;
            this.displayName = metadata.displayName;
            this.version     = metadata.version;
            this.type        = metadata.category;
            this.endpoints   = endpoints(metadata, env);
        } else {
            this.shortName   = null;
            this.displayName = null;
            this.version     = null;
            this.type        = null;
            this.endpoints   = ImmutableList.of();
        }
    }

    private Addon(Path path, Addon meta) {
        this.path        = path;
        this.shortName   = meta.shortName;
        this.displayName = meta.displayName;
        this.version     = meta.version;
        this.type        = meta.type;
        this.endpoints   = meta.endpoints;
        this.failure     = meta.failure;
    }

    public Addon copyOf(Path path) {
        return new Addon(path, this);
    }

    private ImmutableList<Endpoint> endpoints(MetaData.Addon metadata, Map<String,String> env) {
        // @formatter:off
        return metadata.endpoints.stream().map(ep -> {
            Endpoint endpoint = new Endpoint();
            String env_prefix = "CLOUDWAY_" + shortName + "_";

            endpoint.setPrivateHostName((env_prefix + ep.privateHostName).toUpperCase());
            endpoint.setPrivatePortName((env_prefix + ep.privatePortName).toUpperCase());
            endpoint.setPrivateHost(env.get(endpoint.getPrivateHostName()));
            endpoint.setPrivatePort(ep.privatePort);

            endpoint.setProxyMappings(select.
                from(ep.proxyMappings, (MetaData.ProxyMapping pm) ->
                from(split(pm.protocols), (String p) ->
                yield(new ProxyMapping(pm.frontend, pm.backend, p))))
                .collect(toImmutableList())
            );

            return endpoint;
        }).collect(toImmutableList());
        // @formatter:on
    }

    private static Stream<String> split(String protocols) {
        return isNullOrEmpty(protocols) ? Stream.of("http") : Stream.of(protocols.split(","));
    }

    public Path getPath() {
        return path;
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
        return endpoints;
    }

    public List<ProxyMapping> getProxyMappings() {
        return endpoints.stream().flatMap(ep ->
            ep.getProxyMappings().stream().map((ProxyMapping pm) ->
                new ProxyMapping(getFrontendUri(pm), getBackendUri(ep, pm), pm.getProtocol())
            )).collect(toImmutableList());
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

    public String toString() {
        if (failure == null) {
            return MoreObjects.toStringHelper(this)
                .add("path",        path)
                .add("shortName",   shortName)
                .add("displayName", displayName)
                .add("version",     version)
                .add("type",        type)
                .add("endpoints",   endpoints)
                .toString();
        } else {
            return MoreObjects.toStringHelper(this)
                .add("path",        path)
                .add("failure",     failure)
                .toString();
        }
    }

    public class Endpoint {
        private String privateHostName;
        private String privatePortName;
        private String privateHost;
        private int    privatePort;
        private List<ProxyMapping> mappings;

        public String getPrivateHostName() {
            return privateHostName;
        }

        void setPrivateHostName(String privateHostName) {
            this.privateHostName = privateHostName;
        }

        public String getPrivatePortName() {
            return privatePortName;
        }

        void setPrivatePortName(String privatePortName) {
            this.privatePortName = privatePortName;
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

        List<ProxyMapping> getProxyMappings() {
            return mappings;
        }

        void setProxyMappings(List<ProxyMapping> mappings) {
            this.mappings = mappings;
        }

        public String getInfo() {
            String prefix = "CLOUDWAY_" + Addon.this.getName() + "_";
            return Addon.this.getDisplayName() + ", " +
                getPrivateHostName().substring(prefix.length()) + ":" +
                getPrivatePortName().substring(prefix.length());
        }

        public String toString() {
            return MoreObjects.toStringHelper(this)
                .add("privateHostName", privateHostName)
                .add("privatePortName", privatePortName)
                .add("privateHost",     privateHost)
                .add("privatePort",     privatePort)
                .add("mappings",        mappings)
                .toString();
        }
    }
}
