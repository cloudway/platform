/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container;

import java.io.BufferedReader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import com.cloudway.platform.common.util.FileUtils;

public class Addon
{
    private final ApplicationContainer container;
    private final Path path;
    private AddonType type;
    private final List<Endpoint> endpoints = new ArrayList<>();
    private Exception failure;

    public static boolean isAddonDirectory(Path dir) {
        return Files.exists(FileUtils.join(dir, "metadata", "addon.xml"));
    }

    public static Addon load(ApplicationContainer container, Path path) {
        Addon addon = new Addon(container, path);
        String shortName = addon.getName().toUpperCase();
        Map<String, String> env = Environ.loadAll(container);

        Path file = FileUtils.join(path, "metadata", "addon.xml");
        try (BufferedReader reader = Files.newBufferedReader(file)) {
            com.cloudway.platform.container.schema.addon.Addon metadata =
                (com.cloudway.platform.container.schema.addon.Addon)
                com.cloudway.platform.container.schema.addon.Addon.unmarshal(reader);

            addon.type = AddonType.valueOf(metadata.getCategory().toUpperCase());

            Stream.of(metadata.getEndpoint()).forEach(ep -> {
                Endpoint endpoint = new Endpoint(
                    "CLOUDWAY_" + shortName + "_" + ep.getPrivateIpName(),
                    "CLOUDWAY_" + shortName + "_" + ep.getPrivatePortName()
                );
                endpoint.setPrivateIP(env.get(endpoint.getPrivateIPName()));
                endpoint.setPrivatePort(ep.getPrivatePort());
                Stream.of(ep.getProxyMapping()).forEach(map ->
                    endpoint.addProxyMapping(map.getFrontEnd(), map.getBackEnd()));
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

    public String getName() {
        return path.getFileName().toString();
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
                mappings.put(getFrontEndUri(f), getBackEndUri(ep, b))));
        return mappings;
    }

    private String getFrontEndUri(String uri) {
        if (uri.endsWith("/"))
            uri = uri.substring(0, uri.length()-1);
        if (!uri.isEmpty() && !uri.startsWith("/"))
            uri = "/" + uri;
        return container.getDomainName() + uri;
    }

    private String getBackEndUri(Endpoint ep, String uri) {
        if (uri.startsWith("/") && uri.endsWith("/")) {
            uri = uri.substring(0, uri.length()-1);
        }
        if (uri.isEmpty() || uri.startsWith("/")) {
            return ep.getPrivateIP() + ":" + ep.getPrivatePort() + uri;
        } else {
            return uri; // GONE, FORBIDDEN, REDIRECT:/url, etc
        }
    }

    public static class Endpoint {
        private String privateIPName;
        private String privatePortName;
        private String privateIP;
        private int    privatePort;
        private Map<String,String> mappings;

        Endpoint(String privateIPName, String privatePortName) {
            this.privateIPName   = privateIPName;
            this.privatePortName = privatePortName;
        }

        public String getPrivateIPName() {
            return privateIPName;
        }

        public String getPrivatePortName() {
            return privatePortName;
        }

        public String getPrivateIP() {
            return privateIP;
        }

        public void setPrivateIP(String ip) {
            this.privateIP = ip;
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
    }
}
