/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import com.cloudway.platform.common.util.FileUtils;

public class Addon
{
    public static boolean isAddonDirectory(Path dir) {
        return Files.exists(FileUtils.join(dir, "metadata", "addon.xml"));
    }

    public static Addon load(Path path) {
        Addon addon = new Addon(path);

        // TODO: for test only
        Endpoint endpoint = new Endpoint();
        endpoint.setPrivateIPName("CLOUDWAY_" + addon.getName().toUpperCase() + "_IP");
        endpoint.setPrivatePortName("CLOUDWAY_" + addon.getName().toUpperCase() + "_PORT");
        endpoint.setPrivatePort(8080);
        addon.getEndpoints().add(endpoint);

        return addon;
    }

    private final Path path;
    private final List<Endpoint> endpoints = new ArrayList<>();

    private Addon(Path path) {
        this.path = path;
    }

    public Path getPath() {
        return path;
    }

    public String getName() {
        return path.getFileName().toString();
    }

    public AddonType getType() {
        return AddonType.FRAMEWORK; // FIXME
    }

    public boolean isValid() {
        return true; // FIXME
    }

    public List<Endpoint> getEndpoints() {
        return endpoints;
    }

    public static class Endpoint {
        private String privateIPName;
        private String privatePortName;
        private String privateIP;
        private int    privatePort;

        public String getPrivateIPName() {
            return privateIPName;
        }

        public void setPrivateIPName(String name) {
            this.privateIPName = name;
        }

        public String getPrivatePortName() {
            return privatePortName;
        }

        public void setPrivatePortName(String name) {
            this.privatePortName = name;
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
    }
}
