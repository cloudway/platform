/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

public class Addon
{
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

    private final Path path;
    private final List<Endpoint> endpoints = new ArrayList<>();

    public Addon(Path path) {
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

    public List<Endpoint> getEndpoints() {
        return endpoints;
    }
}
