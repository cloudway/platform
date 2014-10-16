/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import com.cloudway.platform.common.util.ExtendedProperties;

public class Config
{
    private final ExtendedProperties conf;

    private static final Map<Path, ExtendedProperties> conf_parsed = new HashMap<>();
    private static final Map<Path, Long> conf_mtimes = new HashMap<>();

    public static final Path HOME_DIR, CONF_DIR, VAR_DIR, LOG_DIR;
    public static final Path LOCK_DIR;

    private static final String HOME_DIR_KEY = "CLOUDWAY_HOME";
    private static final String DEFAULT_HOME_DIR = "/opt/cloudway";
    private static final String CONF_DIR_KEY = "CLOUDWAY_CONF";
    private static final String DEFAULT_CONF_DIR = "/etc/cloudway";
    private static final String VAR_DIR_KEY = "CLOUDWAY_VAR_DIR";
    private static final String DEFAULT_VAR_DIR = "/var/lib/cloudway";
    private static final String LOG_DIR_KEY = "CLOUDWAY_LOCK_DIR";
    private static final String DEFAULT_LOG_DIR = "/var/log/cloudway";

    static {
        HOME_DIR = Paths.get(getProperty(HOME_DIR_KEY, DEFAULT_HOME_DIR)).toAbsolutePath().normalize();
        CONF_DIR = Paths.get(getProperty(CONF_DIR_KEY, DEFAULT_CONF_DIR)).toAbsolutePath().normalize();
        VAR_DIR  = Paths.get(getProperty(VAR_DIR_KEY, DEFAULT_VAR_DIR)).toAbsolutePath().normalize();
        LOG_DIR  = Paths.get(getProperty(LOG_DIR_KEY, DEFAULT_LOG_DIR)).toAbsolutePath().normalize();
        LOCK_DIR = VAR_DIR.resolve(".lock");
    }

    private static String getProperty(String key, String defaultValue) {
        String value = System.getProperty(key);
        if (value == null) {
            value = System.getenv(key);
            if (value == null) {
                value = defaultValue;
            }
        }
        return value;
    }

    public static Config getDefault() {
        return new Config("node.conf");
    }

    public Config() {
        this(null, null);
    }

    public Config(String name) {
        this(CONF_DIR.resolve(name), null);
    }

    public Config(Path path, Properties defaults) {
        if (path != null) {
            synchronized (conf_parsed) {
                try {
                    long conf_mtime = Files.getLastModifiedTime(path).toMillis();
                    if (!conf_parsed.containsKey(path) || conf_mtime != conf_mtimes.get(path)) {
                        ExtendedProperties conf = new ExtendedProperties(defaults);
                        try (InputStream in = Files.newInputStream(path)) {
                            conf.load(in);
                        }
                        conf_parsed.put(path, conf);
                        conf_mtimes.put(path, conf_mtime);
                    }
                    this.conf = conf_parsed.get(path);
                } catch (IOException ex) {
                    throw new IllegalArgumentException(
                        String.format("Could not open config file %s: %s", path, ex.getMessage()));
                }
            }
        } else {
            this.conf = new ExtendedProperties(defaults);
        }
    }

    public String get(String name) {
        String value = System.getProperty(name);
        if (value == null)
            value = conf.getProperty(name);
        return value;
    }

    public String get(String name, String deflt) {
        String value = System.getProperty(name);
        if (value == null)
            value = conf.getProperty(name, deflt);
        return value;
    }

    public boolean getBool(String name) {
        return getBool(name, false);
    }

    public boolean getBool(String name, boolean deflt) {
        String value = get(name);
        return value != null ? Boolean.valueOf(value) : deflt;
    }

    public int getInt(String name) {
        return getInt(name, -1);
    }

    public int getInt(String name, int deflt) {
        String value = get(name);
        return value != null ? Integer.parseInt(value) : deflt;
    }

    public Collection<String> keys() {
        return conf.stringPropertyNames();
    }

    public ExtendedProperties category(String name) {
        return conf.category(name);
    }

    public Set<String> categoryKeys() {
        return conf.categoryKeys();
    }

    public String toString() {
        return conf.toString();
    }
}
