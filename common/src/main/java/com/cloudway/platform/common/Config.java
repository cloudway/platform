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
import java.util.Optional;
import static java.util.Objects.requireNonNull;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.google.common.util.concurrent.Futures;
import com.google.common.util.concurrent.ListenableFuture;

import com.cloudway.platform.common.util.Optionals;
import com.cloudway.platform.common.util.ExtendedProperties;

public class Config
{
    @SuppressWarnings("serial")
    private static class Configuration extends ExtendedProperties {
        long mtime;
    }

    private static class ConfigurationLoader extends CacheLoader<Path, Configuration> {
        @Override
        public Configuration load(Path path) throws IOException {
            try (InputStream in = Files.newInputStream(path)) {
                Configuration conf = new Configuration();
                conf.load(in);
                conf.mtime = Files.getLastModifiedTime(path).toMillis();
                return conf;
            }
        }

        @Override
        public ListenableFuture<Configuration> reload(Path path, Configuration conf) throws Exception {
            long mtime = Files.getLastModifiedTime(path).toMillis();
            if (mtime != conf.mtime) {
                return super.reload(path, conf);
            } else {
                return Futures.immediateFuture(conf);
            }
        }
    }

    private final Configuration conf;

    private static final LoadingCache<Path, Configuration> conf_cache =
        CacheBuilder.newBuilder().build(new ConfigurationLoader());

    public static final Path HOME_DIR, CONF_DIR, VAR_DIR, LOG_DIR;
    public static final Path LOCK_DIR;

    private static final String HOME_DIR_KEY = "CLOUDWAY_HOME";
    private static final String DEFAULT_HOME_DIR = "/usr/share/cloudway";
    private static final String VAR_DIR_KEY = "CLOUDWAY_VAR_DIR";
    private static final String DEFAULT_VAR_DIR = "/var/lib/cloudway";
    private static final String LOG_DIR_KEY = "CLOUDWAY_LOG_DIR";
    private static final String DEFAULT_LOG_DIR = "/var/log/cloudway";

    static {
        HOME_DIR = Paths.get(getProperty(HOME_DIR_KEY, DEFAULT_HOME_DIR)).toAbsolutePath().normalize();
        CONF_DIR = HOME_DIR.resolve("conf");

        Config config = getDefault();
        VAR_DIR = Paths.get(config.get(VAR_DIR_KEY, DEFAULT_VAR_DIR));
        LOG_DIR = Paths.get(config.get(LOG_DIR_KEY, DEFAULT_LOG_DIR));
        LOCK_DIR = VAR_DIR.resolve(".lock");
    }

    private static String getProperty(String key, String defaultValue) {
        return Optionals.or(System.getProperty(key), () -> Optionals.or(System.getenv(key), defaultValue));
    }

    public static Config getDefault() {
        return new Config("container.conf");
    }

    public Config(String name) {
        this(CONF_DIR.resolve(requireNonNull(name)));
    }

    public Config(Path path) {
        conf_cache.refresh(requireNonNull(path));
        this.conf = conf_cache.getUnchecked(path);
    }

    public Optional<String> get(String name) {
        Optional<String> val = Optional.ofNullable(System.getProperty(name));
        return val.isPresent() ? val : conf.getOptionalProperty(name);
    }

    public String get(String name, String deflt) {
        return get(name).orElse(deflt);
    }

    public boolean getBoolean(String name, boolean deflt) {
        return get(name).map(Boolean::valueOf).orElse(deflt);
    }

    public int getInt(String name, int deflt) {
        return get(name).flatMap(Optionals.of(Integer::parseInt)).orElse(deflt);
    }

    public ExtendedProperties getProperties() {
        return conf;
    }

    public String toString() {
        return conf.toString();
    }
}
