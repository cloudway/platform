/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.os;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

import com.google.common.base.Ticker;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.google.common.util.concurrent.Futures;
import com.google.common.util.concurrent.ListenableFuture;
import com.cloudway.platform.common.util.ExtendedProperties;

import static java.nio.file.Files.*;
import static java.util.Objects.requireNonNull;

class DefaultConfiguration extends ExtendedProperties implements Configuration
{
    private static final long serialVersionUID = 367619579773949417L;

    static final long RELOAD_CHECK_TIME = TimeUnit.SECONDS.toNanos(60);
    static Ticker ticker = Ticker.systemTicker();
    private long load_time, check_time;

    @Override
    public Optional<String> getProperty(String name) {
        Optional<String> val = Optional.ofNullable(System.getProperty(name));
        return val.isPresent() ? val : getOptionalProperty(name);
    }

    static class ConfigurationLoader extends CacheLoader<Path, DefaultConfiguration> {
        @Override
        public DefaultConfiguration load(Path path) throws IOException {
            DefaultConfiguration conf = new DefaultConfiguration();
            conf.check_time = ticker.read();
            try (InputStream in = newInputStream(path)) {
                conf.load(in);
                conf.load_time = getLastModifiedTime(path).toMillis();
            } catch (NoSuchFileException ex) {
                // use defaults if configuration file not found
            }
            return conf;
        }

        @Override
        public ListenableFuture<DefaultConfiguration>
        reload(Path path, DefaultConfiguration conf) throws Exception {
            DefaultConfiguration newConf = conf;
            try {
                if (ticker.read() - conf.check_time > RELOAD_CHECK_TIME &&
                    conf.load_time != getLastModifiedTime(path).toMillis()) {
                    newConf = load(path);
                }
            } catch (IOException ex) {
                // use previous configuration if file not found
            }
            return Futures.immediateFuture(newConf);
        }
    }

    @SuppressWarnings("ClassNameSameAsAncestorName")
    static class Provider implements Configuration.Provider {
        private final LoadingCache<Path, DefaultConfiguration> cache =
            CacheBuilder.newBuilder().build(new ConfigurationLoader());

        @Override
        public Configuration load(String name) {
            Path path = Config.CONF_DIR.resolve(requireNonNull(name));
            cache.refresh(path);
            return cache.getUnchecked(path);
        }
    }
}
