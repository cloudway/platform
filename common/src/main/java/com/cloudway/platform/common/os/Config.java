/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.os;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Optional;
import java.util.function.Supplier;

import com.cloudway.platform.common.util.MoreFiles;
import com.cloudway.platform.common.fp.data.Optionals;

public final class Config
{
    private static final Configuration.Provider DEFAULT_PROVIDER = new DefaultConfiguration.Provider();
    private static Configuration.Provider provider = DEFAULT_PROVIDER;

    public static void setProvider(Configuration.Provider prov) {
        provider = prov != null ? prov : DEFAULT_PROVIDER;
    }

    private final Configuration conf;

    public Config(String name) {
        conf = provider.load(name);
    }

    public static Config getDefault() {
        return new Config("container.conf");
    }

    public Optional<String> get(String name) {
        return conf.getProperty(name);
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

    public String toString() {
        return conf.toString();
    }

    @FunctionalInterface
    public interface PathProperty extends Supplier<Path> {
        @Override Path get();

        default Path resolve(String other) {
            return get().resolve(other);
        }

        default Path resolve(String... other) {
            return MoreFiles.join(get(), other);
        }
    }

    public static final String HOME_DIR_KEY = "CLOUDWAY_HOME";
    public static final String DEFAULT_HOME_DIR = "/usr/share/cloudway";
    public static final String VAR_DIR_KEY = "CLOUDWAY_VAR_DIR";
    public static final String DEFAULT_VAR_DIR = "/var/lib/cloudway";
    public static final String LOG_DIR_KEY = "CLOUDWAY_LOG_DIR";
    public static final String DEFAULT_LOG_DIR = "/var/log/cloudway";

    // bootstrap path properties
    public static final Path HOME_DIR, CONF_DIR;

    static {
        HOME_DIR = Paths.get(getProperty(HOME_DIR_KEY, DEFAULT_HOME_DIR)).toAbsolutePath().normalize();
        CONF_DIR = HOME_DIR.resolve("conf");
    }

    private static String getProperty(String key, String defaultValue) {
        return Optionals.or(System.getProperty(key), () -> Optionals.or(System.getenv(key), defaultValue));
    }

    public static PathProperty path(String key, String deflt) {
        return new DefaultPathProperty(key, deflt);
    }

    public static PathProperty path(PathProperty base, String name) {
        return new RelativePathProperty(base, name);
    }

    public static Supplier<String> property(String key, String deflt) {
        return new StringProperty(key, deflt);
    }

    public static final PathProperty VAR_DIR  = path(VAR_DIR_KEY, DEFAULT_VAR_DIR);
    public static final PathProperty LOG_DIR  = path(LOG_DIR_KEY, DEFAULT_LOG_DIR);
    public static final PathProperty LOCK_DIR = path(VAR_DIR, ".lock");

    /**
     * A marker used to identify an application container host user. This can
     * be configured in the global configuration file.
     */
    public static final Supplier<String> GECOS = property("CLOUDWAY_GECOS", "Cloudway Application");

    /**
     * The shell used by an application container host user. This can be
     * configured in the global configuration file.
     */
    public static final Supplier<String> SHELL = property("CLOUDWAY_SHELL", "/bin/bash");

    /**
     * The domain name used by all applications in the system. This can be
     * configured in the global configuration file.
     */
    public static final Supplier<String> DOMAIN = property("CLOUDWAY_DOMAIN", "cloudway.local");

    private static class DefaultPathProperty implements PathProperty {
        private final String key, deflt;
        
        DefaultPathProperty(String key, String deflt) {
            this.key = key;
            this.deflt = deflt;
        }
        
        @Override
        public Path get() {
            return Paths.get(getDefault().get(key, deflt));
        }

        public String toString() {
            return get().toString();
        }
    }
    
    private static class RelativePathProperty implements PathProperty {
        private final PathProperty base;
        private final String name;
        
        RelativePathProperty(PathProperty base, String name) {
            this.base = base;
            this.name = name;
        }
        
        @Override
        public Path get() {
            return base.get().resolve(name);
        }

        public String toString() {
            return get().toString();
        }
    }

    private static class StringProperty implements Supplier<String> {
        private final String key, deflt;

        StringProperty(String key, String deflt) {
            this.key = key;
            this.deflt = deflt;
        }

        @Override
        public String get() {
            return getDefault().get(key, deflt);
        }

        public String toString() {
            return get();
        }
    }
}
