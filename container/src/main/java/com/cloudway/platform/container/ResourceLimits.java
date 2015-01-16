/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container;

import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.util.Optional;
import java.util.function.Supplier;
import java.util.stream.Stream;

import com.cloudway.platform.common.Config;
import com.cloudway.platform.common.util.ExtendedProperties;
import com.cloudway.platform.common.util.Optionals;

public final class ResourceLimits
{
    private static final ResourceLimits INSTANCE = load();

    private static ResourceLimits load() {
        try (InputStream in = Files.newInputStream(Config.CONF_DIR.resolve("limits.conf"))) {
            ExtendedProperties p = new ExtendedProperties();
            p.load(in);
            return new ResourceLimits(p);
        } catch (IOException ex) {
            throw new UncheckedIOException(ex);
        }
    }

    public static ResourceLimits getInstance() {
        return INSTANCE;
    }

    private final ExtendedProperties limits;

    private ResourceLimits(ExtendedProperties limits) {
        this.limits = limits;
    }

    public Optional<String> getProperty(String profile, String key) {
        Optional<String> value = limits.getOptionalProperty(profile, key);
        return value.isPresent() ? value : limits.getOptionalProperty(key);
    }

    @SuppressWarnings({"unchecked", "rawtypes"})
    public Optional<Object> getProperty(String profile, String key, Supplier<Optional<Object>> supplier) {
        Optional value = getProperty(profile, key);
        return value.isPresent() ? value : supplier.get();
    }
    
    public int getIntProperty(String profile, String key, int deflt) {
        return getProperty(profile, key).flatMap(Optionals.of(Integer::parseInt)).orElse(deflt);
    }

    public String getGlobalProperty(String key, String deflt) {
        return limits.getProperty(key, deflt);
    }
    
    public int getGlobalProperty(String key, int deflt) {
        return limits.getIntProperty(key, deflt);
    }

    public Stream<String> profiles() {
        return limits.categories().keySet().stream();
    }

    public Stream<String> keys() {
        return Stream.concat(
            limits.global().keySet().stream(),
            limits.categories().values().stream().flatMap(p -> p.keySet().stream()));
    }

    public String toString() {
        return limits.toString();
    }
}
