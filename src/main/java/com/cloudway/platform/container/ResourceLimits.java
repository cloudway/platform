/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;

import com.cloudway.platform.common.Config;
import com.cloudway.platform.common.util.ExtendedProperties;
import com.cloudway.platform.common.util.RuntimeIOException;

@SuppressWarnings("serial")
public final class ResourceLimits extends ExtendedProperties
{
    private static final ResourceLimits INSTANCE = load();

    private static ResourceLimits load() {
        try (InputStream in = Files.newInputStream(Config.CONF_DIR.resolve("limits.conf"))) {
            ResourceLimits limits = new ResourceLimits();
            limits.load(in);
            return limits;
        } catch (IOException ex) {
            throw new RuntimeIOException(ex);
        }
    }

    public static ResourceLimits getInstance() {
        return INSTANCE;
    }

    private ResourceLimits() {}

    public String getProperty(String category, String key, String deflt) {
        ExtendedProperties cat = category(category);
        String value = cat != null ? cat.getProperty(key) : null;
        return value != null ? value : super.getProperty(key, deflt);
    }

    public int getInt(String category, String key, int deflt) {
        String sval = getProperty(category, key, null);
        return sval != null ? Integer.parseInt(sval) : deflt;
    }
}
