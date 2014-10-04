/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

/**
 * This class extends java.util.Properties to support property grouping.
 */
public class ExtendedProperties extends Properties
{
    private static final long serialVersionUID = -6453705529513128143L;

    protected Map<String, ExtendedProperties> groups;
    protected ExtendedProperties current;

    /**
     * Create an empty property list with no default values.
     */
    public ExtendedProperties() {
    }

    /**
     * Creates an empty property list with the specified defaults.
     *
     * @param defaults the defaults
     */
    public ExtendedProperties(Properties defaults) {
        super(defaults);
    }

    @Override
    public synchronized Object put(Object key, Object value) {
        if (key instanceof String) {
            String skey = (String)key;
            if (skey.startsWith("[") && skey.endsWith("]")) {
                skey = skey.substring(1, skey.length()-1);
                if (groups == null)
                    groups = new HashMap<>();
                current = groups.computeIfAbsent(skey, x -> new ExtendedProperties());
                return null;
            }
        }
        if (current != null) {
            return current.put(key, value);
        } else {
            return super.put(key, value);
        }
    }

    public ExtendedProperties category(String name) {
        return groups != null ? groups.get(name) : null;
    }

    public Set<String> categoryKeys() {
        return groups != null ? groups.keySet() : Collections.emptySet();
    }

    public boolean getBool(String key, boolean deflt) {
        String value = getProperty(key);
        return value != null ? Boolean.valueOf(value) : deflt;
    }

    public boolean getBool(String key) {
        return getBool(key, false);
    }

    public int getInt(String key, int deflt) {
        String value = getProperty(key);
        return value != null ? Integer.parseInt(value) : deflt;
    }

    public int getInt(String key) {
        return getInt(key, -1);
    }
}