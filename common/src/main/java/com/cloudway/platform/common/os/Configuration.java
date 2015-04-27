/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.os;

import com.cloudway.fp.data.Maybe;

/**
 * The Configuration interface maintains all properties read from
 * a configuration file.
 */
public interface Configuration
{
    /**
     * Get a property as an optional value.
     *
     * @param name the property name
     * @return the property value encapsulate in a {@code Maybe}
     */
    Maybe<String> getProperty(String name);

    /**
     * The provider interface that responsible to load properties from
     * a configuration file.
     */
    interface Provider {
        /**
         * Load the configuration file
         *
         * @param name the name of the configuration file
         * @return all configuration properties
         */
        Configuration load(String name);
    }
}
