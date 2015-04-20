/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container.adapters;

import java.io.IOException;
import java.nio.file.Path;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import com.google.common.collect.ImmutableTable;
import com.cloudway.platform.common.fp.data.Maybe;
import com.cloudway.platform.container.ResourceLimits;
import static com.cloudway.platform.common.fp.control.StringPredicates.*;

/**
 * Low level cgroup interface.
 */
public interface LibCgroup
{
    /**
     * Returns the default implementation of LibCgroup.
     */
    static LibCgroup getDefault() {
        return LinuxCgroup.Default.INSTANCE;
    }

    /**
     * Returns true if cgroup is enabled on the system.
     */
    boolean enabled();

    /**
     * Returns cgroup paths.
     */
    Map<String,Path> paths();

    /**
     * Returns cgroup parameters.
     */
    Map<String,Object> parameters();

    /**
     * Load cgroup profiles from resource limits configuration.
     */
    default ImmutableTable<String, String, Object> load_profiles(ResourceLimits limits) {
        if (!enabled()) {
            return ImmutableTable.of();
        }

        ImmutableTable.Builder<String, String, Object> profiles = ImmutableTable.builder();

        // make a uniform key set from all profiles
        Set<String> keys = limits.keys()
            .filter(startsWith(Cgroup.CG_KEY_PREFIX))
            .collect(Collectors.toSet());
        keys.add("cgroup.freezer.state"); // used to restore freezer state

        // load configuration for each cgroup profiles
        limits.profiles().forEach(profile -> keys.forEach(key -> {
            String ck = key.substring(Cgroup.CG_KEY_PREFIX.length());
            limits.getProperty(profile, key, () -> Maybe.ofNullable(parameters().get(ck)))
                  .ifPresent(v -> profiles.put(profile, ck, v));
        }));

        return profiles.build();
    }

    /**
     * Create a cgroup namespace for the application container.
     */
    void create(String user, int uid, Map<String,Object> defaults)
        throws IOException;

    /**
     * Delete a cgroup namespace for the application container.
     */
    void delete(String user) throws IOException;

    /**
     * Fetch a parameter for a specific user.
     */
    Maybe<Object> fetch(String user, String key) throws IOException;

    /**
     * Store cgroup parameters for a specific user.
     */
    void store(String user, Map<String,Object> vals) throws IOException;

    /**
     * Helper method to freeze a container.
     */
    void freeze(String user) throws IOException;

    /**
     * Helper method to unfreeze a container.
     */
    void thaw(String user) throws IOException;

    /**
     * List tasks in a cgroup.
     */
    int[] tasks(String user) throws IOException;
}
