/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container;

import java.io.IOException;
import java.nio.file.Path;
import java.util.Map;
import java.util.stream.Stream;
import com.cloudway.fp.data.Maybe;

public interface AddonControl
{
    /**
     * Returns all addons installed in the container.
     */
    Map<String, Addon> addons();

    /**
     * Returns all valid addons installed in the container.
     */
    Stream<Addon> validAddons();

    /**
     * Get an addon by name.
     */
    Maybe<Addon> addon(String name);

    /**
     * Get the framework addon in the container.
     */
    Maybe<Addon> getFrameworkAddon();

    /**
     * Install a new addon from given path, and populate application repository.
     *
     * @param source the source of addon
     * @param repo the repository specification
     * @throws IOException if an I/O error occurs
     */
    void install(Path source, String repo) throws IOException;

    /**
     * Remove an addon from the container.
     *
     * @param name the addon name
     * @throws IOException if an I/O error occurs
     */
    void remove(String name) throws IOException;

    /**
     * Destroy all addons installed in the container.
     */
    void destroy();

    /**
     * Start all addons installed in the container.
     */
    void start() throws IOException;

    /**
     * Stop all addons installed in the container.
     */
    void stop() throws IOException;

    /**
     * Restart all addons installed in the container.
     */
    void restart() throws IOException;

    /**
     * Cleans up all installed addons.
     */
    void tidy() throws IOException;

    /**
     * Perform custom action for all installed addons.
     */
    void control(String action, boolean enable_action_hooks, boolean process_templates)
        throws IOException;
}
