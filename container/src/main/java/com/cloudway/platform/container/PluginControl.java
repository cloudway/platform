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

public interface PluginControl
{
    /**
     * Returns all plugins installed in the container.
     */
    Map<String, Plugin> plugins();

    /**
     * Returns all valid plugins installed in the container.
     */
    Stream<Plugin> validPlugins();

    /**
     * Get a plugin by name.
     */
    Maybe<Plugin> plugin(String name);

    /**
     * Get the framework plugin in the container.
     */
    Maybe<Plugin> getFrameworkPlugin();

    /**
     * Install a new plugin from given path, and populate application repository.
     *
     * @param source the source of plugin
     * @param repo the repository specification
     * @throws IOException if an I/O error occurs
     */
    void install(Path source, String repo) throws IOException;

    /**
     * Remove a plugin from the container.
     *
     * @param name the plugin name
     * @throws IOException if an I/O error occurs
     */
    void remove(String name) throws IOException;

    /**
     * Destroy all plugins installed in the container.
     */
    void destroy();

    /**
     * Start all plugins installed in the container.
     */
    void start() throws IOException;

    /**
     * Stop all plugins installed in the container.
     */
    void stop() throws IOException;

    /**
     * Restart all plugins installed in the container.
     */
    void restart() throws IOException;

    /**
     * Cleans up all installed plugins.
     */
    void tidy() throws IOException;

    /**
     * Perform custom action for all installed plugins.
     */
    void control(String action, boolean enable_action_hooks, boolean process_templates)
        throws IOException;
}
