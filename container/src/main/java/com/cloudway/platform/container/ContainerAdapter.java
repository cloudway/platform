/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import com.cloudway.platform.common.os.Exec;

/**
 * Bridge interface for operating system specific application
 * container implementation.
 */
public interface ContainerAdapter
{
    static void setFactory(Function<ApplicationContainer, ContainerAdapter> factory) {
        Helper.setContainerAdapterFactory(factory);
    }

    /**
     * Create a new instance of container adapter.
     */
    static ContainerAdapter adapt(ApplicationContainer container) {
        return Helper.createContainerAdapter(container);
    }

    /**
     * Create an empty application container.
     */
    void create() throws IOException;

    /**
     * Destroys an application container by stopping all processes and removing all files.
     */
    void destroy();

    /**
     * Start the application container.
     */
    void start() throws IOException;

    /**
     * Stop an application container.
     */
    void stop(long term_delay, TimeUnit unit) throws IOException;

    /**
     * Configure the specified Exec object to run the command in container
     * context.
     */
    Exec join(Exec exec) throws IOException;

    /**
     * Deterministically constructs an IP address for the given UID based on the
     * given host identifier (LSB of IP). The host identifier must be a value
     * between 1-127 inclusive.
     */
    String getIpAddress(int host_id);

    /**
     * Check to see if the specified IP/port is bound.
     */
    boolean isAddressInUse(String ip, int port);

    /**
     * Add an SSH key to a users authorized_keys file.
     *
     * @param id the ID for the authorized key
     * @param key the String value of the key
     * @throws IOException if reading or writing authorized_keys file
     */
    void addAuthorizedKey(String id, String key) throws IOException;

    /**
     * Remove an SSH key from a users authorized_keys file.
     *
     * @param key the String value of the key
     * @throws IOException if reading or writing authorized_keys file
     */
    void removeAuthorizedKey(String key) throws IOException;

    /**
     * Get all SSH keys from a users authorized_keys file.
     *
     * @throws IOException if reading authorized_keys file
     */
    List<String> getAuthorizedKeys() throws IOException;

    /**
     * Set file permission to be readonly by guest processes.
     */
    void setFileReadOnly(Path file) throws IOException;

    /**
     * Set file permission can be read and write by guest processes.
     */
    void setFileReadWrite(Path file) throws IOException;

    /**
     * Set all files and directories in a tree to be readonly by guest processes.
     */
    void setFileTreeReadOnly(Path dir) throws IOException;

    /**
     * Set all files and directories in a tree to be read and write by guest processes.
     */
    void setFileTreeReadWrite(Path dir) throws IOException;
}
