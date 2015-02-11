/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container;

import java.io.IOException;
import java.nio.file.Path;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import com.cloudway.platform.common.os.Exec;

/**
 * This is a container interface that provide environment information
 * for other components.
 */
public interface ApplicationContainer
{
    /**
     * Utility method to make a unique container id.
     */
    static String makeUUID() {
        return Helper.makeUUID();
    }

    /**
     * Return an ApplicationContainer object loaded from the container id on the system.
     *
     * @param id the container id
     * @throws NoSuchContainerException if the container id is not mapped to a container
     */
    static ApplicationContainer fromId(String id)
        throws NoSuchContainerException {
        return DefaultApplicationContainer.fromId(id);
    }

    /**
     * Create a container.
     */
    static ApplicationContainer create(String id, String name, String namespace, String capacity)
        throws IOException {
        return DefaultApplicationContainer.create(id, name, namespace, capacity);
    }

    /**
     * Returns true if the container with given ID is exists, false otherwise.
     */
    static boolean exists(String id) {
        return DefaultApplicationContainer.exists(id);
    }

    /**
     * Return a Collection which provides a list of ids for every cloudway
     * guest in the system.
     */
    static Collection<String> ids() {
        return DefaultApplicationContainer.ids();
    }

    /**
     * Returns a Stream which provides a list of ApplicationContainer
     * objects for every cloudway guest in the system.
     */
    static Stream<ApplicationContainer> all() {
        return ids().stream().map(ApplicationContainer::fromId);
    }

    /**
     * Returns the unique identifier of the container.
     */
    String getId();

    /**
     * Returns the container name.
     */
    String getName();

    /**
     * Returns the container namespace.
     */
    String getNamespace();

    /**
     * Returns the domain name of the container.
     */
    String getDomainName();

    /**
     * Get all endpoints of the container.
     */
    List<Addon.Endpoint> getEndpoints();

    /**
     * Get the container capacity.
     */
    String getCapacity();

    /**
     * Get the user ID of the operating system user that hosts the container.
     */
    int getUID();

    /**
     * Set the user ID of the operating system user that hosts the container.
     */
    void setUID(int uid);

    /**
     * Get the group ID of the operating system user that hosts the container.
     */
    int getGID();

    /**
     * Set the group ID of the operating system user that hosts the container.
     */
    void setGID(int gid);

    /**
     * Get the shell program of the operating system used by this container.
     */
    String getShell();

    /**
     * Returns the home directory of the container.
     */
    Path getHomeDir();

    /**
     * Returns the env directory of the container.
     */
    default Path getEnvDir() {
        return getHomeDir().resolve(".env");
    }

    /**
     * Returns the application root directory.
     */
    default Path getAppDir() {
        return getHomeDir().resolve("app");
    }

    /**
     * Returns the application repository directory.
     */
    default Path getRepoDir() {
        return getAppDir().resolve("repo");
    }

    /**
     * Returns the application data directory.
     */
    default Path getDataDir() {
        return getAppDir().resolve("data");
    }

    /**
     * Load environment variables from container.
     */
    Map<String, String> environ();

    /**
     * Get value of a environment variable.
     *
     * @param name the variable name
     * @return the environment variable value
     */
    String getenv(String name);

    /**
     * Adds the variable {@code name} to the environment with the value
     * {@code value}, if {@code name} does not already exist.  if {@code name}
     * does exist in the environment, then its value is changed to {@code value}.
     *
     * @param name the variable name
     * @param value the variable value
     */
    void setenv(String name, String value);

    /**
     * Removes the variable {@code name} from the environment.  If {@code name}
     * does not exist in the environment then the environment is unchanged.
     *
     * @param name the variable name
     */
    void unsetenv(String name);

    /**
     * Process templates from the given path, given the variable mappings.
     *
     * @param path the directory start searching template files
     * @param env the variable mappings
     * @param securing true if generated files have the readonly mode,
     * false otherwise
     * @throws IOException if I/O error occurs
     */
    void processTemplates(Path path, Map<String,String> env, boolean securing)
        throws IOException;

    /**
     * Set a file to the readonly mode.
     *
     * @param file the file to change mode
     * @throws IOException if I/O error occurs
     */
    void setFileReadOnly(Path file) throws IOException;

    /**
     * Set a file to the read-write mode.
     *
     * @param file the file to change mode
     * @throws IOException if I/O error occurs
     */
    void setFileReadWrite(Path file) throws IOException;

    /**
     * Set all files under a directory and it's subdirectory to the readonly mode.
     *
     * @param dir the directory contains files to change mode
     * @throws IOException if I/O error occurs
     */
    void setFileTreeReadOnly(Path dir) throws IOException;

    /**
     * Set all files under a directory and it's subdirectory to the read-write mode.
     *
     * @param dir the directory contains files to change mode
     * @throws IOException if I/O error occurs
     */
    void setFileTreeReadWrite(Path dir) throws IOException;

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
     * Compute an IP address from the given host ID.
     *
     * @param host_id the host ID used to distinguish IP addresses
     * @return an IP address derived from the given host ID
     */
    String getIpAddress(int host_id);

    /**
     * Determines whether if the give address is in use.
     *
     * @param ip the ip address
     * @param port the port number
     * @return true if the address is in use, false otherwise
     */
    boolean isAddressInUse(String ip, int port);

    /**
     * Destroy the container.
     */
    void destroy() throws IOException;

    /**
     * Get the current application state.
     */
    ApplicationState getState();

    /**
     * Sets the application state.
     */
    void setState(ApplicationState new_state) throws IOException;

    /**
     * Set the application state to "started" and starts the guest.
     */
    void start() throws IOException;

    /**
     * Sets the application state to "stopped" and stops the guest.
     */
    void stop() throws IOException;

    /**
     * Restart the application container.
     */
    void restart() throws IOException;

    /**
     * Sets the application state to "idle" and stops the guest.
     */
    void idle() throws IOException;

    /**
     * Unidle the application. Must be called by privileged user.
     */
    void unidle() throws IOException;

    /**
     * Cleans up the guest, providing any installed addons with the
     * opportunity to perform their own cleanup operations via the
     * tidy hook.
     */
    void tidy() throws IOException;

    /**
     * Execute user defined control action.
     */
    void control(String action, boolean enable_action_hooks)
        throws IOException;

    /**
     * Decorate the specified Exec object.
     */
    default Exec exec(Exec exec) {
        return exec;
    }

    /**
     * Configure the specified Exec object to run the command in container context.
     */
    Exec join(Exec exec) throws IOException;

    /**
     * Install an add-on.
     *
     * @param source the add-on source, a directory or an archive file
     * @param repoUrl the URL used to populate repository
     */
    void install(Path source, String repoUrl) throws IOException;

    /**
     * Remove an add-on.
     *
     * @param name the add-on name
     */
    void uninstall(String name) throws IOException;

    /**
     * Called by Git pre-receive hook.
     */
    void pre_receive() throws IOException;

    /**
     * Called by Git post-receive hook.
     */
    void post_receive() throws IOException;
}
