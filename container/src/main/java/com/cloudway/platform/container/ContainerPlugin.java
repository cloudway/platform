/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.net.Socket;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.TimeUnit;
import java.util.stream.Stream;

import com.cloudway.platform.common.os.Config;
import com.cloudway.platform.common.os.Exec;
import com.cloudway.platform.common.io.MoreFiles;
import com.cloudway.platform.common.io.IO;
import com.cloudway.platform.container.plugin.LinuxContainerPlugin;
import com.cloudway.platform.container.plugin.MacOSContainerPlugin;
import com.cloudway.platform.container.plugin.UnixContainerPlugin;

import jnr.ffi.Platform;
import static jnr.ffi.Platform.OS.*;
import static com.cloudway.platform.common.util.function.Predicates.*;
import static com.cloudway.platform.common.util.Conditionals.*;

public abstract class ContainerPlugin
{
    public static ContainerPlugin newInstance(ApplicationContainer container) {
        Platform platform = Platform.getNativePlatform();
        return with().<ContainerPlugin>get()
          .when(platform.getOS(), is(LINUX),  () -> new LinuxContainerPlugin(container))
          .when(platform.getOS(), is(DARWIN), () -> new MacOSContainerPlugin(container))
          .when(platform::isUnix,             () -> new UnixContainerPlugin(container))
          .orElseThrow(() -> new IllegalStateException("unsupported platform: " + platform.getName()));
    }

    protected final ApplicationContainer container;

    /**
     * Create a new instance of ContainerPlugin.
     */
    protected ContainerPlugin(ApplicationContainer container) {
        this.container = container;
    }

    /**
     * Create an empty application container.
     */
    public void create()
        throws IOException
    {
        try {
            createUser();
            createHomeDir();
        } catch (IOException ex) {
            // cleanup when creating container failed
            nothrow(this::deleteUser);
            throw ex;
        }
    }

    /**
     * Destroys an application container stopping all processes and removing all files.
     */
    public void destroy() {
        nothrow(this::killProcs);
        nothrow(this::deleteUser);
    }

    /**
     * Implemented by subclass to create the operating system user.
     */
    protected abstract void createUser()
        throws IOException;

    /**
     * Implemented by subclass to delete the operating system user.
     */
    protected abstract void deleteUser()
        throws IOException;

    /**
     * Create and populate the users home dir.
     */
    protected abstract void createHomeDir()
        throws IOException;

    /**
     * Start the application container.
     */
    public void start()
        throws IOException
    {
        // implemented by subclass
    }

    /**
     * Stop an application container.
     */
    public void stop(long term_delay, TimeUnit unit)
        throws IOException
    {
        killProcs(term_delay, unit);
    }

    /**
     * Configure the specified Exec object to run the command in container
     * context.
     */
    public abstract Exec join(Exec exec) throws IOException;

    /**
     * Kill processes belonging to this application container.
     */
    protected void killProcs() throws IOException {
        killProcs(0, null);
    }

    /**
     * Kill processes belonging to this application container.
     *
     * @param term_delay send SIGTERM first, wait term_delay then send SIGKILL.
     */
    protected abstract void killProcs(long term_delay, TimeUnit unit)
        throws IOException;

    /**
     * Deterministically constructs an IP address for the given UID based on the
     * given host identifier (LSB of IP). The host identifier must be a value
     * between 1-127 inclusive.
     */
    public String getIpAddress(int host_id) {
        long uid = container.getUID();

        if (uid < 0 || uid > Integer.MAX_VALUE)
            throw new IllegalArgumentException("User uid " + uid + " must be unsigned 32 bit integers.");
        if (host_id < 1 || host_id > 127)
            throw new IllegalArgumentException("Supplied host identifier " + host_id + " must be between 1 and 127");

        Config config = Config.getDefault();
        int uid_wraparound = config.getInt("UID_WRAPAROUND", 65536);
        int ip_offset = config.getInt("IP_OFFSET", 1);

        if (ip_offset >= 1000)
            ip_offset = 0;
        if (uid < uid_wraparound)
            ip_offset = 0;

        // Generate an IP (32-bit unsigned) in the user's range
        int loopback_start = 0x7F000000;
        int ip = loopback_start + (int)((uid % uid_wraparound + ip_offset) << 7) + host_id;

        // Return the IP in dotted-quad notation
        return (ip >> 24) + "." + ((ip >> 16) & 0xFF) + "." + ((ip >> 8) & 0xFF) + "." + (ip & 0xFF);
    }

    /**
     * Check to see if the specified IP/port is bound.
     */
    public boolean isAddressInUse(String ip, int port) {
        try (Socket ignored = new Socket(ip, port)) {
            return true;
        } catch (IOException ex) {
            return false;
        }
    }

    /**
     * Add an environment variable to a given container.
     *
     * @param key The String value of target environment variable.
     * @param value The String value to place inside the environment variable.
     * @param prefix The String value to append in front of key.
     */
    public void addEnvVar(String key, String value, boolean prefix) {
        try {
            Path file = envfile(key, prefix);
            MoreFiles.writeText(file, value);
            setFileReadOnly(file);
        } catch (IOException ex) {
            throw new UncheckedIOException(ex);
        }
    }

    /**
     * Remove an environment variable from a given container.
     *
     * @param key name of the environment variable to remove
     * @param prefix The String value to append in front of key.
     */
    public void removeEnvVar(String key, boolean prefix) {
        try {
            Path file = envfile(key, prefix);
            Files.deleteIfExists(file);
        } catch (IOException ex) {
            throw new UncheckedIOException(ex);
        }
    }

    private Path envfile(String key, boolean prefix) {
        String filename = Objects.requireNonNull(key);
        if (prefix) filename = "CLOUDWAY_" + filename;
        return container.getEnvDir().resolve(filename);
    }

    /**
     * Add an SSH key to a users authorized_keys file.
     *
     * @param id the ID for the authorized key
     * @param key the String value of the key
     * @throws IOException if reading or writing authorized_keys file
     */
    public abstract void addAuthorizedKey(String id, String key)
        throws IOException;

    /**
     * Remove an SSH key from a users authorized_keys file.
     *
     * @param key the String value of the key
     * @throws IOException if reading or writing authorized_keys file
     */
    public abstract void removeAuthorizedKey(String key)
        throws IOException;

    /**
     * Get all SSH keys from a users authorized_keys file.
     *
     * @throws IOException if reading authorized_keys file
     */
    public abstract List<String> getAuthorizedKeys()
        throws IOException;

    /**
     * Set file permission to be readonly by guest processes.
     */
    public abstract void setFileReadOnly(Path file) throws IOException;

    /**
     * Set file permission can be read and write by guest processes.
     */
    public abstract void setFileReadWrite(Path file) throws IOException;

    /**
     * Set all files and directories in a tree to be readonly by guest processes.
     */
    public void setFileTreeReadOnly(Path dir)
        throws IOException
    {
        try (Stream<Path> files = Files.walk(dir)) {
            IO.forEach(files, this::setFileReadOnly);
        }
    }

    /**
     * Set all files and directories in a tree to be read and write by guest processes.
     */
    public void setFileTreeReadWrite(Path dir)
        throws IOException
    {
        try (Stream<Path> files = Files.walk(dir)) {
            IO.forEach(files, this::setFileReadWrite);
        }
    }

    @FunctionalInterface
    protected interface ExceptionAction {
        void run() throws Exception;
    }

    protected static void nothrow(ExceptionAction action) {
        try {
            action.run();
        } catch (Exception ex) {
            // log and ignore
        }
    }

    protected static void nothrow(ExceptionAction... actions) {
        Stream.of(actions).forEach(ContainerPlugin::nothrow);
    }
}
