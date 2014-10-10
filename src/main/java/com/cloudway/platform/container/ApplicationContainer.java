/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import com.cloudway.platform.common.Config;
import com.cloudway.platform.common.util.Exec;
import com.cloudway.platform.common.util.FileUtils;
import jnr.posix.POSIX;
import jnr.posix.POSIXFactory;
import jnr.posix.Passwd;

public class ApplicationContainer
{
    private String uuid;
    private String name;
    private String namespace;
    private String capacity;
    private Path   home_dir;
    private String shell;
    private int    uid, gid;

    private ContainerPlugin plugin;
    private AddonControl addons;

    private static final POSIX posix = POSIXFactory.getPOSIX();

    public static final String GECOS, SHELL, DOMAIN;
    public static final String DEFAULT_CAPACITY;

    static {
        Config config = Config.getDefault();
        GECOS = config.get("GUEST_GECOS", "Cloudway Guest");
        SHELL = config.get("GUEST_SHELL", "/bin/bash");
        DOMAIN = config.get("CLOUDWAY_DOMAIN", "cloudway.local");
        DEFAULT_CAPACITY = config.get("DEFAULT_CAPACITY", "small");
    }

    /**
     * Construct a new ApplicationContainer.
     *
     * @param uuid the UUID for the application container
     * @param name the name of the application container
     * @param namespace the namespace used for proxy
     * @param pwent the passwd entry for the guest
     */
    private ApplicationContainer(String uuid, String name, String namespace, String capacity, Passwd pwent) {
        this.uuid = uuid;
        this.name = name;
        this.namespace = namespace;
        this.capacity = capacity != null ? capacity : DEFAULT_CAPACITY;

        if (pwent != null) {
            this.uid = (int)pwent.getUID();
            this.gid = (int)pwent.getGID();
            this.home_dir = Paths.get(pwent.getHome());
            this.shell = pwent.getShell();
        } else {
            this.home_dir = Config.VAR_DIR.resolve(uuid);
            this.shell = SHELL;
        }

        this.plugin = ContainerPlugin.newInstance(this);
        this.addons = new AddonControl(this);
    }

    /**
     * Return an ApplicationContainer object loaded from the guest uuid on the system.
     *
     * @param uuid the guest uuid
     */
    public static ApplicationContainer fromUuid(String uuid) {
        Passwd pwent = pwent(uuid).orElseThrow(
            () -> new IllegalArgumentException("Not a cloudway guest: " + uuid));

        Path envdir = Paths.get(pwent.getHome(), ".env");
        Map<String,String> env = Environ.load(envdir, "CLOUDWAY_APP{NAME,DNS,SIZE}*");

        String appname  = env.get("CLOUDWAY_APP_NAME");
        String dns      = env.get("CLOUDWAY_APP_DNS");
        String capacity = env.get("CLOUDWAY_APP_SIZE");

        String namespace;
        try {
            namespace = dns.substring(dns.indexOf('-') + 1, dns.indexOf('.'));
        } catch (Exception ex) {
            // malformed DNS
            namespace = null;
        }

        return new ApplicationContainer(uuid, appname, namespace, capacity, pwent);
    }

    static Optional<Passwd> pwent(String uuid) {
        Passwd pwent = posix.getpwnam(uuid);
        if (pwent == null || !GECOS.equals(pwent.getGECOS())) {
            return Optional.empty();
        } else {
            return Optional.of(pwent);
        }
    }

    public static boolean exists(String uuid) {
        return pwent(uuid).isPresent();
    }

    /**
     * Return a Collection which provides a list of uuids
     * for every cloudway guest in the system.
     */
    public static Collection<String> uuids() {
        List<String> uuids = new ArrayList<>();

        try {
            for (Passwd pwent = posix.getpwent(); pwent != null; pwent = posix.getpwent()) {
                if (GECOS.equals(pwent.getGECOS()) && Files.exists(Paths.get(pwent.getHome()))) {
                    uuids.add(pwent.getLoginName());
                }
            }
        } finally {
            posix.endpwent();
        }

        return uuids;
    }

    /**
     * Returns a Collection which provides a list of ApplicationContainer
     * objects for every cloudway guest in the system.
     */
    public static Collection<ApplicationContainer> all() {
        return uuids().stream()
            .map(ApplicationContainer::fromUuid)
            .collect(Collectors.toList());
    }

    public String getUuid() {
        return uuid;
    }

    public String getName() {
        return name;
    }

    public String getNamespace() {
        return namespace;
    }

    public String getDomainName() {
        return name + "-" + namespace + "." + DOMAIN;
    }

    public String getIpAddress(int host_id) {
        return plugin.getIpAddress(host_id);
    }

    public String getCapacity() {
        return capacity;
    }

    public int getUID() {
        return uid;
    }

    public void setUID(int uid) {
        if (this.uid != 0)
            throw new IllegalStateException("uid was set");
        this.uid = uid;
    }

    public int getGID() {
        return gid;
    }

    public void setGID(int gid) {
        if (this.gid != 0)
            throw new IllegalStateException("gid was set");
        this.gid = gid;
    }

    public Path getHomeDir() {
        return home_dir;
    }

    public String getShell() {
        return shell;
    }

    /**
     * Create a container.
     */
    public static ApplicationContainer create(String uuid, String name, String namespace, String capacity)
        throws IOException
    {
        ApplicationContainer container =
            new ApplicationContainer(Objects.requireNonNull(uuid),
                                     Objects.requireNonNull(name),
                                     Objects.requireNonNull(namespace),
                                     Objects.requireNonNull(capacity),
                                     null);
        container.plugin.create();
        container.setState(ApplicationState.NEW);
        return container;
    }

    /**
     * Destroy container.
     */
    public void destroy() throws IOException {
        addons.destroy();
        plugin.destroy();
    }

    /**
     * Sets the application state to "started" and starts the guest.
     */
    public void start() throws IOException {
        plugin.start();
        start_guest();
        setState(ApplicationState.STARTED);
    }

    /**
     * Sets the application state to "stopped" and stops the guest.
     */
    public void stop() throws IOException {
        setState(ApplicationState.STOPPED);
        stop_guest(true, 0, null);
    }

    /**
     * Sets the application state to "idle" and stops the guest.
     */
    public void idle() throws IOException {
        if (getState() != ApplicationState.STOPPED) {
            stop_guest(true, 30, TimeUnit.SECONDS);
            setState(ApplicationState.IDLE);
        }
    }

    private void start_guest() throws IOException {
        addons.start();
    }

    private void stop_guest(boolean force, long term_delay, TimeUnit unit)
        throws IOException
    {
        // stop addons gracefully
        addons.stop();

        // force to stop all guest processes
        if (force) {
            plugin.stop(term_delay, unit);
        }
    }

    /**
     * Sets the application state.
     */
    public void setState(ApplicationState new_state) throws IOException {
        Objects.requireNonNull(new_state);
        Path state_file = state_file();
        FileUtils.write(state_file, new_state.name());
        plugin.setFileReadOnly(state_file);
    }

    /**
     * Get the current application state.
     */
    public ApplicationState getState() {
        try {
            return ApplicationState.valueOf(FileUtils.read(state_file()));
        } catch (Exception ex) {
            return ApplicationState.UNKNOWN;
        }
    }

    private Path state_file() {
        return FileUtils.join(home_dir, "app", ".state");
    }

    /**
     * Cleans up the guest, providing any installed addons with the
     * opportunity to perform their own cleanup operations via the
     * tidy hook.
     *
     * The generic guest-level cleanup flow is:
     * * Stop the guest
     * * Guest temp dir cleanup
     * * Addon tidy hook executions
     * * Git cleanup
     * * Start the guest
     *
     * Raises an Exception if an internal error occurs, and ignores
     * failed addon tidy hook executions.
     */
    public void tidy() throws IOException {
        stop_guest(false, 0, null);

        // Perform the guest- and addon- level tidy actions. At this point,
        // the guest has been stopped; we'll attempt to start the guest
        // no matter what tidy operations fail.
        try {
            // clear out the temp dir
            FileUtils.emptyDirectory(home_dir.resolve(".tmp"));

            // Delegate to addon control to perform addon-level tidy operations
            // for all installed addons.
            addons.tidy();

            // do this last to maximize room for git to write changes
            ApplicationRepository.newInstance(this).tidy();
        } finally {
            start_guest();
        }
    }

    public void installAddon(String name, Path source, String templateUrl)
        throws IOException
    {
        addons.install(name, source, templateUrl);
    }

    /**
     * Configure the specified Exec object to run the command in container context.
     */
    public Exec join(Exec exec) throws IOException {
        if (exec.directory() == null) {
            exec.directory(home_dir);
        }
        return plugin.join(exec);
    }

    public void addAuthorizedKey(String id, String key)
        throws IOException
    {
        plugin.addAuthorizedKey(id, key);
    }

    public void removeAuthorizedKey(String key)
        throws IOException
    {
        plugin.removeAuthorizedKey(key);
    }

    public List<String> getAuthorizedKeys()
        throws IOException
    {
        return plugin.getAuthorizedKeys();
    }

    public void setFileReadOnly(Path file) throws IOException {
        plugin.setFileReadOnly(file);
    }

    public void setFileReadWrite(Path file) throws IOException {
        plugin.setFileReadWrite(file);
    }

    public void setFileTreeReadOnly(Path dir) throws IOException {
        plugin.setFileTreeReadOnly(dir);
    }

    public void setFileTreeReadWrite(Path dir) throws IOException {
        plugin.setFileTreeReadWrite(dir);
    }
}
