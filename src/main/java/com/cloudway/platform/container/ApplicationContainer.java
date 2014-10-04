/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Objects;
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
    private Path   home_dir;
    private String shell;
    private int    uid, gid;

    private ContainerPlugin plugin;

    private static final POSIX posix = POSIXFactory.getPOSIX();

    public static final String GECOS, SHELL, DOMAIN;

    static {
        Config config = Config.getDefault();
        GECOS = config.get("EXECUTOR_GECOS", "Cloudway Executor");
        SHELL = config.get("EXECUTOR_SHELL", "/bin/bash");
        DOMAIN = config.get("CLOUDWAY_DOMAIN", "cloudway.local");
    }

    /**
     * Construct a new ApplicationContainer.
     *
     * @param uuid the UUID for the application container
     * @param name the name of the application container
     * @param namespace the namespace used for proxy
     * @param pwent the passwd entry for the executor
     */
    private ApplicationContainer(String uuid, String name, String namespace, Passwd pwent) {
        this.uuid = uuid;
        this.name = name;
        this.namespace = namespace;

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
    }

    /**
     * Return an ApplicationContainer object loaded from the executor uuid on the system.
     *
     * @param uuid the executor uuid
     */
    public static ApplicationContainer fromUUID(String uuid) {
        Passwd pwent = passwdFor(uuid);
        Path envdir = Paths.get(pwent.getHome(), ".env");
        Map<String,String> env = Environ.load(envdir, "CLOUDWAY_APP_{NAME,DNS}*");

        String appname = env.get("CLOUDWAY_APP_NAME");
        String dns = env.get("CLOUDWAY_APP_DNS");

        String namespace;
        try {
            namespace = dns.substring(dns.indexOf('-') + 1, dns.indexOf('.'));
        } catch (Exception ex) {
            // malformed DNS
            namespace = null;
        }

        return new ApplicationContainer(uuid, appname, namespace, pwent);
    }

    static Passwd passwdFor(String uuid) {
        Passwd pwent = posix.getpwnam(uuid);
        if (pwent == null || !GECOS.equals(pwent.getGECOS())) {
            throw new IllegalArgumentException("Not a cloudway executor: " + uuid);
        }
        return pwent;
    }

    public static boolean exists(String uuid) {
        try {
            passwdFor(uuid);
            return true;
        } catch (IllegalArgumentException ex) {
            return false;
        }
    }

    /**
     * Return a Collection which provides a list of uuids
     * for every cloudway executor in the system.
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
     * objects for every cloudway executor in the system.
     */
    public static Collection<ApplicationContainer> all() {
        return uuids().stream()
            .map(ApplicationContainer::fromUUID)
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
    public static ApplicationContainer create(String uuid, String name, String namespace)
        throws IOException
    {
        ApplicationContainer container =
            new ApplicationContainer(Objects.requireNonNull(uuid),
                                     Objects.requireNonNull(name),
                                     Objects.requireNonNull(namespace),
                                     null);
        container.plugin.create();
        return container;
    }

    /**
     * Destroy container.
     */
    public void destroy() throws IOException {
        if (home_dir == null || !Files.isDirectory(home_dir)) {
            // executor seems to have been deleted already... suppress any error
            if (home_dir != null && Files.exists(home_dir)) {
                FileUtils.deleteTree(home_dir);
            }
            return;
        }

        plugin.destroy();
    }

    /**
     * Sets the app state to "stopped" and causes an immediate forced
     * termination of all executor processes.
     */
    public void stop() throws IOException {
        plugin.stop();
    }

    /**
     * Cleans up the gear, providing any installed
     * cartridges with the opportunity to perform their own
     * cleanup operations via the tidy hook.
     *
     * The generic executor-level cleanup flow is:
     * * Stop the executor
     * * Executor temp dir cleanup
     * * Cartridge tidy hook executions
     * * Git cleanup
     * * Start the executor
     *
     * Raises an Exception if an internal error occurs, and ignores
     * failed cartridge tidy hook executions.
     */
    public void tidy() throws IOException {
        Path repo_dir = FileUtils.join(home_dir, "git", name + ".git");
        Path temp_dir = FileUtils.join(home_dir, ".tmp");

        stopExecutor();

        // Perform the executor- and cart- level tidy actions. At this point,
        // the executor has been stopped; we'll attempt to start the executor
        // no matter what tidy operations fail.
        try {
            // clear out the temp dir
            FileUtils.emptyDirectory(temp_dir);

            // Delegate to cartridge model to perform cart-level tidy operations
            // for all installed carts.
            // TODO: cartridge_model.tidy();

            // git gc - do this last to maximize room for git to write changes
            runInContext(Exec.args("git", "prune").directory(repo_dir).checkError());
            runInContext(Exec.args("git", "gc", "--aggressive").directory(repo_dir).checkError());
        } finally {
            startExecutor();
        }
    }

    /**
     * Sets the application state to STARTED and starts the executor. Executor
     * state implementation is model specific, but options is provided to the
     * implementation.
     */
    public void startExecutor() {
        // TODO
    }

    /**
     * Sets the application state to STOPPED and stops the executor. Executor
     * state implementation is model specific, but options is provided to the
     * implementation.
     */
    public void stopExecutor() {
        // TODO
    }

    /**
     * Executes specified command in container context and return its exit status.
     * Or, raise exceptions if certain conditions are not met.
     */
    public int runInContext(Exec exec) throws IOException {
        if (exec.directory() == null) {
            exec.directory(home_dir);
        }

        return plugin.runInContext(exec);
    }

    /**
     * Executes specified command in container context and substitute standard
     * output to a string content, or raise exception if certain conditions
     * are not met.
     */
    public String substInContext(Exec exec) throws IOException {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        exec.redirectOutputStream(out);
        runInContext(exec);

        byte[] contents = out.toByteArray();
        int i = contents.length - 1;
        while (i >= 0) {
            if (contents[i] == '\n' || contents[i] == '\r') {
                i--;
            } else {
                break;
            }
        }

        return new String(contents, 0, i);
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
}
