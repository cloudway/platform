/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.io.Writer;
import java.net.URL;
import java.net.URLConnection;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Properties;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.cloudway.platform.common.Config;
import com.cloudway.platform.common.util.Etc;
import com.cloudway.platform.common.util.Exec;
import com.cloudway.platform.common.util.FileUtils;
import com.cloudway.platform.common.util.IO;
import com.cloudway.platform.container.proxy.HttpProxy;
import static com.cloudway.platform.container.ApplicationState.*;

import com.cloudway.platform.container.velocity.Alt;
import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.VelocityEngine;

public class ApplicationContainer
{
    private String id;
    private String name;
    private String namespace;
    private String capacity;
    private Path   home_dir;
    private String shell;
    private int    uid, gid;

    private ContainerPlugin plugin;
    private AddonControl addons;

    public static final String GECOS, SHELL, DOMAIN;
    public static final String DEFAULT_CAPACITY;

    static {
        Config config = Config.getDefault();
        GECOS = config.get("CLOUDWAY_GECOS", "Cloudway Guest");
        SHELL = config.get("CLOUDWAY_SHELL", "/bin/bash");
        DOMAIN = config.get("CLOUDWAY_DOMAIN", "cloudway.local");
        DEFAULT_CAPACITY = config.get("DEFAULT_CAPACITY", "small");
    }

    /**
     * Construct a new ApplicationContainer.
     *
     * @param id the unique ID for the application container
     * @param name the name of the application container
     * @param namespace the namespace used for proxy
     * @param pwent the passwd entry for the guest
     */
    private ApplicationContainer(String id, String name, String namespace, String capacity, Etc.PASSWD pwent) {
        this.id = id;
        this.name = name;
        this.namespace = namespace;
        this.capacity = capacity != null ? capacity : DEFAULT_CAPACITY;

        if (pwent != null) {
            this.uid = pwent.pw_uid;
            this.gid = pwent.pw_gid;
            this.home_dir = Paths.get(pwent.pw_dir);
            this.shell = pwent.pw_shell;
        } else {
            this.home_dir = Config.VAR_DIR.resolve(id);
            this.shell = SHELL;
        }

        this.plugin = ContainerPlugin.newInstance(this);
        this.addons = new AddonControl(this);
    }

    /**
     * Return an ApplicationContainer object loaded from the container id on the system.
     *
     * @param id the container id
     */
    public static ApplicationContainer fromId(String id)
        throws NoSuchContainerException
    {
        Etc.PASSWD pwent = pwent(id).orElseThrow(
            () -> new NoSuchContainerException(id, "Not a cloudway application container"));

        if (Etc.getuid() != 0 && Etc.getuid() != pwent.pw_uid) {
            throw new NoSuchContainerException(id, "Access denied");
        }

        Path envdir = Paths.get(pwent.pw_dir, ".env");
        Map<String,String> env = Environ.load(envdir, "CLOUDWAY_APP_{NAME,DNS,SIZE}*");

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

        return new ApplicationContainer(id, appname, namespace, capacity, pwent);
    }

    static Optional<Etc.PASSWD> pwent(String id) {
        if (id == null) {
            return Optional.empty();
        }

        Etc.PASSWD pwent = Etc.getpwnam(id);
        if (pwent == null || !GECOS.equals(pwent.pw_gecos)) {
            return Optional.empty();
        } else {
            return Optional.of(pwent);
        }
    }

    public static boolean exists(String id) {
        return pwent(id).isPresent();
    }

    /**
     * Return a Collection which provides a list of ids
     * for every cloudway guest in the system.
     */
    public static Collection<String> ids() {
        int uid = Etc.getuid();

        if (uid != 0) {
            Etc.PASSWD pw = Etc.getpwuid(uid);
            if (pw != null && GECOS.equals(pw.pw_gecos) && Files.exists(Paths.get(pw.pw_dir))) {
                return Collections.singleton(pw.pw_name);
            } else {
                return Collections.emptyList();
            }
        } else {
            List<String> ids = new ArrayList<>();
            Etc.getpwent(pw -> {
                if (GECOS.equals(pw.pw_gecos) && Files.exists(Paths.get(pw.pw_dir))) {
                    ids.add(pw.pw_name);
                }
            });
            return ids;
        }
    }

    /**
     * Returns a Stream which provides a list of ApplicationContainer
     * objects for every cloudway guest in the system.
     */
    public static Stream<ApplicationContainer> all() {
        return ids().stream().map(ApplicationContainer::fromId);
    }

    public String getId() {
        return id;
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

    public List<Addon.Endpoint> getEndpoints() {
        return addons.valid_addons()
            .flatMap(a -> a.getEndpoints().stream())
            .collect(Collectors.toList());
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

    public String getShell() {
        return shell;
    }

    public Path getHomeDir() {
        return home_dir;
    }

    public Path getEnvDir() {
        return home_dir.resolve(".env");
    }

    public Path getAppDir() {
        return home_dir.resolve("app");
    }

    public Path getRepoDir() {
        return getAppDir().resolve("repo");
    }

    public Path getDataDir() {
        return getAppDir().resolve("data");
    }

    private static final Pattern RE_ID = Pattern.compile("^[a-z0-9]+$");
    private static final Pattern RE_NAME = Pattern.compile("^[a-z][a-z_0-9]*$");

    /**
     * Create a container.
     */
    public static ApplicationContainer create(String id, String name, String namespace, String capacity)
        throws IOException
    {
        Objects.requireNonNull(id);
        Objects.requireNonNull(name);
        Objects.requireNonNull(namespace);
        Objects.requireNonNull(capacity);

        if (!RE_ID.matcher(id).matches()) {
            throw new IllegalArgumentException("Invalid container ID");
        }
        if (ids().contains(id)) {
            throw new IllegalStateException("Application container \"" + id + "\" already exists");
        }

        if (!RE_NAME.matcher(name).matches() || !RE_NAME.matcher(namespace).matches()) {
            throw new IllegalArgumentException("The name and namespace can only contains " +
                                               "lower case letters, digits, or underscore");
        }

        String fqdn = name + "-" + namespace + "." + DOMAIN;
        if (all().anyMatch(c -> fqdn.equals(c.getDomainName()))) {
            throw new IllegalStateException("Domain name \"" + fqdn + "\" already exists");
        }

        ApplicationContainer container = new ApplicationContainer(id, name, namespace, capacity, null);
        container.plugin.create();
        container.setState(NEW);
        return container;
    }

    /**
     * Destroy container.
     */
    public void destroy() throws IOException {
        try {
            addons.destroy();
        } finally {
            plugin.destroy();
        }
    }

    /**
     * Sets the application state to "started" and starts the guest.
     */
    public void start() throws IOException {
        int uid = Etc.getuid();
        ApplicationState state = getState();

        if (uid != 0 && state == IDLE && HttpProxy.getInstance().isIdle(getDomainName())) {
            privileged_unidle();
            return;
        }

        if (uid == 0) {
            if (state != STARTED)
                plugin.start();
            if (state == IDLE)
                HttpProxy.getInstance().unidle(getDomainName());
        }

        processTemplates();
        start_guest();
        setState(STARTED);
    }

    private void privileged_unidle() throws IOException {
        // Send request to oddjob to activate container
        if (Files.isExecutable(Paths.get("/usr/bin/oddjob_request"))) {
            try {
                Exec.args("/usr/bin/oddjob_request",
                          "-s", "com.cloudway.oddjob",
                          "-o", "/com/cloudway/oddjob",
                          "-i", "com.cloudway.oddjob.unidler",
                          "privileged_unidle")
                    .run();
                return;
            } catch (IOException ex) {
                // oddjob not configured, try next activation method
            }
        }

        // Contact proxy to activate container
        try {
            URL url = new URL("http://127.0.0.1/");
            URLConnection con = url.openConnection();
            con.setRequestProperty("X-Cloudway-Host", getDomainName());
            con.getInputStream().close();
        } catch (IOException ex) {
            // log and ignore
        }
    }

    /**
     * Sets the application state to "stopped" and stops the guest.
     */
    public void stop() throws IOException {
        setState(STOPPED);
        stop_guest(true, 0, null);
    }

    /**
     * Restart the application container.
     */
    public void restart() throws IOException {
        if (getState() == STARTED) {
            processTemplates();
            addons.restart();
        } else {
            start();
        }
    }

    /**
     * Sets the application state to "idle" and stops the guest.
     */
    public void idle() throws IOException {
        if (getState() == STARTED) {
            stop_guest(true, 30, TimeUnit.SECONDS);
            HttpProxy.getInstance().idle(getDomainName(), getId());
            setState(IDLE);
        }
    }

    /**
     * Unidle the application. Must be called by privileged user.
     */
    public void unidle() throws IOException {
        if (Etc.getuid() == 0) {
            // make sure the container is really idled to prevent DoS attack
            if (HttpProxy.getInstance().unidle(getDomainName())) {
                plugin.start();
                start_guest();
                setState(STARTED);
            }
        }
    }

    /**
     * Execute user defined control action.
     */
    public void control(String action, boolean enable_action_hooks)
        throws IOException
    {
        addons.control_all(action, enable_action_hooks, false);
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
        if (force && Etc.getuid() == 0) {
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
        plugin.setFileReadWrite(state_file);
    }

    /**
     * Get the current application state.
     */
    public ApplicationState getState() {
        try {
            return ApplicationState.valueOf(FileUtils.read(state_file()));
        } catch (Exception ex) {
            return UNKNOWN;
        }
    }

    private Path state_file() {
        return getAppDir().resolve(".state");
    }

    /**
     * Cleans up the guest, providing any installed addons with the
     * opportunity to perform their own cleanup operations via the
     * tidy hook.
     */
    public void tidy() throws IOException {
        boolean running = getState() == STARTED;

        if (running) {
            stop_guest(false, 0, null);
        }

        try {
            // clear out the temp dir
            if (Etc.getuid() == 0) {
                FileUtils.emptyDirectory(home_dir.resolve(".tmp"));
            }

            // Delegate to addon control to perform addon-level tidy operations
            // for all installed addons.
            addons.tidy();

            // do this last to maximize room for git to write changes
            ApplicationRepository.newInstance(this).tidy();
        } finally {
            if (running) {
                start_guest();
            }
        }
    }

    /**
     * Install an add-on.
     *
     * @param source the add-on source, a directory or an archive file
     * @param repoUrl the URL used to populate repository
     */
    public void install(Path source, String repoUrl)
        throws IOException
    {
        addons.install(source, repoUrl);
    }

    /**
     * Remove an add-on.
     *
     * @param name the add-on name
     */
    public void remove(String name) throws IOException {
        addons.remove(name);
    }

    /**
     * Called by Git pre-receive hook.
     */
    public void pre_receive() throws IOException {
        // TODO
    }

    /**
     * Called by Git post-receive hook.
     */
    public void post_receive() throws IOException {
        boolean running = getState() == STARTED;

        if (running) {
            stop_guest(false, 0, null);
        }

        try {
            ApplicationRepository repo = ApplicationRepository.newInstance(this);
            repo.checkout(getRepoDir());
            processTemplates();
        } finally {
            if (running) {
                start_guest();
            }
        }
    }

    public static final String  TEMPLATE_EXT = ".cwt";
    public static final Pattern TEMPLATE_RE = Pattern.compile("\\A\\.?(.*)\\.cwt\\Z");

    public void processTemplates(Path path, Map<String,String> env, boolean securing)
        throws IOException
    {
        VelocityEngine ve = new VelocityEngine();

        // Configuring velocity engine.
        Properties vconf = new Properties();
        try (InputStream ins = getClass().getResourceAsStream("velocity.properties")) {
            vconf.load(ins);
        }

        // Disallow access resources outside of template root.
        vconf.setProperty("resource.loader", "file");
        vconf.setProperty("file.resource.loader.path", path.toString());
        ve.init(vconf);

        // Create global context
        VelocityContext vc = new VelocityContext(env);
        vc.put("alt", new Alt());

        try (Stream<Path> files = Files.walk(path)) {
            IO.forEach(files, input -> {
                // Create output file name from input file name
                //   foo.txt.cwt  => foo.txt
                //   .foo.txt.cwt => foo.txt
                String filename = input.getFileName().toString();
                Matcher matcher = TEMPLATE_RE.matcher(filename);

                if (matcher.matches()) {
                    Path output = input.resolveSibling(matcher.group(1));
                    try (Reader reader = Files.newBufferedReader(input)) {
                        try (Writer writer = Files.newBufferedWriter(output)) {
                            VelocityContext outer = new VelocityContext(vc);
                            ve.evaluate(outer, writer, filename, reader);
                        }
                    }

                    if (securing) {
                        setFileReadOnly(input);
                        setFileReadWrite(output);
                    }
                }
            });
        }
    }

    private void processTemplates() throws IOException {
        processTemplates(getRepoDir(), Environ.loadAll(this), false);
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

    public void addEnvVar(String key, String value) {
        plugin.addEnvVar(key, value, true);
    }

    public void addEnvVar(String key, String value, boolean prefix) {
        plugin.addEnvVar(key, value, prefix);
    }

    public void removeEnvVar(String key) {
        plugin.removeEnvVar(key, true);
    }

    public void removeEnvVar(String key, boolean prefix) {
        plugin.removeEnvVar(key, prefix);
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

    public String getIpAddress(int host_id) {
        return plugin.getIpAddress(host_id);
    }

    public boolean isAddressInUse(String ip, int port) {
        return plugin.isAddressInUse(ip, port);
    }
}
