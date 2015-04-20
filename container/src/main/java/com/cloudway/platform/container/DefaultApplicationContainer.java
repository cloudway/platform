/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.io.UncheckedIOException;
import java.io.Writer;
import java.net.URL;
import java.net.URLConnection;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.TimeUnit;
import java.util.function.Supplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import com.google.common.collect.ImmutableList;
import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.VelocityEngine;

import com.cloudway.platform.common.fp.data.Maybe;
import com.cloudway.platform.common.fp.io.IOAction;
import com.cloudway.platform.common.os.Config;
import com.cloudway.platform.common.os.Etc;
import com.cloudway.platform.common.os.Exec;
import com.cloudway.platform.common.fp.io.IO;
import com.cloudway.platform.container.proxy.HttpProxy;
import com.cloudway.platform.container.velocity.Alt;

import static com.cloudway.platform.container.ApplicationState.*;
import static com.cloudway.platform.common.util.MoreCollectors.*;
import static com.cloudway.platform.common.util.MoreFiles.*;
import static java.util.Objects.requireNonNull;

class DefaultApplicationContainer implements ApplicationContainer
{
    private final String id;
    private final String name;
    private final String namespace;
    private final String capacity;
    private final Path   home_dir;
    private final String shell;
    private int          uid, gid;

    private final ContainerAdapter adapter;
    private AddonControl addons;

    static final Supplier<String> DEFAULT_CAPACITY = Config.property("DEFAULT_CAPACITY", "small");

    /**
     * Construct a new ApplicationContainer.
     *
     * @param id the unique ID for the application container
     * @param name the name of the application container
     * @param namespace the namespace used for proxy
     * @param pwent the passwd entry for the guest
     */
    private DefaultApplicationContainer(String id, String name, String namespace, String capacity, Etc.PASSWD pwent) {
        this.id = id;
        this.name = name;
        this.namespace = namespace;
        this.capacity = capacity != null ? capacity : DEFAULT_CAPACITY.get();

        if (pwent != null) {
            this.uid = pwent.pw_uid;
            this.gid = pwent.pw_gid;
            this.home_dir = Paths.get(pwent.pw_dir);
            this.shell = pwent.pw_shell;
        } else {
            this.home_dir = Config.VAR_DIR.resolve(id);
            this.shell = Config.SHELL.get();
        }

        this.adapter = ContainerAdapter.adapt(this);
        this.addons = new DefaultAddonControl(this);
    }

    // Package private for unit test
    void setAddonControl(AddonControl addons) {
        this.addons = addons;
    }

    /**
     * Return an ApplicationContainer object loaded from the container id on the system.
     *
     * @param id the container id
     */
    static ApplicationContainer fromId(String id)
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

        return new DefaultApplicationContainer(id, appname, namespace, capacity, pwent);
    }

    static Maybe<Etc.PASSWD> pwent(String id) {
        if (id == null) {
            return Maybe.empty();
        }

        Etc.PASSWD pwent = Etc.getpwnam(id);
        if (pwent == null || !Config.GECOS.get().equals(pwent.pw_gecos)) {
            return Maybe.empty();
        } else {
            return Maybe.of(pwent);
        }
    }

    static boolean exists(String id) {
        return pwent(id).isPresent();
    }

    /**
     * Return a Collection which provides a list of ids
     * for every cloudway guest in the system.
     */
    static Collection<String> ids() {
        ImmutableList.Builder<String> ids = ImmutableList.builder();
        int uid = Etc.getuid();
        String gecos = Config.GECOS.get();

        if (uid != 0) {
            Etc.PASSWD pw = Etc.getpwuid(uid);
            if (pw != null && gecos.equals(pw.pw_gecos) && Files.exists(Paths.get(pw.pw_dir))) {
                ids.add(pw.pw_name);
            }
        } else {
            Etc.getpwent(pw -> {
                if (gecos.equals(pw.pw_gecos) && Files.exists(Paths.get(pw.pw_dir))) {
                    ids.add(pw.pw_name);
                }
            });
        }
        return ids.build();
    }

    @Override
    public String getId() {
        return id;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public String getNamespace() {
        return namespace;
    }

    @Override
    public String getDomainName() {
        return name + "-" + namespace + "." + Config.DOMAIN;
    }

    @Override
    public List<Addon.Endpoint> getEndpoints() {
        return addons.validAddons()
            .flatMap(a -> a.getEndpoints().stream())
            .collect(toImmutableList());
    }

    @Override
    public String getCapacity() {
        return capacity;
    }

    @Override
    public int getUID() {
        return uid;
    }

    @Override
    public void setUID(int uid) {
        if (this.uid != 0)
            throw new IllegalStateException("uid was set");
        this.uid = uid;
    }

    @Override
    public int getGID() {
        return gid;
    }

    @Override
    public void setGID(int gid) {
        if (this.gid != 0)
            throw new IllegalStateException("gid was set");
        this.gid = gid;
    }

    @Override
    public String getShell() {
        return shell;
    }

    @Override
    public Path getHomeDir() {
        return home_dir;
    }

    private static final Pattern RE_ID = Pattern.compile("^[a-z0-9]+$");
    private static final Pattern RE_NAME = Pattern.compile("^[a-z][a-z_0-9]*$");

    /**
     * Create a container.
     */
    static DefaultApplicationContainer create(String id, String name, String namespace, String capacity)
        throws IOException
    {
        requireNonNull(id);
        requireNonNull(name);
        requireNonNull(namespace);
        requireNonNull(capacity);

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

        DefaultApplicationContainer container =
            new DefaultApplicationContainer(id, name, namespace, capacity, null);
        container.adapter.create();
        container.setState(NEW);
        return container;
    }

    /**
     * Destroy container.
     */
    @Override
    public void destroy() throws IOException {
        try {
            addons.destroy();
        } finally {
            adapter.destroy();
        }
    }

    /**
     * Sets the application state to "started" and starts the guest.
     */
    @Override
    public void start() throws IOException {
        int uid = Etc.getuid();
        ApplicationState state = getState();

        if (uid != 0 && state == IDLE && HttpProxy.isIdle(this)) {
            try {
                // contact proxy to activate container for unprivileged user
                URL url = new URL("http://127.0.0.1/");
                URLConnection con = url.openConnection();
                con.setRequestProperty("X-Container-Id", getId());
                con.getInputStream().close();
            } catch (IOException ex) {
                // log and ignore
            }
            return;
        }

        if (uid == 0) {
            if (state != STARTED)
                adapter.start();
            if (state == IDLE)
                HttpProxy.unidle(this);
        }

        processTemplates();
        start_guest();
        setState(STARTED);
    }

    /**
     * Sets the application state to "stopped" and stops the guest.
     */
    @Override
    public void stop() throws IOException {
        setState(STOPPED);
        stop_guest(true, 0, null);
    }

    /**
     * Restart the application container.
     */
    @Override
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
    @Override
    public void idle() throws IOException {
        if (getState() == STARTED) {
            stop_guest(true, 30, TimeUnit.SECONDS);
            HttpProxy.idle(this);
            setState(IDLE);
        }
    }

    /**
     * Unidle the application. Must be called by privileged user.
     */
    @Override
    public void unidle() throws IOException {
        if (Etc.getuid() == 0) {
            // make sure the container is really idled to prevent DoS attack
            if (HttpProxy.unidle(this)) {
                adapter.start();
                start_guest();
                setState(STARTED);
            }
        }
    }

    /**
     * Execute user defined control action.
     */
    @Override
    public void control(String action, boolean enable_action_hooks)
        throws IOException
    {
        addons.control(action, enable_action_hooks, false);
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
            adapter.stop(term_delay, unit);
        }
    }

    private void pause_guest(IOAction action) throws IOException{
        if (getState() == STARTED) {
            stop_guest(false, 0, null);
            try {
                action.run();
            } finally {
                start_guest();
            }
        } else {
            action.run();
        }
    }

    /**
     * Sets the application state.
     */
    @Override
    public void setState(ApplicationState new_state) throws IOException {
        requireNonNull(new_state);
        Path state_file = state_file();
        writeText(state_file, new_state.name());
        setFileReadWrite(state_file);
    }

    /**
     * Get the current application state.
     */
    @Override
    public ApplicationState getState() {
        try {
            return ApplicationState.valueOf(readText(state_file()));
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
    @Override
    public void tidy() throws IOException {
        pause_guest(() -> {
            // clear out the temp dir
            if (Etc.getuid() == 0) {
                try {
                    emptyDirectory(home_dir.resolve(".tmp"));
                } catch (IOException ignore) {}
            }

            // Delegate to addon control to perform addon-level tidy operations
            // for all installed addons.
            addons.tidy();

            // do this last to maximize room for git to write changes
            ApplicationRepository.of(this).tidy();
        });
    }

    /**
     * Install an add-on.
     *
     * @param source the add-on source, a directory or an archive file
     * @param repoUrl the URL used to populate repository
     */
    @Override
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
    @Override
    public void uninstall(String name) throws IOException {
        addons.remove(name);
    }

    /**
     * Called by Git pre-receive hook.
     */
    @Override
    public void pre_receive() throws IOException {
        // TODO
    }

    /**
     * Called by Git post-receive hook.
     */
    @Override
    public void post_receive() throws IOException {
        pause_guest(() -> {
            ApplicationRepository.of(this).checkout(getRepoDir());
            processTemplates();
        });
    }

    static final String  TEMPLATE_EXT = ".cwt";
    static final Pattern TEMPLATE_RE = Pattern.compile("\\A\\.?(.*)\\.cwt\\Z");

    @Override
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
                    try (Reader reader = Files.newBufferedReader(input);
                         Writer writer = Files.newBufferedWriter(output)) {
                        VelocityContext outer = new VelocityContext(vc);
                        ve.evaluate(outer, writer, filename, reader);
                    }

                    if (securing)
                        setFileReadOnly(input);
                    setFileReadWrite(output);
                }
            });
        }
    }

    private void processTemplates() throws IOException {
        processTemplates(getRepoDir(), environ(), false);
    }

    /**
     * Configure the specified Exec object to run the command in container context.
     */
    @Override
    public Exec join(Exec exec) throws IOException {
        if (exec.directory() == null) {
            exec.directory(home_dir);
        }
        return adapter.join(exec);
    }

    @Override
    public Map<String, String> environ() {
        return Environ.loadAll(this);
    }

    @Override
    public String getenv(String name) {
        try {
            return readText(envfile(name));
        } catch (NoSuchFileException ex) {
            return null;
        } catch (IOException ex) {
            throw new UncheckedIOException(ex);
        }
    }

    @Override
    public void setenv(String name, String value) {
        try {
            Path file = envfile(name);
            writeText(file, value);
            setFileReadOnly(file);
        } catch (IOException ex) {
            throw new UncheckedIOException(ex);
        }
    }

    @Override
    public void unsetenv(String name) {
        try {
            Files.deleteIfExists(envfile(name));
        } catch (IOException ex) {
            throw new UncheckedIOException(ex);
        }
    }

    private Path envfile(String name) {
        return getEnvDir().resolve(requireNonNull(name));
    }

    @Override
    public void addAuthorizedKey(String id, String key) throws IOException {
        adapter.addAuthorizedKey(id, key);
    }

    @Override
    public void removeAuthorizedKey(String key) throws IOException {
        adapter.removeAuthorizedKey(key);
    }

    @Override
    public List<String> getAuthorizedKeys() throws IOException {
        return adapter.getAuthorizedKeys();
    }

    @Override
    public void setFileReadOnly(Path file) throws IOException {
        adapter.setFileReadOnly(file);
    }

    @Override
    public void setFileReadWrite(Path file) throws IOException {
        adapter.setFileReadWrite(file);
    }

    @Override
    public void setFileTreeReadOnly(Path dir) throws IOException {
        adapter.setFileTreeReadOnly(dir);
    }

    @Override
    public void setFileTreeReadWrite(Path dir) throws IOException {
        adapter.setFileTreeReadWrite(dir);
    }

    @Override
    public String getIpAddress(int host_id) {
        return adapter.getIpAddress(host_id);
    }

    @Override
    public boolean isAddressInUse(String ip, int port) {
        return adapter.isAddressInUse(ip, port);
    }
}
