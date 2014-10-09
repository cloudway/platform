/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Stream;
import static java.util.stream.Collectors.toMap;

import com.cloudway.platform.common.util.Exec;
import com.cloudway.platform.common.util.FileUtils;
import com.cloudway.platform.common.util.RuntimeIOException;

public class AddonControl
{
    private final ApplicationContainer container;
    private Map<String, Addon> addons;

    public AddonControl(ApplicationContainer container) {
        this.container = container;
    }

    public Map<String, Addon> addons() {
        if (addons == null) {
            try (Stream<Path> paths = Files.list(container.getHomeDir())) {
                addons = paths
                    .filter(p -> Files.exists(FileUtils.join(p, "metadata", "addon.xml")))
                    .collect(toMap(p -> p.getFileName().toString(), Addon::new));
            } catch (IOException ex) {
                throw new RuntimeIOException(ex);
            }
        }
        return addons;
    }

    public Optional<Addon> getAddon(String name) {
        return Optional.ofNullable(addons().get(name));
    }

    public Optional<Addon> getPrimaryAddon() {
        return addons().values().stream()
            .filter(a -> a.getType() == AddonType.FRAMEWORK)
            .findFirst();
    }

    public void start() throws IOException {
        control_all("start");
    }

    public void stop() throws IOException {
        control_all("stop");
    }

    public void tidy() throws IOException {
        control_all("tidy");
    }

    public void install(String name, Path source, String templateUrl)
        throws IOException
    {
        // TODO
    }

    public void remove(String name) throws IOException {
        // TODO
    }

    public void destroy() throws IOException {
        // TODO
    }

    private void control_all(String action) throws IOException {
        for (Addon addon : addons().values()) {
            control(addon, action);
        }
    }

    private void control(Addon addon, String action)
        throws IOException
    {
        Path path = addon.getPath();

        // Make sure this addon's environments overrides that of other addons
        Map<String, String> env = Environ.loadAll(container.getHomeDir());
        env.putAll(Environ.load(path));

        Path control = FileUtils.join(path, "bin", "control");
        Exec exec = Exec.args(control, action);
        exec.environment().putAll(env);
        exec.directory(path);
        container.join(exec).run();
    }
}
