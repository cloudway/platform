/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container;

import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.nio.file.FileSystem;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.PathMatcher;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import static java.util.stream.Collectors.toMap;
import static java.nio.file.StandardCopyOption.*;
import static java.nio.file.LinkOption.NOFOLLOW_LINKS;

import com.cloudway.platform.common.Config;
import com.cloudway.platform.common.util.AbstractFileVisitor;
import com.cloudway.platform.common.util.Exec;
import com.cloudway.platform.common.util.FileUtils;
import com.cloudway.platform.common.util.IO;
import com.cloudway.platform.common.util.RuntimeIOException;

import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.VelocityEngine;

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

    public Optional<Addon> getFrameworkAddon() {
        return addons().values().stream()
            .filter(a -> a.getType() == AddonType.FRAMEWORK)
            .findFirst();
    }

    public void start() throws IOException {
        control_all("start", true);
    }

    public void stop() throws IOException {
        control_all("stop", true);
    }

    public void tidy() throws IOException {
        control_all("tidy", false);
    }

    public void install(String name, Path source, String templateUrl)
        throws IOException
    {
        Map<String, Addon> addons = addons(); // may load already installed addons
        Path target = container.getHomeDir().resolve(name);

        if (addons.containsKey(name) || Files.exists(target))
            throw new IllegalStateException("Addon already installed: " + name);
        if (getFrameworkAddon().isPresent())
            throw new IllegalStateException("A framework addon already installed");

        try {
            // copy source files into target
            installFiles(source.toRealPath(), target);

            // TODO: load addon metadata
            Addon addon = new Addon(target);

            // add environment variables
            addAddonEnvVar(target, name + "_DIR", target.toString(), true);
            if (addon.getType() == AddonType.FRAMEWORK) {
                container.addEnvVar("FRAMEWORK", name);
                container.addEnvVar("FRAMEWORK_DIR", target.toString());
            }

            // run addon actions with files unlocked
            with_unlocked(target, () -> {
                process_templates(target);
                do_addon_action(target, null, "setup");
            });

            // populate template repository
            if (addon.getType() == AddonType.FRAMEWORK) {
                populate_repository(target, templateUrl);
            }

            // put addon into cache
            addons.put(name, addon);
        } catch (IOException|RuntimeException|Error ex) {
            FileUtils.deleteTree(target);
            throw ex;
        }
    }

    public void remove(String name) throws IOException {
        // TODO
    }

    public void destroy() throws IOException {
        // TODO
    }

    private void installFiles(Path source, Path target)
        throws IOException
    {
        if (Files.isDirectory(source)) {
            copydir(source, target);
        } else {
            extract(source, target);
        }
        container.setFileTreeReadWrite(target);
    }

    private void copydir(Path source, Path target)
        throws IOException
    {
        String[] sharedFiles = getSharedFiles(source);
        if (sharedFiles == null || sharedFiles.length == 0) {
            FileUtils.copyTree(source, target);
            return;
        }

        FileSystem fs = source.getFileSystem();
        PathMatcher[] matchers = Arrays.stream(sharedFiles)
            .map(s -> fs.getPathMatcher("glob:" + s))
            .toArray(PathMatcher[]::new);

        Files.walkFileTree(source, new AbstractFileVisitor()
        {
            @Override
            protected FileVisitResult visit(Path path) throws IOException {
                Path relativePath = source.relativize(path);
                Path targetPath = target.resolve(relativePath);

                if (Stream.of(matchers).anyMatch(m -> m.matches(relativePath))) {
                    Files.createSymbolicLink(targetPath, path);
                    if (Files.isDirectory(path)) {
                        return FileVisitResult.SKIP_SUBTREE;
                    }
                } else {
                    Files.copy(path, targetPath, REPLACE_EXISTING, COPY_ATTRIBUTES, NOFOLLOW_LINKS);
                }

                return FileVisitResult.CONTINUE;
            }
        });
    }

    private void extract(Path source, Path target)
        throws IOException
    {
        String name = source.getFileName().toString();
        FileUtils.mkdir(target);

        if (name.endsWith(".zip")) {
            Exec.args("unzip", "-d", target, source)
                .silentIO()
                .checkError()
                .run();
        } else if (name.endsWith(".tar.gz") && name.endsWith(".tgz")) {
            Exec.args("tar", "-C", target, "-xzpf", source)
                .silentIO()
                .checkError()
                .run();
        } else if (name.endsWith(".tar")) {
            Exec.args("tar", "-C", target, "-xpf", source)
                .silentIO()
                .checkError()
                .run();
        } else {
            throw new IOException("Unsupported addon archive file: " + source);
        }
    }

    private static final String TEMPLATE_EXT = ".cwt";

    private void process_templates(Path path)
        throws IOException
    {
        Map<String,String> env = getAddonEnv(path, null);

        HashMap<String,String> cfg = new HashMap<>();
        Config config = Config.getDefault();
        config.keys().forEach(key -> cfg.put(key, config.get(key)));

        VelocityEngine ve = new VelocityEngine();
        ve.init();

        try (Stream<Path> files = getTemplateFiles(path)) {
            IO.forEach(files, input -> {
                String filename = input.getFileName().toString();
                filename = filename.substring(0, filename.length() - TEMPLATE_EXT.length());
                Path output = input.resolveSibling(filename);

                try (Reader reader = Files.newBufferedReader(input)) {
                    try (Writer writer = Files.newBufferedWriter(output)) {
                        VelocityContext vc = new VelocityContext();
                        env.forEach(vc::put);
                        vc.put("config", cfg.clone());
                        ve.evaluate(vc, writer, path.getFileName().toString(), reader);
                    }
                }

                Files.deleteIfExists(input);
            });
        }
    }

    private Stream<Path> getTemplateFiles(Path path)
        throws IOException
    {
        // TODO: The template file pattern may be configured in the addon metadata
        PathMatcher matcher = path.getFileSystem().getPathMatcher("glob:*" + TEMPLATE_EXT);
        return Files.find(path, Integer.MAX_VALUE, (p,a) -> matcher.matches(p.getFileName()));
    }

    private String[] getSharedFiles(Path source)
        throws IOException
    {
        Path shared_files = FileUtils.join(source, "metadata", "shared_files");
        if (Files.exists(shared_files)) {
            try (Stream<String> lines = Files.lines(shared_files)) {
                return lines.map(String::trim)
                            .filter(s -> !s.isEmpty())
                            .toArray(String[]::new);
            }
        } else if (Files.exists(source.resolve("share"))) {
            return new String[]{"share"};
        } else {
            return null;
        }
    }

    private static final Pattern GLOB_CHARS = Pattern.compile("[*?\\[\\]{}]");
    private static final Pattern PROTECTED_FILES = Pattern.compile("^\\.(ssh|tmp|env)");

    private Collection<String> getLockedFiles(Path target)
        throws IOException
    {
        Path locked_files = FileUtils.join(target, "metadata", "locked_files");
        if (!Files.exists(locked_files)) {
            return Collections.emptyList();
        }

        SortedSet<String> entries = new TreeSet<>();
        SortedSet<String> patterns = new TreeSet<>();

        try (Stream<String> lines = Files.lines(locked_files)) {
            lines.map(s -> makeRelative(target, s))
                 .forEach(entry -> {
                     if (entry != null) {
                         if (GLOB_CHARS.matcher(entry).find()) {
                             patterns.add(entry);
                         } else {
                             entries.add(entry);
                         }
                     }
                 });
        }

        if (!patterns.isEmpty()) {
            Path home = container.getHomeDir();
            List<PathMatcher> matchers = patterns.stream()
                .map(pattern -> home.getFileSystem().getPathMatcher("glob:" + pattern))
                .collect(Collectors.toList());

            Files.walkFileTree(home, new AbstractFileVisitor()
            {
                @Override
                protected FileVisitResult visit(Path path) throws IOException {
                    Path rel = home.relativize(path);
                    if (matchers.stream().anyMatch(m -> m.matches(rel))) {
                        entries.add(rel.toString());
                    }

                    if (Files.isDirectory(path)) {
                        if (patterns.stream().noneMatch(p -> home.resolve(p).startsWith(path))) {
                            return FileVisitResult.SKIP_SUBTREE;
                        }
                    }

                    return FileVisitResult.CONTINUE;
                }
            });
        }

        return entries;
    }

    private String makeRelative(Path target, String entry) {
        if ((entry = entry.trim()).isEmpty()) {
            return null;
        }

        Path home = container.getHomeDir();
        Path path = entry.startsWith("~/")
            ? home.resolve(entry.substring(2))
            : target.resolve(entry);
        path = path.normalize();

        // Ensure the files are in the home directory
        if (!path.startsWith(home)) {
            // TODO: log warning
            System.err.println("invalid lock file entry: " + entry);
            return null;
        }

        // Get the relative path rooted at home directory
        String rel = home.relativize(path).toString();
        if (entry.endsWith("/") && !rel.endsWith("/")) {
            rel = rel.concat("/");
        }

        if (rel.startsWith(".")) {
            // Allow files in dot files/dirs except for protected files
            if (PROTECTED_FILES.matcher(rel).find()) {
                System.err.println("invalid lock file entry: " + entry);
                return null;
            }
        } else {
            // Only allow files in app or addon directory
            if (!path.startsWith(target) && !path.startsWith(home.resolve("app"))) {
                System.err.println("invalid lock file entry: " + entry);
                return null;
            }
        }

        return rel;
    }

    private void with_unlocked(Path target, IO.Runnable action)
        throws IOException
    {
        Collection<String> locked_files = getLockedFiles(target);
        do_unlock(target, locked_files);
        try {
            action.run();
        } finally {
            do_lock(target, locked_files);
        }
    }

    private void do_lock(Path target, Collection<String> entries)
        throws IOException
    {
        Path home = container.getHomeDir();

        IO.forEach(entries, entry -> {
            Path path = home.resolve(entry);
            if (Files.exists(path)) {
                container.setFileReadOnly(path);
            }
        });

        container.setFileReadOnly(home);
        container.setFileReadOnly(target);
    }

    private void do_unlock(Path target, Collection<String> entries)
        throws IOException
    {
        Path home = container.getHomeDir();

        IO.forEach(entries, entry -> {
            Path path = home.resolve(entry);
            if (!Files.exists(path)) {
                if (entry.endsWith("/")) {
                    FileUtils.mkdir(path, 0755);
                } else {
                    FileUtils.touch(path, 0644);
                }
            }
            container.setFileReadWrite(path);
        });

        container.setFileReadWrite(home);
        container.setFileReadWrite(target);
    }

    private void populate_repository(Path path, String url)
        throws IOException
    {
        ApplicationRepository repo = ApplicationRepository.newInstance(container);

        if (url == null || url.isEmpty()) {
            repo.populateFromTemplate(path);
        } else if (url.equals("empty")) {
            repo.populateEmpty();
        } else {
            repo.populateFromURL(url);
        }

        if (repo.exists()) {
            repo.checkout(container.getRepoDir());
        }
    }

    private void control_all(String action, boolean enable_action_hooks)
        throws IOException
    {
        Map<String, String> env = Environ.loadAll(container.getHomeDir());
        for (Addon addon : addons().values()) {
            Path path = addon.getPath();
            if (enable_action_hooks) {
                do_action_hook("pre_" + action, env);
                do_addon_action(path, env, "control", action);
                do_action_hook("post_" + action, env);
            } else {
                do_addon_action(path, env, "control", action);
            }
        }
    }

    private void do_action_hook(String action, Map<String,String> env)
        throws IOException
    {
        Path hooks_dir = FileUtils.join(container.getRepoDir(), ".cloudway", "hooks");
        Path action_hook = hooks_dir.resolve(action);
        if (Files.isExecutable(action_hook)) {
            container.join(Exec.args(action_hook))
                     .environment(env)
                     .checkError()
                     .run();
        }
    }

    private void do_addon_action(Path path, Map<String,String> env, String action, String... args)
        throws IOException
    {
        Path executable = FileUtils.join(path, "bin", action);

        if (Files.exists(executable)) {
            if (!Files.isExecutable(executable)) {
                FileUtils.chmod(executable, 0755);
            }

            Exec exec = Exec.args(executable);
            exec.command().addAll(Arrays.asList(args));
            container.join(exec)
                     .directory(path)
                     .environment(getAddonEnv(path, env))
                     .checkError()
                     .run();
        }
    }

    private void addAddonEnvVar(Path path, String name, String value, boolean prefix)
        throws IOException
    {
        Path envdir = path.resolve("env");
        if (!Files.exists(envdir)) {
            FileUtils.mkdir(envdir);
        }

        String filename = name.toUpperCase();
        if (prefix) filename = "CLOUDWAY_" + filename;
        FileUtils.write(envdir.resolve(filename), value);
    }

    private Map<String, String> getAddonEnv(Path path, Map<String,String> app_env) {
        // make sure the addon's environments overrides that of other addons
        Map<String, String> env =
            app_env == null ? Environ.loadAll(container.getHomeDir())
                            : new HashMap<>(app_env);
        env.putAll(Environ.load(path.resolve("env")));
        return env;
    }
}
