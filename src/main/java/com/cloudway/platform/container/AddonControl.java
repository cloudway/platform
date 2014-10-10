/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container;

import java.io.IOException;
import java.nio.file.FileSystem;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.PathMatcher;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import static java.util.stream.Collectors.toMap;
import static java.nio.file.StandardCopyOption.*;
import static java.nio.file.LinkOption.NOFOLLOW_LINKS;

import com.cloudway.platform.common.util.AbstractFileVisitor;
import com.cloudway.platform.common.util.Exec;
import com.cloudway.platform.common.util.FileUtils;
import com.cloudway.platform.common.util.IO;
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
        control("start");
    }

    public void stop() throws IOException {
        control("stop");
    }

    public void tidy() throws IOException {
        control("tidy");
    }

    public void install(String name, Path source, String templateUrl)
        throws IOException
    {
        Map<String, Addon> addons = addons(); // may load already installed addons
        Path target = container.getHomeDir().resolve(name);

        if (addons.containsKey(name) || Files.exists(target)) {
            throw new IllegalArgumentException("Addon already installed");
        }

        try {
            // copy source files into target
            installFiles(source.toRealPath(), target);
            container.setFileTreeReadWrite(target);

            // TODO: load addon metadata
            Addon addon = new Addon(target);
            addon.setType(AddonType.FRAMEWORK);

            // add environment variables
            Path envdir = target.resolve("env");
            addEnvVar(envdir, name + "_DIR", target.toString() + "/", true);

            // run addon actions
            with_unlocked(target, () -> action(addon, "install"));

            // populate template repository
            if (addon.getType() == AddonType.FRAMEWORK) {
                ApplicationRepository repo = ApplicationRepository.newInstance(container);
                if (templateUrl == null || templateUrl.isEmpty()) {
                    repo.populateFromTemplate(target);
                } else if (templateUrl.equals("empty")) {
                    repo.populateEmpty();
                } else {
                    repo.populateFromURL(templateUrl);
                }
            }

            // put addon into cache
            addons.put(name, addon);
        } catch (IOException|RuntimeException|Error ex) {
            FileUtils.deleteTree(target);
            throw ex;
        }
    }

    private void installFiles(Path source, Path target)
        throws IOException
    {
        if (Files.isDirectory(source)) {
            copydir(source, target);
        } else {
            extract(source, target);
        }
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

        if (name.endsWith(".zip")) {
            Exec.args("unzip", "-d", target, source)
                .silentIO()
                .checkError()
                .run();
            try (Stream<Path> files = Files.list(target.resolve("bin"))) {
                IO.forEach(files, file -> FileUtils.chmod(file, 0755));
            }
        } else if (name.endsWith(".tar.gz") && name.endsWith(".tgz")) {
            FileUtils.mkdir(target);
            Exec.args("tar", "-C", target, "-xzpf", source)
                .silentIO()
                .checkError()
                .run();
        } else if (name.endsWith(".tar")) {
            FileUtils.mkdir(target);
            Exec.args("tar", "-C", target, "-xpf", source)
                .silentIO()
                .checkError()
                .run();
        } else {
            throw new IOException("Unsupported addon archive file: " + source);
        }
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
                 .filter(Objects::nonNull)
                 .forEach(entry -> {
                     if (GLOB_CHARS.matcher(entry).find()) {
                         patterns.add(entry);
                     } else {
                         entries.add(entry);
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
            // Only allow files in app, the addon directory, or dot files/dirs
            if (!path.startsWith(home.resolve("app")) && !path.startsWith(target)) {
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

    private void addEnvVar(Path path, String name, String value, boolean prefix)
        throws IOException
    {
        if (!Files.exists(path)) {
            FileUtils.mkdir(path);
        }

        name = name.toUpperCase();
        if (prefix) {
            name = "CLOUDWAY_" + name;
        }

        Path file = path.resolve(name);
        FileUtils.write(file, value);
    }

    public void remove(String name) throws IOException {
        // TODO
    }

    public void destroy() throws IOException {
        // TODO
    }

    private void control(String action) throws IOException {
        for (Addon addon : addons().values()) {
            action(addon, "control", action);
        }
    }

    private void action(Addon addon, String action, String... args)
        throws IOException
    {
        Path path = addon.getPath();

        Path executable = FileUtils.join(path, "bin", action);
        if (!Files.isExecutable(executable)) {
            return;
        }

        // Make sure this addon's environments overrides that of other addons
        Map<String, String> env = Environ.loadAll(container.getHomeDir());
        env.putAll(Environ.load(path.resolve("env")));

        Exec exec = Exec.args(executable.toString());
        if (args.length > 0)
            exec.command().addAll(Arrays.asList(args));
        exec.environment().putAll(env);
        exec.directory(path);
        container.join(exec).checkError().run();
    }
}
