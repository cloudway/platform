/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.channels.FileChannel;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileVisitResult;
import java.nio.file.FileVisitor;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.PathMatcher;
import java.nio.file.StandardCopyOption;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.FileOwnerAttributeView;
import java.nio.file.attribute.PosixFileAttributeView;
import java.nio.file.attribute.PosixFilePermission;
import java.nio.file.attribute.UserPrincipalLookupService;
import java.util.EnumSet;
import java.util.Set;
import java.util.function.BiPredicate;
import java.util.stream.Stream;

import static java.nio.file.StandardOpenOption.*;
import static java.nio.file.attribute.PosixFilePermissions.*;

public class FileUtils
{
    public static Path join(Path first, String... more) {
        for (String e : more) {
            first = first.resolve(e);
        }
        return first;
    }

    public static Path mkdir(Path dir)
        throws IOException
    {
        return Files.createDirectories(dir);
    }

    public static Path mkdir(Path dir, int perms)
        throws IOException
    {
        return Files.createDirectories(dir, asFileAttribute(fromBits(perms)));
    }

    public static Path mkdir(Path dir, String perms)
        throws IOException
    {
        return Files.createDirectories(dir, asFileAttribute(fromString(perms)));
    }

    public static Path touch(Path file, int perms)
        throws IOException
    {
        return Files.createFile(file, asFileAttribute(fromBits(perms)));
    }

    public static Path touch(Path file, String perms)
        throws IOException
    {
        return Files.createFile(file, asFileAttribute(fromString(perms)));
    }

    public static void chown(Path path, String owner, String group)
        throws IOException
    {
        UserPrincipalLookupService lookup =
            path.getFileSystem().getUserPrincipalLookupService();

        if (group == null) {
            FileOwnerAttributeView view =
                Files.getFileAttributeView(path, FileOwnerAttributeView.class);
            if (view == null)
                throw new UnsupportedOperationException();
            view.setOwner(lookup.lookupPrincipalByName(owner));
        } else {
            PosixFileAttributeView view =
                Files.getFileAttributeView(path, PosixFileAttributeView.class);
            if (view == null)
                throw new UnsupportedOperationException();
            view.setOwner(lookup.lookupPrincipalByName(owner));
            view.setGroup(lookup.lookupPrincipalByGroupName(group));
        }
    }

    public static void chmod(Path path, int perms)
        throws IOException
    {
        Files.setPosixFilePermissions(path, fromBits(perms));
    }

    public static void chmod(Path path, String perms)
        throws IOException
    {
        Files.setPosixFilePermissions(path, fromString(perms));
    }

    public static int getFileMode(Path path)
        throws IOException
    {
        return toBits(Files.getPosixFilePermissions(path));
    }

    private static PosixFilePermission[] PERM_BITS = {
        PosixFilePermission.OTHERS_EXECUTE,
        PosixFilePermission.OTHERS_WRITE,
        PosixFilePermission.OTHERS_READ,
        PosixFilePermission.GROUP_EXECUTE,
        PosixFilePermission.GROUP_WRITE,
        PosixFilePermission.GROUP_READ,
        PosixFilePermission.OWNER_EXECUTE,
        PosixFilePermission.OWNER_WRITE,
        PosixFilePermission.OWNER_READ
    };

    private static Set<PosixFilePermission> fromBits(int bits) {
        Set<PosixFilePermission> set = EnumSet.noneOf(PosixFilePermission.class);
        for (PosixFilePermission p : PERM_BITS) {
            if ((bits & 1) != 0)
                set.add(p);
            bits >>= 1;
        }
        return set;
    }

    private static int toBits(Set<PosixFilePermission> set) {
        int bits = 0;
        for (int i = PERM_BITS.length - 1; i >= 0; i--) {
            if (set.contains(PERM_BITS[i]))
                bits |= 1;
            bits <<= 1;
        }
        return bits >> 1;
    }

    public static Stream<Path> find(Path start, int maxDepth, String glob)
        throws IOException
    {
        PathMatcher matcher = start.getFileSystem().getPathMatcher("glob:" + glob);
        return Files.find(start, maxDepth, (path, attrs) -> matcher.matches(path.getFileName()));
    }

    public static void deleteTree(Path file) throws IOException {
        if (Files.exists(file)) {
            if (Files.isDirectory(file))
                emptyDirectory(file);
            Files.delete(file);
        }
    }

    public static void emptyDirectory(Path start) throws IOException {
        Files.walkFileTree(start, new FileVisitor<Path>() {
            public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) {
                return FileVisitResult.CONTINUE;
            }

            public FileVisitResult visitFile(Path file, BasicFileAttributes attrs)
                throws IOException
            {
                Files.delete(file);
                return FileVisitResult.CONTINUE;
            }

            public FileVisitResult visitFileFailed(Path file, IOException exc)
                throws IOException
            {
                throw exc;
            }

            public FileVisitResult postVisitDirectory(Path dir, IOException exc)
                throws IOException
            {
                if (exc != null)
                    throw exc;
                if (!dir.equals(start))
                    Files.delete(dir);
                return FileVisitResult.CONTINUE;
            }
        });
    }

    public static void copyTree(Path source, Path target)
        throws IOException
    {
        try (Stream<Path> stream = Files.walk(source)) {
            stream.forEach(IO.wrap((Path file) -> copyFile(source, target, file)));
        } catch (RuntimeIOException ex) {
            throw ex.getCause();
        }
    }

    public static void copyTree(Path source, Path target, String glob)
        throws IOException
    {
        if (glob == null || glob.equals("*")) {
            copyTree(source, target);
            return;
        }

        // create a matcher and return a filter that uses it.
        PathMatcher matcher = source.getFileSystem().getPathMatcher("glob:" + glob);
        BiPredicate<Path, BasicFileAttributes> filter =
            (file, attrs) -> Files.isDirectory(file) || matcher.matches(file.getFileName());

        try (Stream<Path> stream = Files.find(source, Integer.MAX_VALUE, filter)) {
            stream.forEach(IO.wrap((Path file) -> copyFile(source, target, file)));
        } catch (RuntimeIOException ex) {
            throw ex.getCause();
        }
    }

    private static void copyFile(Path sourceDir, Path targetDir, Path file)
        throws IOException
    {
        Files.copy(file, targetDir.resolve(sourceDir.relativize(file)),
                   StandardCopyOption.REPLACE_EXISTING,
                   StandardCopyOption.COPY_ATTRIBUTES,
                   LinkOption.NOFOLLOW_LINKS);
    }

    public static String read(Path file)
        throws IOException
    {
        byte[] contents = Files.readAllBytes(file);
        return new String(contents, StandardCharsets.UTF_8);
    }

    public static void write(Path file, String contents)
        throws IOException
    {
        try (BufferedWriter out = Files.newBufferedWriter(file, StandardCharsets.UTF_8)) {
            out.write(contents);
        }
    }

    /**
     * Create a file lock for the duration of the provided block.
     *
     * @param file path including file to use for locking
     * @param action the block to run
     */
    public static void flock(Path file, IO.Runnable action)
        throws IOException
    {
        try (FileChannel channel = FileChannel.open(file, READ, WRITE, CREATE, TRUNCATE_EXISTING)) {
            channel.lock();
            action.run();
        }
    }
}
