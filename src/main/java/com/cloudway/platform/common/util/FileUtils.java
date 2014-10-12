/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.channels.FileChannel;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileVisitResult;
import java.nio.file.FileVisitor;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.PathMatcher;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.FileOwnerAttributeView;
import java.nio.file.attribute.PosixFileAttributeView;
import java.nio.file.attribute.PosixFilePermission;
import java.nio.file.attribute.UserPrincipalLookupService;
import java.util.EnumSet;
import java.util.Set;
import java.util.stream.Stream;

import static java.nio.file.FileVisitResult.*;
import static java.nio.file.StandardOpenOption.*;
import static java.nio.file.StandardCopyOption.*;
import static java.nio.file.LinkOption.NOFOLLOW_LINKS;
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

    public static void touch(Path file, int perms)
        throws IOException
    {
        if (!Files.exists(file)) {
            Files.createFile(file, asFileAttribute(fromBits(perms)));
        }
    }

    public static void chown(Path path, String owner, String group)
        throws IOException
    {
        if (Files.isSymbolicLink(path)) {
            // FIXME: JDK implementation always change owner to the target of a symbolic link
            owner = group == null ? owner : owner + ":" + group;
            Exec.args("chown", "-h", owner, path.toString()).silentIO().run();
        } else {
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
    }

    public static void chmod(Path path, int perms)
        throws IOException
    {
        // chmod never changes the permissions of symbolic links;
        // the chmod system call cannot change their permissions.
        // This is not a problem since the permissions of symbolic
        // links are never used.
        if (!Files.isSymbolicLink(path)) {
            Files.setPosixFilePermissions(path, fromBits(perms));
        }
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
            if (Files.isDirectory(file) && !Files.isSymbolicLink(file))
                emptyDirectory(file);
            Files.delete(file);
        }
    }

    public static void emptyDirectory(Path start) throws IOException {
        Files.walkFileTree(start, new FileVisitor<Path>() {
            public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) {
                return CONTINUE;
            }

            public FileVisitResult visitFile(Path file, BasicFileAttributes attrs)
                throws IOException
            {
                Files.delete(file);
                return CONTINUE;
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
                return CONTINUE;
            }
        });
    }

    public static void copyTree(final Path source, final Path target)
        throws IOException
    {
        Path realSource = source.toRealPath();
        try (Stream<Path> files = Files.walk(realSource)) {
            IO.forEach(files, file -> {
                Files.copy(file, target.resolve(realSource.relativize(file)),
                           REPLACE_EXISTING, COPY_ATTRIBUTES, NOFOLLOW_LINKS);
            });
        }
    }

    public static void write(Path file, String contents)
        throws IOException
    {
        try (BufferedWriter out = Files.newBufferedWriter(file, StandardCharsets.UTF_8)) {
            out.write(contents);
        }
    }

    public static String read(Path file)
        throws IOException
    {
        return read(file, StandardCharsets.UTF_8);
    }

    public static String read(Path file, Charset cs)
        throws IOException
    {
        return new String(Files.readAllBytes(file), cs);
    }

    public static String chomp(Path file)
        throws IOException
    {
        return chomp(Files.readAllBytes(file), StandardCharsets.ISO_8859_1);
    }

    public static String chomp(byte[] b, Charset cs) {
        int i = b.length - 1;
        while (i >= 0) {
            if (b[i] == '\n' || b[i] == '\r') {
                i--;
            } else {
                break;
            }
        }
        return new String(b, 0, i+1, cs);
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
