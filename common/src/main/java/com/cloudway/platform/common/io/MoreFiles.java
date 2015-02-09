/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.io;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.channels.FileChannel;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileVisitResult;
import java.nio.file.FileVisitor;
import java.nio.file.Files;
import java.nio.file.OpenOption;
import java.nio.file.Path;
import java.nio.file.PathMatcher;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.FileOwnerAttributeView;
import java.nio.file.attribute.FileTime;
import java.nio.file.attribute.PosixFileAttributeView;
import java.nio.file.attribute.PosixFilePermission;
import java.nio.file.attribute.UserPrincipalLookupService;
import java.util.EnumSet;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.locks.Lock;
import java.util.stream.Stream;

import com.cloudway.platform.common.os.Exec;
import com.google.common.io.ByteSink;
import com.google.common.io.ByteSource;
import com.google.common.io.CharSink;
import com.google.common.io.CharSource;
import com.google.common.io.LineProcessor;

import static java.nio.file.FileVisitResult.*;
import static java.nio.file.StandardOpenOption.*;
import static java.nio.file.StandardCopyOption.*;
import static java.nio.file.LinkOption.NOFOLLOW_LINKS;
import static java.nio.file.attribute.PosixFilePermissions.*;

/**
 * Provides utility methods for working with files.
 */
public final class MoreFiles
{
    private MoreFiles() {}

    /**
     * Concatenate path components to create a Path object.
     *
     * @param first the first component in the path
     * @param more other components in the path
     * @return the path
     */
    public static Path join(Path first, String... more) {
        Path path = first;
        for (String e : more) {
            path = path.resolve(e);
        }
        return path;
    }

    /**
     * Creates a directory by creating all nonexistent parent directories first.
     *
     * @param dir the directory to create
     * @return the directory
     * @throws IOException if an I/O error occurs
     */
    public static Path mkdir(Path dir)
        throws IOException
    {
        return Files.createDirectories(dir);
    }

    /**
     * Creates a directory by creating all nonexistent parent directories first.
     *
     * @param dir the directory to create
     * @param perms the unix style directory permission bits.
     * @return the directory
     * @throws IOException if an I/O error occurs
     */
    public static Path mkdir(Path dir, int perms)
        throws IOException
    {
        return Files.createDirectories(dir, asFileAttribute(fromBits(perms)));
    }

    /**
     * Creates any necessary but nonexistent parent directories of the specified
     * file. Note that if this operation fails it may have succeeded in creating
     * some (but not all) of the necessary parent directories.
     *
     * @throws IOException if an I/O error occurs, or if any necessary but
     *    nonexistent parent directories of the specified file could not be
     *    created.
     */
    public static void createParentDirs(Path file) throws IOException {
        Objects.requireNonNull(file);
        Path parent = file.toRealPath().getParent();
        if (parent == null) {
            // The given directory is a filesystem root. All zero of its ancestors
            // exist. This doesn't mean that the root itself exists -- consider x:\ on
            // a Windows machine without such a drive -- or even that the caller can
            // create it, but this method makes no such guarantees even for non-root
            // files.
            return;
        }
        mkdir(parent);
    }

    /**
     * Create an empty file or updates the last updated timestamp on the
     * same as the unix command of the same name.
     *
     * @param file the file to create or update
     * @param perms the unix style file permission bits
     * @throws IOException if an I/O error occurs
     */
    public static void touch(Path file, int perms)
        throws IOException
    {
        if (!Files.exists(file)) {
            Files.createFile(file, asFileAttribute(fromBits(perms)));
        } else {
            Files.setLastModifiedTime(file, FileTime.fromMillis(System.currentTimeMillis()));
        }
    }

    /**
     * Change the owner of a file or directory on the same as the unix command of
     * the same name.
     *
     * @param path the path to change owner
     * @param owner the owner of the file
     * @param group the group of the file
     * @throws IOException if an I/O error occurs
     */
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

    /**
     * Change permissions of a file or directory on the same as the unix command of
     * the same name.
     *
     * @param path the path to change permissions
     * @param perms the unix style file permission bits
     * @throws IOException if an I/O error occurs
     */
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

    /**
     * Returns the file permission bits in unix style.
     *
     * @param path the path to return the file permissions
     * @return the file permission bits in unix style
     * @throws IOException if an I/O error occurs
     */
    public static int getFileMode(Path path)
        throws IOException
    {
        return toBits(Files.getPosixFilePermissions(path));
    }

    private static final PosixFilePermission[] PERM_BITS = {
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

    public static void deleteFileTree(Path file) throws IOException {
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

    public static void copyFileTree(final Path source, final Path target)
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

    /**
     * Returns a new {@link ByteSource} for reading bytes from the given file.
     */
    public static ByteSource asByteSource(Path path) {
        return new FileByteSource(path);
    }

    private static final class FileByteSource extends ByteSource {
        private final Path path;

        private FileByteSource(Path path) {
            this.path = Objects.requireNonNull(path);
        }

        @Override
        public InputStream openStream() throws IOException {
            return Files.newInputStream(path, READ);
        }

        @Override
        public long size() throws IOException {
            if (!Files.isRegularFile(path)) {
                throw new FileNotFoundException(path.toString());
            }
            return Files.size(path);
        }

        @Override
        public byte[] read() throws IOException {
            return Files.readAllBytes(path);
        }

        @Override
        public String toString() {
            return "ByteSource(" + path + ")";
        }
    }

    /**
     * Returns as new {@link ByteSink} for writing bytes to the given file. The
     * given {@code options} control how the file is opened for writing. When no
     * option is provided, the file will be truncated before writing. When the
     * {@link StandardOpenOption#APPEND APPEND} mode is provided, writes will
     * append to the end of the file without truncating it.
     */
    public static ByteSink asByteSink(Path path, OpenOption... options) {
        return new FileByteSink(path, options);
    }

    private static final class FileByteSink extends ByteSink {
        private final Path path;
        private final OpenOption[] options;

        private FileByteSink(Path path, OpenOption... options) {
            this.path = Objects.requireNonNull(path);
            this.options = options.clone();
        }

        @Override
        public OutputStream openStream() throws IOException {
            return Files.newOutputStream(path, options);
        }
    }

    /**
     * Returns a new {@link CharSource} for reading character data from the given
     * file using the given character set.
     */
    public static CharSource asCharSource(Path path, Charset charset) {
        return asByteSource(path).asCharSource(charset);
    }

    /**
     * Returns a new {@link CharSource} for reading character data from the given
     * file using the UTF-8 character set.
     */
    public static CharSource asCharSource(Path path) {
        return asCharSource(path, StandardCharsets.UTF_8);
    }

    /**
     * Returns a new {@link CharSink} for writing character data to the given
     * file using the given character set. The given {@code options} control how
     * the file is opened for writing. When no options is provided, the file
     * will be truncated before writing.
     */
    public static CharSink asCharSink(Path path, Charset charset, OpenOption... options) {
        return asByteSink(path, options).asCharSink(charset);
    }

    /**
     * Returns a new {@link CharSink} for writing character data to the given
     * file using the UTF-8 character set. The given {@code options} control how
     * the file is opened for writing. When no options is provided, the file
     * will be truncated before writing.
     */
    public static CharSink asCharSink(Path path, OpenOption... options) {
        return asCharSink(path, StandardCharsets.UTF_8, options);
    }

    /**
     * Reads all characters from a file into a {@link String}, using the UTF-8
     * character set.
     *
     * @param file the file to read from
     * @return a string containing all the characters from the file
     * @throws IOException if an I/O error occurs
     */
    public static String readText(Path file)
        throws IOException
    {
        return readText(file, StandardCharsets.UTF_8);
    }

    /**
     * Reads all characters from a file into a {@link String}, using the given
     * character set.
     *
     * @param file the file to read from
     * @param charset the charset used to decode the input stream
     * @return a string containing all the characters from the file
     * @throws IOException if an I/O error occurs
     */
    public static String readText(Path file, Charset charset)
        throws IOException
    {
        return asCharSource(file, charset).read();
    }

    /**
     * Writes a character sequence (such as a string) to a file using the
     * UTF-8 character set.
     *
     * @param file the destination file
     * @param contents the character sequence to write
     * @throws IOException if an I/O error occurs
     */
    public static void writeText(Path file, CharSequence contents)
        throws IOException
    {
        writeText(file, contents, StandardCharsets.UTF_8);
    }

    /**
     * Writes a character sequence (such as a string) to a file using the
     * given character set.
     *
     * @param file the destination file
     * @param contents the character sequence to write
     * @param charset the charset used to encode the output stream
     * @throws IOException if an I/O error occurs
     */
    public static void writeText(Path file, CharSequence contents, Charset charset)
        throws IOException
    {
        asCharSink(file, charset).write(contents);
    }

    /**
     * Streams lines from {@link Path}.
     *
     * @param file the file to read from
     * @param charset the charset used to decode the input stream
     * @param callback the {@link IOConsumer consumer} to use to handle the lines
     * @throws IOException if an I/O error occurs
     */
    public static void readLines(Path file, Charset charset, IOConsumer<String> callback)
        throws IOException
    {
        asCharSource(file, charset).readLines(new LineProcessor<Void>() {
            @Override
            public boolean processLine(String line) throws IOException {
                callback.consume(line);
                return true;
            }

            @Override
            public Void getResult() {
                return null;
            }
        });
    }

    /**
     * Streams lines from {@link Path}.
     *
     * @param file the file to read from
     * @param callback the {@link IOConsumer consumer} to use to handle the lines
     * @throws IOException if an I/O error occurs
     */
    public static void readLines(Path file, IOConsumer<String> callback)
        throws IOException
    {
        readLines(file, StandardCharsets.UTF_8, callback);
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
     * <p>Create a file lock for the duration of the provided block.</p>
     *
     * <p>A local lock is also used because file lock cannot be overlapped
     * in a Java virtual machine.</p>
     *
     * @param lock_file path including file to use for locking
     * @param local_lock lock used for multi-threaded locking
     * @param action the block to run
     */
    public static void flock(Path lock_file, Lock local_lock, IOConsumer<FileChannel> action)
        throws IOException
    {
        createParentDirs(lock_file);

        local_lock.lock();
        try (FileChannel file = FileChannel.open(lock_file, READ, WRITE, CREATE)) {
            file.lock();
            action.consume(file);
        } finally {
            local_lock.unlock();
        }
    }
}
