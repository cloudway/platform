/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container.adapters;

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.net.Socket;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.stream.Stream;

import com.cloudway.fp.io.IO;
import com.cloudway.fp.function.ExceptionAction;
import com.cloudway.platform.common.os.AuthorizedKey;
import com.cloudway.platform.common.os.Config;
import com.cloudway.platform.common.util.MoreFiles;
import com.cloudway.platform.container.ApplicationContainer;
import com.cloudway.platform.container.ContainerAdapter;

import static java.lang.String.format;
import static java.nio.file.Files.*;
import static com.cloudway.platform.common.util.MoreFiles.*;
import static com.cloudway.platform.common.util.MoreCollectors.*;
import static com.cloudway.fp.control.Predicates.*;

public abstract class AbstractContainerAdapter implements ContainerAdapter
{
    protected final ApplicationContainer container;

    /**
     * Create a new instance of ContainerAdapter.
     */
    protected AbstractContainerAdapter(ApplicationContainer container) {
        this.container = container;
    }

    @Override
    public void create()
        throws IOException
    {
        try {
            createUser();
            createHomeDir();
        } catch (IOException ex) {
            // cleanup when creating container failed
            nothrow(this::deleteUser);
            throw ex;
        }
    }

    @Override
    public void destroy() {
        nothrow(this::killProcs);
        nothrow(this::deleteUser);
    }

    /**
     * Implemented by subclass to create the operating system user.
     */
    protected abstract void createUser()
        throws IOException;

    /**
     * Implemented by subclass to delete the operating system user.
     */
    protected abstract void deleteUser()
        throws IOException;

    /**
     * Create and populate the users home dir.
     */
    protected void createHomeDir() throws IOException {
        createHomeSkeleton();
        populateHome();
        secureHome();
    }

    protected void createHomeSkeleton() throws IOException {
        Path homedir = container.getHomeDir();

        // Required for polyinstantiated tmp dirs to work
        mkdir(homedir.resolve(".tmp"), 0000);

        Path env_dir = mkdir(homedir.resolve(".env"), 0750);
        setFileReadOnly(env_dir);

        Path ssh_dir = mkdir(homedir.resolve(".ssh"), 0750);
        setFileReadOnly(ssh_dir);

        Path app_dir = mkdir(homedir.resolve("app"));

        Path log_dir = mkdir(app_dir.resolve("logs"), 0750);
        setenv("CLOUDWAY_LOG_DIR", log_dir.toString());

        Path data_dir = mkdir(app_dir.resolve("data"));
        setenv("CLOUDWAY_DATA_DIR", data_dir.toString());

        Path repo_dir = mkdir(app_dir.resolve("repo"));
        setenv("CLOUDWAY_REPO_DIR", repo_dir.toString());

        // add environment variables
        setenv("CLOUDWAY_APP_ID", container.getId());
        setenv("CLOUDWAY_APP_NAME", container.getName());
        setenv("CLOUDWAY_APP_DNS", container.getDomainName());
        setenv("CLOUDWAY_APP_SIZE", container.getCapacity());
        setenv("CLOUDWAY_HOME_DIR", homedir.toString());
        setenv("HOME", homedir.toString());
    }

    protected void populateHome() throws IOException {
        Path homedir = container.getHomeDir();
        Path datadir = container.getDataDir();

        // setup shell environment
        setenv("HISTFILE", datadir.resolve(".bash_history").toString());
        Path profile = datadir.resolve(".bash_profile");
        writeText(profile,
                  "# Warning: Be careful with modification to this file,\n" +
                  "#          Your changes may cause your application to fail\n");
        Path vimrc = datadir.resolve(".vimrc");
        writeText(vimrc, "set viminfo+=n" + datadir.resolve(".viminfo"));
        createSymbolicLink(homedir.resolve(".vimrc"), vimrc);
    }

    protected void secureHome() throws IOException {
        // update all directory entries in ~/app
        Path appdir = container.getAppDir();
        setFileTreeReadWrite(appdir);
        setFileReadOnly(appdir);

        // finally set home directory's permission
        Path homedir = container.getHomeDir();
        chmod(homedir, 0750);
        setFileReadOnly(homedir);
    }

    protected final void setenv(String key, String value) {
        container.setenv(key, value);
    }

    @Override
    public void start() throws IOException {
        // implemented by subclass
    }

    @Override
    public void stop(long term_delay, TimeUnit unit) throws IOException {
        killProcs(term_delay, unit);
    }

    /**
     * Kill processes belonging to this application container.
     */
    protected void killProcs() throws IOException {
        killProcs(0, null);
    }

    /**
     * Kill processes belonging to this application container.
     *
     * @param term_delay send SIGTERM first, wait term_delay then send SIGKILL.
     */
    protected abstract void killProcs(long term_delay, TimeUnit unit)
        throws IOException;

    @Override
    public String getIpAddress(int host_id) {
        long uid = container.getUID();

        if (uid < 0 || uid > Integer.MAX_VALUE)
            throw new IllegalArgumentException("User uid " + uid + " must be unsigned 32 bit integers.");
        if (host_id < 1 || host_id > 127)
            throw new IllegalArgumentException("Supplied host identifier " + host_id + " must be between 1 and 127");

        Config config = Config.getDefault();
        int uid_wraparound = config.getInt("UID_WRAPAROUND", 65536);
        int ip_offset = config.getInt("IP_OFFSET", 1);

        if (ip_offset >= 1000)
            ip_offset = 0;
        if (uid < uid_wraparound)
            ip_offset = 0;

        // Generate an IP (32-bit unsigned) in the user's range
        int loopback_start = 0x7F000000;
        int ip = loopback_start + (int)((uid % uid_wraparound + ip_offset) << 7) + host_id;

        // Return the IP in dotted-quad notation
        return (ip >> 24) + "." + ((ip >> 16) & 0xFF) + "." + ((ip >> 8) & 0xFF) + "." + (ip & 0xFF);
    }

    @Override
    public boolean isAddressInUse(String ip, int port) {
        try (Socket ignored = new Socket(ip, port)) {
            return true;
        } catch (IOException ex) {
            return false;
        }
    }

    private static List<AuthorizedKey> loadAuthorizedKeys(RandomAccessFile file)
        throws IOException
    {
        List<AuthorizedKey> keys = new ArrayList<>();
        String line;
        while ((line = file.readLine()) != null) {
            keys.add(AuthorizedKey.parse(line));
        }
        return keys;
    }

    private static void saveAuthorizedKeys(RandomAccessFile file, List<AuthorizedKey> keys)
        throws IOException
    {
        file.seek(0);
        IO.forEach(keys, key -> {
            file.write(key.toString().getBytes());
            file.write('\n');
        });
        file.setLength(file.getFilePointer());
    }

    private File authorizedKeysFile() {
        return MoreFiles.join(container.getHomeDir(), ".ssh", "authorized_keys").toFile();
    }

    @Override
    public void addAuthorizedKey(String name, String key)
        throws IOException
    {
        AuthorizedKey newkey = AuthorizedKey.parser()
            .withId(container.getId(), name)
            .withOptions(format("command=\"%s\",no-X11-forwarding", container.getShell()))
            .parsePublicKey(key);

        try (RandomAccessFile file = new RandomAccessFile(authorizedKeysFile(), "rw")) {
            List<AuthorizedKey> keys = loadAuthorizedKeys(file);
            if (keys.stream().anyMatch(k -> newkey.getComment().equals(k.getComment()) ||
                                            newkey.getBits().equals(k.getBits()))) {
                throw new IllegalArgumentException("Authorized key already exist: " + name);
            }

            keys.add(newkey);
            saveAuthorizedKeys(file, keys);
        }
    }

    @Override
    public void removeAuthorizedKey(String key)
        throws IOException
    {
        String keybits = AuthorizedKey.parsePublicKey(key).getBits();
        try (RandomAccessFile file = new RandomAccessFile(authorizedKeysFile(), "rw")) {
            List<AuthorizedKey> keys = loadAuthorizedKeys(file);
            if (keys.removeIf(having(AuthorizedKey::getBits, is(keybits)))) {
                saveAuthorizedKeys(file, keys);
            }
        }
    }

    @Override
    public List<String> getAuthorizedKeys()
        throws IOException
    {
        try (RandomAccessFile file = new RandomAccessFile(authorizedKeysFile(), "r")) {
            // convert to public key string
            return loadAuthorizedKeys(file).stream()
                .map(k -> k.toPublicKey().toString())
                .collect(toImmutableList());
        }
    }

    @Override
    public void setFileReadOnly(Path file) throws IOException {
        chown(file, "root", container.getId());
    }

    @Override
    public void setFileReadWrite(Path file) throws IOException {
        chown(file, container.getId(), container.getId());
    }

    @Override
    public void setFileTreeReadOnly(Path dir)
        throws IOException
    {
        try (Stream<Path> files = Files.walk(dir)) {
            IO.forEach(files, this::setFileReadOnly);
        }
    }

    @Override
    public void setFileTreeReadWrite(Path dir)
        throws IOException
    {
        try (Stream<Path> files = Files.walk(dir)) {
            IO.forEach(files, this::setFileReadWrite);
        }
    }

    protected static void nothrow(ExceptionAction<Exception> action) {
        try {
            action.perform();
        } catch (Exception ex) {
            // log and ignore
        }
    }

    @SafeVarargs
    protected static void nothrow(ExceptionAction<Exception>... actions) {
        Stream.of(actions).forEach(AbstractContainerAdapter::nothrow);
    }
}
