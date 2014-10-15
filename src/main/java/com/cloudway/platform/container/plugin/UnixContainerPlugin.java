/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container.plugin;

import java.io.File;
import java.io.IOException;
import java.io.InterruptedIOException;
import java.io.RandomAccessFile;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import com.cloudway.platform.common.util.Etc;
import com.cloudway.platform.common.util.IO;
import com.cloudway.platform.container.ApplicationContainer;
import com.cloudway.platform.container.ContainerPlugin;
import com.cloudway.platform.common.Config;
import com.cloudway.platform.common.util.Exec;
import com.cloudway.platform.common.util.FileUtils;
import com.cloudway.platform.common.AuthorizedKey;

/**
 * Generic UNIX container plugin.
 */
public class UnixContainerPlugin extends ContainerPlugin
{
    private static final Path create_lock_file = Config.LOCK_DIR.resolve("create.lck");
    private static final Lock create_lock = new ReentrantLock();

    public UnixContainerPlugin(ApplicationContainer container) {
        super(container);
    }

    @Override
    protected void createUser() throws IOException {
        Config config = Config.getDefault();
        int min_uid = config.getInt("GUEST_MIN_UID", 1000);
        int max_uid = config.getInt("GUEST_MAX_UID", 2000);

        // Synchronized to prevent race condition on obtaining a UNIX user uid.
        FileUtils.flock(create_lock_file, create_lock, () -> {
            // Determine next available user id
            int uid = IntStream.rangeClosed(min_uid, max_uid)
                .filter(i -> Etc.getpwuid(i) == null && Etc.getgrgid(i) == null)
                .findFirst()
                .getAsInt(); // XXX may throw NoSuchElementException

            // Create operating system user
            createUser(container.getUuid(),
                       uid,
                       container.getHomeDir(),
                       config.get("GUEST_SKEL", Config.CONF_DIR.resolve("skel").toString()),
                       ApplicationContainer.SHELL,
                       ApplicationContainer.GECOS,
                       config.get("GUEST_SUPPLEMENTARY_GROUPS"));

            container.setUID(uid);
            container.setGID(uid);
        });
    }

    protected void createUser(String uuid, int uid, Path home, String skel, String shell, String gecos, String groups)
        throws IOException
    {
        Exec.args("groupadd", "-g", uid, uuid).silentIO().checkError().run();

        Exec exec = Exec.args(
            "useradd",
            "-u", uid,
            "-g", uid,
            "-d", home,
            "-s", shell,
            "-c", gecos,
            "-m",
            "-k", skel,
            uuid);
        if (groups != null) {
            exec.command().add("-G");
            exec.command().add(groups);
        }
        exec.silentIO().checkError().run();
    }

    @Override
    protected void deleteUser() throws IOException {
        Exec.args("userdel", "-rf", container.getUuid()).silentIO().run();
        FileUtils.deleteTree(container.getHomeDir());
    }

    @Override
    protected void createHomeDir()
        throws IOException
    {
        Path homedir = container.getHomeDir();

        // Required for polyinstantiated tmp dirs to work
        FileUtils.mkdir(homedir.resolve(".tmp"), 0000);

        Path env_dir = FileUtils.mkdir(homedir.resolve(".env"), 0750);
        setFileReadOnly(env_dir);

        Path ssh_dir = FileUtils.mkdir(homedir.resolve(".ssh"), 0750);
        setFileReadOnly(ssh_dir);

        Path app_dir = FileUtils.mkdir(homedir.resolve("app"));

        Path log_dir = FileUtils.mkdir(app_dir.resolve("logs"), 0750);
        addEnvVar("LOG_DIR", log_dir.toString(), true);

        Path data_dir = FileUtils.mkdir(app_dir.resolve("data"));
        addEnvVar("DATA_DIR", data_dir.toString(), true);

        Path repo_dir = FileUtils.mkdir(app_dir.resolve("repo"));
        addEnvVar("REPO_DIR", repo_dir.toString(), true);

        // setup shell environment
        addEnvVar("HISTFILE", data_dir.resolve(".bash_history").toString(), false);
        Path profile = data_dir.resolve(".bash_profile");
        FileUtils.write(profile,
            "# Warning: Be careful with modification to this file,\n" +
            "#          Your changes may cause your application to fail\n");
        Path vimrc = data_dir.resolve(".vimrc");
        FileUtils.write(vimrc, "set viminfo+=n" + data_dir.resolve(".viminfo"));
        Files.createSymbolicLink(homedir.resolve(".vimrc"), vimrc);

        // update all directory entries in ~/app
        setFileTreeReadWrite(app_dir);
        setFileReadOnly(app_dir);

        mk_ssh_dir(homedir);
        generate_ssh_key();

        // finally set home directory's permission
        FileUtils.chmod(homedir, 0750);
        setFileReadOnly(homedir);

        // add environment variables
        addEnvVar("APP_UUID", container.getUuid(), true);
        addEnvVar("APP_NAME", container.getName(), true);
        addEnvVar("APP_DNS",  container.getDomainName(), true);
        addEnvVar("APP_SIZE", container.getCapacity(), true);
        addEnvVar("HOME_DIR", homedir.toString(), true);
        addEnvVar("HOME",     homedir.toString(), false);
    }

    protected void mk_ssh_dir(Path homedir) throws IOException {
        Path ssh_dir     = homedir.resolve(".cloudway_ssh");
        Path known_hosts = ssh_dir.resolve("known_hosts");
        Path ssh_config  = ssh_dir.resolve("config");
        Path ssh_key     = ssh_dir.resolve("id_rsa");
        Path ssh_pub_key = ssh_dir.resolve("id_rsa.pub");

        FileUtils.mkdir(ssh_dir, 0750);
        FileUtils.touch(known_hosts, 0660);
        FileUtils.touch(ssh_config, 0660);
        setFileTreeReadWrite(ssh_dir);

        addEnvVar("APP_SSH_KEY", ssh_key.toString(), true);
        addEnvVar("APP_SSH_PUBLIC_KEY", ssh_pub_key.toString(), true);
    }

    /**
     * Generate an RSA ssh key.
     */
    protected void generate_ssh_key() throws IOException {
        Path ssh_dir = container.getHomeDir().resolve(".cloudway_ssh");
        Path ssh_key = ssh_dir.resolve("id_rsa");
        Path ssh_pub_key = ssh_dir.resolve("id_rsa.pub");

        container.join(Exec.args("ssh-keygen", "-N", "", "-f", ssh_key.toString()))
                 .silentIO()
                 .checkError()
                 .run();

        FileUtils.chmod(ssh_key, 0600);
        FileUtils.chmod(ssh_pub_key, 0600);
        setFileTreeReadWrite(ssh_dir);
    }

    private List<AuthorizedKey> loadAuthorizedKeys(RandomAccessFile file)
        throws IOException
    {
        List<AuthorizedKey> keys = new ArrayList<>();
        String line;
        while ((line = file.readLine()) != null) {
            keys.add(AuthorizedKey.parse(line));
        }
        return keys;
    }

    private void saveAuthorizedKeys(RandomAccessFile file, List<AuthorizedKey> keys)
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
        return FileUtils.join(container.getHomeDir(), ".ssh", "authorized_keys").toFile();
    }

    @Override
    public void addAuthorizedKey(String id, String key)
        throws IOException
    {
        AuthorizedKey newkey = AuthorizedKey.parsePublicKey(key);
        String uuid = container.getUuid();

        try (RandomAccessFile file = new RandomAccessFile(authorizedKeysFile(), "rw")) {
            List<AuthorizedKey> keys = loadAuthorizedKeys(file);
            if (keys.stream()
                    .filter(k -> id.equals(k.getId(uuid)) || newkey.getBits().equals(k.getBits()))
                    .findAny()
                    .isPresent()) {
                throw new IllegalArgumentException("Authorized key already exist: " + id);
            }

            newkey.setId(uuid, id);
            newkey.setOptions(String.format("command=\"%s\",no-X11-forwarding", container.getShell()));
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
            if (keys.removeIf(k -> keybits.equals(k.getBits()))) {
                saveAuthorizedKeys(file, keys);
            }
        }
    }

    @Override
    public List<String> getAuthorizedKeys()
        throws IOException
    {
        String uuid = container.getUuid();
        try (RandomAccessFile file = new RandomAccessFile(authorizedKeysFile(), "r")) {
            // convert to public key string
            return loadAuthorizedKeys(file).stream()
                .map(k -> k.getType() + " " + k.getBits() + " " + k.getId(uuid))
                .collect(Collectors.toList());
        }
    }

    @Override
    public Exec join(Exec exec) throws IOException {
        List<String> command = new ArrayList<>();
        command.add(System.getProperty("java.home") + "/bin/java");
        command.add("-classpath");
        command.add(System.getProperty("java.class.path"));
        command.add("-Dposix.uid=" + container.getUID());
        command.add(ExecHelper.class.getName());
        command.addAll(exec.command());
        return exec.command(command);
    }

    @Override
    protected void killProcs(long term_delay, TimeUnit unit)
        throws IOException
    {
        // REMIND:
        // The pgrep and pkill utilities return one of the following values upon exit:
        //   0  One or more processes were matched.
        //   1  No processes were matched.
        //   2  Invalid options were specified on the command line.
        //   3  An internal error occurred.

        long uid = container.getUID();

        try {
            // If force is not specified, try to terminate processes nicely
            // first and wait for them to die.
            if (term_delay > 0 && unit != null) {
                Exec.args("/usr/bin/pkill", "-u", uid).silentIO().run();

                long etime = System.nanoTime() + unit.toNanos(term_delay);
                while (System.nanoTime() <= etime) {
                    if (Exec.args("/usr/bin/pgrep", "-u", uid).silentIO().run() == 0) {
                        Thread.sleep(500);
                    } else {
                        break;
                    }
                }
            }

            String oldproclist = "";
            int stuckcount = 0;
            while (stuckcount <= 10) {
                if (Exec.args("/usr/bin/pkill", "-9", "-u", uid).silentIO().run() != 0) {
                    break;
                }

                Thread.sleep(500);

                String out = Exec.args("/usr/bin/pgrep", "-u", uid).silentIO().subst();
                if (oldproclist.equals(out)) {
                    stuckcount++;
                } else {
                    oldproclist = out;
                    stuckcount = 0;
                }
            }

            if (Exec.args("/usr/bin/pgrep", "-u", uid).silentIO().run() == 0) {
                // log.error("Failed to kill all processes for %d", uid);
            }
        } catch (InterruptedException ie) {
            throw new InterruptedIOException("*interrupted*");
        }
    }

    @Override
    public void setFileReadOnly(Path file) throws IOException {
        FileUtils.chown(file, "root", container.getUuid());
    }

    @Override
    public void setFileReadWrite(Path file) throws IOException {
        FileUtils.chown(file, container.getUuid(), container.getUuid());
    }

    @Override
    public boolean isAddressInUse(String ip, int port) {
        try {
            int rc = Exec.args("/usr/sbin/lsof",
                               "-sTCP:^CLOSE_WAIT,^FIN_WAIT1,^FIN_WAIT2",
                               "-i", "@" + ip + ":" + port)
                         .silentIO().run();
            return rc == 0;
        } catch (IOException ex) {
            throw new UncheckedIOException(ex);
        }
    }
}
