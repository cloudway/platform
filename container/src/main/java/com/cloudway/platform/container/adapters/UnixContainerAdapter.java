/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container.adapters;

import java.io.IOException;
import java.io.InterruptedIOException;
import java.io.UncheckedIOException;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import com.cloudway.fp.data.Maybe;
import com.cloudway.platform.common.os.Config;
import com.cloudway.platform.common.os.Etc;
import com.cloudway.platform.common.os.Exec;
import com.cloudway.platform.container.Container;

import static com.cloudway.platform.common.util.MoreFiles.*;

/**
 * Generic UNIX container adapter.
 */
public class UnixContainerAdapter extends AbstractContainerAdapter
{
    private static final Lock create_lock = new ReentrantLock();

    public UnixContainerAdapter(Container container) {
        super(container);
    }

    @Override
    protected void createUser() throws IOException {
        Config config = Config.getDefault();
        int min_uid = config.getInt("GUEST_MIN_UID", 1000);
        int max_uid = config.getInt("GUEST_MAX_UID", 2000);

        // Synchronized to prevent race condition on obtaining a UNIX user uid.
        Path lock_file = Config.LOCK_DIR.resolve("create.lck");
        flock(lock_file, create_lock, (file) -> {
            int uid = next_uid(file, min_uid, max_uid);

            // Create operating system user
            createUser(container.getId(),
                       uid,
                       container.getHomeDir(),
                       config.get("GUEST_SKEL", Config.CONF_DIR.resolve("skel").toString()),
                       Config.SHELL.get(),
                       Config.GECOS.get(),
                       config.get("GUEST_SUPPLEMENTARY_GROUPS"));

            container.setUID(uid);
            container.setGID(uid);
        });
    }

    protected int next_uid(FileChannel file, int min_uid, int max_uid) throws IOException {
        int uid = min_uid;

        // Read last user id from seed file
        ByteBuffer bb = ByteBuffer.allocate(4);
        if (file.read(bb) == 4) {
            bb.flip();
            uid = bb.getInt();
            if (uid < min_uid || uid > max_uid) {
                uid = min_uid;
            }
        }

        // Determine next available user id
        for (int i = min_uid; ; i++) {
            if (i > max_uid)
                throw new IllegalStateException("Too many guest users");
            if (Etc.getpwuid(uid) == null && Etc.getgrgid(uid) == null)
                break;
            if (++uid > max_uid)
                uid = min_uid;
        }

        // Save next user id into seed file
        bb.clear();
        bb.putInt(uid + 1);
        bb.flip();
        file.position(0).write(bb);

        return uid;
    }

    protected void createUser(String name, int uid, Path home, String skel, String shell, String gecos, Maybe<String> groups)
        throws IOException
    {
        Exec.args("groupadd", "-g", uid, name).silentIO().checkError().run();

        Exec exec = Exec.args(
            "useradd",
            "-u", uid,
            "-g", uid,
            "-d", home,
            "-s", shell,
            "-c", gecos,
            "-m",
            "-k", skel,
            name);
        groups.ifPresent(g -> {
            exec.command().add("-G");
            exec.command().add(g);
        });
        exec.silentIO().checkError().run();
    }

    @Override
    protected void deleteUser() throws IOException {
        Exec.args("userdel", "-rf", container.getId()).silentIO().run();
        deleteFileTree(container.getHomeDir());
    }

    @Override
    protected void populateHome() throws IOException {
        super.populateHome();
        generateSSHKey();
    }

    protected void generateSSHKey() throws IOException {
        Path homedir     = container.getHomeDir();
        Path ssh_dir     = homedir.resolve(".cloudway_ssh");
        Path known_hosts = ssh_dir.resolve("known_hosts");
        Path ssh_config  = ssh_dir.resolve("config");
        Path ssh_key     = ssh_dir.resolve("id_rsa");
        Path ssh_pub_key = ssh_dir.resolve("id_rsa.pub");

        mkdir(ssh_dir, 0750);
        touch(known_hosts, 0660);
        touch(ssh_config, 0660);
        setFileTreeReadWrite(ssh_dir);

        container.join(Exec.args("ssh-keygen", "-N", "", "-f", ssh_key.toString()))
                 .silentIO().checkError().run();

        chmod(ssh_key, 0600);
        chmod(ssh_pub_key, 0600);
        setFileReadWrite(ssh_key);
        setFileReadWrite(ssh_pub_key);

        setenv("CLOUDWAY_APP_SSH_KEY", ssh_key.toString());
        setenv("CLOUDWAY_APP_SSH_PUBLIC_KEY", ssh_pub_key.toString());
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
