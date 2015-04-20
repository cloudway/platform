/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container;

import java.io.IOException;
import java.nio.file.Path;
import java.util.concurrent.TimeUnit;

import com.cloudway.platform.common.fp.data.Maybe;
import com.cloudway.platform.common.os.Exec;
import com.cloudway.platform.container.adapters.UnixContainerAdapter;

import static com.cloudway.platform.common.util.MoreFiles.*;
import static com.cloudway.platform.common.os.Config.GECOS;
import static com.cloudway.platform.common.os.Config.SHELL;
import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

public class FakeContainerAdapter extends UnixContainerAdapter
{
    private final ContainerAdapter delegate;
    private final UserDatabase     userdb;
    private final FilePermissions  files;

    public FakeContainerAdapter(ApplicationContainer container,
                                ContainerAdapter delegate,
                                UserDatabase userdb,
                                FilePermissions files) {
        super(container);
        this.delegate = delegate;
        this.userdb   = userdb;
        this.files    = files;
    }

    @Override
    protected void createUser(String id, int uid, Path home, String skel, String shell, String gecos, Maybe<String> groups)
        throws IOException
    {
        assertThat(shell, is(SHELL.get()));
        assertThat(gecos, is(GECOS.get()));

        userdb.add(UserDatabase.mockPASSWD(id, uid, gecos, home.toString()));
        mkdir(home, 0750);
    }

    @Override
    protected void deleteUser() throws IOException {
        userdb.remove(container.getId());
        deleteFileTree(container.getHomeDir());
    }

    @Override
    protected void createHomeDir() throws IOException {
        super.createHomeDir();

        // this hack is necessary for non-root user
        chmod(container.getHomeDir().resolve(".tmp"), 0700);
    }

    @Override
    protected void generateSSHKey() throws IOException {
        // do nothing
    }

    @Override
    protected void killProcs(long term_delay, TimeUnit unit) throws IOException {
        // do nothing
    }

    @Override
    public Exec join(Exec exec) throws IOException {
        return exec;
    }

    @Override
    public void setFileReadOnly(Path file) throws IOException {
        files.setReadOnly(file);
    }

    @Override
    public void setFileReadWrite(Path file) throws IOException {
        files.setReadWrite(file);
    }

    @Override
    public String getIpAddress(int host_id) {
        delegate.getIpAddress(host_id);
        return super.getIpAddress(host_id);
    }

    @Override
    public boolean isAddressInUse(String ip, int port) {
        return false;
    }

    @Override
    public void start() throws IOException {
        delegate.start();
    }

    @Override
    public void stop(long term_delay, TimeUnit unit) throws IOException {
        delegate.stop(term_delay, unit);
    }

    @Override
    public void destroy() {
        super.destroy();
        delegate.destroy();
    }
}
