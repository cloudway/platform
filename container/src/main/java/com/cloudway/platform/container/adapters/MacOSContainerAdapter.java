/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container.adapters;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import com.cloudway.platform.common.fp.data.Maybe;
import com.cloudway.platform.common.os.Exec;
import com.cloudway.platform.container.ApplicationContainer;
import static com.cloudway.platform.common.util.MoreFiles.*;

public class MacOSContainerAdapter extends UnixContainerAdapter
{
    private static final String dscl = "/usr/bin/dscl";

    public MacOSContainerAdapter(ApplicationContainer container) {
        super(container);
    }

    @Override
    protected void createUser(String name, int uid, Path home, String skel, String shell, String gecos, Maybe<String> groups)
        throws IOException
    {
        String grouppath = "/Groups/" + name;
        String userpath = "/Users/" + name;

        String[][] commands = {
            {dscl, ".", "-create", grouppath},
            {dscl, ".", "-create", grouppath, "PrimaryGroupID", String.valueOf(uid)},
            {dscl, ".", "-create", userpath},
            {dscl, ".", "-create", userpath, "UserShell", shell},
            {dscl, ".", "-create", userpath, "RealName", gecos},
            {dscl, ".", "-create", userpath, "UniqueID", String.valueOf(uid)},
            {dscl, ".", "-create", userpath, "PrimaryGroupID", String.valueOf(uid)},
            {dscl, ".", "-create", userpath, "NFSHomeDirectory", home.toString()}
        };

        // create group and user
        for (String[] command : commands) {
            Exec.args(command).silentIO().checkError().run();
        }

        // TODO: add supplementary groups

        // create home directory
        if (!Files.exists(home)) {
            mkdir(home);
        }

        // copy skeleton directory
        if (Files.isDirectory(Paths.get(skel))) {
            emptyDirectory(home);
            copyFileTree(Paths.get(skel), home);
        }

        // temporally set owner of home directory to root, will change
        // back when home directory populated
        chown(home, "root", "wheel");
    }

    @Override
    protected void deleteUser() throws IOException {
        String user = container.getId();
        Exec.args(dscl, ".", "-delete", "/Users/" + user).silentIO().checkError().run();
        Exec.args(dscl, ".", "-delete", "/Groups/" + user).silentIO().checkError().run();
        deleteFileTree(container.getHomeDir());
    }
}
