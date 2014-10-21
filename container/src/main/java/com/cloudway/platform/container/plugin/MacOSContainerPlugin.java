/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container.plugin;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import com.cloudway.platform.common.util.Exec;
import com.cloudway.platform.common.util.FileUtils;
import com.cloudway.platform.container.ApplicationContainer;

public class MacOSContainerPlugin extends UnixContainerPlugin
{
    private static final String dscl = "/usr/bin/dscl";

    public MacOSContainerPlugin(ApplicationContainer container) {
        super(container);
    }

    @Override
    protected void createUser(String name, int uid, Path home, String skel, String shell, String gecos, String groups)
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
            FileUtils.mkdir(home);
        }

        // copy skeleton directory
        if (skel != null && Files.isDirectory(Paths.get(skel))) {
            FileUtils.emptyDirectory(home);
            FileUtils.copyTree(Paths.get(skel), home);
        }

        // temporally set owner of home directory to root, will change
        // back when home directory populated
        FileUtils.chown(home, "root", "wheel");
    }

    @Override
    protected void deleteUser() throws IOException {
        String user = container.getId();
        Exec.args(dscl, ".", "-delete", "/Users/" + user).silentIO().checkError().run();
        Exec.args(dscl, ".", "-delete", "/Groups/" + user).silentIO().checkError().run();
        FileUtils.deleteTree(container.getHomeDir());
    }
}