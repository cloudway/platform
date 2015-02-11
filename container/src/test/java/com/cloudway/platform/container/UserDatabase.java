/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import org.jmock.Expectations;
import static com.cloudway.platform.container.Actions.*;

import jnr.posix.Passwd;
import jnr.posix.Group;
import com.cloudway.platform.common.os.Etc.PASSWD;
import com.cloudway.platform.common.os.Etc.GROUP;
import com.cloudway.platform.common.os.Posix;
import static com.cloudway.platform.common.os.Config.SHELL;

// @formatter:off
public class UserDatabase extends Expectations
{
    private final List<PASSWD> userdb = new ArrayList<>();
    private String currentUser = "root";

    @SuppressWarnings("unchecked")
    public UserDatabase(Posix posix) {
        allowing(posix).getpwnam(with.is(anything()));
            will(apply("get user by name", this::getpwname));

        allowing(posix).getpwuid(with.intIs(anything()));
            will(apply("get user by id", this::getpwuid));

        allowing(posix).getgrgid(with.intIs(anything()));
            will(apply("get group by id", this::getgrgid));

        allowing(posix).getpwent(with.is(anything()));
            will(perform("list passwd database", userdb::forEach));

        allowing(posix).getuid();
            will(apply("get current uid", () -> {
                PASSWD pw = getpwname(currentUser);
                return pw != null ? pw.pw_uid : -1;
            }));
    }

    public UserDatabase add(PASSWD pw) {
        Objects.requireNonNull(pw);
        userdb.add(pw);
        return this;
    }

    public UserDatabase remove(String name) {
        Objects.requireNonNull(name);
        userdb.removeIf(pw -> name.equals(pw.pw_name));
        return this;
    }

    public PASSWD getpwname(String name) {
        Objects.requireNonNull(name);
        return userdb.stream().filter(pw -> name.equals(pw.pw_name)).findFirst().orElse(null);
    }

    public PASSWD getpwuid(int uid) {
        return userdb.stream().filter(pw -> uid == pw.pw_uid).findFirst().orElse(null);
    }

    public GROUP getgrgid(int gid) {
        PASSWD pw = getpwuid(gid);
        return pw == null ? null : mockGROUP(pw.pw_name, gid, pw.pw_name);
    }

    public int getNextUid() {
        return userdb.stream().mapToInt(pw -> pw.pw_uid).max().orElse(1000) + 1;
    }

    public void setCurrentUser(String user) {
        currentUser = Objects.requireNonNull(user);
    }

    @SuppressWarnings("override")
    public static PASSWD mockPASSWD(String id, int uid, String gecos, String home) {
        return PASSWD.from(new Passwd() {
            public String getLoginName()     { return id; }
            public String getPassword()      { return "*"; }
            public long getUID()             { return uid; }
            public long getGID()             { return uid; }
            public int getPasswdChangeTime() { return 0; }
            public String getAccessClass()   { return null; }
            public String getGECOS()         { return gecos; }
            public String getHome()          { return home; }
            public String getShell()         { return SHELL.get(); }
            public int getExpire()           { return 0; }
        });
    }

    @SuppressWarnings("override")
    public static GROUP mockGROUP(String name, int gid, String... members) {
        return GROUP.from(new Group() {
            public String getName()      { return name; }
            public String getPassword()  { return "*"; }
            public long getGID()         { return gid; }
            public String[] getMembers() { return members; }
        });
    }
}
