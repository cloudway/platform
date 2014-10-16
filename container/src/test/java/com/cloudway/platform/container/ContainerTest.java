/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.UUID;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import com.cloudway.platform.common.AuthorizedKey;
import com.cloudway.platform.common.Config;
import com.cloudway.platform.common.util.Exec;
import com.cloudway.platform.common.util.FileUtils;
import com.cloudway.platform.common.util.IO;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

public class ContainerTest
{
    private static String uuid;
    private static String name;
    private static String capacity;
    private static String pubkey;

    private static ApplicationContainer container;

    @BeforeClass
    public static void create() throws IOException {
        uuid = mkuuid();
        name = mkappname();
        capacity = System.getProperty("container.size", "small");
        pubkey = loadPublicKey();

        container = ApplicationContainer.create(uuid, name, "demo", capacity);
        container.addAuthorizedKey("default", pubkey);
        installAndStart();
        container.tidy();
    }

    @AfterClass
    public static void destroy() throws IOException {
        boolean keep = Boolean.getBoolean("container.keep");

        // destroy all application containers
        ApplicationContainer.all()
            .filter(c -> !(keep && uuid.equals(c.getUuid())))
            .forEach(log(ApplicationContainer::destroy));
    }

    @Test
    public void info() throws IOException {
        assertEquals(uuid, container.getUuid());
        assertEquals(name, container.getName());
        assertEquals("demo", container.getNamespace());
        assertEquals(name + "-demo.cloudway.com", container.getDomainName());
        assertEquals(capacity, container.getCapacity());
        assertEquals("/opt/cloudway/bin/cwsh", container.getShell());
        assertEquals(Config.VAR_DIR.resolve(uuid), container.getHomeDir());
    }

    @Test
    public void authorizedKeys() throws IOException {
        List<String> akeys = container.getAuthorizedKeys();
        AuthorizedKey pkey = AuthorizedKey.parsePublicKey(pubkey);
        assertEquals(1, akeys.size());
        AuthorizedKey akey = AuthorizedKey.parsePublicKey(akeys.get(0));
        assertEquals(pkey.getBits(), akey.getBits());
    }

    @Test
    public void runInContext() throws IOException {
        String who = container.join(Exec.args("whoami")).checkError().subst();
        assertEquals(uuid, who);

        if (Boolean.getBoolean("container.runshell")) {
            container.join(Exec.args("/bin/bash", "-i"))
                     .environment(Environ.loadAll(container))
                     .redirectInput(new File("/dev/tty"))
                     .redirectOutput(new File("/dev/tty"))
                     .redirectError(new File("/dev/tty"))
                     .run();
        }
    }

    private static String loadPublicKey() throws IOException {
        Path sshdir = Paths.get(System.getProperty("user.home"), ".cloudway_ssh");
        Path keyfile = sshdir.resolve("id_rsa");

        if (!Files.exists(keyfile)) {
            FileUtils.mkdir(sshdir);
            Exec.args("ssh-keygen", "-N", "", "-f", keyfile).checkError().run();
        }
        return FileUtils.read(sshdir.resolve("id_rsa.pub"));
    }

    private static void installAndStart() throws IOException {
        String source = System.getProperty("addon.dir");
        String repo = System.getProperty("repo.url");

        if (source != null) {
            Path path = Paths.get(source).toAbsolutePath();
            assertEquals(ApplicationState.NEW, container.getState());
            container.install(path, repo);
            assertEquals(ApplicationState.NEW, container.getState());
            container.start();
            assertEquals(ApplicationState.STARTED, container.getState());
        }
    }

    private static String mkuuid() {
        UUID uuid = UUID.randomUUID();
        return digits(uuid.getLeastSignificantBits()) + digits(uuid.getLeastSignificantBits());
    }

    private static String digits(long val) {
        String str = Long.toHexString(val);
        while (str.length() < 16)
            str = "0" + str;
        return str;
    }

    private static String mkappname() {
        List<ApplicationContainer> all = ApplicationContainer.all().collect(Collectors.toList());
        return IntStream.range(0, 1000)
            .mapToObj(i -> "test" + (i == 0 ? "" : String.valueOf(i)))
            .filter(n -> !all.stream().anyMatch(a -> n.equals(a.getName())))
            .findAny()
            .orElseThrow(() -> new RuntimeException("Too many application containers"));
    }

    private static <T> Consumer<T> log(IO.Consumer<T> action) {
        return t -> {
            try {
                action.accept(t);
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        };
    }
}
