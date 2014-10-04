/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.UUID;
import java.util.function.Consumer;

import com.cloudway.platform.common.AuthorizedKey;
import com.cloudway.platform.common.util.FileUtils;
import com.cloudway.platform.common.util.IO;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

public class ContainerTest
{
    private static String uuid;
    private static String capacity;
    private static String pubkey;

    private static ApplicationContainer container;

    @BeforeClass
    public static void create() throws IOException {
        uuid = mkuuid();
        capacity = System.getProperty("container.size", "small");
        pubkey = loadPublicKey();

        container = ApplicationContainer.create(uuid, "test", "demo", capacity);
        container.addAuthorizedKey("default", pubkey);
    }

    @AfterClass
    public static void destroy() throws IOException {
        boolean keep = Boolean.getBoolean("container.keep");

        // destroy all application containers
        ApplicationContainer.all().stream()
            .filter(c -> !(keep && uuid.equals(c.getUuid())))
            .forEach(log(ApplicationContainer::destroy));
    }

    @Test
    public void info() throws IOException {
        assertEquals(uuid, container.getUuid());
        assertEquals("test", container.getName());
        assertEquals("demo", container.getNamespace());
        assertEquals("test-demo.cloudway.com", container.getDomainName());
        assertEquals(capacity, container.getCapacity());
        assertEquals("/opt/cloudway/bin/cwsh", container.getShell());
        assertEquals("/var/lib/cloudway/" + uuid, container.getHomeDir().toString());
    }

    @Test
    public void authorizedKeys() throws IOException {
        List<String> akeys = container.getAuthorizedKeys();
        AuthorizedKey pkey = AuthorizedKey.parsePublicKey(pubkey);

        assertEquals(1, akeys.size());
        AuthorizedKey akey = AuthorizedKey.parsePublicKey(akeys.get(0));
        assertEquals(pkey.getBits(), akey.getBits());
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

    private static String loadPublicKey() throws IOException {
        Path keyfile = Paths.get(System.getProperty("user.home"), ".ssh", "id_rsa.pub");
        return FileUtils.read(keyfile);
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
