/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.UUID;

import com.cloudway.platform.common.util.FileUtils;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

public class ContainerTest
{
    private static String uuid;
    private static ApplicationContainer container;

    @BeforeClass
    public static void create() throws IOException {
        uuid = mkuuid();
        container = ApplicationContainer.create(uuid, "test", "demo");
        container.addAuthorizedKey("default", loadPublicKey());
    }

    private static String loadPublicKey() throws IOException {
        Path keyfile = Paths.get(System.getProperty("user.home"), ".ssh", "id_rsa.pub");
        return FileUtils.read(keyfile);
    }

    @AfterClass
    public static void destroy() throws IOException {
        boolean keep = Boolean.getBoolean("container.keep");

        // destroy all application containers
        ApplicationContainer.all().forEach(c -> {
            if (!(keep && uuid.equals(c.getUuid()))) {
                try {
                    c.destroy();
                } catch (Exception ex) {
                    ex.printStackTrace();
                }
            }
        });
    }

    @Test
    public void info() {
        assertEquals(uuid, container.getUuid());
        assertEquals("test", container.getName());
        assertEquals("demo", container.getNamespace());
        assertEquals("test-demo.cloudway.com", container.getDomainName());
        assertEquals("/opt/cloudway/bin/cwsh", container.getShell());
        assertEquals("/var/lib/cloudway/" + uuid, container.getHomeDir().toString());
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
}
