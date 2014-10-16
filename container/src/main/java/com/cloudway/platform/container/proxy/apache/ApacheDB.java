/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container.proxy.apache;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.Consumer;
import java.util.stream.Stream;

import static java.nio.file.StandardCopyOption.*;
import static java.nio.file.StandardOpenOption.*;

import com.cloudway.platform.common.Config;
import com.cloudway.platform.common.util.Exec;
import com.cloudway.platform.common.util.FileUtils;
import com.cloudway.platform.common.util.IO;

public class ApacheDB
{
    private static final Path   BASEDIR = Config.VAR_DIR.resolve(".httpd");
    private static final String SUFFIX = ".txt";

    private static final String httxt2dbm =
        Stream.of("/usr/bin", "/usr/sbin", "/bin", "/sbin")
              .map(d -> Paths.get(d, "httxt2dbm"))
              .filter(Files::exists)
              .map(Path::toString)
              .findFirst()
              .orElse("httxt2dbm");

    private String mapname;

    public ApacheDB(String mapname) {
        this.mapname = mapname;
    }

    public synchronized void reading(Consumer<Map<String,String>> action)
        throws IOException
    {
        Map<String, String> map = new LinkedHashMap<>();
        load(map);
        action.accept(map);
    }

    public synchronized void writting(Consumer<Map<String,String>> action)
        throws IOException
    {
        Map<String, String> map = new LinkedHashMap<>();
        load(map);
        action.accept(map);
        store(map);
    }

    private void load(Map<String,String> map) throws IOException {
        Path file = BASEDIR.resolve(mapname + SUFFIX);
        try (Stream<String> lines = Files.lines(file)) {
            lines.forEach(line -> {
                if (line.indexOf(' ') != -1) {
                    String[] pair = line.split(" ");
                    map.put(pair[0], pair[1]);
                }
            });
        } catch (NoSuchFileException ex) {
            // ok
        }
    }

    private void store(Map<String, String> map) throws IOException {
        if (!Files.exists(BASEDIR)) {
            FileUtils.mkdir(BASEDIR);
        }

        Path txt = BASEDIR.resolve(mapname + SUFFIX);
        Path dbm = BASEDIR.resolve(mapname + ".db");

        try (BufferedWriter f = Files.newBufferedWriter(txt, WRITE, CREATE, TRUNCATE_EXISTING)) {
            IO.forEach(map, (k,v) -> f.write(k + " " + v + "\n"));
        }

        Path wd = Files.createTempDirectory(BASEDIR, mapname + ".db~");
        try {
            Path tmpdb = wd.resolve("new.db");
            Exec.args(httxt2dbm, "-f", "DB", "-i", txt, "-o", tmpdb)
                .silentIO().checkError().run();
            Files.move(tmpdb, dbm, REPLACE_EXISTING, ATOMIC_MOVE);
        } finally {
            FileUtils.deleteTree(wd);
        }
    }
}
