/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container.proxy.apache;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;
import java.util.stream.Stream;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import com.cloudway.platform.common.os.Config;
import com.cloudway.platform.common.os.Exec;
import com.cloudway.platform.common.fp.io.IO;

import static java.nio.file.Files.*;
import static java.nio.file.StandardCopyOption.*;
import static com.cloudway.platform.common.util.MoreFiles.*;
import static com.cloudway.platform.common.util.MoreCollectors.*;

class ApacheDB
{
    private static final String SUFFIX = ".txt";

    private static final String httxt2dbm =
        Stream.of("/usr/bin", "/usr/sbin", "/bin", "/sbin")
              .map(d -> Paths.get(d, "httxt2dbm"))
              .filter(Files::exists)
              .map(Path::toString)
              .findFirst()
              .orElse("httxt2dbm");

    private static final boolean DBM_SUPPORTED = checkDbmSupport();

    private static boolean checkDbmSupport() {
        try {
            Path tmp = createTempFile("db", ".tmp");
            Exec.args(httxt2dbm, "-f", "DB", "-i", "/dev/null", "-o", tmp.toString())
                .silentIO().checkError().run();
            deleteIfExists(tmp);
            return true;
        } catch (IOException ex) {
            return false;
        }
    }

    private final String mapname;
    private final boolean nodbm;

    private Map<String, String> map = new LinkedHashMap<>();
    private long mtime = -1;

    public ApacheDB(String mapname, boolean nodbm) {
        this.mapname = mapname;
        this.nodbm = nodbm || !DBM_SUPPORTED;
    }

    public ApacheDB(String mapname) {
        this(mapname, false);
    }

    public synchronized void reading(Consumer<Map<String,String>> action)
        throws IOException
    {
        action.accept(load());
    }

    public synchronized boolean writting(Consumer<Map<String,String>> action)
        throws IOException
    {
        Map<String, String> newmap = new LinkedHashMap<>(load());
        action.accept(newmap);

        if (map.equals(newmap)) {
            return false;
        } else {
            map = newmap;
            store(newmap);
            return true;
        }
    }

    private Map<String, String> load() throws IOException {
        Path file = basedir().resolve(mapname + SUFFIX);
        if (Files.exists(file)) {
            long last_mtime = getLastModifiedTime(file).to(TimeUnit.NANOSECONDS);
            if (last_mtime != this.mtime) {
                try (Stream<String> lines = Files.lines(file)) {
                    this.map = lines.collect(toSplittingMap(' '));
                    this.mtime = last_mtime;
                }
            }
        } else {
            map.clear();
        }
        return map;
    }

    private void store(Map<String, String> map) throws IOException {
        Path base = basedir();
        if (!Files.exists(base)) {
            mkdir(base);
        }

        Path txt = base.resolve(mapname + SUFFIX);
        try (BufferedWriter f = newBufferedWriter(txt)) {
            IO.forEach(map, (k,v) -> f.write(k + " " + v + "\n"));
        }
        this.mtime = getLastModifiedTime(txt).to(TimeUnit.NANOSECONDS);

        if (!nodbm) {
            Path dbm = base.resolve(mapname + ".db");
            Path wd = createTempDirectory(base, mapname + ".db~");
            try {
                Path tmpdb = wd.resolve("tmp.db");
                Exec.args(httxt2dbm, "-f", "DB", "-i", txt, "-o", tmpdb)
                    .silentIO().checkError().run();
                move(tmpdb, dbm, REPLACE_EXISTING, ATOMIC_MOVE);
            } finally {
                deleteFileTree(wd);
            }
        }
    }

    private static Path basedir() {
        return Config.VAR_DIR.resolve(".httpd");
    }
}
