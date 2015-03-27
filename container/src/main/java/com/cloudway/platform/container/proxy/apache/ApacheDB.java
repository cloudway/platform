/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container.proxy.apache;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;
import java.util.stream.Stream;

import com.cloudway.platform.common.fp.control.StateIO;
import com.cloudway.platform.common.fp.data.HashPMap;
import com.cloudway.platform.common.fp.data.PMap;
import com.cloudway.platform.common.fp.data.Unit;
import com.cloudway.platform.common.fp.io.IO;
import com.cloudway.platform.common.os.Config;
import com.cloudway.platform.common.os.Exec;

import static java.nio.file.Files.*;
import static com.cloudway.platform.common.util.MoreFiles.*;
import static java.nio.file.StandardCopyOption.*;
import static com.cloudway.platform.common.fp.control.StateIO.*;
import static com.cloudway.platform.common.fp.control.Syntax.*;

final class ApacheDB {
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

    private final AtomicReference<MapFile> state = new AtomicReference<>(new MapFile());

    public ApacheDB(String mapname, boolean nodbm) {
        this.mapname = mapname;
        this.nodbm = nodbm || !DBM_SUPPORTED;
    }

    public ApacheDB(String mapname) {
        this(mapname, false);
    }

    public <R> R read(Function<PMap<String, String>, R> action)
        throws IOException
    {
        MapFile file = atomically(load());
        return action.apply(file.map);
    }

    public void write(Function<PMap<String, String>, PMap<String, String>> action)
        throws IOException
    {
        atomically(
            do_(load(), oldmap ->
            let(action.apply(oldmap), newmap ->
            unless(oldmap.equals(newmap), store(newmap))))
        );
    }

    // FIXME: STM (Software Transactional Memory) required
    private MapFile atomically(StateIO<?, MapFile> action) throws IOException {
        MapFile oldfile, newfile;
        do {
            oldfile = state.get();
            newfile = action.exec(oldfile).runIO();
        } while (!state.compareAndSet(oldfile, newfile));
        return newfile;
    }

    static class MapFile {
        final PMap<String,String> map;
        final long mtime;

        public MapFile() {
            this(HashPMap.empty(), 0);
        }

        MapFile(PMap<String,String> map, long mtime) {
            this.map = map;
            this.mtime = mtime;
        }
    }

    private StateIO<PMap<String,String>, MapFile> load() {
        return do_(get(), oldfile ->
               do_(readMap(oldfile), newfile ->
               do_(put(newfile),
               do_(pure(newfile.map)))));
    }

    private StateIO<MapFile, MapFile> readMap(MapFile oldfile) {
        return lift(() -> {
            Path file = basedir().resolve(mapname + SUFFIX);
            if (Files.exists(file)) {
                long last_mtime = mtime(file);
                if (last_mtime != oldfile.mtime) {
                    try (Stream<String> lines = Files.lines(file)) {
                        return new MapFile(parseLines(lines), last_mtime);
                    }
                } else {
                    return oldfile;
                }
            } else {
                return new MapFile();
            }
        });
    }

    private static PMap<String, String> parseLines(Stream<String> lines) {
        return lines.reduce(HashPMap.<String,String>empty(), ApacheDB::parse, PMap::putAll);
    }

    private static PMap<String, String> parse(PMap<String,String> map, String line) {
        int i = line.indexOf(' ');
        if (i != -1) {
            String key = line.substring(0, i).trim();
            String value = line.substring(i + 1).trim();
            return map.put(key, value);
        } else {
            return map;
        }
    }

    private StateIO<Unit, MapFile> store(PMap<String, String> map) {
        return do_(lift(() -> mkdir(basedir())), base ->
               let(base.resolve(mapname + SUFFIX), txt ->
               let(base.resolve(mapname + ".db"), dbm ->
               do_(writeMap(txt, map), modtime ->
               do_(unless(nodbm, writeDBM(base, dbm, txt)),
               do_(put(new MapFile(map, modtime))))))));
    }

    private static StateIO<Long, MapFile> writeMap(Path file, PMap<String, String> map) {
        return lift(() -> {
            try (BufferedWriter f = newBufferedWriter(file)) {
                IO.forEach(map, (k, v) -> f.write(k + " " + v + "\n"));
            }
            return mtime(file);
        });
    }

    private static StateIO<Unit, MapFile> writeDBM(Path base, Path dbm, Path txt) {
        return lift_(() -> {
            Path wd = createTempDirectory(base, dbm.getFileName() + "~");
            try {
                Path tmpdb = wd.resolve("tmp.db");
                Exec.args(httxt2dbm, "-f", "DB", "-i", txt, "-o", tmpdb)
                    .silentIO().checkError().run();
                move(tmpdb, dbm, REPLACE_EXISTING, ATOMIC_MOVE);
            } finally {
                deleteFileTree(wd);
            }
        });
    }

    private static Path basedir() {
        return Config.VAR_DIR.resolve(".httpd");
    }

    private static long mtime(Path file) throws IOException {
        return getLastModifiedTime(file).to(TimeUnit.MILLISECONDS);
    }
}
