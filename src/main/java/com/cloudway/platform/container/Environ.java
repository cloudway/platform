/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container;

import java.io.IOException;
import java.nio.file.FileSystem;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.PathMatcher;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import com.cloudway.platform.common.Config;
import com.cloudway.platform.common.util.FileUtils;

public class Environ
{
    /**
     * Load the combined environments for a guest.
     *
     * @param homeDir Home directory of the guest.
     */
    public static Map<String, String> loadAll(Path homeDir) {
        // Load system env vars
        Map<String, String> env = load(Config.CONF_DIR.resolve("env"));

        // Merge addon env vars
        try (Stream<Path> stream = Files.list(homeDir)) {
            stream.map(d -> d.resolve("env"))
                  .filter(Files::isDirectory)
                  .map(Environ::load)
                  .forEach(env::putAll);
        } catch (IOException ex) {
            // log and ignore
        }

        // Merge guest env vars
        env.putAll(load(homeDir.resolve(".env")));

        return env;
    }

    /**
     * Read environment variables into a environ map.
     *
     * @param dir of gear to be read.
     * @return the environment variables names to values map.
     */
    public static Map<String, String> load(Path dir) {
        // find, read and load environment variables into a hash
        Map<String, String> env = new HashMap<>();
        try (Stream<Path> stream = Files.walk(dir)) {
            stream.forEach(file -> readEnvFile(env, file));
        } catch (IOException ex) {
            // log and ignore
        }
        return env;
    }

    /**
     * Read environment variables into a environ map.
     *
     * @param dir of gear to be read.
     * @param glob env name pattern to be filtered.
     * @return the environment variables names to values map.
     */
    public static Map<String, String> load(Path dir, String glob) {
        // avoid creating a matcher if all entries are required.
        if (glob == null || glob.equals("*")) {
            return load(dir);
        }

        // create a matcher and return a filter that uses it.
        FileSystem fs = dir.getFileSystem();
        PathMatcher matcher = fs.getPathMatcher("glob:" + glob);

        // find, read and load environment variables into a hash
        Map<String, String> env = new HashMap<>();
        try (Stream<Path> stream = Files.walk(dir)) {
            stream.filter(file -> matcher.matches(file.getFileName()))
                  .forEach(file -> readEnvFile(env, file));
        } catch (IOException ex) {
            // log and ignore
        }
        return env;
    }

    private static final Pattern VALID_ENV_KEY = Pattern.compile("\\A[A-Z_0-9]+\\Z");

    private static void readEnvFile(Map<String,String> env, Path file) {
        if (Files.isRegularFile(file) && Files.isReadable(file)) {
            String key = file.getFileName().toString();
            if (VALID_ENV_KEY.matcher(key).matches()) {
                try {
                    env.put(key, FileUtils.read(file));
                } catch (IOException ex) {
                    // log and ignore
                }
            }
        }
    }
}
