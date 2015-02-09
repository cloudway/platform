/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.FileSystem;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.PathMatcher;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.cloudway.platform.common.os.Config;
import com.cloudway.platform.common.io.MoreFiles;
import static com.cloudway.platform.common.util.function.Predicates.*;
import static com.cloudway.platform.common.util.function.StringPredicates.*;

public final class Environ
{
    private Environ() {}

    private static final Pattern VALID_ENV_KEY = Pattern.compile("\\A[A-Z_0-9]+\\Z");

    /**
     * Load the combined environments for a guest.
     */
    public static Map<String, String> loadAll(ApplicationContainer container) {
        Map<String, String> env;

        // Load user env vars
        env = loadEnvFile(MoreFiles.join(container.getRepoDir(), ".cloudway", "env"));

        // Merge system env vars
        env.putAll(load(Config.CONF_DIR.resolve("env")));

        // Merge application env vars
        env.putAll(load(container.getEnvDir()));

        // Merge addon env vars
        try (Stream<Path> stream = Files.list(container.getHomeDir())) {
            stream.filter(Addon::isAddonDirectory)
                  .map(d -> load(d.resolve("env")))
                  .forEach(env::putAll);
        } catch (IOException ex) {
            // log and ignore
        }

        // Collect PATH elements
        env.put("PATH", collectPathElements(env, "PATH"));
        env.put("LD_LIBRARY_PATH", collectPathElements(env, "LD_LIBRARY_PATH"));

        // Add system environments
        env.putIfAbsent("CLOUDWAY_HOME", Config.HOME_DIR.toString());
        env.putIfAbsent("JAVA_HOME", System.getProperty("java.home"));
        env.putIfAbsent("CLOUDWAY_CLASSPATH", collectJavaClasspath());

        return env;
    }

    private static String collectPathElements(Map<String, String> env, String var_name) {
        if ("PATH".equals(var_name)) {
            // Prevent conflict with the PATH variable
            env = new HashMap<>(env);
            env.keySet().removeIf(endsWith("_LD_LIBRARY_PATH_ELEMENT"));
        }

        Stream<String> elements = env.keySet().stream()
           .filter(startsWith("CLOUDWAY_").and(endsWith("_" + var_name + "_ELEMENT")))
           .map(env::get);

        String system_path = env.get(var_name);
        if (system_path == null) {
            system_path = System.getenv(var_name);
            if ("PATH".equals(var_name)) {
                if (system_path == null)
                    system_path = "/bin:/usr/bin:/usr/sbin";
                system_path += ":" + Config.HOME_DIR.resolve("bin");
            }
        }
        if (system_path != null) {
            elements = Stream.concat(elements, Stream.of(system_path.split(File.pathSeparator)));
        }

        return elements.distinct().collect(Collectors.joining(File.pathSeparator));
    }

    private static String collectJavaClasspath() {
        Path libdir = Config.HOME_DIR.resolve("lib");
        try (Stream<Path> files = MoreFiles.find(libdir, 1, "*.jar")) {
            return files.map(Path::toString).collect(Collectors.joining(File.pathSeparator));
        } catch (IOException ex) {
            return "";
        }
    }

    /**
     * Read environment variables into a environ map.
     *
     * @param dir the directory to be read.
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
     * @param dir the directory to be read.
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
            stream.filter(having(Path::getFileName, on(matcher::matches)))
                  .forEach(file -> readEnvFile(env, file));
        } catch (IOException ex) {
            // log and ignore
        }
        return env;
    }

    /**
     * List environment variable names in the specified directory.
     * @param dir the directory to be list.
     * @return the environment variables names
     */
    public static Set<String> list(Path dir) {
        try (Stream<Path> stream = Files.walk(dir)) {
            return stream.filter(both(Files::isRegularFile, Files::isReadable))
                         .map(f -> f.getFileName().toString())
                         .filter(matches(VALID_ENV_KEY))
                         .collect(Collectors.toSet());
        } catch (IOException ex) {
            // log and ignore
            return Collections.emptySet();
        }
    }

    private static void readEnvFile(Map<String,String> env, Path file) {
        if (Files.isRegularFile(file) && Files.isReadable(file)) {
            String key = file.getFileName().toString();
            if (VALID_ENV_KEY.matcher(key).matches()) {
                try {
                    String val = MoreFiles.chomp(file);
                    if (val.indexOf('\0') == -1) { // ignore illegal env vars
                        env.put(key, val);
                    }
                } catch (IOException ex) {
                    // log and ignore
                }
            }
        }
    }

    private static Map<String,String> loadEnvFile(Path file) {
        Map<String,String> env = new HashMap<>();
        if (Files.exists(file)) {
            try (InputStream ins = Files.newInputStream(file)) {
                Properties props = new Properties();
                props.load(ins);
                props.stringPropertyNames().stream()
                    .filter(matches(VALID_ENV_KEY))
                    .forEach(key -> env.put(key, props.getProperty(key)));
            } catch (IOException ex) {
                // log and ignore
            }
        }
        return env;
    }
}
