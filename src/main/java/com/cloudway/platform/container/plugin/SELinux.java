/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container.plugin;

import java.io.IOException;
import java.io.InterruptedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import com.cloudway.platform.common.util.FileUtils;
import jnr.constants.platform.Errno;
import jnr.ffi.LibraryLoader;
import jnr.ffi.Pointer;
import jnr.ffi.Struct;
import jnr.ffi.annotations.IgnoreError;
import jnr.ffi.annotations.Out;
import jnr.ffi.types.mode_t;

/**
 * The native libselinux interface.
 */
public final class SELinux
{
    private SELinux() {}

    public static class Context extends Struct {
        public final Pointer ptr = new Pointer();

        public Context(jnr.ffi.Runtime runtime) {
            super(runtime);
        }
    }

    public static interface LibSELinux {
        @IgnoreError int is_selinux_enabled();

        Context context_new(String context_str);
        String context_str(Context con);
        void context_free(Context con);

        String context_user_get(Context con);
        String context_role_get(Context con);
        String context_type_get(Context con);
        String context_range_get(Context con);

        int context_user_set(Context con, String user);
        int context_role_set(Context con, String role);
        int context_type_set(Context con, String type);
        int context_range_set(Context con, String range);

        int lgetfilecon(String path, @Out Pointer[] con);
        int lsetfilecon(String path, String con);

        int matchpathcon_init(String path);
        int matchpathcon_fini();
        int matchpathcon(String path, @mode_t int mode, @Out Pointer[] con);

        void freecon(Pointer con);
    }

    private static final LibSELinux lib =
        LibraryLoader.create(LibSELinux.class).load("selinux");

    public static boolean enabled() {
        try {
            return lib.is_selinux_enabled() != 0;
        } catch (UnsatisfiedLinkError error) {
            return false;
        }
    }

    private static Errno getLastError() {
        return Errno.valueOf(jnr.ffi.Runtime.getRuntime(lib).getLastError());
    }

    /**
     * Set the context of a single file or directory.
     *
     * @param path file to set selinux
     * @param level selinux level to set for path, defaults to Cloudway policy
     * @param type selinux type to set for path, defaults to selinux policy
     * @param role selinux role to set for path, defaults to selinux policy
     * @param user selinux user to set for path, defaults to selinux policy
     */
    public static void chcon(Path path, String level, String type, String role, String user)
        throws IOException
    {
        String filename = path.toString();
        String actual = lgetfilecon(filename);
        String expected = matchpathcon(filename, FileUtils.getFileMode(path));

        Context expected_con = context_new(expected);
        try {
            if (level != null)
                context_range_set(expected_con, level);
            if (type != null)
                context_type_set(expected_con, type);
            if (role != null)
                context_role_set(expected_con, role);
            if (user != null)
                context_user_set(expected_con, user);
            expected = context_str(expected_con);
        } finally {
            context_free(expected_con);
        }

        if (!expected.equals(actual)) {
            lsetfilecon(filename, expected);
        }
    }

    public static Context context_new(String context_str) {
        return lib.context_new(context_str);
    }

    public static String context_str(Context con) {
        return lib.context_str(con);
    }

    public static void context_free(Context con) {
        lib.context_free(con);
    }

    public static String context_user_get(Context con) {
        return lib.context_user_get(con);
    }

    public static String context_role_get(Context con) {
        return lib.context_role_get(con);
    }

    public static String context_type_get(Context con) {
        return lib.context_type_get(con);
    }

    public static String context_range_get(Context con) {
        return lib.context_range_get(con);
    }

    public static int context_user_set(Context con, String user) {
        return lib.context_user_set(con, user);
    }

    public static int context_role_set(Context con, String role) {
        return lib.context_role_set(con, role);
    }

    public static int context_type_set(Context con, String type) {
        return lib.context_type_set(con, type);
    }

    public static int context_range_set(Context con, String range) {
        return lib.context_range_set(con, range);
    }

    public static String lgetfilecon(String path) throws IOException {
        Pointer[] con = new Pointer[1];
        int len = lib.lgetfilecon(path, con);
        return ptrToStr(con[0], len);
    }

    public static void lsetfilecon(String path, String con) throws IOException {
        if (lib.lsetfilecon(path, con) < 0) {
            throw new IOException(getLastError().description());
        }
    }

    private static String ptrToStr(Pointer ptr, int len)
        throws IOException
    {
        if (len >= 0) {
            String res = ptr.getString(0, len, StandardCharsets.US_ASCII);
            lib.freecon(ptr);
            return res;
        } else {
            throw new IOException(getLastError().description());
        }
    }

    /*
     * Use a background thread for holding the connection to the
     * matchpathcon functions.
     *
     * Note this must be the only access to the matchpathcon functions.
     * Otherwise, the library will seg fault.
     */
    private static ExecutorService matchpathcon_executor = Executors.newSingleThreadExecutor(
        (Runnable r) -> {
            Thread t = new Thread(() -> {
                lib.matchpathcon_init(null);
                try {
                    r.run();
                } finally {
                    lib.matchpathcon_fini();
                }
            });
            t.setDaemon(true);
            return t;
        });

    public static String matchpathcon(String path, int mode)
        throws IOException
    {
        Future<String> task = matchpathcon_executor.submit(() -> matchpathcon_worker(path, mode));

        try {
            return task.get();
        } catch (InterruptedException ex) {
            throw new InterruptedIOException(ex.getMessage());
        } catch (ExecutionException ex) {
            Throwable cause = ex.getCause();
            if (cause instanceof IOException) {
                throw (IOException)cause;
            } else if (cause instanceof RuntimeException) {
                throw (RuntimeException)cause;
            } else if (cause instanceof Error) {
                throw (Error)cause;
            } else {
                throw new IOException(cause);
            }
        }
    }

    static String matchpathcon_worker(String path, int mode)
        throws IOException
    {
        Pointer[] ptr = new Pointer[1];
        if (lib.matchpathcon(path, mode, ptr) < 0) {
            throw new IOException(getLastError().description());
        } else {
            String res = ptr[0].getString(0);
            lib.freecon(ptr[0]);
            return res;
        }
    }
}
