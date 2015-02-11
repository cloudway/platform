/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.os;

import java.util.function.Consumer;
import com.cloudway.platform.common.os.Etc.GROUP;
import com.cloudway.platform.common.os.Etc.PASSWD;
import jnr.constants.platform.Signal;
import jnr.posix.FileStat;

/**
 * The bridge interface to POSIX API.
 */
public interface Posix {
    /**
     * Retrieve user information from user database.
     *
     * @param action the action to perform on every password entry
     */
    void getpwent(Consumer<PASSWD> action);

    /**
     * Search user database for a user ID.
     *
     * @param which the user ID to search
     * @return a password entry if found, otherwise {@code null} if not found
     */
    PASSWD getpwuid(int which);

    /**
     * Search user database for a user name.
     *
     * @param which the user name to search
     * @return a password entry if found, otherwise {@code null} if not found
     */
    PASSWD getpwnam(String which);

    /**
     * Search group database for a group ID.
     *
     * @param which the group ID to search
     * @return a group entry if found, otherwise {@code null} if not found
     */
    GROUP getgrgid(int which);

    /**
     * Get the real user ID.
     *
     * @return the real user ID
     */
    int getuid();

    /**
     * Get the real group ID.
     *
     * @return the real group ID
     */
    int getgid();

    /**
     * Send a signal to a process or a group of process.
     *
     * @param pid the process ID
     * @param signal the signal number
     * @return 0 if success, otherwise an error occurred
     */
    int kill(int pid, Signal signal);

    /**
     * Get file status.
     *
     * @param path the file name to obtain information
     * @return the file status information
     */
    FileStat lstat(String path);
}
