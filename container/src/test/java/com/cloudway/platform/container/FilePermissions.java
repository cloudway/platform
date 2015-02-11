/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container;

import java.nio.file.Path;
import java.util.HashSet;
import java.util.Set;

public class FilePermissions
{
    private final Set<Path> readonlyFiles = new HashSet<>();

    public void setReadOnly(Path path) {
        readonlyFiles.add(path);
    }

    public void setReadWrite(Path path) {
        readonlyFiles.remove(path);
    }

    public boolean isReadOnly(Path path) {
        return readonlyFiles.contains(path);
    }

    public boolean isReadWrite(Path path) {
        return !readonlyFiles.contains(path);
    }
}
