/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container;

import java.nio.file.Path;

public class Addon
{
    private Path path;
    private AddonType type;

    public Addon(Path path) {
        this.path = path;
    }

    public Path getPath() {
        return path;
    }

    public AddonType getType() {
        return type;
    }

    public void setType(AddonType type) {
        this.type = type;
    }
}
