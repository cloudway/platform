/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.io;

import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.FileVisitor;
import java.nio.file.Path;
import java.nio.file.attribute.BasicFileAttributes;

public abstract class AbstractFileVisitor implements FileVisitor<Path>
{
    protected abstract FileVisitResult visit(Path path)
        throws IOException;

    public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs)
        throws IOException
    {
        return visit(dir);
    }

    public FileVisitResult visitFile(Path file, BasicFileAttributes attrs)
        throws IOException
    {
        return visit(file);
    }

    public FileVisitResult visitFileFailed(Path file, IOException exc)
        throws IOException
    {
        throw exc;
    }

    public FileVisitResult postVisitDirectory(Path dir, IOException exc)
        throws IOException
    {
        if (exc != null)
            throw exc;
        return FileVisitResult.CONTINUE;
    }
}
