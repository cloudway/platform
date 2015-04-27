/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util;

import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.FileVisitor;
import java.nio.file.Path;
import java.nio.file.attribute.BasicFileAttributes;

import com.cloudway.fp.function.ExceptionConsumer;

public abstract class SimpleFileVisitor implements FileVisitor<Path>
{
    public static SimpleFileVisitor of(ExceptionConsumer<Path, IOException> action) {
        return new SimpleFileVisitor() {
            @Override
            protected FileVisitResult visit(Path path) throws IOException {
                action.consume(path);
                return FileVisitResult.CONTINUE;
            }
        };
    }

    protected abstract FileVisitResult visit(Path path)
        throws IOException;

    @Override
    public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs)
        throws IOException
    {
        return visit(dir);
    }

    @Override
    public FileVisitResult visitFile(Path file, BasicFileAttributes attrs)
        throws IOException
    {
        return visit(file);
    }

    @Override
    public FileVisitResult visitFileFailed(Path file, IOException exc)
        throws IOException
    {
        throw exc;
    }

    @Override
    public FileVisitResult postVisitDirectory(Path dir, IOException exc)
        throws IOException
    {
        if (exc != null)
            throw exc;
        return FileVisitResult.CONTINUE;
    }
}
