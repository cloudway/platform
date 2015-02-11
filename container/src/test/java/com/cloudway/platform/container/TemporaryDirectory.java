/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import com.cloudway.platform.common.util.MoreFiles;
import org.junit.rules.ExternalResource;

/**
 * The TemporaryDirectory Rule allows creation of files and folders that are
 * guaranteed to be deleted when the test method finishes (whether it passes or
 * fails).
 */
public class TemporaryDirectory extends ExternalResource {
    private final Path parentDir;
    private Path root;

    public TemporaryDirectory() {
        this(null);
    }

    public TemporaryDirectory(Path parentDir) {
        this.parentDir = parentDir;
    }

    @Override
    protected void before() throws IOException {
        root = (parentDir == null) ? Files.createTempDirectory("junit")
                                   : Files.createTempDirectory(parentDir, "junit");
    }

    @Override
    protected void after() {
        try {
            delete();
        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }

    /**
     * Returns a new fresh file with a random name under the temporary directory.
     */
    public Path newFile() throws IOException {
        return Files.createTempFile(root, "junit", ".tmp");
    }

    /**
     * Returns a new fresh file using the given prefix and suffix strings to
     * generate its name.
     */
    public Path newFile(String prefix, String suffix) throws IOException {
        return Files.createTempFile(root, prefix, suffix);
    }

    /**
     * Returns a new fresh folder with a random name under the temporary
     * directory.
     */
    public Path newFolder() throws IOException {
        return Files.createTempDirectory(root, "junit");
    }

    /**
     * Returns the location of this temporary directory.
     */
    public Path getRoot() {
        return root;
    }

    /**
     * Delete all files and folders under the temporary directory. Usually not
     * called directly, since it is automatically applied by the Rule.
     */
    public void delete() throws IOException {
        MoreFiles.deleteFileTree(root);
    }
}
