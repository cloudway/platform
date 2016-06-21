/**
 * Cloudway Platform
 * Copyright (c) 2012-2016 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.bitbucket.plugins;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.channels.Channels;
import java.nio.channels.Pipe;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.PosixFilePermission;
import java.util.EnumSet;
import java.util.Set;

import com.atlassian.bitbucket.hook.HookRequestHandle;
import com.atlassian.bitbucket.hook.HookService;
import com.atlassian.bitbucket.hook.HookUtils;
import com.atlassian.bitbucket.repository.Repository;
import com.atlassian.bitbucket.scm.CommandOutputHandler;
import com.atlassian.bitbucket.scm.git.GitScmConfig;
import com.atlassian.bitbucket.scm.git.command.GitCommand;
import com.atlassian.bitbucket.scm.git.command.GitCommandBuilderFactory;
import com.atlassian.bitbucket.scm.git.command.GitScmCommandBuilder;
import com.atlassian.utils.process.ProcessException;
import com.atlassian.utils.process.Watchdog;
import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import com.google.common.io.ByteStreams;
import org.apache.commons.io.FileUtils;

public class RepoDeployer
{
    private final GitCommandBuilderFactory gitCommandBuilderFactory;
    private final GitScmConfig gitScmConfig;
    private final HookService hookService;

    public RepoDeployer(GitCommandBuilderFactory gitCommandBuilderFactory,
                        GitScmConfig gitScmConfig, HookService hookService) {
        this.gitCommandBuilderFactory = gitCommandBuilderFactory;
        this.gitScmConfig = gitScmConfig;
        this.hookService = hookService;
    }

    public void deploy(Repository repository) throws IOException {
        // Retrieve namespace and name from repository
        String namespace = repository.getProject().getKey().toLowerCase();
        String name = repository.getSlug().toLowerCase();

        // Run git command to generate an archive file
        CommandOutputHandler<Void> handler =
            new DeploymentHandler(name + '-' + namespace);
        GitCommand<Void> command = gitCommandBuilderFactory.builder(repository)
            .command("archive")
            .argument("--format=tar")
            .argument("master")
            .build(handler);

        // The remaining task is performed in the command handler
        command.start();
    }

    static class DeploymentHandler implements CommandOutputHandler<Void> {
        private final String appname;
        private final Path tarfile;
        private final OutputStream tar;

        DeploymentHandler(String appname) throws IOException {
            this.appname = appname;

            // Create a tar archive containing the command output
            tarfile = Files.createTempFile("deploy", ".tar");
            tar = Files.newOutputStream(tarfile);
        }

        @Override
        public void process(InputStream in) throws ProcessException {
            try {
                ByteStreams.copy(in, tar);
            } catch (IOException ex) {
                throw new ProcessException(ex);
            }
        }

        @Override
        public void complete() throws ProcessException {
            try {
                tar.close();

                // Run cwman to deploy the archive
                ProcessBuilder builder = new ProcessBuilder();
                builder.command("/usr/bin/cwman", "deploy", appname, tarfile.toString());
                builder.start().waitFor();
            } catch (Exception ex) {
                throw new ProcessException(ex);
            } finally {
                cleanup();
            }
        }

        private void cleanup() throws ProcessException {
            try {
                Files.delete(tarfile);
            } catch (IOException ex) {
                throw new ProcessException(ex);
            }
        }

        @Override
        public void setWatchdog(Watchdog wdog) {
            // noop
        }

        @Override
        public Void getOutput() {
            return null;
        }
    }

    public InputStream archive(Repository repository) throws IOException {
        Pipe pipe = Pipe.open();
        InputStream pipeIn = Channels.newInputStream(pipe.source());
        OutputStream pipeOut = Channels.newOutputStream(pipe.sink());

        CommandOutputHandler<Void> handler = new LoggingHandler(pipeOut);
        GitCommand<Void> command = gitCommandBuilderFactory.builder(repository)
            .command("archive")
            .argument("--format=tar")
            .argument("master")
            .build(handler);
        command.start();

        return pipeIn;
    }

    static class LoggingHandler implements CommandOutputHandler<Void> {
        private final OutputStream out;

        LoggingHandler(OutputStream out) {
            this.out = out;
        }

        @Override
        public void process(InputStream in) throws ProcessException {
            try {
                ByteStreams.copy(in, out);
            } catch (IOException ex) {
                throw new ProcessException(ex);
            }
        }

        @Override
        public void complete() throws ProcessException {
            try {
                out.close();
            } catch (IOException ex) {
                throw new ProcessException(ex);
            }
        }

        @Override
        public void setWatchdog(Watchdog wdog) {
            // noop
        }

        @Override
        public Void getOutput() {
            return null;
        }
    }

    public void populate(Repository repository, InputStream payload) throws IOException {
        Path tempRepoDir = Files.createTempDirectory("repo");
        untarTemplateFiles(tempRepoDir, payload);
        createTemplateRepo(tempRepoDir);
        pushTemplateToNewRepo(tempRepoDir, repository);
        FileUtils.deleteDirectory(tempRepoDir.toFile());
    }

    public void populate(Repository repository, String url) throws IOException {
        Path tempRepoDir = Files.createTempDirectory("repo");
        cloneTemplateRepo(tempRepoDir, url);
        pushTemplateToNewRepo(tempRepoDir, repository);
        FileUtils.deleteDirectory(tempRepoDir.toFile());
    }

    private static void untarTemplateFiles(Path tempRepoDir, InputStream in) throws IOException {
        TarArchiveInputStream tar = new TarArchiveInputStream(in);
        TarArchiveEntry entry;

        while ((entry = tar.getNextTarEntry()) != null) {
            Path dest = tempRepoDir.resolve(entry.getName());
            if (entry.isDirectory()) {
                Files.createDirectory(dest);
            } else if (entry.isFile()) {
                Files.createDirectories(dest.getParent());
                Files.copy(tar, dest);
            } else {
                continue; // TODO: handle symlinks
            }
            chmod(dest, entry.getMode() & 0777);
        }
    }

    private static void chmod(Path path, int mode) throws IOException {
        // chmod never changes the permissions of symblic links;
        // the chmod system call cannot change their permissions.
        // This is not a problem since the permissions of symbolic
        // links are never used.
        if (!Files.isSymbolicLink(path)) {
            Files.setPosixFilePermissions(path, getPermissions(mode));
        }
    }

    private static final PosixFilePermission[] PERM_BITS = {
        PosixFilePermission.OTHERS_EXECUTE,
        PosixFilePermission.OTHERS_WRITE,
        PosixFilePermission.OTHERS_READ,
        PosixFilePermission.GROUP_EXECUTE,
        PosixFilePermission.GROUP_WRITE,
        PosixFilePermission.GROUP_READ,
        PosixFilePermission.OWNER_EXECUTE,
        PosixFilePermission.OWNER_WRITE,
        PosixFilePermission.OWNER_READ
    };

    private static Set<PosixFilePermission> getPermissions(int bits) {
        Set<PosixFilePermission> set = EnumSet.noneOf(PosixFilePermission.class);
        for (PosixFilePermission p : PERM_BITS) {
            if ((bits & 1) != 0)
                set.add(p);
            bits >>= 1;
        }
        return set;
    }

    private void cloneTemplateRepo(Path tempRepoDir, String url) {
        gitCommandBuilderFactory.builder()
            .workingDirectory(tempRepoDir.toString())
            .command("clone")
            .argument("--bare")
            .argument("--no-hardlinks")
            .argument(url)
            .argument(".")
            .build(new LoggingHandler(System.err))
            .call();
    }

    private void createTemplateRepo(Path tempRepoDir) {
        // git init
        gitCommandBuilderFactory.builder()
            .init()
            .directory(tempRepoDir.toString())
            .build()
            .call();

        // git add
        gitCommandBuilderFactory.builder()
            .add()
            .workingDirectory(tempRepoDir.toString())
            .all(true)
            .path(".")
            .build()
            .call();

        // git commit
        gitCommandBuilderFactory.builder()
            .commit()
            .workingDirectory(tempRepoDir.toString())
            .message("Populate template")
            .author("nobody", "nobody@example.com")
            .build()
            .call();
    }

    private void pushTemplateToNewRepo(Path tempRepoDir, Repository newRepo) {
        GitScmCommandBuilder builder = gitCommandBuilderFactory.builder()
            .workingDirectory(tempRepoDir.toString())
            .command("push")
            .argument("--mirror")
            .argument(gitScmConfig.getRepositoryDir(newRepo).getAbsolutePath());

        HookRequestHandle requestHandle = hookService.registerRequest(newRepo.getId());
        HookUtils.configure(requestHandle, builder);

        builder.build(new LoggingHandler(System.err)).call();
    }
}
