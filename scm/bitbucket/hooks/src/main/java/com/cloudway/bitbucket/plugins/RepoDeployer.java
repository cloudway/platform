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
import java.util.zip.GZIPOutputStream;

import com.atlassian.bitbucket.hook.HookRequestHandle;
import com.atlassian.bitbucket.hook.HookService;
import com.atlassian.bitbucket.hook.HookUtils;
import com.atlassian.bitbucket.hook.repository.RepositoryHookService;
import com.atlassian.bitbucket.repository.Repository;
import com.atlassian.bitbucket.repository.RepositoryService;
import com.atlassian.bitbucket.scm.CommandOutputHandler;
import com.atlassian.bitbucket.scm.git.GitScmConfig;
import com.atlassian.bitbucket.scm.git.command.GitCommand;
import com.atlassian.bitbucket.scm.git.command.GitCommandBuilderFactory;
import com.atlassian.bitbucket.scm.git.command.GitScmCommandBuilder;
import com.atlassian.utils.process.ProcessException;
import com.atlassian.utils.process.Watchdog;
import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import org.apache.commons.compress.archivers.tar.TarArchiveOutputStream;
import org.apache.commons.io.FileUtils;
import com.google.common.io.ByteStreams;

public class RepoDeployer
{
    private final GitCommandBuilderFactory gitCommandBuilderFactory;
    private final GitScmConfig gitScmConfig;
    private final HookService hookService;
    private final RepositoryHookService repositoryHookService;
    private final RepositoryService repositoryService;

    public RepoDeployer(GitCommandBuilderFactory gitCommandBuilderFactory,
                        GitScmConfig gitScmConfig, HookService hookService,
                        RepositoryHookService repositoryHookService,
                        RepositoryService repositoryService) {
        this.gitCommandBuilderFactory = gitCommandBuilderFactory;
        this.gitScmConfig = gitScmConfig;
        this.hookService = hookService;
        this.repositoryHookService = repositoryHookService;
        this.repositoryService = repositoryService;
    }

    public void deploy(Repository repository, boolean asynchronous) throws IOException {
        // Retrieve namespace and name from repository
        String namespace = repository.getProject().getKey().toLowerCase();
        String name = repository.getSlug().toLowerCase();

        // Create a temporary directory to save the repository archive
        Path archiveDir = Files.createTempDirectory("deploy");
        Path archiveFile = archiveDir.resolve(archiveDir.getFileName() + ".tar.gz");
        DeploymentHandler handler = new DeploymentHandler(name, namespace, archiveDir);

        if (repositoryService.isEmpty(repository)) {
            // Create empty archive file
            TarArchiveOutputStream tar =
                new TarArchiveOutputStream(
                    new GZIPOutputStream(
                        Files.newOutputStream(archiveFile)));
            tar.close();

            try {
                handler.complete();
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        } else {
            // Run git command to generate an archive file
            GitCommand<Void> command = gitCommandBuilderFactory.builder(repository)
                .command("archive")
                .argument("--format=tar.gz")
                .argument("-o")
                .argument(archiveFile.toString())
                .argument("master")
                .build(handler);

            // The remaining task is performed in the command handler
            if (asynchronous) {
                command.start();
            } else {
                command.call();
            }
        }
    }

    static class DeploymentHandler extends LoggingHandler {
        private final String name, namespace;
        private final Path archiveDir;

        DeploymentHandler(String name, String namespace, Path archiveDir) {
            super(System.err);
            this.name = name;
            this.namespace = namespace;
            this.archiveDir = archiveDir;
        }

        @Override
        public void complete() throws ProcessException {
            try {
                // Run cwman to deploy the archive
                ProcessBuilder builder = new ProcessBuilder();
                builder.command("/usr/bin/cwman", "deploy", name, namespace, archiveDir.toString());
                builder.redirectError(ProcessBuilder.Redirect.INHERIT);
                builder.redirectOutput(ProcessBuilder.Redirect.INHERIT);
                builder.start().waitFor();
            } catch (Exception ex) {
                throw new ProcessException(ex);
            } finally {
                cleanup();
            }
        }

        private void cleanup() throws ProcessException {
            try {
                FileUtils.deleteDirectory(archiveDir.toFile());
            } catch (IOException ex) {
                throw new ProcessException(ex);
            }
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
            if (out != System.out && out != System.err) {
                try {
                    out.close();
                } catch (IOException ex) {
                    throw new ProcessException(ex);
                }
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

    private static final String HOOK_KEY = "com.cloudway.bitbucket.plugins.repo-deployer:repo-deployer";

    private void pushTemplateToNewRepo(Path tempRepoDir, Repository newRepo) {
        // temprarily disable post-receive hook
        repositoryHookService.disable(newRepo, HOOK_KEY);
        try {
            GitScmCommandBuilder builder = gitCommandBuilderFactory.builder()
                .workingDirectory(tempRepoDir.toString())
                .command("push")
                .argument("--mirror")
                .argument(gitScmConfig.getRepositoryDir(newRepo).getAbsolutePath());

            HookRequestHandle requestHandle = hookService.registerRequest(newRepo.getId());
            HookUtils.configure(requestHandle, builder);

            builder.build(new LoggingHandler(System.err)).call();
        } finally {
            repositoryHookService.enable(newRepo, HOOK_KEY);
        }
    }
}
