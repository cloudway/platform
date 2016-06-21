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

import com.atlassian.bitbucket.repository.Repository;
import com.atlassian.bitbucket.scm.CommandOutputHandler;
import com.atlassian.bitbucket.scm.git.command.GitCommand;
import com.atlassian.bitbucket.scm.git.command.GitCommandBuilderFactory;
import com.atlassian.utils.process.ProcessException;
import com.atlassian.utils.process.Watchdog;
import com.google.common.io.ByteStreams;

public class RepoDeployer
{
    private final GitCommandBuilderFactory gitCommandBuilderFactory;

    public RepoDeployer(GitCommandBuilderFactory gitCommandBuilderFactory) {
        this.gitCommandBuilderFactory = gitCommandBuilderFactory;
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
}
