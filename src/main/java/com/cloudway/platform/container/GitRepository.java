/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container;

import java.io.IOException;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;
import static java.lang.String.format;

import com.cloudway.platform.common.util.Exec;
import com.cloudway.platform.common.util.FileUtils;

/**
 * This class represents an application's Git repository.
 */
class GitRepository implements ApplicationRepository
{
    private static final String GIT_DIR = "git";

    private final ApplicationContainer container;
    private final String repo_name;
    private final Path git_dir, repo_dir;

    public GitRepository(ApplicationContainer container)
        throws IOException
    {
        this.container = container;
        this.repo_name = container.getName() + ".git";
        this.git_dir   = container.getHomeDir().resolve(GIT_DIR);
        this.repo_dir  = git_dir.resolve(repo_name);

        if (!Files.exists(git_dir)) {
            FileUtils.mkdir(git_dir, 0750);
            container.setFileReadOnly(git_dir);
        }
    }

    public boolean exists() {
        return Files.isDirectory(repo_dir);
    }

    public void populateFromTemplate(Path basedir)
        throws IOException
    {
        if (exists()) {
            return;
        }

        Optional<Path> t = Stream.of("template", "template.git")
            .map(basedir::resolve)
            .filter(Files::isDirectory)
            .findFirst();

        if (t.isPresent()) {
            Path template = t.get();
            if (template.toString().endsWith(".git")) {
                FileUtils.copyTree(template, repo_dir);
            } else {
                buildFromTemplate(template);
            }
            configure();
        } else {
            populateEmpty();
        }
    }

    /**
     * Copy a file tree structure and build an application repository.
     */
    private void buildFromTemplate(Path template) throws IOException {
        Path tmpdir = git_dir.resolve("template");
        FileUtils.deleteTree(tmpdir);
        FileUtils.copyTree(template, tmpdir);

        try {
            Exec.args("/bin/sh", "-c", GIT_INIT)
                .directory(tmpdir).checkError().run();
            Exec.args("/bin/sh", "-c", format(GIT_CLONE, "template", repo_name))
                .directory(git_dir).checkError().run();
        } finally {
            FileUtils.deleteTree(tmpdir);
        }
    }

    private static List<String> ALLOWED_SCHEMES = Arrays.asList(
        "git", "http", "https", "ftp", "ftps", "rsync");

    public void populateFromURL(String url)
        throws IOException
    {
        if (exists()) {
            return;
        }

        // extract the commit ref from fragment
        String ref = null;
        int i = url.indexOf('#');
        if (i != -1) {
            ref = url.substring(i+1);
            url = url.substring(0, i);
            if (ref.isEmpty() || !ref.matches("\\A[\\w\\d\\/\\-_\\.\\^~]+\\Z")) {
                ref = null;
            }
        }

        // check validity of the url
        URI uri = URI.create(url);
        if (uri.getScheme() == null || !ALLOWED_SCHEMES.contains(uri.getScheme())) {
            throw new IllegalArgumentException("Unsupported Git clone scheme: " + uri.getScheme());
        }

        // execute the git commands
        String command = ref == null
                ? format(GIT_CLONE,     url, repo_name)
                : format(GIT_URL_CLONE, url, repo_name, ref);
        Exec.args("/bin/sh", "-c", command).directory(git_dir).checkError().run();

        configure();
    }

    public void populateEmpty() throws IOException {
        if (exists()) {
            return;
        }

        FileUtils.mkdir(repo_dir);

        Exec.args("/bin/sh", "-c", GIT_INIT_BARE)
            .directory(repo_dir)
            .checkError()
            .run();

        configure();
    }

    public void tidy() throws IOException {
        if (exists()) {
            container.join(Exec.args("git", "prune"))
                     .directory(repo_dir)
                     .silentIO()
                     .checkError()
                     .run();
            container.join(Exec.args("git", "gc", "--aggressive"))
                     .directory(repo_dir)
                     .silentIO()
                     .checkError()
                     .run();
        }
    }

    public void destroy() throws IOException {
        FileUtils.deleteTree(repo_dir);
    }

    /**
     * Install repository hooks and set permissions.
     */
    private void configure() throws IOException {
        container.setFileTreeReadWrite(repo_dir);

        FileUtils.write(container.getHomeDir().resolve(".gitconfig"), GIT_CONFIG);

        Path hooks = repo_dir.resolve("hooks");
        addHook(hooks, "pre-receive", GIT_PRE_RECEIVE);
        addHook(hooks, "post-receive", GIT_POST_RECEIVE);
        container.setFileTreeReadOnly(hooks);
    }

    private void addHook(Path hooks, String name, String contents)
        throws IOException
    {
        Path file = hooks.resolve(name);
        FileUtils.write(file, contents);
        FileUtils.chmod(file, 0750);
    }

    private static final String GIT_INIT =
        "set -xe;" +
        "git init;" +
        "git config core.logAllRefUpdates true;" +
        "git add -f .;" +
        "git commit -a -m \"Creating template\";";

    private static final String GIT_INIT_BARE =
        "set -xe;" +
        "git init --bare;" +
        "git config core.logAllRefUpdates true;";

    private static final String GIT_CLONE =
        "set -xe;" +
        "git clone --bare --no-hardlinks '%1$s' %2$s;" +
        "GIT_DIR=./%2$s git config core.logAllRefUpdates true;" +
        "GIT_DIR=./%2$s git repack;";

    private static final String GIT_URL_CLONE =
        "set -xe;" +
        "git clone --bare --no-hardlinks '%1$s' %2$s;" +
        "GIT_DIR=./%2$s git config core.logAllRefUpdates true;" +
        "GIT_DIR=./%2$s git reset --soft '%3$s';" +
        "GIT_DIR=./%2$s git branch master || true;" +
        "GIT_DIR=./%2$s git repack;";

    private static final String GIT_CONFIG =
        "[user]\n" +
        "  name = Cloudway User\n" +
        "[gc]\n" +
        "  auto = 100\n";

    private static final String GIT_PRE_RECEIVE =
        "echo placeholder for git pre-receive\n";
    private static final String GIT_POST_RECEIVE =
        "echo placeholder for git post-receive\n";
}
