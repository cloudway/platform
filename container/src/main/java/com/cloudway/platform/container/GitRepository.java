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
import java.util.regex.Pattern;
import com.google.common.collect.ImmutableSet;
import static java.lang.String.format;

import com.cloudway.platform.common.fp.data.Maybe;
import com.cloudway.platform.common.fp.data.Seq;
import com.cloudway.platform.common.os.Config;
import com.cloudway.platform.common.os.Exec;

import static java.nio.file.Files.*;
import static com.cloudway.platform.common.util.MoreFiles.*;

/**
 * This class represents an application's Git repository.
 */
class GitRepository implements ApplicationRepository
{
    private static final String GIT_DIR = "git";

    private final ApplicationContainer container;
    private final String repo_name;
    private final Path git_dir, repo_dir;

    private static final String TEMPLATES[] = {
        "template", "template.git", "share/template", "share/template.git"
    };

    public GitRepository(ApplicationContainer container)
        throws IOException
    {
        this.container = container;
        this.repo_name = container.getName() + ".git";
        this.git_dir   = container.getHomeDir().resolve(GIT_DIR);
        this.repo_dir  = git_dir.resolve(repo_name);

        if (!Files.exists(git_dir)) {
            mkdir(git_dir, 0750);
            container.setFileReadOnly(git_dir);
        }
    }

    @Override
    public boolean exists() {
        return isDirectory(repo_dir);
    }

    @Override
    public void populateFromTemplate(Path basedir)
        throws IOException
    {
        if (exists()) {
            return;
        }

        Maybe<Path> t = Seq.of(TEMPLATES)
            .map(basedir::resolve)
            .filter(Files::isDirectory)
            .peek();

        if (t.isPresent()) {
            Path template = t.get();
            if (template.toString().endsWith(".git")) {
                copyFileTree(template, repo_dir);
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
        Path tmp_dir = git_dir.resolve("template");
        deleteFileTree(tmp_dir);
        copyFileTree(template, tmp_dir);

        try {
            exec(tmp_dir, "/bin/sh", "-c", format(GIT_INIT, Config.DOMAIN));
            exec(git_dir, "/bin/sh", "-c", format(GIT_CLONE, "template", repo_name));
        } finally {
            deleteFileTree(tmp_dir);
        }
    }

    private static final ImmutableSet<String> ALLOWED_SCHEMES =
        ImmutableSet.of("git", "http", "https", "ftp", "ftps", "rsync", "file");

    private static final Pattern REF_PATTERN =
        Pattern.compile("\\A[\\w\\d\\/\\-_\\.\\^~]+\\Z");

    @Override
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
            if (ref.isEmpty() || !REF_PATTERN.matcher(ref).matches()) {
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
        exec(git_dir, "/bin/sh", "-c", command);
        configure();
    }

    @Override
    public void populateEmpty() throws IOException {
        if (exists()) {
            return;
        }

        mkdir(repo_dir);
        exec(repo_dir, "/bin/sh", "-c", GIT_INIT_BARE);
        configure();
    }

    @Override
    public void checkout(Path target)
        throws IOException
    {
        if (exists()) {
            emptyDirectory(target);
            execInContext(repo_dir, "/bin/sh", "-c", format(GIT_CHECKOUT, target));
        }
    }

    @Override
    public void tidy() throws IOException {
        if (exists()) {
            execInContext(repo_dir, "git", "prune");
            execInContext(repo_dir, "git", "gc", "--aggressive");
        }
    }

    @Override
    public void destroy() throws IOException {
        deleteFileTree(repo_dir);
    }

    @SuppressWarnings("MethodMayBeStatic")
    private void exec(Path dir, String... args) throws IOException {
        container.exec(Exec.args(args)).directory(dir).silentIO().checkError().run();
    }

    private void execInContext(Path dir, String... args) throws IOException {
        container.join(Exec.args(args)).directory(dir).silentIO().checkError().run();
    }

    /**
     * Install repository hooks and set permissions.
     */
    private void configure() throws IOException {
        container.setFileTreeReadWrite(repo_dir);

        writeText(container.getHomeDir().resolve(".gitconfig"), GIT_CONFIG);

        Path hooks = repo_dir.resolve("hooks");
        String bin = Config.HOME_DIR.resolve("bin").toString();
        addHook(hooks, "pre-receive",  format("%s/cwctl pre-receive%n",  bin));
        addHook(hooks, "post-receive", format("%s/cwctl post-receive%n", bin));
        container.setFileTreeReadOnly(hooks);
    }

    private static void addHook(Path hooks, String name, String contents)
        throws IOException
    {
        Path file = hooks.resolve(name);
        writeText(file, contents);
        chmod(file, 0750);
    }

    private static final String GIT_INIT =
        "set -xe;" +
        "git init;" +
        "git config core.logAllRefUpdates true;" +
        "git config user.email \"guest@%1$s\"\n" +
        "git config user.name \"Cloudway Guest\"\n" +
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

    private static final String GIT_CHECKOUT =
        "set -xe;\n" +
        "shopt -s dotglob;\n" +
        "if [ \"$(find objects -type f 2>/dev/null | wc -l)\" -ne \"0\" ]; then\n" +
        "  git archive --format=tar master | (cd %1$s && tar -xf -);\n" +
        "fi\n";

    private static final String GIT_CONFIG =
        "[user]\n" +
        "  name = Cloudway Guest\n" +
        "[gc]\n" +
        "  auto = 100\n";
}
