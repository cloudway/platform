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

import com.cloudway.platform.common.Config;
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

        Optional<Path> t = Stream.of(TEMPLATES)
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
        Path tmp_dir = git_dir.resolve("template");
        FileUtils.deleteTree(tmp_dir);
        FileUtils.copyTree(template, tmp_dir);

        try {
            exec(tmp_dir, "/bin/sh", "-c", format(GIT_INIT, ApplicationContainer.DOMAIN));
            exec(git_dir, "/bin/sh", "-c", format(GIT_CLONE, "template", repo_name));
        } finally {
            FileUtils.deleteTree(tmp_dir);
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
        exec(git_dir, "/bin/sh", "-c", command);
        configure();
    }

    public void populateEmpty() throws IOException {
        if (exists()) {
            return;
        }

        FileUtils.mkdir(repo_dir);
        exec(repo_dir, "/bin/sh", "-c", GIT_INIT_BARE);
        configure();
    }

    public void checkout(Path target)
        throws IOException
    {
        if (exists()) {
            FileUtils.emptyDirectory(target);
            execInContext(repo_dir, "/bin/sh", "-c", format(GIT_CHECKOUT, target));
        }
    }

    public void tidy() throws IOException {
        if (exists()) {
            execInContext(repo_dir, "git", "prune");
            execInContext(repo_dir, "git", "gc", "--aggressive");
        }
    }

    public void destroy() throws IOException {
        FileUtils.deleteTree(repo_dir);
    }

    private void exec(Path dir, String... args) throws IOException {
        Exec.args(args).directory(dir).silentIO().checkError().run();
    }

    private void execInContext(Path dir, String... args) throws IOException {
        container.join(Exec.args(args)).directory(dir).silentIO().checkError().run();
    }

    /**
     * Install repository hooks and set permissions.
     */
    private void configure() throws IOException {
        container.setFileTreeReadWrite(repo_dir);

        FileUtils.write(container.getHomeDir().resolve(".gitconfig"), GIT_CONFIG);

        Path hooks = repo_dir.resolve("hooks");
        String bin = Config.HOME_DIR.resolve("bin").toString();
        addHook(hooks, "pre-receive",  format("%s/cwctl pre-receive%n",  bin));
        addHook(hooks, "post-receive", format("%s/cwctl post-receive%n", bin));
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
