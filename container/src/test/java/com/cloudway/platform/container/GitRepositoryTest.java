/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container;

import java.io.IOException;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Base64;
import java.util.Random;
import java.util.function.Function;

import org.jmock.Expectations;
import org.jmock.integration.junit4.JUnitRuleMockery;
import org.junit.Rule;
import org.junit.Test;
import static org.junit.Assert.*;
import static org.hamcrest.CoreMatchers.*;

import com.cloudway.fp.function.ExceptionBiConsumer;
import com.cloudway.platform.common.util.SimpleFileVisitor;
import com.cloudway.platform.common.os.Exec;
import static com.cloudway.platform.common.util.MoreFiles.*;
import static com.cloudway.platform.container.Actions.*;
import static java.nio.file.Files.*;

public class GitRepositoryTest
{
    public final @Rule JUnitRuleMockery context = new JUnitRuleMockery();
    public final @Rule TemporaryDirectory temporary = new TemporaryDirectory();

    private final Random random = new Random();
    private int test_id = 0;

    private static final String APP_NAME = "mock";

    private GitRepository newRepository(Path home) throws IOException {
        Container container = context.mock(Container.class, "ApplicationContainer_" + (++test_id));

        context.checking(new Expectations() {{
            allowing(container).getName();
                will(returnValue(APP_NAME));
            allowing(container).getHomeDir();
                will(returnValue(home));
            allowing(container).exec(with(any(Exec.class)));
                will(apply("execute command", Function.identity()));
            allowing(container).join(with(any(Exec.class)));
                will(apply("execute command in context", Function.identity()));
            allowing(container);
        }});

        return new GitRepository(container);
    }

    @Test
    public void populate_empty_repository() throws IOException {
        Path home = temporary.newFolder();
        Path target = mkdir(home.resolve("target"));
        Path git_repo = join(home, "git", APP_NAME + ".git");

        GitRepository repo = newRepository(home);
        repo.populateEmpty();
        repo.checkout(target);

        assertTrue(isDirectory(git_repo));
        walkFileTree(target, SimpleFileVisitor.of(path -> assertThat(path, is(target))));

        repo.tidy();
        repo.destroy();
        assertFalse(exists(git_repo));
    }

    @Test
    public void populate_repository_from_template() throws IOException {
        Path home     = temporary.newFolder();
        Path source   = mkdir(home.resolve("source"));
        Path template = mkdir(source.resolve("template"));
        Path target   = mkdir(home.resolve("target"));
        Path git_repo = join(home, "git", APP_NAME + ".git");

        GitRepository repo = newRepository(home);
        shuffleDirectory(template);
        repo.populateFromTemplate(source);
        repo.checkout(target);

        assertTrue(isDirectory(git_repo));
        compareDirectory(template, target);
        compareDirectory(target, template);

        repo.tidy();
        repo.destroy();
        assertFalse(exists(git_repo));
    }

    @Test
    public void populate_repository_from_template_repo() throws IOException {
        populateRepositoryFromExternalRepo(GitRepository::populateFromTemplate);
    }

    @Test
    public void populate_repository_from_external_url() throws IOException {
        populateRepositoryFromExternalRepo((repo, source) ->
            repo.populateFromURL(source.resolve("template.git").toUri().toString()));
    }

    @Test
    public void populate_repository_from_external_url_with_ref() throws IOException {
        populateRepositoryFromExternalRepo((repo, source) ->
            repo.populateFromURL(source.resolve("template.git").toUri().toString() + "#master"));
    }

    public void populateRepositoryFromExternalRepo(
        ExceptionBiConsumer<GitRepository, Path, IOException> populator) throws IOException
    {
        Path home       = temporary.newFolder();
        Path tmp_source = temporary.newFolder();
        Path ext_source = temporary.newFolder();
        Path target     = mkdir(home.resolve("target"));
        Path git_repo   = join(home, "git", APP_NAME + ".git");

        GitRepository repo = newRepository(home);
        buildTemplateRepository(tmp_source, ext_source);
        populator.consume(repo, ext_source);
        repo.checkout(target);

        assertTrue(isDirectory(git_repo));
        compareDirectory(tmp_source.resolve("template"), target);
        compareDirectory(target, tmp_source.resolve("template"));

        repo.tidy();
        repo.destroy();
        assertFalse(exists(git_repo));
    }

    private void buildTemplateRepository(Path tmp_source, Path ext_source) throws IOException {
        Path home     = temporary.newFolder();
        Path git_repo = join(home, "git", APP_NAME + ".git");

        GitRepository repo = newRepository(home);
        shuffleDirectory(mkdir(tmp_source.resolve("template")));
        repo.populateFromTemplate(tmp_source);
        move(git_repo, ext_source.resolve("template.git"));
    }

    private void shuffleDirectory(Path root) throws IOException {
        randomFiles(root, 10);
        randomFiles(randomDirectory(root), 10);
        randomFiles(randomDirectory(root), 10);
    }

    private Path randomDirectory(Path root) throws IOException {
        return mkdir(randomPath(root));
    }

    private void randomFiles(Path dir, int count) throws IOException {
        for (int i = 0; i < count; i++) {
            writeText(randomPath(dir), randomText());
        }
    }

    private Path randomPath(Path root) {
        Path path;
        do {
            path = root.resolve(randomName());
        } while (exists(path));
        return path;
    }

    private String randomName() {
        int len = random.nextInt(9) + 1;
        int[] chars = new int[len];
        Arrays.setAll(chars, i -> random.nextInt('z' - 'a') + 'a');
        return new String(chars, 0, chars.length);
    }

    private String randomText() {
        int len = random.nextInt(1024 - 256) + 256;
        byte[] bytes = new byte[len];
        random.nextBytes(bytes);
        return Base64.getEncoder().encodeToString(bytes);
    }

    private static void compareDirectory(Path src, Path dst) throws IOException {
        walkFileTree(src, SimpleFileVisitor.of(path -> {
            Path target = dst.resolve(src.relativize(path));
            assertTrue(exists(target));
            if (isRegularFile(path)) {
                assertEquals(readText(path), readText(target));
            }
        }));
    }
}
