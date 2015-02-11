/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container;

import java.io.File;
import java.io.IOException;
import java.io.StringWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Marshaller;

import org.hamcrest.Matcher;
import org.jmock.Expectations;
import org.jmock.States;
import org.jmock.api.Action;
import org.jmock.api.Invocation;
import org.jmock.auto.Mock;
import org.jmock.integration.junit4.JUnitRuleMockery;
import org.jmock.lib.action.CustomAction;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;
import static com.cloudway.platform.container.Actions.*;

import com.cloudway.platform.common.util.SimpleFileVisitor;
import com.cloudway.platform.common.fp.io.IO;
import com.cloudway.platform.common.os.Config;
import com.cloudway.platform.common.os.Exec;
import com.cloudway.platform.common.fp.function.ExceptionBiConsumer;
import com.cloudway.platform.common.fp.function.ExceptionConsumer;
import com.cloudway.platform.container.proxy.HttpProxy;
import com.cloudway.platform.container.proxy.HttpProxyUpdater;
import com.cloudway.platform.container.proxy.ProxyMapping;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;

import static java.nio.file.Files.*;
import static com.cloudway.platform.common.util.MoreFiles.*;
import static com.cloudway.platform.common.fp.data.Optionals.or;

// @formatter:off
public class AddonControlTest
{
    public final @Rule JUnitRuleMockery context = new JUnitRuleMockery();
    public final @Rule TemporaryDirectory temporary = new TemporaryDirectory();

    private @Mock ApplicationContainer container;
    private @Mock ApplicationRepository repository;
    private @Mock HttpProxyUpdater proxy;

    @Before public void setUp() {
        HttpProxy.setProvider(() -> proxy);
        ApplicationRepository.setFactory(c -> repository);
    }

    @After public void tearDown() {
        HttpProxy.setProvider(null);
        ApplicationRepository.setFactory(null);
    }

    private static final String APP_ID = ApplicationContainer.makeUUID();
    private static final String APP_NAME = "test";
    private static final String APP_NAMESPACE = "demo";
    private static final String APP_DNS = APP_NAME + "-" + APP_NAMESPACE + "." + Config.DOMAIN;
    private static final String APP_TEST_INFO = "app test info";

    // define default behavior of an application container
    private void mockDefaults(Path home, Map<String, String> env) throws Exception {
        env.put("CLOUDWAY_APP_ID", APP_ID);
        env.put("CLOUDWAY_APP_NAME", APP_NAME);
        env.put("CLOUDWAY_APP_DNS", APP_DNS);
        env.put("CLOUDWAY_APP_TEST", APP_TEST_INFO);
        env.put("CLOUDWAY_HOME_DIR", home.toString());

        context.checking(new Expectations() {{
            allowing(container).getId();
                will(returnValue(APP_ID));
            allowing(container).getName();
                will(returnValue(APP_NAME));
            allowing(container).getNamespace();
                will(returnValue(APP_NAMESPACE));
            allowing(container).getDomainName();
                will(returnValue(APP_DNS));

            allowing(container).getHomeDir();
                will(returnValue(home));
            allowing(container).getEnvDir();
                will(returnValue(home.resolve(".env")));
            allowing(container).getAppDir();
                will(returnValue(home.resolve("app")));
            allowing(container).getRepoDir();
                will(returnValue(home.resolve("app").resolve("repo")));
            allowing(container).getDataDir();
                will(returnValue(home.resolve("app").resolve("data")));

            allowing(container).environ();
                will(apply("load environment variables", () -> new HashMap<>(env)));
            allowing(container).getenv(with(any(String.class)));
                will(apply("get environment variable value", env::get));
            allowing(container).setenv(with(any(String.class)), with(any(String.class)));
                will(perform("set environment variable", env::put));
            allowing(container).unsetenv(with(any(String.class)));
                will(perform("remove environment variable", (String name) -> env.remove(name)));

            allowing(container).exec(with(any(Exec.class)));
                will(execute("command"));
        }});
    }

    @Test
    public void install_addon() throws Exception {
        Path source = prepareAddonDir(withContent("bin/setup", "#exec"));

        context.checking(new Expectations() {{
            oneOf(container).join(with(anAction("setup")));
                will(execute("setup"));
        }});

        install(source, (control, home) ->
            compareDirectory(source, home.resolve("mock"))
        );
    }

    @Test
    public void install_addon_with_implicit_shared_files() throws Exception {
        Path source = prepareAddonDir(withContent("share/test", "shared file"));
        install(source, (control, home) -> {
            Path sourceFile = join(source, "share", "test");
            Path sharedDir = join(home, "mock", "share");
            Path sharedFile = sharedDir.resolve("test");

            assertTrue(isSymbolicLink(sharedDir));
            assertEquals(sourceFile.toRealPath(), sharedFile.toRealPath());
        });
    }

    @Test
    public void install_addon_with_explicit_shared_files() throws Exception {
        Path source = prepareAddonDir(withContents()
            .put("metadata/shared_files", "common\n*.x\nb/*.y\n")
            .put("common/test", "shared file")
            .put("a.x", "ax")
            .put("a.y", "ay")
            .put("b/b.x", "bx")
            .put("b/b.y", "by")
            .build());

        install(source, (control, home) -> {
            Path sourceFile = join(source, "common", "test");
            Path sharedDir = join(home, "mock", "common");
            Path sharedFile = sharedDir.resolve("test");

            assertTrue(isSymbolicLink(sharedDir));
            assertEquals(sourceFile.toRealPath(), sharedFile.toRealPath());

            assertTrue(isSymbolicLink(join(home, "mock", "a.x")));
            assertFalse(isSymbolicLink(join(home, "mock", "a.y")));
            assertFalse(isSymbolicLink(join(home, "mock", "b", "b.x")));
            assertTrue(isSymbolicLink(join(home, "mock", "b", "b.y")));
        });
    }

    @Test
    public void install_addon_from_zip_file() throws Exception {
        Path dir = prepareAddonDir(withNoContents());
        Path zip = createZipFile(dir, temporary.newFile("tmp", ".zip"));
        install(zip, (c,p) -> {});
    }

    private static Path createZipFile(Path source, Path zipfile) throws IOException {
        try (ZipOutputStream zout = new ZipOutputStream(newOutputStream(zipfile))) {
            walkFileTree(source, SimpleFileVisitor.of(path -> {
                boolean isDir = isDirectory(path);
                String name = source.relativize(path).toString();
                if (name.isEmpty())
                    return;
                if (isDir && !name.endsWith(File.separator))
                    name += File.separator;
                name = name.replace(File.separatorChar, '/');

                long size = isDir ? 0 : Files.size(path);
                ZipEntry ze = new ZipEntry(name);
                ze.setTime(getLastModifiedTime(path).toMillis());
                if (size == 0) {
                    ze.setMethod(ZipEntry.STORED);
                    ze.setSize(0);
                    ze.setCrc(0);
                }
                zout.putNextEntry(ze);
                if (!isDir) {
                    Files.copy(path, zout);
                }
                zout.closeEntry();
            }));
            return zipfile;
        }
    }

    @Test(expected = IllegalStateException.class)
    public void disallow_addon_overwrite_existing_directory() throws Exception {
        Path home = prepareApplicationHome(withNoContents());
        assertTrue(exists(home.resolve("app")));

        Path source = prepareAddonDir(
            inTemporaryFolder(),
            new MetaData.Addon() {{ name = "app"; }},
            withNoContents());

        mockDefaults(home, new HashMap<>());
        AddonControl control = new DefaultAddonControl(container);
        control.install(source, null);
    }

    @Test(expected = IllegalStateException.class)
    public void disallow_install_addon_multiple_times() throws Exception {
        Path source = prepareAddonDir(withNoContents());
        install(source, (control, home) -> {
            control.install(source, null);
        });
    }

    @Test(expected = IllegalStateException.class)
    public void disallow_install_multiple_framework_addons() throws Exception {
        install(prepareAddonDir(withNoContents()), (control, home) -> {
            Path other = prepareAddonDir(inTemporaryFolder(),
                new MetaData.Addon() {{
                    name = "other";
                    category = AddonType.FRAMEWORK;
                }}, withNoContents());

            control.install(other, null);
        });
    }

    @Test
    public void allow_install_multiple_component_addons() throws Exception {
        install(prepareAddonDir(withNoContents()), (control, home) -> {
            Path other1 = prepareAddonDir(inTemporaryFolder(),
                new MetaData.Addon() {{
                    name = "other1";
                    category = AddonType.COMPONENT;
                }}, withNoContents());
            Path other2 = prepareAddonDir(inTemporaryFolder(),
                new MetaData.Addon() {{
                    name = "other2";
                    category = AddonType.SERVICE;
                }}, withNoContents());

            control.install(other1, null);
            control.install(other2, null);

            assertThat(control.addons().size(), is(3));
            assertTrue(Files.exists(home.resolve("other1")));
            assertTrue(Files.exists(home.resolve("other2")));
        });
    }

    private void install(Path source, ExceptionBiConsumer<AddonControl, Path, Exception> checkMore)
        throws Exception
    {
        Path home = prepareApplicationHome(withNoContents());
        Path target = home.resolve("mock");
        Map<String, String> env = new HashMap<>();
        String ip = "a.b.c.d";

        mockDefaults(home, env);

        context.checking(new Expectations() {{
            oneOf(container).getIpAddress(with.intIs(anything()));
                will(returnValue(ip));
            oneOf(container).isAddressInUse(ip, 8080);
                will(returnValue(false));
            oneOf(proxy).addMappings(with(container),
                with(aProxyMapping("", "http://" + ip + ":8080", "http")));

            oneOf(container).processTemplates(with(target), with.is(anything()), with(any(boolean.class)));
                will(perform("process template", (Path path, Map<String,String> e, Boolean secure) -> {
                    assertThat(e.get("CLOUDWAY_APP_ID"), is(APP_ID));
                    assertThat(e.get("CLOUDWAY_APP_NAME"), is(APP_NAME));
                    assertThat(e.get("CLOUDWAY_APP_DNS"), is(APP_DNS));
                    assertThat(e.get("CLOUDWAY_APP_TEST"), is(APP_TEST_INFO));
                    assertThat(e.get("CLOUDWAY_HOME_DIR"), is(home.toString()));
                    assertThat(e.get("CLOUDWAY_MOCK_IP"), is(ip));
                    assertThat(e.get("CLOUDWAY_MOCK_PORT"), is("8080"));
                    assertThat(e.get("CLOUDWAY_MOCK_DIR"), is(target.toString()));
                    assertThat(e.get("CLOUDWAY_MOCK_TEST"), is("test info"));
                    assertThat(e.get("CLOUDWAY_FRAMEWORK"), is("mock"));
                    assertThat(e.get("CLOUDWAY_FRAMEWORK_DIR"), is(target.toString()));
                }));

            ignoring(container);
            ignoring(repository);
        }});

        AddonControl control = new DefaultAddonControl(container);
        control.install(source, null);

        assertThat(env.get("CLOUDWAY_MOCK_IP"), is(ip));
        assertThat(env.get("CLOUDWAY_MOCK_PORT"), is("8080"));
        assertThat(env.get("CLOUDWAY_MOCK_DIR"), is(target.toString()));
        assertThat(env.get("CLOUDWAY_FRAMEWORK"), is("mock"));
        assertThat(env.get("CLOUDWAY_FRAMEWORK_DIR"), is(target.toString()));

        checkMore.consume(control, home);
    }

    private final States repo_exists = context.states("repo-exists");

    @Test
    public void install_addon_with_empty_repo() throws Exception {
        Path home = prepareApplicationHome(withNoContents());
        Path source = prepareAddonDir(withNoContents());

        context.checking(new Expectations() {{
            oneOf(repository).populateEmpty();
                then(repo_exists.is("yes"));
        }});

        installWithRepo(home, source, "empty");
    }

    @Test
    public void install_addon_with_template_repo() throws Exception {
        Path home = prepareApplicationHome(withNoContents());
        Path source = prepareAddonDir(withNoContents());

        context.checking(new Expectations() {{
            oneOf(repository).populateFromTemplate(join(home, "mock"));
                then(repo_exists.is("yes"));
        }});

        installWithRepo(home, source, null);
    }

    @Test
    public void install_addon_with_remote_repo() throws Exception {
        Path home = prepareApplicationHome(withNoContents());
        Path source = prepareAddonDir(withNoContents());
        String url = "git://github.com/example.git";

        context.checking(new Expectations() {{
            oneOf(repository).populateFromURL(url);
                then(repo_exists.is("yes"));
        }});

        installWithRepo(home, source, url);
    }

    private void installWithRepo(Path home, Path source, String repo)
        throws Exception
    {
        mockDefaults(home, new HashMap<>());
        context.checking(new Expectations() {{
            allowing(repository).exists();
                when(repo_exists.is("yes"));
                will(returnValue(true));
            oneOf(repository).checkout(with(join(home, "app", "repo")));
                when(repo_exists.is("yes"));

            allowing(container).getIpAddress(with.intIs(anything()));
                will(apply("get ip address", (Integer id) -> "a.b.c." + id));

            ignoring(container);
            ignoring(proxy);
        }});

        AddonControl control = new DefaultAddonControl(container);
        control.install(source, repo);
    }

    @Test(expected = IOException.class)
    public void install_addon_with_invalid_repo() throws Exception {
        Path home = prepareApplicationHome(withNoContents());
        Path source = prepareAddonDir(withNoContents());
        String url = "git://github.com/example.git";

        mockDefaults(home, new HashMap<>());
        context.checking(new Expectations() {{
            allowing(repository).exists();
                will(returnValue(false));
            oneOf(repository).populateFromURL(url);
                will(throwException(new IOException()));

            allowing(container).getIpAddress(with.intIs(anything()));
                will(apply("get ip address", (Integer id) -> "a.b.c." + id));

            ignoring(container);
            ignoring(proxy);
        }});

        AddonControl control = new DefaultAddonControl(container);
        control.install(source, url);
    }

    @Test
    public void allow_create_dot_file_on_home_directory() throws Exception {
        installAddonWithAction(home -> touch(home.resolve(".custom"), 0644));
    }

    @Test
    public void allow_create_dot_directory_on_home_directory() throws Exception {
        installAddonWithAction(home -> mkdir(home.resolve(".custom")));
    }

    @Test(expected = IllegalStateException.class)
    public void disallow_create_garbage_on_home_directory() throws Exception {
        installAddonWithAction(home -> touch(home.resolve("garbage"), 0644));
    }

    @Test
    public void allow_create_addon_specific_environment_variable() throws Exception {
        installAddonWithAction(home -> touch(join(home, "mock", "env", "MOCK_CUSTOM"), 0644));
    }

    @Test(expected = IllegalStateException.class)
    public void disallow_override_container_environment_variable() throws Exception {
        installAddonWithAction(home -> touch(join(home, "mock", "env", "CLOUDWAY_APP_ID"), 0644));
    }

    private void installAddonWithAction(ExceptionConsumer<Path, IOException> action) throws Exception {
        Path home = prepareApplicationHome(withNoContents());
        Path source = prepareAddonDir(withContent("bin/setup", "#exec"));

        mockDefaults(home, new HashMap<>());
        context.checking(new Expectations() {{
            oneOf(container).join(with(anAction("setup")));
                will(new CustomAction("setup addon") {
                    @Override
                    public Object invoke(Invocation inv) throws Throwable {
                        action.consume(home);
                        return inv.getParameter(0);
                    }
                });

            ignoring(container);
            ignoring(repository);
            ignoring(proxy);
        }});

        AddonControl control = new DefaultAddonControl(container);
        control.install(source, null);
    }

    @Test
    public void load_addon() throws Exception {
        String ip = "a.b.c.d";

        Path home = prepareApplicationHome(withNoContents());
        Map<String, String> env = new HashMap<>();

        env.put("CLOUDWAY_MOCK_IP", ip);
        env.put("CLOUDWAY_MOCK_PORT", "8080");

        prepareAddonDir(home.resolve("mock"), withNoContents());
        mockDefaults(home, env);

        AddonControl control = new DefaultAddonControl(container);
        Addon addon = control.addon("mock").get();
        addon.validate();

        assertThat(addon.getProxyMappings(), is(aProxyMapping("", "http://" + ip + ":8080", "http")));
    }

    @Test
    public void remove_addon() throws Exception {
        String ip = "a.b.c.d";

        Path home = prepareApplicationHome(withContents()
            .put("mock/bin/control", "#exec")
            .put("mock/bin/setup", "#exec")
            .put("mock/bin/teardown", "#exec")
            .build());

        Map<String, String> env = new HashMap<>();
        env.put("CLOUDWAY_MOCK_IP", ip);
        env.put("CLOUDWAY_MOCK_PORT", "8080");

        prepareAddonDir(home.resolve("mock"), withNoContents());
        mockDefaults(home, env);

        context.checking(new Expectations() {{
            oneOf(container).join(with(aControlAction("stop")));
                will(execute("stop"));
            oneOf(container).join(with(anAction("teardown")));
                will(execute("teardown"));
            oneOf(proxy).removeMappings(with(container),
                with(aProxyMapping("", "http://" + ip + ":8080", "http")));
            ignoring(container);
        }});

        AddonControl control = new DefaultAddonControl(container);
        control.remove("mock");

        assertTrue(control.addons().isEmpty());
        assertFalse(Files.exists(home.resolve("mock")));
        assertFalse(env.containsKey("CLOUDWAY_MOCK_IP"));
        assertFalse(env.containsKey("CLOUDWAY_MOCK_PORT"));
    }

    @Test(expected = IllegalStateException.class)
    public void dont_remove_application_directory() throws Exception {
        Path home = prepareApplicationHome(withNoContents());
        mockDefaults(home, new HashMap<>());
        AddonControl control = new DefaultAddonControl(container);

        try {
            control.remove("app");
        } finally {
            assertTrue(Files.exists(home.resolve("app")));
        }
    }

    @Test
    public void destroy_all_addons() throws Exception {
        context.checking(new Expectations() {{
            oneOf(container).join(with(aControlAction("stop")));
                will(execute("stop"));
            oneOf(container).join(with(anAction("teardown")));
                will(execute("teardown"));
            oneOf(proxy).purge(container);
        }});

        install(prepareAddonDir(
            withContents()
                .put("bin/control", "#exec")
                .put("bin/teardown", "#exec")
                .build()),
            (control, home) -> control.destroy());
    }

    @Test
    public void run_control_actions() throws Exception {
        context.checking(new Expectations() {{
            oneOf(container).join(with(aControlAction("start")));
                will(execute("start"));
            oneOf(container).join(with(aControlAction("stop")));
                will(execute("stop"));
            oneOf(container).join(with(aControlAction("restart")));
                will(execute("restart"));
            oneOf(container).join(with(aControlAction("tidy")));
                will(execute("tidy"));
            oneOf(container).join(with(aControlAction("custom")));
                will(execute("custom"));
        }});

        install(prepareAddonDir(withContent("bin/control", "#exec")), (control, home) -> {
            control.start();
            control.stop();
            control.restart();
            control.tidy();
            control.control("custom", false, false);
        });
    }

    private Path prepareApplicationHome(Map<String, String> files) throws IOException {
        return prepareDirectory(inTemporaryFolder(), withContents()
            .put(".env/CLOUDWAY_APP_ID",   APP_ID)
            .put(".env/CLOUDWAY_APP_NAME", APP_NAME)
            .put(".env/CLOUDWAY_APP_DNS",  APP_DNS)
            .put("app/repo/", "")
            .put("app/data/", "")
            .put(".tmp/", "")
            .putAll(files).build());
    }

    private Path prepareAddonDir(Map<String, String> withContents) throws Exception {
        return prepareAddonDir(inTemporaryFolder(), withContents);
    }

    private static Path prepareAddonDir(Path dir, Map<String, String> files) throws Exception{
        return prepareAddonDir(dir, new MetaData.Addon() {{
            endpoints = ImmutableList.of(
                new MetaData.Endpoint() {{
                    privateHostName = "IP";
                    privatePortName = "PORT";
                    privatePort = 8080;
                    proxyMappings = ImmutableList.of(
                        new MetaData.ProxyMapping() {{
                            frontend = "/";
                            backend = "/";
                            protocols = "http";
                        }}
                    );
            }});
        }}, files);
    }

    private static Path prepareAddonDir(Path dir, MetaData.Addon metadata, Map<String, String> files)
        throws Exception
    {
        metadata.name         = or(metadata.name, "mock");
        metadata.displayName  = or(metadata.displayName, "Mock");
        metadata.version      = or(metadata.version, "1.0");
        metadata.vendor       = or(metadata.vendor, "cloudway");
        metadata.addonVersion = or(metadata.addonVersion, "1.0");
        metadata.addonVendor  = or(metadata.addonVendor, "cloudway");
        metadata.category     = or(metadata.category, AddonType.FRAMEWORK);

        JAXBContext jaxb = JAXBContext.newInstance(
            MetaData.Addon.class, MetaData.Endpoint.class, MetaData.ProxyMapping.class);
        Marshaller marshaller = jaxb.createMarshaller();
        StringWriter metatext = new StringWriter();
        marshaller.marshal(metadata, metatext);

        return prepareDirectory(dir, withContents()
            .put("metadata/addon.xml", metatext.toString())
            .put("env/CLOUDWAY_MOCK_TEST", "test info")
            .putAll(files).build());
    }

    private static Path prepareDirectory(Path dir, Map<String, String> files) throws IOException {
        IO.forEach(files, (file, text) -> {
            Path path = dir.resolve(file);
            if (file.endsWith("/")) {
                assertTrue(text.isEmpty());
                mkdir(path);
            } else {
                createParentDirs(path);
                writeText(path, text);
                if (text.startsWith("#exec")) {
                    chmod(path, 0755);
                }
            }
        });
        return dir;
    }

    private Path inTemporaryFolder() throws IOException {
        return temporary.newFolder();
    }

    private static Map<String, String> withNoContents() {
        return ImmutableMap.of();
    }

    private static Map<String, String> withContent(String key, String value) {
        return ImmutableMap.of(key, value);
    }

    private static ImmutableMap.Builder<String,String> withContents() {
        return ImmutableMap.builder();
    }

    private static void compareDirectory(Path src, Path dst) throws IOException {
        walkFileTree(src, SimpleFileVisitor.of(path -> {
            if (isRegularFile(path)) {
                Path relativePath = src.relativize(path);
                Path targetPath = dst.resolve(relativePath);
                assertTrue(Files.exists(targetPath));
                assertEquals(readText(path), readText(targetPath));
            }
        }));
    }

    private static Matcher<Collection<ProxyMapping>>
    aProxyMapping(String frontend, String backend, String protocol) {
        ProxyMapping mapping = new ProxyMapping(frontend, backend, protocol);
        return Matchers.matches("has proxy mapping " + mapping,
                                items -> items.contains(mapping));
    }

    private static Matcher<Exec> aControlAction(String action) {
        return Matchers.matches("executing control action", exec -> {
            List<String> args = exec.command();
            return args.size() >= 2
                && args.get(0).endsWith("bin/control")
                && args.get(1).equals(action);
        });
    }

    private static Matcher<Exec> anAction(String action) {
        return Matchers.matches("executing action: " + action, exec -> {
            List<String> args = exec.command();
            return args.size() >= 1
                && args.get(0).endsWith("bin/" + action);
        });
    }

    private static Action execute(String command) {
        return Actions.apply("executing " + command, Function.identity());
    }

    // ----------------------------------------------------------------------

    private static final String FAKE_HOME = "/var/lib/cloudway/test";
    private static final String MOCK_ADDON = "mock";

    private static final Path fack_home = Paths.get(FAKE_HOME);
    private static final Path target = fack_home.resolve(MOCK_ADDON);

    private static String makeRelative(String entry) {
        return DefaultAddonControl.makeRelative(fack_home, target, entry);
    }

    private static Matcher<String> relativeTo(String base, String entry) {
        String path = base + "/" + entry;
        return describedAs("relative to %0", is(path), path);
    }

    private static Matcher<String> relativeToAddon(String entry) {
        return relativeTo(MOCK_ADDON, entry);
    }

    private static Matcher<Object> invalidEntry() {
        return nullValue();
    }

    @Test
    public void testMakeRelative() {
        assertThat(makeRelative(""),          is(nullValue()));
        assertThat(makeRelative("#comment"),  is(nullValue()));

        assertThat(makeRelative("a"),         is(relativeToAddon("a")));
        assertThat(makeRelative("a/"),        is(relativeToAddon("a/")));
        assertThat(makeRelative("a/*"),       is(relativeToAddon("a/*")));
        assertThat(makeRelative("/a"),        is(invalidEntry()));
        assertThat(makeRelative("/a/*"),      is(invalidEntry()));

        assertThat(makeRelative("~/mock/a"),  is(relativeToAddon("a")));
        assertThat(makeRelative("~/app/a"),   is(relativeTo("app", "a")));
        assertThat(makeRelative("~/a"),       is(invalidEntry()));
        assertThat(makeRelative("~/.a"),      is(".a"));
        assertThat(makeRelative("~/.env"),    is(invalidEntry()));
        assertThat(makeRelative("~/.ssh"),    is(invalidEntry()));
        assertThat(makeRelative("~/.tmp"),    is(invalidEntry()));

        assertThat(makeRelative(".a"),        is(relativeToAddon(".a")));
        assertThat(makeRelative("a/."),       is(relativeToAddon("a")));
        assertThat(makeRelative("a/./b"),     is(relativeToAddon("a/b")));
        assertThat(makeRelative("./a"),       is(relativeToAddon("a")));
        assertThat(makeRelative("./a/."),     is(relativeToAddon("a")));
        assertThat(makeRelative("a/.."),      is(MOCK_ADDON));
        assertThat(makeRelative("a/../.."),   is(invalidEntry()));
        assertThat(makeRelative("a/../b"),    is(relativeToAddon("b")));
        assertThat(makeRelative("a/../b/."),  is(relativeToAddon("b")));
        assertThat(makeRelative("a/../b/.."), is(MOCK_ADDON));
        assertThat(makeRelative("../a"),      is(invalidEntry()));
        assertThat(makeRelative("../../a"),   is(invalidEntry()));
        assertThat(makeRelative("../mock"),   is(MOCK_ADDON));
        assertThat(makeRelative("../app"),    is("app"));
        assertThat(makeRelative("../.a"),     is(".a"));
        assertThat(makeRelative("../.env"),   is(invalidEntry()));
    }

    @Test
    public void default_locked_files_include_env_directory() throws IOException {
        Path home = prepareLockedFiles();
        checkLockedFiles(home);
    }

    @Test
    public void allow_lock_directories_in_addon_directory() throws IOException {
        Path home = prepareLockedFiles();
        createLockFile(home, "data/");
        checkLockedFiles(home, "mock/data/");
    }

    @Test
    public void allow_lock_files_and_directories_in_addon_directory() throws IOException {
        Path home = prepareLockedFiles();
        createLockFile(home, "data/", "data/*");
        checkLockedFiles(home, "mock/data/", "mock/data/DATA");
    }

    @Test
    public void wildcard_files_are_expanded() throws IOException {
        Path home = prepareLockedFiles();
        createLockFile(home, "data/*");
        checkLockedFiles(home, "mock/data/DATA");
    }

    @Test
    public void allow_lock_files_and_directories_in_application_directory() throws IOException {
        Path home = prepareLockedFiles();
        createLockFile(home, "~/app/conf/", "~/app/conf/*");
        checkLockedFiles(home, "app/conf/", "app/conf/CONF");
    }

    @Test
    public void allow_create_dot_files_in_home_directory() throws IOException {
        Path home = prepareLockedFiles();
        createLockFile(home, "~/.pref");
        checkLockedFiles(home, ".pref");
    }

    @Test
    public void files_in_other_directories_are_excluded() throws IOException {
        Path home = prepareLockedFiles();
        createLockFile(home, "~/other", "~/other/", "~/other/*");
        checkLockedFiles(home);
    }

    @Test
    public void missing_files_are_included_and_will_be_created() throws IOException {
        Path home = prepareLockedFiles();
        createLockFile(home,   "data/new_file", "data/new_dir/",
                               "new_data/new_file", "new_data/new_dir/");
        checkLockedFiles(home, "mock/data/new_file", "mock/data/new_dir/",
                               "mock/new_data/new_file", "mock/new_data/new_dir/");
    }

    @Test
    public void wildcard_files_cannot_be_created_and_excluded() throws IOException {
        Path home = prepareLockedFiles();
        createLockFile(home, "missing/*", "data/missing/*");
        checkLockedFiles(home);
    }

    private Path prepareLockedFiles() throws IOException {
        Path home  = temporary.newFolder();
        Path addon = mkdir(home.resolve(MOCK_ADDON));
        Path env   = mkdir(addon.resolve("env"));
        Path data  = mkdir(addon.resolve("data"));
        Path app   = mkdir(home.resolve("app"));
        Path conf  = mkdir(app.resolve("conf"));
        Path other = mkdir(home.resolve("other"));

        touch(env.resolve("ENV"), 0644);
        touch(data.resolve("DATA"), 0644);
        touch(conf.resolve("CONF"), 0644);
        touch(other.resolve("OTHER"), 0644);
        touch(home.resolve(".pref"), 0644);

        return home;
    }

    private static void createLockFile(Path home, String... files)
        throws IOException
    {
        Path conf = join(home, MOCK_ADDON, "metadata", "locked_files");
        createParentDirs(conf);
        writeText(conf, String.join("\n", files));
    }

    private static void checkLockedFiles(Path home, String... entries)
        throws IOException
    {
        Collection<String> expected = new ArrayList<>();
        expected.addAll(Arrays.asList(entries));
        expected.addAll(Arrays.asList("mock/env/", "mock/env/ENV"));

        Collection<String> actual = DefaultAddonControl.getLockedFiles(home, home.resolve(MOCK_ADDON));

        actual.forEach(x -> assertThat(expected, hasItem(x)));
        expected.forEach(y -> assertThat(actual, hasItem(y)));
    }
}
