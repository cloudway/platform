/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container;

import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Base64;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Random;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.hamcrest.Description;
import org.hamcrest.Matcher;
import org.hamcrest.TypeSafeMatcher;
import org.jmock.Expectations;
import org.jmock.States;
import org.jmock.integration.junit4.JUnitRuleMockery;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import com.cloudway.fp.data.Maybe;
import com.cloudway.fp.data.IntRef;
import com.cloudway.fp.control.Predicates;
import com.cloudway.fp.function.ExceptionConsumer;
import com.cloudway.fp.io.IO;
import com.cloudway.platform.common.os.AuthorizedKey;
import com.cloudway.platform.common.os.Configuration;
import com.cloudway.platform.common.os.Etc;
import com.cloudway.platform.common.os.Etc.PASSWD;
import com.cloudway.platform.common.os.Posix;
import com.cloudway.platform.common.os.Config;
import com.cloudway.platform.container.proxy.HttpProxy;
import com.cloudway.platform.container.proxy.HttpProxyUpdater;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;

import static com.cloudway.platform.common.util.MoreFiles.*;
import static com.cloudway.platform.common.os.Config.GECOS;
import static com.cloudway.platform.common.os.Config.DOMAIN;
import static com.cloudway.platform.container.ApplicationContainer.makeUUID;
import static com.cloudway.platform.container.ApplicationState.*;
import static com.cloudway.platform.container.Actions.*;

public class ApplicationContainerTest
{
    public final @Rule JUnitRuleMockery context = new JUnitRuleMockery();
    public final @Rule TemporaryDirectory temporary = new TemporaryDirectory();

    private final Posix posix = context.mock(Posix.class);
    private final ContainerAdapter adapter = context.mock(ContainerAdapter.class);
    private final AddonControl addons = context.mock(AddonControl.class);
    private final ApplicationRepository repository = context.mock(ApplicationRepository.class);
    private final HttpProxyUpdater proxy = context.mock(HttpProxyUpdater.class);

    private final UserDatabase userDatabase = new UserDatabase(posix);
    private final FilePermissions filePermissions = new FilePermissions();

    @Before
    public void setUp() throws Exception {
        configure();
        prepareUserDatabase();
        mockObjects();
    }

    @After
    public void tearDown() throws Exception {
        unmockObjects();
    }

    private void configure() {
        Configuration configuration = context.mock(Configuration.class);
        Configuration.Provider provider = context.mock(Configuration.Provider.class);
        context.checking(new Expectations() {{
            allowing(configuration).getProperty(Config.VAR_DIR_KEY);
                will(returnValue(Maybe.of(temporary.getRoot().toString())));
            allowing(configuration).getProperty(with(any(String.class)));
                will(returnValue(Maybe.empty()));
            allowing(provider).load("container.conf");
                will(returnValue(configuration));
        }});
        Config.setProvider(provider);
    }

    private void mockObjects() {
        Etc.setPosixProvider(() -> posix);
        ContainerAdapter.setFactory(this::makeFakeContainerAdapter);
        ApplicationRepository.setFactory(c -> repository);
        HttpProxy.setProvider(() -> proxy);
    }

    private void unmockObjects() {
        Config.setProvider(null);
        Etc.setPosixProvider(null);
        ContainerAdapter.setFactory(null);
        ApplicationRepository.setFactory(null);
        HttpProxy.setProvider(null);
    }

    private ContainerAdapter makeFakeContainerAdapter(ApplicationContainer c) {
        return new FakeContainerAdapter(c, adapter, userDatabase, filePermissions);
    }

    private static DefaultApplicationContainer createContainer(String id) throws IOException {
        return DefaultApplicationContainer.create(id, "mock", "demo", "small");
    }

    private static DefaultApplicationContainer createContainer() throws IOException {
        return createContainer(makeUUID());
    }

    private ApplicationContainer createMockContainer(String id) throws IOException {
        DefaultApplicationContainer ac = createContainer(id);
        ac.setAddonControl(addons);
        return ac;
    }

    private ApplicationContainer createMockContainer() throws IOException {
        return createMockContainer(makeUUID());
    }

    private void prepareUserDatabase() throws IOException {
        context.checking(userDatabase);

        nativeUser("root",     0,    "root",        "/root");
        nativeUser("alice",    1000, "alice",       createHomeDir("alice").toString());
        nativeUser("homeless", 1001,  GECOS.get(),  "/home_should_not_exist");
        createUser("normal",   1002, "normal",      "demo");
        createUser("other",    1003, "other",       "demo");
    }

    private void nativeUser(String id, int uid, String gecos, String home) {
        userDatabase.add(UserDatabase.mockPASSWD(id, uid, gecos, home));
    }

    private PASSWD createUser(String id, int uid, String name, String namespace, Path home)
        throws IOException
    {
        Path env = mkdir(home.resolve(".env"));
        writeText(env.resolve("CLOUDWAY_APP_ID"), id);
        writeText(env.resolve("CLOUDWAY_APP_NAME"), name);
        writeText(env.resolve("CLOUDWAY_APP_DNS"), name + "-" + namespace + "." + DOMAIN);
        writeText(env.resolve("CLOUDWAY_APP_SIZE"), "small");
        writeText(env.resolve("TEST"), "test info");

        PASSWD user = UserDatabase.mockPASSWD(id, uid, GECOS.get(), home.toString());
        userDatabase.add(user);
        return user;
    }

    private PASSWD createUser(String id, int uid, String name, String namespace) throws IOException {
        return createUser(id, uid, name, namespace, createHomeDir(id));
    }

    private PASSWD createUser(String name, String namespace) throws IOException {
        String id   = makeUUID();
        int    uid  = userDatabase.getNextUid();
        Path   home = createHomeDir(id);
        return createUser(id, uid, name, namespace, home);
    }

    private static Path createHomeDir(String id) throws IOException {
        return mkdir(Config.VAR_DIR.resolve(id), 0750);
    }

    // -----------------------------------------------------------------------

    @Test
    public void container_user_must_have_a_well_known_gecos() {
        assertThat("alice", seeNothing());
    }

    @Test
    public void container_user_must_have_a_home_directory() {
        assertThat("homeless", seeNothing());
    }

    @Test
    public void normal_user_see_its_own_container() {
        assertThat("normal", see("normal"));
        assertThat("other", see("other"));
    }

    @Test
    public void root_user_see_all_containers() {
        assertThat("root", see("normal", "other"));
    }

    private Matcher<String> see(String... items) {
        return new See(items);
    }

    private Matcher<String> seeNothing() {
        return see();
    }

    private class See extends TypeSafeMatcher<String> {
        private final String[] items;
        private Collection<String> ids;

        public See(String[] items) {
            this.items = items.clone();
        }

        @Override
        protected boolean matchesSafely(String user) {
            userDatabase.setCurrentUser(user);
            ids = ApplicationContainer.ids();
            return ids.size() == items.length
                && ids.containsAll(Arrays.asList(items));
        }

        @Override
        public void describeTo(Description description) {
            description.appendText(
                items.length == 0 ? "has no items"
                                  : "has items: [" + String.join(", ", items) + "]");
            description.appendText(" but got " + ids);
        }
    }

    // -----------------------------------------------------------------------

    @Test
    public void load_container_from_id() throws IOException {
        String id   = makeUUID();
        int    uid  = userDatabase.getNextUid();
        Path   home = createHomeDir(id);
        PASSWD user = createUser(id, uid, "mock", "demo", home);

        ApplicationContainer ac = ApplicationContainer.fromId(id);
        assertThat(ac.getId(), is(id));
        assertThat(ac.getName(), is("mock"));
        assertThat(ac.getNamespace(), is("demo"));
        assertThat(ac.getDomainName(), is("mock-demo." + DOMAIN));
        assertThat(ac.getCapacity(), is("small"));
        assertThat(ac.getUID(), is(user.pw_uid));
        assertThat(ac.getGID(), is(user.pw_gid));
        assertThat(ac.getShell(), is(user.pw_shell));
        assertThat(ac.getHomeDir(), is(home));
        assertThat(ac.environ().get("TEST"), is("test info"));
    }

    @Test(expected = NoSuchContainerException.class)
    public void load_container_from_invalid_id() throws IOException {
         ApplicationContainer.fromId("alice");
    }

    @Test(expected = NoSuchContainerException.class)
    public void load_container_from_other_user() throws IOException {
        userDatabase.setCurrentUser("normal");
        ApplicationContainer.fromId("other");
    }

    @Test
    public void load_all_containers_by_normal_user() {
        userDatabase.setCurrentUser("normal");
        checkContainers("normal");
    }

    @Test
    public void load_all_containers_by_native_user() {
        userDatabase.setCurrentUser("alice");
        checkContainers();
    }

    @Test
    public void load_all_containers_by_root_user() throws IOException {
        userDatabase.setCurrentUser("root");
        createUser("mock", "demo");
        checkContainers("normal", "other", "mock");
    }

    private void checkContainers(String... names) {
        IntRef count = new IntRef(0);
        ApplicationContainer.all().forEach(ac -> {
            assertThat(Arrays.asList(names), hasItem(ac.getName()));
            count.increment();
            checkContainer(ac);
        });
        assertThat(count.get(), is(names.length));
    }

    private void checkContainer(ApplicationContainer ac) {
        PASSWD pw = userDatabase.getpwname(ac.getId());
        assertNotNull(pw);

        assertThat(pw.pw_name,  is(ac.getId()));
        assertThat(pw.pw_uid,   is(ac.getUID()));
        assertThat(pw.pw_gid,   is(ac.getGID()));
        assertThat(pw.pw_dir,   is(ac.getHomeDir().toString()));
        assertThat(pw.pw_shell, is(ac.getShell()));
        assertThat(pw.pw_gecos, is(GECOS.get()));
    }

    // -----------------------------------------------------------------------

    @Test
    public void create_application_container() throws IOException {
        String id = makeUUID();
        ApplicationContainer ac = createContainer(id);

        checkContainer(ac);
        assertThat(ac.getState(), is(NEW));

        Path home = ac.getHomeDir();
        assertThat(ac.getAppDir(), is(home.resolve("app")));
        assertThat(ac.getRepoDir(), is(home.resolve("app").resolve("repo")));
        assertThat(ac.getDataDir(), is(home.resolve("app").resolve("data")));
        assertThat(ac.getEnvDir(), is(home.resolve(".env")));

        assertThat(ac.getAppDir(), is(readOnly()));
        assertThat(ac.getRepoDir(), is(readWrite()));
        assertThat(ac.getDataDir(), is(readWrite()));
        assertThat(ac.getEnvDir(), is(readOnly()));
        assertThat(ac.getEnvDir().resolve("CLOUDWAY_APP_ID"), is(readOnly()));
        assertThat(ac.getHomeDir().resolve(".ssh"), is(readOnly()));

        checkEnvVar(ac, "CLOUDWAY_APP_ID", id);
        checkEnvVar(ac, "CLOUDWAY_APP_NAME", "mock");
        checkEnvVar(ac, "CLOUDWAY_APP_DNS", "mock-demo." + DOMAIN);
        checkEnvVar(ac, "CLOUDWAY_HOME_DIR", ac.getHomeDir().toString());
        checkEnvVar(ac, "CLOUDWAY_REPO_DIR", ac.getRepoDir().toString());
        checkEnvVar(ac, "CLOUDWAY_DATA_DIR", ac.getDataDir().toString());

        ac.setenv("TEST", "test info");
        checkEnvVar(ac, "TEST", "test info");

        ac.unsetenv("TEST");
        checkEnvVar(ac, "TEST", null);
    }

    @Test(expected = IllegalStateException.class)
    public void disallow_create_application_containers_with_duplicate_id() throws IOException {
        String id = makeUUID();
        ApplicationContainer.create(id, "mock", "demo", "small");
        ApplicationContainer.create(id, "mock", "demo", "small");
    }

    @Test
    public void allow_create_application_containers_with_same_name_for_scaling() throws IOException {
        ApplicationContainer.create(makeUUID(), "mock", "demo", "small");
        ApplicationContainer.create(makeUUID(), "mock", "demo", "small");
    }

    @Test
    public void disallow_create_application_container_with_invalid_arguments() throws IOException {
        String id = makeUUID();
        failedToCreateContainer(null, "foo", "bar");
        failedToCreateContainer(id, null, "bar");
        failedToCreateContainer(id, "foo", null);
        failedToCreateContainer("", "foo", "bar");
        failedToCreateContainer(id, "", "bar");
        failedToCreateContainer(id, "foo", "");
        failedToCreateContainer("aaa-bbb", "foo", "bar");
        failedToCreateContainer(id, "foo-bar", "bar");
        failedToCreateContainer(id, "foo", "foo-bar");
    }

    @Test
    public void destroy_application_container() throws IOException {
        String id = makeUUID();
        ApplicationContainer ac = createMockContainer(id);
        Path home = ac.getHomeDir();

        context.checking(new Expectations() {{
            oneOf(addons).destroy();
            oneOf(adapter).destroy();
        }});

        ac.destroy();

        assertFalse(ApplicationContainer.exists(id));
        assertFalse(userDatabase.getpwname(id) != null);
        assertFalse(Files.exists(home));
    }

    private static void checkEnvVar(ApplicationContainer ac, String key, String value)
        throws IOException
    {
        if (value != null) {
            assertThat(ac.getenv(key), is(value));
            assertThat(ac.environ().get(key), is(value));
            assertThat(readText(ac.getEnvDir().resolve(key)), is(value));
        } else {
            assertThat(ac.getenv(key), is(nullValue()));
            assertThat(ac.environ().get(key), is(nullValue()));
            assertFalse(Files.exists(ac.getEnvDir().resolve(key)));
        }
    }

    private static void failedToCreateContainer(String id, String name, String namespace) throws IOException {
        try {
            ApplicationContainer.create(id, name, namespace, "small");
            fail("created application container with invalid arguments");
        } catch (RuntimeException ex) {
            // ok
        }
    }

    private Matcher<Path> readOnly() {
        return Matchers.matches("readonly",
            Predicates.both(Files::exists, filePermissions::isReadOnly));
    }

    private Matcher<Path> readWrite() {
        return Matchers.matches("read write",
            Predicates.both(Files::exists, filePermissions::isReadWrite));
    }

    // -----------------------------------------------------------------------

    @Test
    public void add_authorized_key() throws IOException {
        String id   = "test";
        String type = "ssh-rsa";
        String bits = makeKeyBits();
        String key  = type + " " + bits + " info";

        ApplicationContainer ac = createMockContainer();
        ac.addAuthorizedKey(id, key);
        assertTrue(readText(join(ac.getHomeDir(), ".ssh", "authorized_keys")).contains(bits));

        List<String> aks = ac.getAuthorizedKeys();
        assertThat(aks.size(), is(1));
        assertTrue(aks.get(0).contains(bits));

        AuthorizedKey ak = AuthorizedKey.parsePublicKey(aks.get(0));
        assertThat(ak.getType(), is(type));
        assertThat(ak.getBits(), is(bits));
        assertThat(ak.getComment().get(), is(id));

        ac.removeAuthorizedKey(key);
        assertTrue(ac.getAuthorizedKeys().isEmpty());
    }

    @Test(expected = RuntimeException.class)
    public void add_duplicate_authorized_key() throws IOException {
        String key = "ssh-rsa " + makeKeyBits() + " info";

        ApplicationContainer ac = createMockContainer();
        ac.addAuthorizedKey("a", key);
        ac.addAuthorizedKey("b", key);
    }

    @Test(expected = RuntimeException.class)
    public void add_authorized_key_with_duplicate_id() throws IOException {
        String key1 = "ssh-rsa" + makeKeyBits() + " info";
        String key2 = "ssh-rsa" + makeKeyBits() + " info";

        ApplicationContainer ac = createMockContainer();
        ac.addAuthorizedKey("a", key1);
        ac.addAuthorizedKey("a", key2);
    }

    private static String makeKeyBits() {
        byte[] random = new byte[1024];
        new Random().nextBytes(random);
        return Base64.getEncoder().encodeToString(random);
    }

    // -----------------------------------------------------------------------

    // @formatter:off
    private class ContainerStateMachine {
        private final ApplicationContainer container;

        public ContainerStateMachine(ApplicationContainer container) {
            this.container = container;
        }

        private void allow_adapter_start() throws IOException {
            context.checking(new Expectations() {{
                oneOf(adapter).start();
            }});
        }

        private void allow_adapter_stop() throws IOException {
            context.checking(new Expectations() {{
                oneOf(adapter).stop(with(any(long.class)), with.is(anything()));
            }});
        }

        private void allow_addons_start() throws IOException {
            context.checking(new Expectations() {{
                oneOf(addons).start();
            }});
        }

        private void allow_addons_stop() throws IOException {
            context.checking(new Expectations() {{
                oneOf(addons).stop();
            }});
        }

        private void allow_addons_restart() throws IOException {
            context.checking(new Expectations() {{
                oneOf(addons).restart();
            }});
        }

        private void allow_proxy_idle() throws IOException {
            context.checking(new Expectations() {{
                oneOf(proxy).idle(container);
            }});
        }

        private void allow_proxy_unidle(boolean success) throws IOException {
            context.checking(new Expectations() {{
                oneOf(proxy).unidle(container);
                    will(returnValue(success));
            }});
        }

        public ContainerStateMachine start() throws IOException {
            ApplicationState state = container.getState();

            if (state != STARTED)
                allow_adapter_start();
            if (state == IDLE)
                allow_proxy_unidle(true);
            allow_addons_start();

            container.start();
            expect(STARTED);
            return this;
        }

        public ContainerStateMachine restart() throws IOException {
            ApplicationState state = container.getState();

            if (state == STARTED) {
                allow_addons_restart();
            } else {
                allow_adapter_start();
                allow_addons_start();
            }
            if (state == IDLE) {
                allow_proxy_unidle(true);
            }

            container.restart();
            expect(STARTED);
            return this;
        }

        public ContainerStateMachine stop() throws IOException {
            allow_addons_stop();
            allow_adapter_stop();

            container.stop();
            expect(STOPPED);
            return this;
        }

        public ContainerStateMachine idle() throws IOException {
            ApplicationState state = container.getState();

            if (state == STARTED) {
                allow_addons_stop();
                allow_adapter_stop();
                allow_proxy_idle();
            }

            container.idle();
            expect(state == STARTED ? IDLE : state);
            return this;
        }

        public ContainerStateMachine unidle() throws IOException {
            ApplicationState state = container.getState();

            if (state == IDLE) {
                allow_proxy_unidle(true);
                allow_adapter_start();
                allow_addons_start();
            } else {
                allow_proxy_unidle(false);
            }

            container.unidle();
            expect(state == IDLE ? STARTED : state);
            return this;
        }

        public ContainerStateMachine tidy() throws IOException {
            ApplicationState state = container.getState();

            context.checking(new Expectations() {{
                if (state == STARTED) {
                    States state = context.states("addons").startsAs("init");
                    oneOf(addons).stop();
                        when(state.is("init"));
                        then(state.is("stopped"));
                    oneOf(addons).tidy();
                        when(state.is("stopped"));
                    oneOf(repository).tidy();
                        when(state.is("stopped"));
                    oneOf(addons).start();
                        when(state.is("stopped"));
                        then(state.is("started"));
                } else {
                    oneOf(addons).tidy();
                    oneOf(repository).tidy();
                }
            }});

            container.tidy();
            expect(state);
            return this;
        }

        public ContainerStateMachine post_receive() throws IOException {
            ApplicationState state = container.getState();

            context.checking(new Expectations() {{
                if (state == STARTED) {
                    States state = context.states("addons").startsAs("init");
                    oneOf(addons).stop();
                        when(state.is("init"));
                        then(state.is("stopped"));
                    oneOf(repository).checkout(container.getRepoDir());
                        when(state.is("stopped"));
                    oneOf(addons).start();
                        when(state.is("stopped"));
                        then(state.is("started"));
                } else {
                    oneOf(repository).checkout(container.getRepoDir());
                }
            }});

            container.post_receive();
            expect(state);
            return this;
        }

        private void expect(ApplicationState state) {
            assertThat(container.getState(), is(state));
        }
    }
    // @formatter:on

    private ContainerStateMachine forContainerStates() throws IOException {
        return new ContainerStateMachine(createMockContainer());
    }

    @Test
    public void start_and_stop_container() throws IOException {
        forContainerStates().start().stop();
    }

    @Test
    public void idle_and_unidle_container() throws IOException {
        forContainerStates().start().idle().unidle();
    }

    @Test
    public void unidle_started_container() throws IOException {
        forContainerStates().start().unidle();
    }

    @Test
    public void unidle_stopped_container() throws IOException {
        forContainerStates().stop().unidle();
    }

    @Test
    public void stop_idled_container() throws IOException {
        forContainerStates().start().idle().stop();
    }

    @Test
    public void stop_stopped_container() throws IOException {
        forContainerStates().start().stop().stop();
    }

    @Test
    public void idle_stopped_container() throws IOException {
        forContainerStates().stop().idle();
    }

    @Test
    public void idle_idled_container() throws IOException {
        forContainerStates().start().idle().idle();
    }

    @Test
    public void tidy_application() throws IOException {
        forContainerStates().start().tidy();
        forContainerStates().start().stop().tidy();
    }

    @Test
    public void git_post_receive() throws IOException {
        forContainerStates().start().post_receive();
        forContainerStates().start().stop().post_receive();
    }

    @Test
    public void random_container_actions() throws IOException {
        List<ExceptionConsumer<ContainerStateMachine, IOException>> actions = ImmutableList.of(
            ContainerStateMachine::start,
            ContainerStateMachine::stop,
            ContainerStateMachine::restart,
            ContainerStateMachine::idle,
            ContainerStateMachine::unidle
        );

        Random rnd = new Random();
        IO.forEach(IntStream.range(0, 10).boxed(), i -> {
            ContainerStateMachine machine = forContainerStates();
            IO.forEach(IntStream.range(0, 10).map(j -> rnd.nextInt(actions.size())).mapToObj(actions::get),
                       action -> action.consume(machine));
        });
    }

    // -----------------------------------------------------------------------

    @Test
    public void install_addon_with_empty_repo() throws Exception {
        ApplicationContainer container = createContainer();
        Path source = prepareAddonDir(withNoContents());

        context.checking(new Expectations() {{
            oneOf(repository).populateEmpty();
        }});

        installAddonWithRepo(container, source, "empty");
    }

    @Test
    public void install_addon_with_template_repo() throws Exception {
        ApplicationContainer container = createContainer();
        Path source = prepareAddonDir(withNoContents());

        context.checking(new Expectations() {{
            oneOf(repository).populateFromTemplate(join(container.getHomeDir(), "mock"));
        }});

        installAddonWithRepo(container, source, null);
    }

    @Test
    public void install_addon_with_external_repo() throws Exception {
        ApplicationContainer container = createContainer();
        Path source = prepareAddonDir(withNoContents());
        String repo = "git://github.com/example.git";

        context.checking(new Expectations() {{
            oneOf(repository).populateFromURL(repo);
        }});

        installAddonWithRepo(container, source, repo);
    }

    @Test
    public void process_template() throws Exception {
        String template = String.join("\n",
            "id=${CLOUDWAY_APP_ID}",
            "ip=${CLOUDWAY_MOCK_IP}",
            "#set($port = $CLOUDWAY_MOCK_PORT.intValue()+1)",
            "port=${port}",
            "test=${MOCK_TEST}",
            "alt=$alt.of($SOME_VAR, \"default\")",
            "$alt.insert(\"info=%s\", $MOCK_TEST)"
        );

        ApplicationContainer container = createContainer();
        Path source = prepareAddonDir(withContents()
            .put("metadata/locked_files", "locked/*")
            .put("env/MOCK_TEST", "test info")
            .put("env/CLOUDWAY_MOCK_LOG_DIR.cwt", "${CLOUDWAY_MOCK_DIR}/logs")
            .put("config.cwt", template)
            .put("locked/config.cwt", template)
            .build());

        context.checking(new Expectations() {{
            oneOf(repository).populateEmpty();
        }});

        String ip = installAddonWithRepo(container, source, "empty");
        Path home = container.getHomeDir();
        Path target = home.resolve("mock");
        Map<String,String> env = container.environ();

        assertTrue(Files.exists(target.resolve("config")));
        assertTrue(Files.exists(target.resolve("config.cwt")));
        assertTrue(Files.exists(target.resolve("locked").resolve("config")));
        assertFalse(Files.exists(target.resolve("locked").resolve("config.cwt")));
        assertTrue(Files.exists(target.resolve("env").resolve("CLOUDWAY_MOCK_LOG_DIR")));
        assertFalse(Files.exists(target.resolve("env").resolve("CLOUDWAY_MOCK_LOG_DIR.cwt")));

        assertThat(env.get("CLOUDWAY_MOCK_LOG_DIR"), is(target.resolve("logs").toString()));

        IO.forEach(Stream.of("config", "locked/config").map(target::resolve), conf -> {
            Properties p = new Properties();
            try (InputStream ins = Files.newInputStream(conf)) {
                p.load(ins);
            }

            assertThat(p.getProperty("id"), is(container.getId()));
            assertThat(p.getProperty("ip"), is(ip));
            assertThat(p.getProperty("port"), is("8081"));
            assertThat(p.getProperty("test"), is("test info"));
            assertThat(p.getProperty("alt"), is("default"));
            assertThat(p.getProperty("info"), is("test info"));
        });
    }

    private String installAddonWithRepo(ApplicationContainer container, Path source, String repo)
        throws IOException
    {
        IntRef hostId = new IntRef();

        context.checking(new Expectations() {{
            allowing(repository).exists(); will(returnValue(true));
            oneOf(repository).checkout(container.getRepoDir());
            atLeast(1).of(adapter).getIpAddress(with(any(int.class)));
                will(perform("get IP address", hostId::set));
            oneOf(proxy).addMappings(with(container), with.is(anything()));
        }});

        container.install(source, repo);

        Path home = container.getHomeDir();
        Path target = home.resolve("mock");
        Map<String,String> env = container.environ();
        String ip = container.getIpAddress(hostId.get());

        assertTrue(Files.isDirectory(target));
        assertThat(env.get("CLOUDWAY_MOCK_IP"), is(ip));
        assertThat(env.get("CLOUDWAY_MOCK_PORT"), is("8080"));
        assertThat(env.get("CLOUDWAY_MOCK_DIR"), is(target.toString()));

        List<Addon.Endpoint> eps = container.getEndpoints();
        assertThat(eps.size(), is(1));
        Addon.Endpoint ep = eps.get(0);
        assertThat(ep.getPrivateHostName(), is("CLOUDWAY_MOCK_IP"));
        assertThat(ep.getPrivatePortName(), is("CLOUDWAY_MOCK_PORT"));
        assertThat(ep.getPrivateHost(), is(ip));
        assertThat(ep.getPrivatePort(), is(8080));

        return ip;
    }

    private Path prepareAddonDir(Map<String, String> files) throws Exception{
        return prepareDirectory(temporary.newFolder(), withContents()
            .put("metadata/addon.xml", createAddonXml())
            .put("bin/control", "#exec")
            .put("bin/setup", "#exec")
            .put("bin/teardown", "#exec")
            .putAll(files).build());
    }

    private static String createAddonXml() throws JAXBException {
        MetaData.Addon metadata = new MetaData.Addon() {{
            name         = "mock";
            displayName  = "Mock";
            version      = "1.0";
            vendor       = "cloudway";
            addonVersion = "1.0";
            addonVendor  = "cloudway";
            category     = AddonType.FRAMEWORK;
            endpoints    = ImmutableList.of(new MetaData.Endpoint() {{
                privateHostName = "IP";
                privatePortName = "PORT";
                privatePort = 8080;
                proxyMappings = ImmutableList.of(new MetaData.ProxyMapping() {{
                    frontend = "/";
                    backend = "/";
                    protocols = "http";
                }});
            }});
        }};

        JAXBContext jaxb = JAXBContext.newInstance(
            MetaData.Addon.class, MetaData.Endpoint.class, MetaData.ProxyMapping.class);
        Marshaller marshaller = jaxb.createMarshaller();
        StringWriter text = new StringWriter();
        marshaller.marshal(metadata, text);
        return text.toString();
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

    private static Map<String, String> withNoContents() {
        return ImmutableMap.of();
    }

    private static ImmutableMap.Builder<String,String> withContents() {
        return ImmutableMap.builder();
    }
}
