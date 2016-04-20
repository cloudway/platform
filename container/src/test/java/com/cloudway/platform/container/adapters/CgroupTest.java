package com.cloudway.platform.container.adapters;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableTable;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import com.cloudway.fp.data.Maybe;
import com.cloudway.fp.io.IO;
import com.cloudway.platform.common.util.ExtendedProperties;
import com.cloudway.platform.container.Container;
import com.cloudway.platform.container.ResourceLimits;
import com.cloudway.platform.container.TemporaryDirectory;

import static com.cloudway.platform.common.util.MoreFiles.*;

public class CgroupTest {
    public final @Rule TemporaryDirectory temporary = new TemporaryDirectory();

    private static final String APP_USER = Container.makeUUID();
    private static final int APP_UID = 1234;

    private static final String[] TEST_SUBSYSTEMS = { "cpu", "memory", "freezer" };

    private static final Map<String, String> TEST_PARAMETERS = ImmutableMap.of(
        "cpu.shares",           "1024",
        "memory.limits",        "512M",
        "memory.kmem.limits",   "1024M",
        "freezer.state",        "THAWED"
    );

    private Path sysroot() {
        return temporary.getRoot();
    }

    private Path sysfile(String name) {
        assertTrue(name.startsWith("/"));
        return sysroot().resolve(name.substring(1));
    }

    @Before
    public void prepareCgroup() throws IOException {
        mkdir(sysfile("/etc"));
        mkdir(sysfile("/proc"));

        writeText(sysfile("/proc/mounts"),
                  Stream.of(TEST_SUBSYSTEMS)
                        .map(this::cgroup_fs_entry)
                        .collect(Collectors.joining("\n")) +
                        "\ntmpfs /tmp tmpfs rw 0 0");

        IO.forEach(TEST_PARAMETERS, this::write_cgparam);
    }

    private String cgroup_fs_entry(String subsys) {
        try {
            return String.format("cgroup %s cgroup rw,nosuid,nodev,noexec,%s 0 0",
                                 mkdir(root_cgpath(subsys)), subsys);
        } catch (IOException ex) {
            throw new UncheckedIOException(ex);
        }
    }

    private Path root_cgpath(String subsys) {
        return sysfile("/sys/fs/cgroup/" + subsys);
    }

    private Path cgpath(String subsys) {
        return root_cgpath(subsys).resolve(Cgroup.DEFAULT_CG_ROOT);
    }

    private Path cgpath(String subsys, String user) {
        return cgpath(subsys).resolve(user);
    }

    private static String subsys(String key) {
        return key.substring(0, key.indexOf('.'));
    }

    private void write_cgparam(String key, String val) throws IOException {
        Path path = mkdir(cgpath(subsys(key)));
        writeText(path.resolve(key), val);
    }

    private void write_cgparam(String user, String key, String val) throws IOException {
        Path path = mkdir(cgpath(subsys(key), user));
        writeText(path.resolve(key), val);
    }

    private String read_cgparam(String user, String key) throws IOException {
        Path files = cgpath(subsys(key), user).resolve(key);
        if (Files.exists(files)) {
            return readText(files);
        } else {
            return null;
        }
    }

    class MockCgroup extends LinuxCgroup {
        MockCgroup() {
            super(sysroot());
        }

        // create a fake cgroup that behaviors like a real cgroup
        @Override
        protected void create_cgroup(Path path, String user, String subsys) throws IOException {
            if (!Files.exists(path)) {
                mkdir(path, 0755);
                IO.forEach(TEST_PARAMETERS, (k, v) -> {
                    if (k.startsWith(subsys + ".")) {
                        writeText(path.resolve(k), v);
                    }
                });
                touch(path.resolve("tasks"), 0644);
            }
        }

        @Override
        protected void delete_cgroup(Path path, String user, String subsys) throws IOException {
            deleteFileTree(path);
        }
    }

    private Cgroup mockCgroup() {
        return new Cgroup(new MockCgroup(), APP_USER, APP_UID);
    }

    @Test
    public void initialize_cgroup() throws IOException {
        LibCgroup cg = new MockCgroup();
        assertThat(cg.enabled(), is(true));
        IO.forEach(Stream.of(TEST_SUBSYSTEMS), subsys ->
            assertThat(cg.paths().get(subsys), is(cgpath(subsys))));
        assertThat(cg.parameters(), is(TEST_PARAMETERS));
    }

    @Test
    public void fetch_and_store_parameters() throws IOException {
        write_cgparam(APP_USER, "memory.limits", "2048M");
        write_cgparam(APP_USER, "other.value", "foo");

        Cgroup cg = mockCgroup();
        assertThat(cg.fetch("memory.limits"), is(Maybe.of("2048M")));
        assertThat(cg.fetch("other.value"), is(Maybe.empty()));

        Map<String,Object> vals = ImmutableMap.of(
            "memory.limits", "4096M",
            "other.value",   "bar"
        );
        cg.store(vals);
        assertThat(cg.fetch("memory.limits"), is(Maybe.of("4096M")));
        assertThat(cg.fetch("other.value"), is(Maybe.empty()));
    }

    @Test
    public void create_and_delete_cgroup() throws IOException {
        Cgroup cg = mockCgroup();
        testCreate(cg);
        testDelete(cg);
    }

    private void testCreate(Cgroup cg) throws IOException {
        Map<String,Object> params = ImmutableMap.of(
            "memory.limits", "2048M",
            "other.value",   "foo"
        );

        cg.create(params);

        IO.forEach(Stream.of(TEST_SUBSYSTEMS), subsys -> {
            assertTrue(Files.exists(cgpath(subsys, APP_USER)));
        });

        assertThat(read_cgparam(APP_USER, "memory.limits"), is("2048M"));
        assertThat(read_cgparam(APP_USER, "other.value"), is(nullValue()));
        assertThat(cg.fetch("other.value"), is(Maybe.empty()));

        IO.forEach(TEST_PARAMETERS, (k, v) -> {
            Object value = Maybe.firstNonNull(params.get(k), v);
            assertThat(cg.fetch(k), is(Maybe.of(value)));
        });

        check_cgrules(APP_USER);
        check_cgconfig(APP_USER, params);
    }

    private void testDelete(Cgroup cg) throws IOException {
        cg.delete();

        IO.forEach(Stream.of(TEST_SUBSYSTEMS), subsys -> {
            assertFalse(Files.exists(cgpath(subsys, APP_USER)));
        });

        assertThat(cg.fetch("memory.limits"), is(Maybe.empty()));

        check_cgrules(null);
        check_cgconfig(null, null);
    }

    private void check_cgrules(String user) throws IOException {
        Path conf = sysfile("/etc/cgrules.conf");
        if (user == null) {
            assertTrue(!Files.exists(conf) || readText(conf).isEmpty());
        } else {
            String[] elements = readText(conf).split("\\s+");
            assertThat(elements.length, is(3));
            assertThat(elements[0], is(user));
            assertThat(elements[2], is(Cgroup.DEFAULT_CG_ROOT + "/" + user));
        }
    }

    private void check_cgconfig(String user, Map<String,Object> params) throws IOException {
        Path conf = sysfile("/etc/cgconfig.conf");
        if (user == null) {
            assertTrue(!Files.exists(conf) || readText(conf).isEmpty());
        } else {
            String text = readText(conf);
            assertThat(text, startsWith("group " + Cgroup.DEFAULT_CG_ROOT + "/" + user));
            params.forEach((k, v) -> {
                String param = k + " = " + v;
                if (TEST_PARAMETERS.containsKey(k)) {
                    assertThat(text, containsString(param));
                } else {
                    assertThat(text, not(containsString(param)));
                }
            });
        }
    }

    @Test
    public void freeze_and_thaw() throws IOException {
        Cgroup cg = mockCgroup();
        cg.freeze();
        assertThat(cg.fetch("freezer.state"), is(Maybe.of("FROZEN")));
        cg.thaw();
        assertThat(cg.fetch("freezer.state"), is(Maybe.of("THAWED")));
    }

    @Test
    public void classify_processes() throws IOException {
        int[] pids = { 11111, 22222, 33333, 44444 };
        prepareProcesses(APP_UID, pids);

        int[] other_pids = { 55555, 66666 };
        prepareProcesses(APP_UID + 1, other_pids);

        Cgroup cg = mockCgroup();
        cg.create(ImmutableMap.of());

        int[] tasks = cg.tasks();
        Arrays.sort(tasks);
        assertArrayEquals(pids, tasks);
    }

    private void prepareProcesses(int uid, int[] pids) throws IOException {
        IO.forEach(IntStream.of(pids).mapToObj(String::valueOf), pid -> {
            Path pid_file = mkdir(sysfile("/proc").resolve(pid));
            Path status = pid_file.resolve("status");
            writeText(status, String.join("\n",
                "Name:  pid-" + pid,
                "State: R (running)",
                "Pid:   " + pid,
                "Uid:   " + uid,
                "Gid:   " + uid));

            Path tasks = mkdir(pid_file.resolve("task"));
            Files.createSymbolicLink(tasks.resolve(pid), pid_file);
        });
    }

    @Test
    public void test_cgroup_profiles() throws IOException {
        ResourceLimits limits = new ResourceLimits(new ExtendedProperties(
            ImmutableMap.<String, String>builder()
                .put("cgroup.memory.limits", "default memory limits")
                .build(),
            ImmutableTable.<String, String, String>builder()
                .put("small", "cgroup.cpu.shares", "small cpu shares")
                .put("medium", "cgroup.memory.limits", "medium memory limits")
                .put("large", "other.profile", "other profile value")
                .build(),
            null));

        ImmutableTable<String, String, Object> profiles = new MockCgroup().load_profiles(limits);

        assertTrue(profiles.rowKeySet().containsAll(Arrays.asList("small", "medium", "large")));
        assertTrue(profiles.columnKeySet().containsAll(Arrays.asList("memory.limits", "cpu.shares", "freezer.state")));

        assertThat(profiles.get("small", "memory.limits"), is("default memory limits"));
        assertThat(profiles.get("small", "cpu.shares"), is("small cpu shares"));
        assertThat(profiles.get("small", "freezer.state"), is("THAWED"));

        assertThat(profiles.get("medium", "memory.limits"), is("medium memory limits"));
        assertThat(profiles.get("medium", "cpu.shares"), is("1024"));
        assertThat(profiles.get("medium", "freezer.state"), is("THAWED"));

        assertThat(profiles.get("large", "memory.limits"), is("default memory limits"));
        assertThat(profiles.get("large", "cpu.shares"), is("1024"));
        assertThat(profiles.get("large", "freezer.state"), is("THAWED"));
    }
}
