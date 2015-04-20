package com.cloudway.platform.container;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Map;
import java.util.stream.Stream;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import org.jmock.Expectations;
import org.jmock.integration.junit4.JUnitRuleMockery;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import com.cloudway.platform.common.fp.data.Maybe;
import com.cloudway.platform.common.os.Configuration;
import com.cloudway.platform.container.proxy.HttpProxy;
import com.cloudway.platform.common.os.Config;
import com.cloudway.platform.container.proxy.ProxyMapping;
import com.cloudway.platform.container.proxy.apache.ApacheProxyUpdater;

import static com.cloudway.platform.common.util.MoreCollectors.*;
import static java.nio.file.Files.*;

public class ApacheProxyUpdaterTest
{
    public final @Rule JUnitRuleMockery context = new JUnitRuleMockery();
    public final @Rule TemporaryDirectory temporary = new TemporaryDirectory();

    private final ApplicationContainer container = context.mock(ApplicationContainer.class);

    private static final String APP_ID = ApplicationContainer.makeUUID();
    private static final String APP_DNS = "mock-demo.example.com";

    private Path container_map, mapping_map, alias_map, idle_map;

    @Before
    public void configure() {
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

        Path basedir = Config.VAR_DIR.resolve(".httpd");
        container_map = basedir.resolve("containers.txt");
        mapping_map = basedir.resolve("mappings.txt");
        alias_map = basedir.resolve("aliases.txt");
        idle_map = basedir.resolve("idles.txt");
    }

    @Before
    public void setUp() throws IOException {
        HttpProxy.setProvider(() -> ApacheProxyUpdater.INSTANCE);
        context.checking(new Expectations() {{
            allowing(container).getId();
                will(returnValue(APP_ID));
            allowing(container).getDomainName();
                will(returnValue(APP_DNS));
        }});
    }

    @Test
    public void add_proxy_mappings() throws IOException {
        ProxyMapping pm1 = new ProxyMapping("", "http://127.0.0.1:8080", "http");
        ProxyMapping pm2 = new ProxyMapping("/admin", "http://127.0.0.1:8080/admin", "https");

        addProxyMappings(pm1);
        checkContainerMap();
        checkProxyMappings(pm1);

        addProxyMappings(pm1, pm2); // add duplicate mapping is ok
        checkContainerMap();
        checkProxyMappings(pm1, pm2);
    }

    @Test
    public void add_proxy_mappings_with_preferred_order() throws IOException {
        ProxyMapping ajp  = new ProxyMapping("", "ajp://127.0.0.1:8080", "ajp");
        ProxyMapping http = new ProxyMapping("", "http://127.0.0.1:8080", "http");

        addProxyMappings(ajp, http);
        checkContainerMap();
        checkProxyMappings(ajp);    // ajp get preferred

        HttpProxy.purge(container); // reset mappings

        addProxyMappings(http, ajp);
        checkContainerMap();
        checkProxyMappings(http);   // http get preferred
    }

    @Test
    public void add_proxy_mapping_with_unsupported_protocol() throws IOException {
        addProxyMappings(new ProxyMapping("", "http://127.0.0.1:8080", "unknown"));
        checkProxyMappings(/*empty*/);
    }

    @Test
    public void remove_proxy_mappings() throws IOException {
        ProxyMapping pm1 = new ProxyMapping("", "http://127.0.0.1:8080", "http");
        ProxyMapping pm2 = new ProxyMapping("/admin", "http://127.0.0.1:8080/admin", "https");

        addProxyMappings(pm1, pm2);
        removeProxyMappings(pm2);
        checkContainerMap();
        checkProxyMappings(pm1);
    }

    @Test
    public void add_and_remove_alias() throws IOException {
        String name = "demo";
        String fqdn = "http://demo.example.com";

        HttpProxy.addAlias(name, fqdn);
        checkMap(alias_map, ImmutableMap.of(name, fqdn));

        HttpProxy.removeAlias(name);
        checkMap(alias_map, ImmutableMap.of());
    }

    @Test
    public void idle_and_unidle_container() throws IOException {
        HttpProxy.idle(container);
        assertTrue(HttpProxy.isIdle(container));
        assertTrue(loadMap(idle_map).containsKey(APP_ID));

        HttpProxy.unidle(container);
        assertFalse(HttpProxy.isIdle(container));
        assertFalse(loadMap(idle_map).containsKey(APP_ID));
    }

    @Test
    public void purge_container() throws IOException {
        ProxyMapping pm = new ProxyMapping("", "http://127.0.0.1:8080", "http");
        HttpProxy.addMappings(container, ImmutableList.of(pm));
        HttpProxy.idle(container);

        checkProxyMappings(pm);
        assertTrue(HttpProxy.isIdle(container));
        assertTrue(loadMap(idle_map).containsKey(APP_ID));

        HttpProxy.purge(container);

        assertTrue(loadMap(container_map).isEmpty());
        assertTrue(loadMap(mapping_map).isEmpty());
        assertTrue(loadMap(idle_map).isEmpty());
    }

    private void checkContainerMap() throws IOException {
        checkMap(container_map, ImmutableMap.of(APP_DNS, APP_ID));
    }

    private void addProxyMappings(ProxyMapping... mappings) throws IOException {
        HttpProxy.addMappings(container, Arrays.asList(mappings));
    }

    private void removeProxyMappings(ProxyMapping... mappings) throws IOException {
        HttpProxy.removeMappings(container, Arrays.asList(mappings));
    }

    private void checkProxyMappings(ProxyMapping... mappings) throws IOException {
        checkMap(mapping_map, Stream.of(mappings).collect(
            toImmutableMap(pm -> APP_ID + pm.getFrontend(), ProxyMapping::getBackend)));
    }

    private static void checkMap(Path file, Map<String, String> expected) throws IOException {
        assertThat(loadMap(file), is(expected));
    }

    private static Map<String, String> loadMap(Path file) throws IOException {
        if (Files.exists(file)) {
            try (Stream<String> lines = lines(file)) {
                return lines.collect(toSplittingMap(' '));
            }
        } else {
            return ImmutableMap.of();
        }
    }
}