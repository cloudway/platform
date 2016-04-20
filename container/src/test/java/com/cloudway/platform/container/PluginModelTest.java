/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.Map;

import org.junit.Rule;
import org.junit.Test;
import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Marshaller;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.cloudway.fp.function.ExceptionConsumer;
import com.cloudway.platform.container.proxy.ProxyMapping;
import static com.cloudway.platform.common.util.MoreFiles.*;

// @formatter:off
public class PluginModelTest
{
    public final @Rule TemporaryDirectory temporary = new TemporaryDirectory();

    static class PluginMetaData extends MetaData.Plugin {
        PluginMetaData(List<MetaData.Endpoint> endpoints) {
            name = "mock";
            displayName = "Mock";
            version = "1.0";
            vendor = "Mock Corp";
            pluginVersion = "1.0";
            pluginVendor = "Mock Corp";
            category = PluginType.FRAMEWORK;
            this.endpoints = endpoints;
        }
    }

    private static void verify(Plugin plugin, Path path) {
        plugin.toString();
        plugin.validate();
        assertThat(plugin.getPath(), is(path));
        assertThat(plugin.getName(), is("mock"));
        assertThat(plugin.getDisplayName(), is("Mock"));
        assertThat(plugin.getVersion(), is("1.0"));
        assertThat(plugin.getType(), is(PluginType.FRAMEWORK));
    }

    @Test
    public void verify_invalid_plugin_directory() throws Exception {
        verifyInvalidPluginDirectory(Paths.get("path_does_not_exists"));
        verifyInvalidPluginDirectory(temporary.newFile());
        verifyInvalidPluginDirectory(temporary.newFolder());
    }

    private static void verifyInvalidPluginDirectory(Path path) throws Exception {
        assertFalse(com.cloudway.platform.container.Plugin.isPluginDirectory(path));

        Plugin plugin = com.cloudway.platform.container.Plugin.load(path, ImmutableMap.of());
        assertFalse(plugin.isValid());

        try {
            plugin.toString();
            plugin.validate();
            fail("the plugin should not be validated");
        } catch (IllegalStateException ex) {
            // ok
        }
    }

    private void prepareDataAndDo(MetaData.Plugin metadata, ExceptionConsumer<Path, Exception> action)
        throws Exception
    {
        Path path = temporary.newFolder();
        Path metadataFile = join(path, "metadata", "plugin.xml");
        createParentDirs(metadataFile);

        JAXBContext jaxb = JAXBContext.newInstance(
            MetaData.Plugin.class, MetaData.Endpoint.class, MetaData.ProxyMapping.class);
        Marshaller marshaller = jaxb.createMarshaller();
        marshaller.marshal(metadata, metadataFile.toFile());

        action.consume(path);
    }

    @Test
    public void load_and_verify_plugin_metadata() throws Exception {
        MetaData.Plugin metadata = new PluginMetaData(ImmutableList.of(
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
            }}
        ));

        prepareDataAndDo(metadata, path -> {
            assertTrue(com.cloudway.platform.container.Plugin.isPluginDirectory(path));

            Map<String, String> env = ImmutableMap.of("CLOUDWAY_MOCK_IP", "1.1.1.1");
            Plugin plugin = com.cloudway.platform.container.Plugin.load(path, env);
            verify(plugin, path);

            assertThat(plugin.getEndpoints().size(), is(1));
            plugin.getEndpoints().forEach(ep -> {
                assertThat(ep.getPrivateHostName(), is("CLOUDWAY_MOCK_IP"));
                assertThat(ep.getPrivatePortName(), is("CLOUDWAY_MOCK_PORT"));
                assertThat(ep.getPrivateHost(), is("1.1.1.1"));
                assertThat(ep.getPrivatePort(), is(8080));

                assertThat(ep.getProxyMappings().size(), is(1));
                ep.getProxyMappings().forEach(pm -> {
                    assertThat(pm.getBackend(), is("/"));
                    assertThat(pm.getFrontend(), is("/"));
                    assertThat(pm.getProtocol(), is("http"));
                });
            });

            assertThat(plugin.getProxyMappings().size(), is(1));
            verifyProxyMapping(plugin, 0, "", "http://1.1.1.1:8080", "http");

            Path target = Paths.get("target");
            Plugin copy = plugin.copyOf(target);
            verify(copy, target);
            assertEquals(copy.getEndpoints(), plugin.getEndpoints());
        });
    }

    @Test
    public void load_and_verify_endpoint_with_multiple_proxy_mappings() throws Exception {
        MetaData.Plugin metadata = new PluginMetaData(ImmutableList.of(
            new MetaData.Endpoint() {{
                privateHostName = "IP";
                privatePortName = "PORT";
                privatePort = 8080;
                proxyMappings = ImmutableList.of(
                    new MetaData.ProxyMapping() {{
                        frontend = "/";
                        backend = "/";
                        protocols = "http";
                    }},
                    new MetaData.ProxyMapping() {{
                        frontend = "/admin";
                        backend = "/admin";
                        protocols = "http";
                    }}
                );
            }}
        ));

        prepareDataAndDo(metadata, path -> {
            Map<String, String> env = ImmutableMap.of("CLOUDWAY_MOCK_IP", "1.1.1.1");
            Plugin plugin = com.cloudway.platform.container.Plugin.load(path, env);
            verify(plugin, path);

            assertThat(plugin.getProxyMappings().size(), is(2));
            verifyProxyMapping(plugin, 0, "", "http://1.1.1.1:8080", "http");
            verifyProxyMapping(plugin, 1, "/admin", "http://1.1.1.1:8080/admin", "http");
        });
    }

    @Test
    public void load_and_verify_endpoint_with_multiple_protocols() throws Exception {
        MetaData.Plugin metadata = new PluginMetaData(ImmutableList.of(
            new MetaData.Endpoint() {{
                privateHostName = "IP";
                privatePortName = "PORT";
                privatePort = 8080;
                proxyMappings = ImmutableList.of(
                    new MetaData.ProxyMapping() {{
                        frontend = "/";
                        backend = "/";
                        protocols = "http,ajp";
                    }}
                );
            }}
        ));

        prepareDataAndDo(metadata, path -> {
            Map<String, String> env = ImmutableMap.of("CLOUDWAY_MOCK_IP", "1.1.1.1");
            Plugin plugin = com.cloudway.platform.container.Plugin.load(path, env);
            verify(plugin, path);

            assertThat(plugin.getProxyMappings().size(), is(2));
            verifyProxyMapping(plugin, 0, "", "http://1.1.1.1:8080", "http");
            verifyProxyMapping(plugin, 1, "", "ajp://1.1.1.1:8080", "ajp");
        });
    }

    @Test
    public void load_and_verify_multiple_endpoints_with_different_host() throws Exception {
        MetaData.Plugin metadata = new PluginMetaData(ImmutableList.of(
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
            }},
            new MetaData.Endpoint() {{
                privateHostName = "ADMIN_IP";
                privatePortName = "ADMIN_PORT";
                privatePort = 8081;
                proxyMappings = ImmutableList.of(
                    new MetaData.ProxyMapping() {{
                        frontend = "/";
                        backend = "/";
                        protocols = "http";
                    }}
                );
            }}
        ));

        prepareDataAndDo(metadata, path -> {
            Map<String, String> env = ImmutableMap.of("CLOUDWAY_MOCK_IP", "1.1.1.1",
                                                      "CLOUDWAY_MOCK_ADMIN_IP", "2.2.2.2");
            Plugin plugin = com.cloudway.platform.container.Plugin.load(path, env);
            verify(plugin, path);

            assertThat(plugin.getProxyMappings().size(), is(2));
            verifyProxyMapping(plugin, 0, "", "http://1.1.1.1:8080", "http");
            verifyProxyMapping(plugin, 1, "", "http://2.2.2.2:8081", "http");
        });
    }

    @Test
    public void load_and_verify_multiple_endpoints_with_same_host() throws Exception {
        MetaData.Plugin metadata = new PluginMetaData(ImmutableList.of(
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
            }},
            new MetaData.Endpoint() {{
                privateHostName = "IP";
                privatePortName = "ADMIN_PORT";
                privatePort = 8081;
                proxyMappings = ImmutableList.of(
                    new MetaData.ProxyMapping() {{
                        frontend = "/";
                        backend = "/";
                        protocols = "https";
                    }}
                );
            }}
        ));

        prepareDataAndDo(metadata, path -> {
            Map<String, String> env = ImmutableMap.of("CLOUDWAY_MOCK_IP", "1.1.1.1");
            Plugin plugin = com.cloudway.platform.container.Plugin.load(path, env);
            verify(plugin, path);

            assertThat(plugin.getProxyMappings().size(), is(2));
            verifyProxyMapping(plugin, 0, "", "http://1.1.1.1:8080", "http");
            verifyProxyMapping(plugin, 1, "", "https://1.1.1.1:8081", "https");
        });
    }

    @Test
    public void load_and_verify_multiple_endpoints_with_multiple_protocols() throws Exception {
        MetaData.Plugin metadata = new PluginMetaData(ImmutableList.of(
            new MetaData.Endpoint() {{
                privateHostName = "IP";
                privatePortName = "PORT";
                privatePort = 8080;
                proxyMappings = ImmutableList.of(
                    new MetaData.ProxyMapping() {{
                        frontend = "/";
                        backend = "/";
                        protocols = "http,ajp";
                    }}
                );
            }},
            new MetaData.Endpoint() {{
                privateHostName = "IP";
                privatePortName = "ADMIN_PORT";
                privatePort = 8081;
                proxyMappings = ImmutableList.of(
                    new MetaData.ProxyMapping() {{
                        frontend = "/admin";
                        backend = "/admin";
                        protocols = "https,wss";
                    }}
                );
            }}
        ));

        prepareDataAndDo(metadata, path -> {
            Map<String, String> env = ImmutableMap.of("CLOUDWAY_MOCK_IP", "1.1.1.1");
            Plugin plugin = com.cloudway.platform.container.Plugin.load(path, env);
            verify(plugin, path);

            assertThat(plugin.getProxyMappings().size(), is(4));
            verifyProxyMapping(plugin, 0, "", "http://1.1.1.1:8080", "http");
            verifyProxyMapping(plugin, 1, "", "ajp://1.1.1.1:8080", "ajp");
            verifyProxyMapping(plugin, 2, "/admin", "https://1.1.1.1:8081/admin", "https");
            verifyProxyMapping(plugin, 3, "/admin", "wss://1.1.1.1:8081/admin", "wss");
        });
    }

    private static void verifyProxyMapping(Plugin plugin, int index, String frontend, String backend, String protocol) {
        ProxyMapping pm = plugin.getProxyMappings().get(index);
        assertThat(pm.getFrontend(), is(frontend));
        assertThat(pm.getBackend(), is(backend));
        assertThat(pm.getProtocol(), is(protocol));
    }

    // -----------------------------------------------------------------------

    @Test
    public void verify_frontend_uri_mapping() {
        verifyFrontendUri("", "");
        verifyFrontendUri("/", "");
        verifyFrontendUri("/admin/", "/admin");
        verifyFrontendUri("admin", "/admin");
        verifyFrontendUri("admin/", "/admin");
    }

    @Test
    public void verify_backend_uri_mapping() throws Exception {
        PluginMetaData metadata = new PluginMetaData(ImmutableList.of(
            new MetaData.Endpoint() {{
                privateHostName = "IP";
                privatePortName = "PORT";
                privatePort = 8080;
            }}
        ));

        prepareDataAndDo(metadata, path -> {
            Map<String, String> env = ImmutableMap.of("CLOUDWAY_MOCK_IP", "ip");
            Plugin plugin = com.cloudway.platform.container.Plugin.load(path, env);
            verify(plugin, path);

            Plugin.Endpoint ep = plugin.getEndpoints().get(0);
            verifyBackendUri(ep, "", "http", "http://ip:8080");
            verifyBackendUri(ep, "/", "http", "http://ip:8080");
            verifyBackendUri(ep, "/admin", "http", "http://ip:8080/admin");
            verifyBackendUri(ep, "/admin/", "http", "http://ip:8080/admin");
            verifyBackendUri(ep, "admin", "http", "admin");
            verifyBackendUri(ep, "REDIRECT:/admin", "http", "REDIRECT:/admin");
        });
    }

    private static void verifyFrontendUri(String uri, String mapped) {
        ProxyMapping pm = new ProxyMapping(uri, "", "http");
        assertThat(com.cloudway.platform.container.Plugin.getFrontendUri(pm), is(mapped));
    }

    private static void verifyBackendUri(Plugin.Endpoint ep, String uri, String protocol, String mapped) {
        ProxyMapping pm = new ProxyMapping("", uri, protocol);
        assertThat(com.cloudway.platform.container.Plugin.getBackendUri(ep, pm), is(mapped));
    }
}