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
public class AddonModelTest
{
    public final @Rule TemporaryDirectory temporary = new TemporaryDirectory();

    static class AddonMetaData extends MetaData.Addon {
        AddonMetaData(List<MetaData.Endpoint> endpoints) {
            name = "mock";
            displayName = "Mock";
            version = "1.0";
            vendor = "Mock Corp";
            addonVersion = "1.0";
            addonVendor = "Mock Corp";
            category = AddonType.FRAMEWORK;
            this.endpoints = endpoints;
        }
    }

    private static void verify(Addon addon, Path path) {
        addon.toString();
        addon.validate();
        assertThat(addon.getPath(), is(path));
        assertThat(addon.getName(), is("mock"));
        assertThat(addon.getDisplayName(), is("Mock"));
        assertThat(addon.getVersion(), is("1.0"));
        assertThat(addon.getType(), is(AddonType.FRAMEWORK));
    }

    @Test
    public void verify_invalid_addon_directory() throws Exception {
        verifyInvalidAddonDirectory(Paths.get("path_does_not_exists"));
        verifyInvalidAddonDirectory(temporary.newFile());
        verifyInvalidAddonDirectory(temporary.newFolder());
    }

    private static void verifyInvalidAddonDirectory(Path path) throws Exception {
        assertFalse(Addon.isAddonDirectory(path));

        Addon addon = Addon.load(path, ImmutableMap.of());
        assertFalse(addon.isValid());

        try {
            addon.toString();
            addon.validate();
            fail("the addon should not be validated");
        } catch (IllegalStateException ex) {
            // ok
        }
    }

    private void prepareDataAndDo(MetaData.Addon metadata, ExceptionConsumer<Path, Exception> action)
        throws Exception
    {
        Path path = temporary.newFolder();
        Path metadataFile = join(path, "metadata", "addon.xml");
        createParentDirs(metadataFile);

        JAXBContext jaxb = JAXBContext.newInstance(
            MetaData.Addon.class, MetaData.Endpoint.class, MetaData.ProxyMapping.class);
        Marshaller marshaller = jaxb.createMarshaller();
        marshaller.marshal(metadata, metadataFile.toFile());

        action.consume(path);
    }

    @Test
    public void load_and_verify_addon_metadata() throws Exception {
        MetaData.Addon metadata = new AddonMetaData(ImmutableList.of(
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
            assertTrue(Addon.isAddonDirectory(path));

            Map<String, String> env = ImmutableMap.of("CLOUDWAY_MOCK_IP", "1.1.1.1");
            Addon addon = Addon.load(path, env);
            verify(addon, path);

            assertThat(addon.getEndpoints().size(), is(1));
            addon.getEndpoints().forEach(ep -> {
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

            assertThat(addon.getProxyMappings().size(), is(1));
            verifyProxyMapping(addon, 0, "", "http://1.1.1.1:8080", "http");

            Path target = Paths.get("target");
            Addon copy = addon.copyOf(target);
            verify(copy, target);
            assertEquals(copy.getEndpoints(), addon.getEndpoints());
        });
    }

    @Test
    public void load_and_verify_endpoint_with_multiple_proxy_mappings() throws Exception {
        MetaData.Addon metadata = new AddonMetaData(ImmutableList.of(
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
            Addon addon = Addon.load(path, env);
            verify(addon, path);

            assertThat(addon.getProxyMappings().size(), is(2));
            verifyProxyMapping(addon, 0, "", "http://1.1.1.1:8080", "http");
            verifyProxyMapping(addon, 1, "/admin", "http://1.1.1.1:8080/admin", "http");
        });
    }

    @Test
    public void load_and_verify_endpoint_with_multiple_protocols() throws Exception {
        MetaData.Addon metadata = new AddonMetaData(ImmutableList.of(
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
            Addon addon = Addon.load(path, env);
            verify(addon, path);

            assertThat(addon.getProxyMappings().size(), is(2));
            verifyProxyMapping(addon, 0, "", "http://1.1.1.1:8080", "http");
            verifyProxyMapping(addon, 1, "", "ajp://1.1.1.1:8080", "ajp");
        });
    }

    @Test
    public void load_and_verify_multiple_endpoints_with_different_host() throws Exception {
        MetaData.Addon metadata = new AddonMetaData(ImmutableList.of(
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
            Addon addon = Addon.load(path, env);
            verify(addon, path);

            assertThat(addon.getProxyMappings().size(), is(2));
            verifyProxyMapping(addon, 0, "", "http://1.1.1.1:8080", "http");
            verifyProxyMapping(addon, 1, "", "http://2.2.2.2:8081", "http");
        });
    }

    @Test
    public void load_and_verify_multiple_endpoints_with_same_host() throws Exception {
        MetaData.Addon metadata = new AddonMetaData(ImmutableList.of(
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
            Addon addon = Addon.load(path, env);
            verify(addon, path);

            assertThat(addon.getProxyMappings().size(), is(2));
            verifyProxyMapping(addon, 0, "", "http://1.1.1.1:8080", "http");
            verifyProxyMapping(addon, 1, "", "https://1.1.1.1:8081", "https");
        });
    }

    @Test
    public void load_and_verify_multiple_endpoints_with_multiple_protocols() throws Exception {
        MetaData.Addon metadata = new AddonMetaData(ImmutableList.of(
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
            Addon addon = Addon.load(path, env);
            verify(addon, path);

            assertThat(addon.getProxyMappings().size(), is(4));
            verifyProxyMapping(addon, 0, "", "http://1.1.1.1:8080", "http");
            verifyProxyMapping(addon, 1, "", "ajp://1.1.1.1:8080", "ajp");
            verifyProxyMapping(addon, 2, "/admin", "https://1.1.1.1:8081/admin", "https");
            verifyProxyMapping(addon, 3, "/admin", "wss://1.1.1.1:8081/admin", "wss");
        });
    }

    private static void verifyProxyMapping(Addon addon, int index, String frontend, String backend, String protocol) {
        ProxyMapping pm = addon.getProxyMappings().get(index);
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
        AddonMetaData metadata = new AddonMetaData(ImmutableList.of(
            new MetaData.Endpoint() {{
                privateHostName = "IP";
                privatePortName = "PORT";
                privatePort = 8080;
            }}
        ));

        prepareDataAndDo(metadata, path -> {
            Map<String, String> env = ImmutableMap.of("CLOUDWAY_MOCK_IP", "ip");
            Addon addon = Addon.load(path, env);
            verify(addon, path);

            Addon.Endpoint ep = addon.getEndpoints().get(0);
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
        assertThat(Addon.getFrontendUri(pm), is(mapped));
    }

    private static void verifyBackendUri(Addon.Endpoint ep, String uri, String protocol, String mapped) {
        ProxyMapping pm = new ProxyMapping("", uri, protocol);
        assertThat(Addon.getBackendUri(ep, pm), is(mapped));
    }
}