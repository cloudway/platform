/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import javax.xml.XMLConstants;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;

import org.xml.sax.SAXException;

public final class MetaData
{
    private MetaData() {}

    private static JAXBContext jaxb;
    private static Schema schema;

    public static Addon load(Path file) throws JAXBException, SAXException {
        return (Addon)getUnmarshaller().unmarshal(file.toFile());
    }

    private static synchronized Unmarshaller getUnmarshaller()
        throws JAXBException, SAXException
    {
        if (jaxb == null) {
            jaxb = JAXBContext.newInstance(Addon.class, Endpoint.class, ProxyMapping.class);
        }

        if (schema == null) {
            SchemaFactory sf = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
            schema = sf.newSchema(MetaData.class.getResource("addon.xsd"));
        }

        Unmarshaller unmarshaller = jaxb.createUnmarshaller();
        unmarshaller.setSchema(schema);
        return unmarshaller;
    }

    @XmlRootElement
    public static class Addon
    {
        @XmlElement
        public String name;

        @XmlElement(name = "display-name")
        public String displayName;

        @XmlElement
        public String version;

        @XmlElement
        public String vendor;

        @XmlElement(name = "addon-version")
        public String addonVersion;

        @XmlElement(name = "addon-vendor")
        public String addonVendor;

        @XmlElement(name = "category")
        public AddonType category;

        @XmlElement(name = "endpoint")
        public List<Endpoint> endpoints = new ArrayList<>();
    }

    @XmlType
    public static class Endpoint
    {
        @XmlElement(name = "private-host-name")
        public String privateHostName;

        @XmlElement(name = "private-port-name")
        public String privatePortName;

        @XmlElement(name = "private-port")
        public int privatePort;

        @XmlElement(name = "proxy-mapping")
        public List<ProxyMapping> proxyMappings = new ArrayList<>();
    }

    @XmlType
    public static class ProxyMapping
    {
        @XmlElement
        public String frontend;

        @XmlElement
        public String backend;

        @XmlElement
        public String protocols;
    }
}
