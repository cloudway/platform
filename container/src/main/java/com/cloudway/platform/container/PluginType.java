/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;

@XmlEnum
public enum PluginType
{
    @XmlEnumValue("Framework") FRAMEWORK,
    @XmlEnumValue("Service")   SERVICE,
    @XmlEnumValue("Component") COMPONENT
}
