/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.container.velocity;

import org.apache.velocity.util.introspection.Info;
import org.apache.velocity.util.introspection.UberspectImpl;
import org.apache.velocity.util.introspection.VelMethod;

public class StringConversionUberspector extends UberspectImpl
{
    @Override
    public VelMethod getMethod(Object obj, String methodName, Object[] args, Info i)
        throws Exception
    {
        if (obj instanceof String) {
            if (methodName.equals("intValue") && args.length == 0) {
                return new IntValueMethod();
            }

            if (methodName.equals("longValue") && args.length == 0) {
                return new LongValueMethod();
            }

            if (methodName.equals("doubleValue") && args.length == 0) {
                return new DoubleValueMethod();
            }
        }

        return null;
    }

    static class IntValueMethod implements VelMethod {
        public Object invoke(Object obj, Object[] actual) {
            return Integer.parseInt((String)obj);
        }

        public boolean isCacheable() {
            return true;
        }

        public String getMethodName() {
            return "intValue";
        }

        public Class getReturnType() {
            return int.class;
        }
    }

    static class LongValueMethod implements VelMethod {
        public Object invoke(Object obj, Object[] actual) {
            return Long.parseLong((String)obj);
        }

        public boolean isCacheable() {
            return true;
        }

        public String getMethodName() {
            return "longValue";
        }

        public Class getReturnType() {
            return long.class;
        }
    }

    static class DoubleValueMethod implements VelMethod {
        public Object invoke(Object obj, Object[] actual) {
            return Double.parseDouble((String)obj);
        }

        public boolean isCacheable() {
            return true;
        }

        public String getMethodName() {
            return "doubleValue";
        }

        public Class getReturnType() {
            return double.class;
        }
    }
}
