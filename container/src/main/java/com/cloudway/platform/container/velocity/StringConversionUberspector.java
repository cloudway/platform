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
            if ("intValue".equals(methodName) && args.length == 0) {
                return new IntValueMethod();
            }

            if ("longValue".equals(methodName) && args.length == 0) {
                return new LongValueMethod();
            }

            if ("doubleValue".equals(methodName) && args.length == 0) {
                return new DoubleValueMethod();
            }
        }

        return null;
    }

    static class IntValueMethod implements VelMethod {
        @Override
        public Object invoke(Object obj, Object[] actual) {
            return Integer.parseInt((String)obj);
        }

        @Override
        public boolean isCacheable() {
            return true;
        }

        @Override
        public String getMethodName() {
            return "intValue";
        }

        @Override @SuppressWarnings("rawtypes")
        public Class getReturnType() {
            return int.class;
        }
    }

    static class LongValueMethod implements VelMethod {
        @Override
        public Object invoke(Object obj, Object[] actual) {
            return Long.parseLong((String)obj);
        }

        @Override
        public boolean isCacheable() {
            return true;
        }

        @Override
        public String getMethodName() {
            return "longValue";
        }

        @Override @SuppressWarnings("rawtypes")
        public Class getReturnType() {
            return long.class;
        }
    }

    static class DoubleValueMethod implements VelMethod {
        @Override
        public Object invoke(Object obj, Object[] actual) {
            return Double.parseDouble((String)obj);
        }

        @Override
        public boolean isCacheable() {
            return true;
        }

        @Override
        public String getMethodName() {
            return "doubleValue";
        }

        @Override @SuppressWarnings("rawtypes")
        public Class getReturnType() {
            return double.class;
        }
    }
}
