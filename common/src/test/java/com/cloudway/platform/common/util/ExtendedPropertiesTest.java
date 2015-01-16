/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util;

import java.io.IOException;
import java.io.StringReader;
import java.io.UncheckedIOException;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableTable;
import org.junit.Test;
import static org.junit.Assert.*;

import static com.cloudway.platform.common.util.ExtendedProperties.LoadingProperties.unquote;

public class ExtendedPropertiesTest
{
    @Test
    public void populate() {
        ExtendedProperties defaults = new ExtendedProperties(
            ImmutableMap.<String,String>builder()
                .put("a", "d1")
                .put("c", "d3")
                .build(),
            ImmutableTable.<String,String,String>builder()
                .put("Y", "a", "dY1")
                .put("Z", "b", "dY2")
                .build(),
            null
        );

        ExtendedProperties p = new ExtendedProperties(
            ImmutableMap.<String,String>builder()
                .put("a", "1")
                .put("b", "2")
                .build(),
            ImmutableTable.<String,String,String>builder()
                .put("X", "a", "X1")
                .put("X", "c", "X3")
                .put("Y", "b", "Y2")
                .build(),
            defaults
        );

        test(p);
    }

    @Test
    public void load() throws IOException {
        ExtendedProperties defaults = new ExtendedProperties();
        defaults.load(new StringReader(String.join("\n"
            ,"a = d1"
            ,"c = d3"
            ,"[Y]"
            ,"a = dY1"
            ,"[Z]"
            ,"b = dY2"
        )));

        ExtendedProperties p = new ExtendedProperties(defaults);
        p.load(new StringReader(String.join("\n"
            ,"a = 1"
            ,"b = 2"
            ,"[X]"
            ,"a = X1"
            ,"c = X3"
            ,"[Y]"
            ,"b = Y2"
        )));

        test(p);
    }

    private static void test(ExtendedProperties p) {
        expect(p, "a", "1");
        expect(p, "b", "2");
        expect(p, "c", "d3");
        expect(p, "d", null);

        expect(p, "X", "a", "X1");
        expect(p, "X", "b", null);
        expect(p, "X", "c", "X3");
        expect(p, "X", "d", null);

        expect(p, "Y", "a", "dY1");
        expect(p, "Y", "b", "Y2");
        expect(p, "Y", "c", null);
        expect(p, "Y", "d", null);

        expect(p, "Z", "a", null);
        expect(p, "Z", "b", "dY2");
        expect(p, "Z", "c", null);
        expect(p, "Z", "d", null);

        expect(p, "-", "a", null);
        expect(p, "-", "-", null);

        assertTrue (p.categories().containsKey("X"));
        assertTrue (p.categories().containsKey("Y"));
        assertTrue (p.categories().containsKey("Z"));
        assertFalse(p.categories().containsKey("-"));
    }

    private static void expect(ExtendedProperties p, String key, String actual) {
        if (actual == null) {
            assertNull  (        p.getProperty(key, null));
            assertEquals("-",    p.getProperty(key, "-"));
            assertFalse (        p.getOptionalProperty(key).isPresent());
            assertEquals("-",    p.getOptionalProperty(key).orElse("-"));
            assertNull  (        p.global().get(key));
            assertFalse (        p.global().getOptional(key).isPresent());
            assertEquals("-",    p.global().getOptional(key).orElse("-"));
            assertNull  (        p.global().get(key, null));
            assertEquals("-",    p.global().get(key, "-"));
            assertFalse (        p.global().containsKey(key));
        } else {
            assertEquals(actual, p.getProperty(key, null));
            assertEquals(actual, p.getProperty(key, "-"));
            assertEquals(actual, p.getOptionalProperty(key).get());
            assertEquals(actual, p.getOptionalProperty(key).orElse("-"));
            assertEquals(actual, p.global().get(key));
            assertEquals(actual, p.global().getOptional(key).get());
            assertEquals(actual, p.global().getOptional(key).orElse("-"));
            assertEquals(actual, p.global().get(key, null));
            assertEquals(actual, p.global().get(key, "-"));
            assertTrue  (        p.global().containsKey(key));
        }
    }

    private static void expect(ExtendedProperties p, String cat, String key, String actual) {
        if (actual == null) {
            assertNull  (        p.getProperty(cat, key, null));
            assertEquals("-",    p.getProperty(cat, key, "-"));
            assertFalse (        p.getOptionalProperty(cat, key).isPresent());
            assertEquals("-",    p.getOptionalProperty(cat, key).orElse("-"));
            assertNull  (        p.category(cat).get(key));
            assertFalse (        p.category(cat).getOptional(key).isPresent());
            assertEquals("-",    p.category(cat).getOptional(key).orElse("-"));
            assertNull  (        p.category(cat).get(key, null));
            assertEquals("-",    p.category(cat).get(key, "-"));
            assertFalse (        p.category(cat).containsKey(key));
        } else {
            assertEquals(actual, p.getProperty(cat, key, null));
            assertEquals(actual, p.getProperty(cat, key, "-"));
            assertEquals(actual, p.getOptionalProperty(cat, key).get());
            assertEquals(actual, p.getOptionalProperty(cat, key).orElse("-"));
            assertEquals(actual, p.category(cat).get(key));
            assertEquals(actual, p.category(cat).getOptional(key).get());
            assertEquals(actual, p.category(cat).getOptional(key).orElse("-"));
            assertEquals(actual, p.category(cat).get(key, null));
            assertEquals(actual, p.category(cat).get(key, "-"));
            assertTrue  (        p.category(cat).containsKey(key));
        }
    }

    @Test
    public void unquoteTest() {
        unq("foo", "foo");
        comment("foo", "foo");

        good("\"foo\"");
        good("\"\"foo\"\"");

        bad("\"foo");
        bad("foo\"");

        bad("f\"oo");
        bad("f\"oo\"");
        bad("f\"o\"o");
        bad("f\"o\"o\"");
        bad("f\"\"oo");
        bad("f\"\"oo\"");

        bad("\"f\"oo");
        bad("\"f\"o\"o");
        bad("\"f\"\"oo");
        bad("\"\"f\"oo");
        bad("\"\"f\"o\"o");
        bad("\"\"f\"\"oo");

        good("\"f\"oo\"");
        good("\"f\"\"oo\"");
        good("\"\"\"foo\"");
        good("\"foo\"\"\"");
        good("\"\"foo\"");
        good("\"foo\"\"");

        good("\"foo\"bar\"\"");
        good("\"\"foo\"bar\"");
        good("\"foo\"bar\"blah\"");

        good("\"#foo\"");
        good("\"foo#\"");
        good("\"f#oo\"");
        unq("\"f#oo", "\"f");
        unq("\"f\"#oo", "f");
    }

    private static void good(String str) {
        assertTrue(str.startsWith("\"") && str.endsWith("\""));
        String expected = str.substring(1, str.length() - 1);
        unq(str, expected);
        unq(str + " ", expected);
        comment(str, expected);
    }

    private static void bad(String str) {
        unq(str, str);
        comment(str, str);
    }

    private static void comment(String str, String expected) {
        unq(str + "#", expected);
        unq(str + " #", expected);
        unq(str + "# ", expected);
        unq(str + " # ", expected);

        unq(str + "#xxx", expected);
        unq(str + "# xxx", expected);
        unq(str + "#xxx ", expected);
        unq(str + "# xxx ", expected);

        unq(str + " #xxx", expected);
        unq(str + " # xxx", expected);
        unq(str + " #xxx ", expected);
        unq(str + " # xxx ", expected);
    }

    private static void unq(String str, String expected) {
        assertEquals(expected, unquote(str));

        try {
            ExtendedProperties p = new ExtendedProperties();
            p.load(new StringReader("x = " + str));
            assertEquals(expected, p.getProperty("x", null));
        } catch (IOException ex) {
            throw new UncheckedIOException(ex);
        }
    }
}
