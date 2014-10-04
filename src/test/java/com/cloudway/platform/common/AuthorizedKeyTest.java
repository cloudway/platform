/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common;

import org.junit.Test;
import static junit.framework.Assert.*;

public class AuthorizedKeyTest
{
    @Test
    public void normalPublicKey1() {
        AuthorizedKey key = AuthorizedKey.parsePublicKey("ssh-rsa XXX test");
        assertEquals("ssh-rsa", key.getType());
        assertEquals("XXX", key.getBits());
        assertEquals("test", key.getComment());
        assertNull(key.getOptions());
    }

    @Test
    public void normalPublicKey2() {
        AuthorizedKey key = AuthorizedKey.parsePublicKey("ssh-rsa XXX");
        assertEquals("ssh-rsa", key.getType());
        assertEquals("XXX", key.getBits());
        assertNull(key.getComment());
        assertNull(key.getOptions());
    }

    @Test
    public void extraSpacesInPublicKey1() {
        AuthorizedKey key = AuthorizedKey.parsePublicKey("  ssh-rsa XXX test");
        assertEquals("ssh-rsa", key.getType());
        assertEquals("XXX", key.getBits());
        assertEquals("test", key.getComment());
        assertNull(key.getOptions());
    }

    @Test
    public void extraSpacesInPublicKey2() {
        AuthorizedKey key = AuthorizedKey.parsePublicKey("ssh-rsa XXX test  ");
        assertEquals("ssh-rsa", key.getType());
        assertEquals("XXX", key.getBits());
        assertEquals("test", key.getComment());
        assertNull(key.getOptions());
    }

    @Test
    public void extraSpacesInPublicKey3() {
        AuthorizedKey key = AuthorizedKey.parsePublicKey("ssh-rsa  XXX test");
        assertEquals("ssh-rsa", key.getType());
        assertEquals("XXX", key.getBits());
        assertEquals("test", key.getComment());
        assertNull(key.getOptions());
    }

    @Test
    public void extraSpacesInPublicKey4() {
        AuthorizedKey key = AuthorizedKey.parsePublicKey("ssh-rsa XXX  test");
        assertEquals("ssh-rsa", key.getType());
        assertEquals("XXX", key.getBits());
        assertEquals("test", key.getComment());
        assertNull(key.getOptions());
    }

    @Test(expected = IllegalArgumentException.class)
    public void malformedPublicKey1() {
        AuthorizedKey.parsePublicKey("");
    }

    @Test(expected = IllegalArgumentException.class)
    public void malformedPublicKey2() {
        AuthorizedKey.parsePublicKey("a");
    }

    @Test(expected = IllegalArgumentException.class)
    public void malformedPublicKey3() {
        AuthorizedKey.parsePublicKey("a b c d");
    }

    @Test
    public void normalAuthorizedKey1() {
        AuthorizedKey key = AuthorizedKey.parse("foo,bar ssh-rsa XXX test");
        assertEquals("foo,bar", key.getOptions());
        assertEquals("ssh-rsa", key.getType());
        assertEquals("XXX", key.getBits());
        assertEquals("test", key.getComment());
    }

    @Test
    public void normalAuthorizedKey2() {
        AuthorizedKey key = AuthorizedKey.parse("foo,bar ssh-rsa XXX");
        assertEquals("foo,bar", key.getOptions());
        assertEquals("ssh-rsa", key.getType());
        assertEquals("XXX", key.getBits());
        assertNull(key.getComment());
    }

    @Test
    public void normalAuthorizedKey3() {
        AuthorizedKey key = AuthorizedKey.parse("foo=\"bar\" ssh-rsa XXX test");
        assertEquals("foo=\"bar\"", key.getOptions());
        assertEquals("ssh-rsa", key.getType());
        assertEquals("XXX", key.getBits());
        assertEquals("test", key.getComment());
    }

    @Test
    public void normalAuthorizedKey4() {
        AuthorizedKey key = AuthorizedKey.parse("foo=\"bar\" ssh-rsa XXX");
        assertEquals("foo=\"bar\"", key.getOptions());
        assertEquals("ssh-rsa", key.getType());
        assertEquals("XXX", key.getBits());
        assertNull(key.getComment());
    }

    @Test
    public void normalAuthorizedKey5() {
        AuthorizedKey key = AuthorizedKey.parse("foo=\"bar baz\" ssh-rsa XXX test");
        assertEquals("foo=\"bar baz\"", key.getOptions());
        assertEquals("ssh-rsa", key.getType());
        assertEquals("XXX", key.getBits());
        assertEquals("test", key.getComment());
    }

    @Test
    public void normalAuthorizedKey6() {
        AuthorizedKey key = AuthorizedKey.parse("foo=\"bar baz\" ssh-rsa XXX");
        assertEquals("foo=\"bar baz\"", key.getOptions());
        assertEquals("ssh-rsa", key.getType());
        assertEquals("XXX", key.getBits());
        assertNull(key.getComment());
    }

    @Test(expected = IllegalArgumentException.class)
    public void malformedAuthorizedKey1() {
        AuthorizedKey.parse("foo=\"bar ssh-rsa XXX");
    }

    @Test(expected = IllegalArgumentException.class)
    public void malformedAuthorizedKey2() {
        AuthorizedKey.parse("foo=bar\" ssh-rsa XXX");
    }

    @Test(expected = IllegalArgumentException.class)
    public void malformedAuthorizedKey3() {
        AuthorizedKey.parse("foo=b\"ar ssh-rsa XXX");
    }
}
