/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.os;

import org.junit.Test;
import org.hamcrest.Description;
import org.hamcrest.Matcher;
import org.hamcrest.TypeSafeMatcher;
import static org.junit.Assert.assertThat;
import static org.hamcrest.CoreMatchers.*;
import com.cloudway.fp.data.Maybe;

public class AuthorizedKeyTest
{
    @Test
    public void normalPublicKey1() {
        AuthorizedKey key = AuthorizedKey.parsePublicKey("ssh-rsa XXX test");
        assertThat(key.getType(), is("ssh-rsa"));
        assertThat(key.getBits(), is("XXX"));
        assertThat(key.getComment(), just("test"));
        assertThat(key.getOptions(), is(nothing()));
    }

    @Test
    public void normalPublicKey2() {
        AuthorizedKey key = AuthorizedKey.parsePublicKey("ssh-rsa XXX");
        assertThat(key.getType(), is("ssh-rsa"));
        assertThat(key.getBits(), is("XXX"));
        assertThat(key.getComment(), is(nothing()));
        assertThat(key.getOptions(), is(nothing()));
    }

    @Test
    public void extraSpacesInPublicKey1() {
        AuthorizedKey key = AuthorizedKey.parsePublicKey("  ssh-rsa XXX test");
        assertThat(key.getType(), is("ssh-rsa"));
        assertThat(key.getBits(), is("XXX"));
        assertThat(key.getComment(), just("test"));
        assertThat(key.getOptions(), is(nothing()));
    }

    @Test
    public void extraSpacesInPublicKey2() {
        AuthorizedKey key = AuthorizedKey.parsePublicKey("ssh-rsa XXX test  ");
        assertThat(key.getType(), is("ssh-rsa"));
        assertThat(key.getBits(), is("XXX"));
        assertThat(key.getComment(), just("test"));
        assertThat(key.getOptions(), is(nothing()));
    }

    @Test
    public void extraSpacesInPublicKey3() {
        AuthorizedKey key = AuthorizedKey.parsePublicKey("ssh-rsa  XXX test");
        assertThat(key.getType(), is("ssh-rsa"));
        assertThat(key.getBits(), is("XXX"));
        assertThat(key.getComment(), just("test"));
        assertThat(key.getOptions(), is(nothing()));
    }

    @Test
    public void extraSpacesInPublicKey4() {
        AuthorizedKey key = AuthorizedKey.parsePublicKey("ssh-rsa XXX  test");
        assertThat(key.getType(), is("ssh-rsa"));
        assertThat(key.getBits(), is("XXX"));
        assertThat(key.getComment(), just("test"));
        assertThat(key.getOptions(), is(nothing()));
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
        assertThat(key.getOptions().get(), is("foo,bar"));
        assertThat(key.getType(), is("ssh-rsa"));
        assertThat(key.getBits(), is("XXX"));
        assertThat(key.getComment(), just("test"));
    }

    @Test
    public void normalAuthorizedKey2() {
        AuthorizedKey key = AuthorizedKey.parse("foo,bar ssh-rsa XXX");
        assertThat(key.getOptions(), just("foo,bar"));
        assertThat(key.getType(), is("ssh-rsa"));
        assertThat(key.getBits(), is("XXX"));
        assertThat(key.getComment(), is(nothing()));
    }

    @Test
    public void normalAuthorizedKey3() {
        AuthorizedKey key = AuthorizedKey.parse("foo=\"bar\" ssh-rsa XXX test");
        assertThat(key.getOptions(), just("foo=\"bar\""));
        assertThat(key.getType(), is("ssh-rsa"));
        assertThat(key.getBits(), is("XXX"));
        assertThat(key.getComment(), just("test"));
    }

    @Test
    public void normalAuthorizedKey4() {
        AuthorizedKey key = AuthorizedKey.parse("foo=\"bar\" ssh-rsa XXX");
        assertThat(key.getOptions(), just("foo=\"bar\""));
        assertThat(key.getType(), is("ssh-rsa"));
        assertThat(key.getBits(), is("XXX"));
        assertThat(key.getComment(), is(nothing()));
    }

    @Test
    public void normalAuthorizedKey5() {
        AuthorizedKey key = AuthorizedKey.parse("foo=\"bar baz\" ssh-rsa XXX test");
        assertThat(key.getOptions(), just("foo=\"bar baz\""));
        assertThat(key.getType(), is("ssh-rsa"));
        assertThat(key.getBits(), is("XXX"));
        assertThat(key.getComment(), just("test"));
    }

    @Test
    public void normalAuthorizedKey6() {
        AuthorizedKey key = AuthorizedKey.parse("foo=\"bar baz\" ssh-rsa XXX");
        assertThat(key.getOptions(), just("foo=\"bar baz\""));
        assertThat(key.getType(), is("ssh-rsa"));
        assertThat(key.getBits(), is("XXX"));
        assertThat(key.getComment(), is(nothing()));
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

    @Test
    public void parsePublicKey() {
        AuthorizedKey key = AuthorizedKey.parser()
            .withId("myid", "myname")
            .withOptions("foo=\"bar\"")
            .parsePublicKey("ssh-rsa XXX test");

        assertThat(key.getOptions(), just("foo=\"bar\""));
        assertThat(key.getType(), is("ssh-rsa"));
        assertThat(key.getBits(), is("XXX"));
        assertThat(key.getId(), just("myid"));
        assertThat(key.getName(), just("myname"));
        assertThat(key.getComment(), just(not("test")));

        AuthorizedKey pubkey = key.toPublicKey();
        assertThat(pubkey.getOptions(), is(nothing()));
        assertThat(pubkey.getType(), is("ssh-rsa"));
        assertThat(pubkey.getBits(), is("XXX"));
        assertThat(pubkey.getComment(), just("myname"));
        assertThat(pubkey.toString(), is("ssh-rsa XXX myname"));
    }

    static <T> Matcher<Maybe<T>> just(Matcher<? extends T> elemenetMatcher) {
        return new MaybeMatcher<>(elemenetMatcher);
    }

    static <T> Matcher<Maybe<T>> just(T element) {
        return just(equalTo(element));
    }

    @SuppressWarnings("unchecked")
    static <T> Matcher<Maybe<?>> present() {
        return (Matcher)just(anything());
    }

    static <T> Matcher<Maybe<?>> nothing() {
        return not(present());
    }

    static class MaybeMatcher<T> extends TypeSafeMatcher<Maybe<T>> {
        private final Matcher<? extends T> elementMatcher;

        public MaybeMatcher(Matcher<? extends T> elementMatcher) {
            this.elementMatcher = elementMatcher;
        }

        @Override
        public boolean matchesSafely(Maybe<T> maybe) {
            return maybe.map(elementMatcher::matches).orElse(false);
        }

        @Override
        public void describeTo(Description description) {
            description.appendText("just ").appendDescriptionOf(elementMatcher);
        }
    }
}
