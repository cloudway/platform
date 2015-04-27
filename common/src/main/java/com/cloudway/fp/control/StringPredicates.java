/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.control;

import java.util.function.Predicate;
import java.util.regex.Pattern;

public final class StringPredicates
{
    private StringPredicates() {}

    public static Predicate<CharSequence> nullOrEmpty() {
        return s -> s == null || s.length() == 0;
    }

    public static boolean isBlank(CharSequence str) {
        int len;
        if (str == null || (len = str.length()) == 0) {
            return true;
        }
        for (int i = 0; i < len; i++) {
            if (!Character.isWhitespace(str.charAt(i))) {
                return false;
            }
        }
        return true;
    }

    public static Predicate<CharSequence> blank() {
        return StringPredicates::isBlank;
    }

    public static Predicate<CharSequence> lengthOf(int len) {
        return s -> s.length() == len;
    }

    public static Predicate<String> startsWith(String prefix) {
        return s -> s.startsWith(prefix);
    }

    public static Predicate<String> endsWith(String suffix) {
        return s -> s.endsWith(suffix);
    }

    public static Predicate<String> containsString(CharSequence cs) {
        return s -> s.contains(cs);
    }

    public static Predicate<String> containsChar(int ch) {
        return s -> s.indexOf(ch) != -1;
    }

    public static Predicate<CharSequence> containsPattern(Pattern regex) {
        return s -> regex.matcher(s).find();
    }

    public static Predicate<CharSequence> containsPattern(String regex) {
        return containsPattern(Pattern.compile(regex));
    }

    public static Predicate<CharSequence> matches(Pattern regex) {
        return s -> regex.matcher(s).matches();
    }

    public static Predicate<CharSequence> matches(String regex) {
        return matches(Pattern.compile(regex));
    }
}
