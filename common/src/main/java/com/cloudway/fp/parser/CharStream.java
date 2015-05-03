/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.parser;

import com.cloudway.fp.data.Maybe;
import com.cloudway.fp.data.Tuple;

/**
 * The string input stream.
 */
public enum CharStream implements Stream<String, Character> {
    INSTANCE;

    @Override
    public Maybe<Tuple<Character, String>> uncons(String s) {
        return s.isEmpty()
               ? Maybe.empty()
               : Maybe.of(Tuple.of(s.charAt(0), s.substring(1)));
    }
}
