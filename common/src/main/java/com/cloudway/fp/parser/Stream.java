/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.parser;

import com.cloudway.fp.data.Maybe;
import com.cloudway.fp.data.Tuple;

public interface Stream<S, T> {
    Maybe<Tuple<T, S>> uncons(S s);
}
