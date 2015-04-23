/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.control;

/**
 * Monads that also support choice and failure.
 */
public interface MonadPlus<M> extends Alternative<M>, Monad<M> {
}
