/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.control.arrow;

import com.cloudway.fp.data.Tuple;
import com.cloudway.fp.π;

/**
 * Some arrows allow application of arrow inputs to other inputs. Instances
 * should satisfy the following laws:
 *
 * <pre>{@code
 * first (arr (\x -> arr (\y -> (x,y)))) >>> app = id
 * first (arr (g >>>)) >>> app = second g >>> app
 * first (arr (>>> h)) >>> app = app >>> h
 * }</pre>
 */
public interface ArrowApply<A> extends Arrow<A> {
    /**
     * <pre>{@code app :: a (a b c, b) c}</pre>
     */
    <B, C> π<A, Tuple<π<A, B, C>, B>, C> app();
}
