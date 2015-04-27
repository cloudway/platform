/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.control.arrow;

import com.cloudway.fp.data.Tuple;
import com.cloudway.fp.π;

/**
 * The {code loop} operator expresses computations in which an output value
 * is fed back as input, although the computation occurs only once. It
 * underlines the rec value recursion construct in arrow notation. Loop should
 * satisfy the following laws:
 *
 * <pre>{@code
 * loop (arr f) = arr (\b -> fst (fix (\(c,d) -> f (b,d))))
 * loop (first h >>> f) = h >>> loop f
 * loop (f >>> first h) = loop f >>> h
 * loop (f >>> arr (id *** k)) = loop (arr (id *** k) >>> f)
 * loop (loop f) = loop (arr unassoc >>> f >>> arr assoc)
 * second (loop f) = loop (arr assoc >>> second f >>> arr unassoc)
 *
 * where
 *   assoc ((a,b),c) = (a,(b,c))
 *   unassoc (a,(b,c)) = ((a,b),c)
 * }</pre>
 */
public interface ArrowLoop<A> extends Arrow<A> {
    /**
     * loop :: a (b, d) (c, d) -> a b c
     */
    <B, C, D> π<A, B, C> loop(π<A, Tuple<B,D>, Tuple<C,D>> f);
}
