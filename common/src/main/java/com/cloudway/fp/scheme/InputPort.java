/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.scheme;

import com.cloudway.fp.$;

/**
 * The interface for scheme input port.
 */
public interface InputPort extends LispVal {
    /**
     * The Scheme object represents the end-of-file.
     */
    LispVal EOF = new LispVal() {
        @Override
        public String show() {
            return "#!eof";
        }
    };

    /**
     * Read converts external representations of Scheme objects into the objects
     * themselves. That is, it is a parser for the nonterminal &lt;datum&gt;.
     * Read returns the next object parsable from the given input port, updating
     * port to point to the first character past the end of the external
     * representation of the object.
     *
     * <p>If an end of file is encountered in the input before any characters
     * are found that can begin an object, the an end of file object is returned.
     * The port remains open, and further attempts to read will also return an
     * end of file object. If an end of file is encountered after the beginning
     * of an object's external representation, but the external representation
     * is incomplete and therefore not parsable, an error is signalled.</p>
     *
     * @param me the evaluator used to initialize the parser
     * @param env the environment used to initialize the parse
     * @return the Scheme object read from external representation
     */
    $<Evaluator, LispVal> read(Evaluator me, Env env);

    /**
     * Close the input port.
     */
    void close();
}
