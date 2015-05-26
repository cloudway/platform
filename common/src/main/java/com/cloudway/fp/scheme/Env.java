/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.scheme;

import com.cloudway.fp.data.Maybe;
import com.cloudway.fp.data.PMap;
import com.cloudway.fp.data.Ref;
import com.cloudway.fp.data.Seq;
import com.cloudway.fp.scheme.LispVal.Atom;

public final class Env {
    private PMap<Atom, Ref<LispVal>> bindings;
    private final int quoteLevel;

    public Env(PMap<Atom, Ref<LispVal>> bindings) {
        this.bindings = bindings;
        this.quoteLevel = 0;
    }

    public Env(Env outer) {
        this.bindings = outer.bindings;
        this.quoteLevel = 0;
    }

    private Env(PMap<Atom, Ref<LispVal>> bindings, int lvl) {
        this.bindings = bindings;
        this.quoteLevel = lvl;
    }

    public Maybe<LispVal> lookup(Atom id) {
        return bindings.lookup(id).map(Ref::get);
    }

    public Maybe<Ref<LispVal>> lookupRef(Atom id) {
        return bindings.lookup(id);
    }

    public void put(Atom id, LispVal value) {
        bindings = bindings.put(id, new Ref<>(value));
    }

    public Env extend(Seq<Atom> params, Maybe<Atom> vararg, Seq<LispVal> args) {
        PMap<Atom, Ref<LispVal>> env = bindings;

        while (!params.isEmpty()) {
            env = env.put(params.head(), new Ref<>(args.head()));
            params = params.tail();
            args = args.tail();
        }

        if (vararg.isPresent()) {
            env = env.put(vararg.get(), new Ref<>(new LispVal.List(args)));
        }

        return new Env(env);
    }

    public int getQL() {
        return quoteLevel;
    }

    public Env updateQL(int delta) {
        return new Env(bindings, quoteLevel + delta);
    }
}
