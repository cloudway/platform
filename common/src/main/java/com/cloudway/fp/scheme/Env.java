/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.scheme;

import com.cloudway.fp.data.Maybe;
import com.cloudway.fp.data.PMap;
import com.cloudway.fp.data.Ref;
import com.cloudway.fp.scheme.LispVal.Symbol;

public final class Env {
    private final Ref<PMap<Symbol, Ref<LispVal>>> bindings;
    private final int quoteLevel;

    public Env(PMap<Symbol, Ref<LispVal>> bindings) {
        this.bindings = new Ref<>(bindings);
        this.quoteLevel = 0;
    }

    private Env(Ref<PMap<Symbol, Ref<LispVal>>> bindings, int lvl) {
        this.bindings = bindings;
        this.quoteLevel = lvl;
    }

    public PMap<Symbol, Ref<LispVal>> getBindings() {
        return bindings.get();
    }

    public Maybe<LispVal> lookup(Symbol id) {
        return bindings.get().lookup(id).map(Ref::get);
    }

    public Maybe<Ref<LispVal>> lookupRef(Symbol id) {
        return bindings.get().lookup(id);
    }

    public void put(Symbol id, LispVal value) {
        bindings.update(b -> b.put(id, new Ref<>(value)));
    }

    public Env extend(PMap<Symbol, Ref<LispVal>> bindings) {
        return new Env(this.bindings.get().putAll(bindings));
    }

    public int getQL() {
        return quoteLevel;
    }

    public Env incrementQL() {
        return new Env(bindings, quoteLevel + 1);
    }

    public Env decrementQL() {
        return new Env(bindings, quoteLevel - 1);
    }
}
