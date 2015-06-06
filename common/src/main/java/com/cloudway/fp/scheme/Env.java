/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.scheme;

import java.util.concurrent.atomic.AtomicLong;

import com.cloudway.fp.data.HashPMap;
import com.cloudway.fp.data.Maybe;
import com.cloudway.fp.data.PMap;
import com.cloudway.fp.data.Ref;

public final class Env implements LispVal {
    private static final class GenSym extends Symbol {
        public GenSym(String name) {
            super(name);
        }
    }

    private final Ref<PMap<Symbol, Ref<Object>>>  system;
    private final Ref<PMap<Symbol, LispVal>>      macros;
    private final Ref<PMap<Symbol, Ref<LispVal>>> bindings;

    private final int quoteLevel;

    private static final Symbol SYMGEN = new Symbol("%SYMGEN%");

    public Env() {
        this.system   = new Ref<>(HashPMap.empty());
        this.macros   = new Ref<>(HashPMap.empty());
        this.bindings = new Ref<>(HashPMap.empty());
        this.quoteLevel = 0;

        setSystem(SYMGEN, new AtomicLong());
    }

    private Env(
            Ref<PMap<Symbol, Ref<Object>>>  s,
            Ref<PMap<Symbol, LispVal>>      m,
            Ref<PMap<Symbol, Ref<LispVal>>> b,
            int q) {
        this.system     = s;
        this.macros     = m;
        this.bindings   = b;
        this.quoteLevel = q;
    }

    public Ref<Object> getSystem(Symbol id, Object init) {
        Maybe<Ref<Object>> slot = system.get().lookup(id);
        if (slot.isAbsent()) {
            Ref<Object> new_slot = new Ref<>(init);
            system.update(b -> b.put(id, new_slot));
            return new_slot;
        } else {
            return slot.get();
        }
    }

    public void setSystem(Symbol id, Object value) {
        system.update(b -> b.put(id, new Ref<>(value)));
    }

    public Maybe<LispVal> lookupMacro(Symbol id) {
        return macros.get().lookup(id);
    }

    public void putMacro(Symbol id, LispVal macro) {
        macros.update(b -> b.put(id, macro));
    }

    public PMap<Symbol, Ref<LispVal>> getBindings() {
        return bindings.get();
    }

    public Maybe<Ref<LispVal>> lookup(Symbol id) {
        return bindings.get().lookup(id);
    }

    public LispVal get(Symbol id) {
        return bindings.get().get(id).get();
    }

    public void put(Symbol id, LispVal value) {
        bindings.update(b -> b.put(id, new Ref<>(value)));
    }

    public Env extend() {
        return new Env(system, new Ref<>(macros.get()), new Ref<>(bindings.get()), quoteLevel);
    }

    public Env extend(PMap<Symbol, Ref<LispVal>> ext) {
        ext = bindings.get().putAll(ext);
        return new Env(system, new Ref<>(macros.get()), new Ref<>(ext), quoteLevel);
    }

    public int getQL() {
        return quoteLevel;
    }

    public Env incrementQL() {
        return new Env(system, macros, bindings, quoteLevel + 1);
    }

    public Env decrementQL() {
        return new Env(system, macros, bindings, quoteLevel - 1);
    }

    public Symbol newsym() {
        return newsym("g");
    }

    public Symbol newsym(String prefix) {
        AtomicLong symgen = (AtomicLong)getSystem(SYMGEN, null).get();
        return new GenSym(prefix + symgen.getAndIncrement());
    }

    @Override
    public String show() {
        return "<namespace>";
    }
}
