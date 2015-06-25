/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.scheme;

import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Supplier;

import com.cloudway.fp.data.HashPMap;
import com.cloudway.fp.data.Maybe;
import com.cloudway.fp.data.PMap;
import com.cloudway.fp.data.Ref;
import com.cloudway.fp.data.Seq;

public final class Env implements LispVal {
    private static final class GenSym extends Symbol {
        public GenSym(String name) {
            super(name);
        }
    }

    private final Env outer;
    private final LispVal source;

    private Ref<PMap<Symbol, Ref<?>>> system;
    private Ref<PMap<Symbol, LispVal>> macros;
    private Ref<PMap<Symbol, Ref<LispVal>>> bindings;

    private int quoteLevel;
    private AtomicLong symgen;

    /**
     * Construct a top-level environment.
     */
    public Env() {
        this.outer      = null;
        this.source     = LispVal.VOID;
        this.system     = new Ref<>(HashPMap.empty());
        this.macros     = new Ref<>(HashPMap.empty());
        this.bindings   = new Ref<>(HashPMap.empty());
        this.quoteLevel = 0;
        this.symgen     = new AtomicLong();
    }

    /**
     * Construct an empty environment, later initialized by copy.
     */
    private Env(Env outer, LispVal source) {
        this.outer = outer;
        this.source = source;
    }

    private Env copy(Env outer, LispVal source) {
        Env copy = new Env(outer, source);
        copy.system     = this.system;
        copy.macros     = this.macros;
        copy.bindings   = this.bindings;
        copy.quoteLevel = quoteLevel;
        copy.symgen     = symgen;
        return copy;
    }

    private Env copy() {
        return copy(this, LispVal.VOID);
    }

    public Seq<LispVal> getCallTrace() {
        Seq<LispVal> trace = Seq.nil();
        for (Env env = this; env != null; env = env.outer) {
            if (env.source != LispVal.VOID) {
                trace = Seq.cons(env.source, trace);
            }
        }
        return trace;
    }

    @SuppressWarnings("unchecked")
    public <A> Ref<A> getSystem(Symbol id, A init) {
        Maybe<Ref<?>> slot = system.get().lookup(id);
        if (slot.isAbsent()) {
            Ref<A> new_slot = new Ref<>(init);
            system.update(b -> b.put(id, new_slot));
            return new_slot;
        } else {
            return (Ref<A>)slot.get();
        }
    }

    @SuppressWarnings("unchecked")
    public <A> Ref<A> getSystem(Symbol id, Supplier<? extends A> init) {
        Maybe<Ref<?>> slot = system.get().lookup(id);
        if (slot.isAbsent()) {
            Ref<A> new_slot = new Ref<>(init.get());
            system.update(b -> b.put(id, new_slot));
            return new_slot;
        } else {
            return (Ref<A>)slot.get();
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

    public void putRef(Symbol id, Ref<LispVal> ref) {
        bindings.update(b -> b.put(id, ref));
    }

    public void put(Symbol id, LispVal value) {
        putRef(id, new Ref<>(value));
    }

    public Env extend(Env outer, LispVal source) {
        Env ext = copy(outer, source);
        ext.bindings = new Ref<>(bindings.get());
        ext.macros = new Ref<>(macros.get());
        return ext;
    }

    public Env extend() {
        return extend(this, LispVal.VOID);
    }

    public Env extend(Env outer, LispVal source, PMap<Symbol, Ref<LispVal>> bindings) {
        Env ext = copy(outer, source);
        ext.bindings = new Ref<>(this.bindings.get().putAll(bindings));
        ext.macros = new Ref<>(macros.get());
        return ext;
    }

    public Env extend(PMap<Symbol, Ref<LispVal>> bindings) {
        return extend(this, LispVal.VOID, bindings);
    }

    public int getQL() {
        return quoteLevel;
    }

    public Env incrementQL() {
        Env ext = copy();
        ext.quoteLevel = this.quoteLevel + 1;
        return ext;
    }

    public Env decrementQL() {
        Env ext = copy();
        ext.quoteLevel = this.quoteLevel - 1;
        return ext;
    }

    public Symbol newsym() {
        return newsym("g");
    }

    public Symbol newsym(String prefix) {
        return new GenSym(prefix + symgen.getAndIncrement());
    }

    @Override
    public String show() {
        return "<namespace>";
    }
}
