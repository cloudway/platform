/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.scheme;

import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;
import java.util.function.Supplier;

import com.cloudway.fp.data.HashPMap;
import com.cloudway.fp.data.Maybe;
import com.cloudway.fp.data.MutablePMap;
import com.cloudway.fp.data.PMap;
import com.cloudway.fp.data.Ref;
import com.cloudway.fp.data.Seq;
import static java.util.Objects.requireNonNull;

public final class Env implements LispVal, Cloneable {
    static final class GenSym extends Symbol {
        public GenSym(String name) {
            super(name);
        }

        public String toString() {
            return "#GenSym('" + name + "')";
        }
    }

    private Env outer;
    private LispVal source;

    private final MutablePMap<Symbol, AtomicReference<?>> system;
    private MutablePMap<Symbol, Ref<LispVal>> bindings;

    private MutablePMap<Symbol, LispVal> macros;
    private MutablePMap<Symbol, Scoped> scopedVars;
    private MutablePMap<Scoped, Symbol> renamedVars;

    private Function<Symbol, LispVal> translator;
    private int quoteLevel;
    private boolean rewriting;

    private final AtomicLong symgen;

    /**
     * Construct a top-level environment.
     */
    public Env() {
        this.outer       = null;
        this.source      = LispVal.VOID;
        this.system      = new MutablePMap<>(HashPMap.empty());
        this.bindings    = new MutablePMap<>(HashPMap.empty());
        this.macros      = new MutablePMap<>(HashPMap.empty());
        this.scopedVars  = new MutablePMap<>(HashPMap.empty());
        this.renamedVars = new MutablePMap<>(HashPMap.empty());
        this.translator  = sym -> sym;
        this.quoteLevel  = 0;
        this.rewriting   = false;
        this.symgen      = new AtomicLong();
    }

    @Override
    protected Env clone() {
        try {
            Env c = (Env)super.clone();
            c.outer = this;
            c.source = VOID;
            return c;
        } catch (CloneNotSupportedException e) {
            throw new InternalError();
        }
    }

    private Env clone(Env outer, LispVal source) {
        Env c = clone();
        c.outer = outer;
        c.source = source;
        return c;
    }

    public Env getOuter() {
        return outer;
    }

    public LispVal getSource() {
        for (Env env = this; env != null; env = env.outer) {
            if (env.source != VOID)
                return env.source;
        }
        return VOID;
    }

    public Seq<LispVal> getCallTrace() {
        Seq<LispVal> trace = Seq.nil();
        for (Env env = this; env != null; env = env.outer) {
            if (env.source != VOID) {
                trace = Seq.cons(env.source, trace);
            }
        }
        return trace;
    }

    @SuppressWarnings("unchecked")
    public <A> AtomicReference<A> getSystem(Symbol id, A init) {
        Maybe<AtomicReference<?>> slot = system.lookup(id);
        if (slot.isAbsent()) {
            AtomicReference<A> new_slot = new AtomicReference<>(init);
            system.put(id, new_slot);
            return new_slot;
        } else {
            return (AtomicReference<A>)slot.get();
        }
    }

    @SuppressWarnings("unchecked")
    public <A> AtomicReference<A> getSystem(Symbol id, Supplier<? extends A> init) {
        Maybe<AtomicReference<?>> slot = system.lookup(id);
        if (slot.isAbsent()) {
            AtomicReference<A> new_slot = new AtomicReference<>(init.get());
            system.put(id, new_slot);
            return new_slot;
        } else {
            return (AtomicReference<A>)slot.get();
        }
    }

    public void setSystem(Symbol id, Object value) {
        system.put(id, new AtomicReference<>(value));
    }

    public Maybe<LispVal> lookupMacro(Symbol id) {
        return macros.lookup(id);
    }

    public void putMacro(Symbol id, LispVal macro) {
        macros.put(id, macro);
    }

    public PMap<Symbol, Ref<LispVal>> getBindings() {
        return bindings.snapshot();
    }

    public Maybe<Ref<LispVal>> lookup(Symbol id) {
        return bindings.lookup(id);
    }

    public LispVal get(Symbol id) {
        return requireNonNull(bindings.get(id)).get();
    }

    public void putRef(Symbol id, Ref<LispVal> ref) {
        bindings.put(id, ref);
    }

    public void put(Symbol id, LispVal value) {
        putRef(id, new Ref<>(value));
    }

    public Env extend() {
        return extend(this, VOID);
    }

    public Env extend(Env outer, LispVal source) {
        Env ext = clone(outer, source);
        ext.bindings = new MutablePMap<>(bindings.snapshot());
        ext.macros = new MutablePMap<>(macros.snapshot());
        return ext;
    }

    public Env extend(Env outer, LispVal source, PMap<Symbol, Ref<LispVal>> bindings) {
        Env ext = clone(outer, source);
        ext.bindings = new MutablePMap<>(getBindings().putAll(bindings));
        ext.macros = new MutablePMap<>(macros.snapshot());
        return ext;
    }

    public Env extend(PMap<Symbol, Ref<LispVal>> bindings) {
        return extend(this, VOID, bindings);
    }

    public Env macroExtend(Env outer, LispVal source) {
        Env ext = extend(outer, source);
        ext.rewriting = true;
        ext.translator = ext::makeScopedVar;
        ext.scopedVars = new MutablePMap<>(HashPMap.empty());
        return ext;
    }

    public Env lexicalExtend() {
        Env ext = clone();
        ext.renamedVars = new MutablePMap<>(this.renamedVars.snapshot());
        return ext;
    }

    private Scoped makeScopedVar(Symbol sym) {
        return scopedVars.computeIfAbsent(sym, k -> new Scoped(this, sym));
    }

    public Symbol rename(LispVal var) {
        if (var instanceof Scoped) {
            return renamedVars.computeIfAbsent((Scoped)var, k -> newsym(k.getSymbolName()));
        } else {
            return var.getSymbol();
        }
    }

    public Symbol rename(LispVal var, Symbol sym) {
        if (var instanceof Scoped) {
            renamedVars.putIfAbsent((Scoped)var, sym);
        }
        return sym;
    }

    public boolean isRenamed(LispVal var) {
        return (var instanceof Scoped) && renamedVars.containsKey(var);
    }

    public Symbol getRenamed(LispVal var) {
        if (var instanceof Scoped) {
            return renamedVars.getOrDefault(var, var.getSymbol());
        } else {
            return var.getSymbol();
        }
    }

    public int getQL() {
        return quoteLevel;
    }

    public Env incrementQL() {
        Env ext = clone();
        ext.quoteLevel = this.quoteLevel + 1;
        return ext;
    }

    public Env decrementQL() {
        Env ext = clone();
        ext.quoteLevel = this.quoteLevel - 1;
        return ext;
    }

    public Env disableRewrite() {
        Env ext = clone();
        ext.rewriting = false;
        return ext;
    }

    public LispVal rewrite(Symbol sym) {
        if (quoteLevel == 1 && rewriting) {
            return translator.apply(sym);
        } else {
            return sym;
        }
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
