/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.data;

import java.util.AbstractSet;
import java.util.Collection;
import java.util.Iterator;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.function.UnaryOperator;

public class MutablePSet<E> extends AbstractSet<E> implements java.io.Serializable {
    private static final long serialVersionUID = 3642011599101526916L;

    /**
     * The underlying persistent set that accessed atomically.
     */
    private final transient AtomicReference<PSet<E>> pure = new AtomicReference<>();

    /**
     * Construct a mutable set from a persistent set.
     */
    public MutablePSet(PSet<E> init) {
        pure.set(Objects.requireNonNull(init));
    }

    /**
     * Returns the backing persistent set that can be used purely.
     */
    public final PSet<E> snapshot() {
        return pure.get();
    }

    private boolean update(UnaryOperator<PSet<E>> updater) {
        PSet<E> prev, next;
        do {
            prev = pure.get();
            next = updater.apply(prev);
        } while (!pure.compareAndSet(prev, next));
        return prev.size() != next.size();
    }

    @Override
    public boolean isEmpty() {
        return snapshot().isEmpty();
    }

    @Override
    public int size() {
        return snapshot().size();
    }

    @Override
    public boolean contains(Object o) {
        return snapshot().contains(o);
    }

    @Override
    public boolean add(E e) {
        return update(t -> t.add(e));
    }

    @Override
    public boolean remove(Object o) {
        return update(t -> t.remove(o));
    }

    @Override
    public void clear() {
        update(PSet::clear);
    }

    @Override
    public void forEach(Consumer<? super E> action) {
        snapshot().forEach(action);
    }

    @Override
    public Iterator<E> iterator() {
        return snapshot().iterator();
    }

    @Override
    public boolean containsAll(Collection<?> c) {
        PSet<E> s = pureFor(c);
        if (s != null) {
            return snapshot().containsAll(s);
        } else {
            return super.containsAll(c);
        }
    }

    @Override
    public boolean addAll(Collection<? extends E> c) {
        PSet<E> s = pureFor(c);
        if (s != null) {
            return update(t -> t.union(s));
        } else {
            return super.addAll(c);
        }
    }

    @Override
    public boolean removeAll(Collection<?> c) {
        PSet<E> s = pureFor(c);
        if (s != null) {
            return update(t -> t.difference(s));
        } else {
            return super.removeAll(c);
        }
    }

    @Override
    public boolean retainAll(Collection<?> c) {
        PSet<E> s = pureFor(c);
        if (s != null) {
            return update(t -> t.intersection(s));
        } else {
            return super.retainAll(c);
        }
    }

    protected PSet<E> pureFor(Collection<?> c) {
        if (c instanceof MutablePSet) {
            @SuppressWarnings("unchecked")
            MutablePSet<E> s = (MutablePSet<E>)c;
            return s.snapshot();
        }
        return null;
    }
}
