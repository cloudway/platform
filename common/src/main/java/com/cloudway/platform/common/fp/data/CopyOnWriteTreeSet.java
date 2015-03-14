/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.data;

import java.util.AbstractSet;
import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.function.UnaryOperator;

/**
 * A thread-safe variant of {@link java.util.TreeSet} which backed by a purely
 * functional tree set.
 */
public class CopyOnWriteTreeSet<E> extends AbstractSet<E> implements java.io.Serializable {
    private static final long serialVersionUID = 2614785590714139335L;

    /**
     * The comparator used to maintain order in this tree set.
     *
     * @serial
     */
    private final Comparator<? super E> comparator;

    /**
     * The underlying purely functional tree set that accessed atomically.
     */
    private final transient AtomicReference<TreeSet<E>> tree = new AtomicReference<>();

    /**
     * Returns the backing tree that can be used purely.
     */
    public final TreeSet<E> getTree() {
        return tree.get();
    }

    final boolean updateTree(UnaryOperator<TreeSet<E>> updater) {
        TreeSet<E> prev, next;
        do {
            prev = tree.get();
            next = updater.apply(prev);
        } while (!tree.compareAndSet(prev, next));
        return prev.size() != next.size();
    }

    /**
     * Construct a new, empty tree set, using the natural ordering of its elements.
     * All elements added into the set must implement the {@link Comparable}
     * interface.  Furthermore, all such elements must be <em>mutually comparable</em>:
     * {@code e1.compareTo(e2)} must not throw a {@code ClassCastException} for
     * any elements {@code e1} and {@code e2} in the set.  If the user attempts to
     * add an element into the set that violates this constraint (for example, the
     * user attempts to add a string element into a set whose elements are integers),
     * the {@code add(Object e)} call will throw a {@code ClassCastException}.
     */
    @SuppressWarnings("unchecked")
    public CopyOnWriteTreeSet() {
        this((Comparator<? super E>)Comparator.naturalOrder());
    }

    /**
     * Construct a new, empty tree set, ordered according to the given comparator.
     * All such elements added into the set must be <em>mutually comparable</em>
     * by the given comparator: {@code e1.compareTo(e2)} must not throw a
     * {@code ClassCastException} for any elements {@code e1} and {@code e2} in
     * the set.  If the user attempts to add an element into the set that violates
     * this constraint, the {@code add(Object e)} call will throw a {@code
     * ClassCastException}.
     *
     * @param comparator the comparator that will be used to order this set
     */
    public CopyOnWriteTreeSet(Comparator<? super E> comparator) {
        Objects.requireNonNull(comparator);
        this.comparator = comparator;
        tree.set(TreeSet.empty(comparator));
    }

    @Override
    public boolean isEmpty() {
        return getTree().isEmpty();
    }

    @Override
    public int size() {
        return getTree().size();
    }

    @Override
    @SuppressWarnings("unchecked")
    public boolean contains(Object o) {
        return getTree().contains((E)o);
    }

    @Override
    public boolean add(E e) {
        return updateTree(t -> t.add(e));
    }

    @Override
    @SuppressWarnings("unchecked")
    public boolean remove(Object o) {
        return updateTree(t -> t.remove((E)o));
    }

    @Override
    public boolean removeIf(Predicate<? super E> filter) {
        return updateTree(t -> t.filter(filter.negate()));
    }

    @Override
    public void clear() {
        updateTree(t -> TreeSet.empty(comparator));
    }

    @Override
    public void forEach(Consumer<? super E> action) {
        getTree().forEach(action);
    }

    @Override
    public Iterator<E> iterator() {
        return getTree().toList().iterator();
    }

    @Override
    public boolean containsAll(Collection<?> c) {
        TreeSet<E> s = forTree(c);
        if (s != null) {
            return getTree().containsAll(s);
        } else {
            return super.containsAll(c);
        }
    }

    @Override
    public boolean addAll(Collection<? extends E> c) {
        TreeSet<E> s = forTree(c);
        if (s != null) {
            return updateTree(t -> t.union(s));
        } else {
            return super.addAll(c);
        }
    }

    @Override
    public boolean removeAll(Collection<?> c) {
        TreeSet<E> s = forTree(c);
        if (s != null) {
            return updateTree(t -> t.difference(s));
        } else {
            return super.removeAll(c);
        }
    }

    @Override
    public boolean retainAll(Collection<?> c) {
        TreeSet<E> s = forTree(c);
        if (s != null) {
            return updateTree(t -> t.intersection(s));
        } else {
            return super.retainAll(c);
        }
    }

    @SuppressWarnings("unchecked")
    private TreeSet<E> forTree(Collection<?> c) {
        if (c instanceof CopyOnWriteTreeSet) {
            CopyOnWriteTreeSet<E> s = (CopyOnWriteTreeSet<E>)c;
            if (Objects.equals(this.comparator, s.comparator)) {
                return s.getTree();
            }
        }
        return null;
    }
}
