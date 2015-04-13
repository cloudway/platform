/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.data;

import java.io.IOException;
import java.util.AbstractList;
import java.util.Collection;
import java.util.ConcurrentModificationException;
import java.util.Deque;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.function.UnaryOperator;

import com.cloudway.platform.common.fp.control.Cont;
import com.cloudway.platform.common.fp.io.IO;

import static com.cloudway.platform.common.fp.control.Cont.generator;
import static com.cloudway.platform.common.fp.control.Cont.yield;

public class MutableVector<A> extends AbstractList<A>
    implements Deque<A>, java.io.Serializable
{
    /**
     * The underlying persistent sequence that accessed atomically.
     */
    private transient AtomicReference<Vector<A>> pure = new AtomicReference<>();

    /**
     * Construct an empty mutable vector.
     */
    public MutableVector() {
        this(Vector.empty());
    }

    /**
     * Construct a mutable vector from a persistent vector.
     */
    public MutableVector(Vector<A> init) {
        pure.set(Objects.requireNonNull(init));
    }

    /**
     * Returns the backing persistent vector that can be used purely.
     */
    public final Vector<A> snapshot() {
        return pure.get();
    }

    private boolean update(UnaryOperator<Vector<A>> updater) {
        Vector<A> prev, next;
        do {
            prev = pure.get();
            next = updater.apply(prev);
        } while (!pure.compareAndSet(prev, next));
        return prev.size() != next.size();
    }

    /**
     * Returns the first element in this list.
     *
     * @return the first element in this list
     * @throws NoSuchElementException if this list is empty
     */
    @Override
    public A getFirst() {
        return snapshot().head();
    }

    /**
     * Returns the last element in this list.
     *
     * @return the last element in this list
     * @throws NoSuchElementException if this list is empty
     */
    @Override
    public A getLast() {
        return snapshot().last();
    }

    /**
     * Removes and returns the first element from this list.
     *
     * @return the first element from this list
     * @throws NoSuchElementException if this list is empty
     */
    @Override
    public A removeFirst() {
        Ref<A> first = new Ref<>();
        update(v -> {
            first.set(v.head());
            return v.tail();
        });
        return first.get();
    }

    /**
     * Removes and returns the last element from this list.
     *
     * @return the last element from this list
     * @throws NoSuchElementException if this list is empty
     */
    @Override
    public A removeLast() {
        Ref<A> last = new Ref<>();
        update(v -> {
            last.set(v.last());
            return v.init();
        });
        return last.get();
    }

    /**
     * Inserts the specified element at the beginning of this list.
     *
     * @param a the element to add
     */
    @Override
    public void addFirst(A a) {
        update(v -> v.cons(a));
    }

    /**
     * Appends the specified element to the end of this list.
     *
     * @param a the element to add
     */
    @Override
    public void addLast(A a) {
        update(v -> v.snoc(a));
    }

    /**
     * Returns {@code true} if this list contains the specified element.
     *
     * @param o element whose presence in this list is to be tested
     * @return {@code true} if this list contains the specified element
     */
    @Override
    public boolean contains(Object o) {
        return snapshot().contains(o);
    }

    /**
     * Returns the number of elements in this list.
     *
     * @return the number of elements in this list
     */
    @Override
    public int size() {
        return snapshot().size();
    }

    /**
     * Return {@code true} if this list contains no elements.
     *
     * @return {@code true} if this list contains no elements
     */
    @Override
    public boolean isEmpty() {
        return snapshot().isEmpty();
    }

    /**
     * Appends the specified element to the end of this list.
     *
     * <p>This method is equivalent to {@link #addLast}.
     *
     * @param a element to be appended to this list
     * @return {@code true}
     */
    @Override
    public boolean add(A a) {
        addLast(a);
        return true;
    }

    /**
     * Removes the first occurrence of the specified element from this list,
     * if it is present. If this list does not contain the element, it is
     * unchanged.
     *
     * @param o element to be removed from this list, if present
     * @return {@code true} if this list contained the specified element
     */
    @Override
    public boolean remove(Object o) {
        return update(v -> {
            int i;
            return (i = v.indexOf(o)) == -1 ? v : v.delete(i);
        });
    }

    // Bulk Modification Operations

    /**
     * Returns <tt>true</tt> if this list contains all of the elements of the
     * specified collection.
     *
     * @param  c collection to be checked for containment in this list
     * @return <tt>true</tt> if this list contains all of the elements of the
     *         specified collection
     */
    @Override
    public boolean containsAll(Collection<?> c) {
        Objects.requireNonNull(c);
        return snapshot().allMatch(c::contains);
    }

    /**
     * Appends all of the elements in the specified collection to the end of this
     * list, in the order that they are returned by the specified collection's
     * iterator. The behavior of this operation is undefined if the specified
     * collection is modified while the operation is in progress.
     *
     * @param c collection containing elements to be added to this list
     * @return {@code true} if this list changed as a result of the call
     * @throws NullPointerException if the specified collection is null
     */
    @Override
    public boolean addAll(Collection<? extends A> c) {
        Objects.requireNonNull(c);
        if (c.isEmpty())
            return false;
        Vector<A> u = fromCollection(c);
        update(v -> v.append(u));
        return true;
    }

    /**
     * Inserts all of the elements in the specified collection into this
     * list, starting at the specified position.  Shifts the element
     * currently at that position (if any) and any subsequent elements to
     * the right (increases their indices).  The new elements will appear
     * in the list in the order that they are returned by the
     * specified collection's iterator.
     *
     * @param index index at which to insert the first element
     * from the specified collection
     * @param c collection containing elements to be added to this list
     * @return {@code true} if this list changed as a result of the call
     * @throws IndexOutOfBoundsException {@inheritDoc}
     * @throws NullPointerException if the specified collection is null
     */
    @Override
    public boolean addAll(int index, Collection<? extends A> c) {
        Objects.requireNonNull(c);
        if (c.isEmpty())
            return false;
        Vector<A> u = fromCollection(c);
        update(v -> v.splitAt(index).as((l, r) -> l.append(u).append(r)));
        return true;
    }

    @SuppressWarnings({"unchecked", "SuspiciousArrayCast"})
    private Vector<A> fromCollection(Collection<? extends A> c) {
        if (c instanceof MutableVector) {
            return ((MutableVector<A>)c).snapshot();
        } else {
            return Vector.of((A[])c.toArray());
        }
    }

    /**
     * Removes from this list all of its elements that are contained in the
     * specified collection (optional operation).
     *
     * @param c collection containing elements to be removed from this list
     * @return <tt>true</tt> if this list changed as a result of the call
     */
    @Override
    public boolean removeAll(Collection<?> c) {
        Objects.requireNonNull(c);
        return update(v -> v.filter(a -> !c.contains(a)));
    }

    /**
     * Removes all of the elements of this list that satisfy the given
     * predicate.  Errors or runtime exceptions thrown during iteration or by
     * the predicate are relayed to the caller.
     *
     * @param filter a predicate which returns {@code true} for elements to be
     * removed
     * @return {@code true} if any elements were removed
     * @throws NullPointerException if the specified filter is null
     */
    @Override
    public boolean removeIf(Predicate<? super A> filter) {
        Objects.requireNonNull(filter);
        return update(v -> v.filter(filter.negate()));
    }

    /**
     * Retains only the elements in this list that are contained in the
     * specified collection.  In other words, removes from this list all
     * of its elements that are not contained in the specified collection.
     *
     * @param c collection containing elements to be retained in this list
     * @return <tt>true</tt> if this list changed as a result of the call
     */
    @Override
    public boolean retainAll(Collection<?> c) {
        Objects.requireNonNull(c);
        return update(v -> v.filter(c::contains));
    }

    /**
     * Replaces each element of this list with the result of applying the
     * operator to that element.  Errors or runtime exceptions thrown by
     * the operator are relayed to the caller.
     *
     * @param operator the operator to apply to each element
     * @throws NullPointerException if the specified operator is null
     */
    @Override
    public void replaceAll(UnaryOperator<A> operator) {
        Objects.requireNonNull(operator);
        update(v -> v.map(operator));
    }

    /**
     * Removes all of the elements from this list.
     * The list will be empty after this call returns.
     */
    @Override
    public void clear() {
        pure.set(Vector.empty());
    }

    // Positional Access Operations

    /**
     * Returns the element at the specified position in this list.
     *
     * @param index index of the element to return
     * @return the element at the specified position in this list
     * @throws IndexOutOfBoundsException {@inheritDoc}
     */
    @Override
    public A get(int index) {
        return snapshot().at(index);
    }

    /**
     * Replaces the element at the specified position in this list with the
     * specified element.
     *
     * @param index index of the element to replace
     * @param element element to be stored at the specified position
     * @return the element previously at the specified position
     * @throws IndexOutOfBoundsException {@inheritDoc}
     */
    @Override
    public A set(int index, A element) {
        Ref<A> oldVal = new Ref<>();
        update(v -> v.modify(index, a -> {
            oldVal.set(a);
            return element;
        }));
        return oldVal.get();
    }

    /**
     * Inserts the specified element at the specified position in this list.
     * Shifts the element currently at that position (if any) and any subsequent
     * element to the right (adds one to their indices).
     *
     * @param index index at which the specified element is to be inserted
     * @param element element to be inserted
     * @throws IndexOutOfBoundsException {@inheritDoc}
     */
    @Override
    public void add(int index, A element) {
        update(v -> v.insert(index, element));
    }

    /**
     * Removes the element at the specified position in this list.  Shifts any
     * subsequent elements to the left (subtracts one from their indices).
     * Returns the element that was removed from the list.
     *
     * @param index the index of the element to be removed
     * @return the element previously at the specified position
     * @throws IndexOutOfBoundsException {@inheritDoc}
     */
    @Override
    public A remove(int index) {
        Ref<A> oldVal = new Ref<>();
        update(v -> {
            oldVal.set(v.at(index));
            return v.delete(index);
        });
        return oldVal.get();
    }

    // Search Operations

    /**
     * Returns the index of the first occurrence of the specified element
     * in this list, or -1 if this list does not contain the element.
     *
     * @param o element to search for
     * @return the index of the first occurrence of the specified element in
     * this list, or -1 if this list does not contain the element
     */
    @Override
    public int indexOf(Object o) {
        return snapshot().indexOf(o);
    }

    /**
     * Returns the index of the last occurrence of the specified element
     * in this list, or -1 if this list does not contain the element.
     *
     * @param o element to search for
     * @return the index of the last occurrence of the specified element in
     * this list, or -1 if this list does not contain the element
     */
    @Override
    public int lastIndexOf(Object o) {
        return snapshot().lastIndexOf(o);
    }

    // Queue Operations

    /**
     * Retrieves, but does not remove, the head (first element) of this list.
     *
     * @return the head of this list, or {@code null} if this list is empty
     */
    @Override
    public A peek() {
        return peekFirst();
    }

    /**
     * Retrieves, but does not remove, the head (first element) of this list.
     *
     * @return the head of this list
     * @throws NoSuchElementException if this list is empty
     */
    @Override
    public A element() {
        return getFirst();
    }

    /**
     * Retrieves and removes the head (first element) of this list.
     *
     * @return the head of this list, or {@code null} if this list is empty
     */
    @Override
    public A poll() {
        return pollFirst();
    }

    /**
     * Retrieves and removes the head (first element) of this list.
     *
     * @return the head of this list
     * @throws NoSuchElementException if this list is empty
     */
    @Override
    public A remove() {
        return removeFirst();
    }

    /**
     * Adds the specified element as the tail (last element) of this list.
     *
     * @param a the element to add
     * @return {@inheritDoc}
     */
    @Override
    public boolean offer(A a) {
        return add(a);
    }

    // Dequeue Operations

    /**
     * Inserts the specified element at the front of this list.
     *
     * @param a the element to insert
     * @return {@inheritDoc}
     */
    @Override
    public boolean offerFirst(A a) {
        addFirst(a);
        return true;
    }

    /**
     * Inserts the specified element at the end of this list.
     *
     * @param a the element to insert
     * @return {@inheritDoc}
     */
    @Override
    public boolean offerLast(A a) {
        addLast(a);
        return true;
    }

    /**
     * Retrieves, but does not remove, the first element of this list,
     * or returns {@code null} if this list is empty.
     *
     * @return the first element of this list, or {@code null}
     * if this list is empty
     */
    @Override
    public A peekFirst() {
        Vector<A> v = snapshot();
        return v.isEmpty() ? null : v.head();
    }

    /**
     * Retrieves, but does not remove, the last element of this list,
     * or returns {@code null} if this list is empty.
     *
     * @return the last element of this list, or {@code null}
     * if this list is empty
     */
    @Override
    public A peekLast() {
        Vector<A> v = snapshot();
        return v.isEmpty() ? null : v.last();
    }

    /**
     * Retrieves and removes the first element of this list,
     * or returns {@code null} if this list is empty.
     *
     * @return the first element of this list, or {@code null} if
     * this list is empty
     */
    @Override
    public A pollFirst() {
        Ref<A> first = new Ref<>();
        update(v -> {
            if (v.isEmpty()) {
                return v;
            } else {
                first.set(v.head());
                return v.tail();
            }
        });
        return first.get();
    }

    /**
     * Retrieves and removes the last element of this list,
     * or returns {@code null} if this list is empty.
     *
     * @return the last element of this list, or {@code null} if
     * this list is empty
     */
    @Override
    public A pollLast() {
        Ref<A> last = new Ref<>();
        update(v -> {
            if (v.isEmpty()) {
                return v;
            } else {
                last.set(v.last());
                return v.init();
            }
        });
        return last.get();
    }

    /**
     * Pushes an element onto the stack represented by this list.  In other
     * words, inserts the element at the front of this list.
     *
     * <p>This method is equivalent to {@link #addFirst}.
     *
     * @param a the element to push
     */
    @Override
    public void push(A a) {
        addFirst(a);
    }

    /**
     * Pops an element from the stack represented by this list.  In other
     * words, removes and returns the first element of this list.
     *
     * <p>This method is equivalent to {@link #removeFirst()}.
     *
     * @return the element at the front of this list (which is the top
     * of the stack represented by this list)
     * @throws NoSuchElementException if this list is empty
     */
    @Override
    public A pop() {
        return removeFirst();
    }

    /**
     * Removes the first occurrence of the specified element in this
     * list (when traversing the list from head to tail).  If the list
     * does not contain the element, it is unchanged.
     *
     * @param o element to be removed from this list, if present
     * @return {@code true} if the list contained the specified element
     */
    @Override
    public boolean removeFirstOccurrence(Object o) {
        return remove(o);
    }

    /**
     * Removes the last occurrence of the specified element in this
     * list (when traversing the list from head to tail).  If the list
     * does not contain the element, it is unchanged.
     *
     * @param o element to be removed from this list, if present
     * @return {@code true} if the list contained the specified element
     */
    @Override
    public boolean removeLastOccurrence(Object o) {
        return update(v -> {
            int i;
            return (i = v.lastIndexOf(o)) == -1 ? v : v.delete(i);
        });
    }

    @Override
    public Iterator<A> iterator() {
        Vector<A> v = snapshot();
        return new Itr(v, v.iterator(), 0, 1);
    }

    @Override
    public Iterator<A> descendingIterator() {
        Vector<A> v = snapshot();
        Iterator<A> i = generator(v.foldLeft((r, x) -> yield(x).then(r), Cont::<A>finish)).iterator();
        return new Itr(v, i, v.size()-1, -1);
    }

    private class Itr implements Iterator<A> {
        Vector<A> vec;
        Iterator<A> it;
        int cursor, incr;
        int lastRet = -1;

        Itr(Vector<A> vec, Iterator<A> it, int cursor, int incr) {
            this.vec = vec;
            this.it = it;
            this.cursor = cursor;
            this.incr = incr;
        }

        @Override
        public boolean hasNext() {
            return it.hasNext();
        }

        @Override
        public A next() {
            int i = cursor;
            A next = it.next();
            lastRet = i;
            cursor = i + incr;
            return next;
        }

        @Override
        public void remove() {
            if (lastRet < 0)
                throw new IllegalStateException();
            Vector<A> mod = vec.delete(lastRet);
            if (!pure.compareAndSet(vec, mod))
                throw new ConcurrentModificationException();
            vec = mod;
            if (lastRet < cursor)
                cursor--;
            lastRet = -1;
        }
    }

    @Override
    public void forEach(Consumer<? super A> action) {
        snapshot().forEach(action);
    }

    @Override
    public Object[] toArray() {
        Vector<A> v = snapshot();
        Object[] r = new Object[v.size()];
        return v.foldLeftWithIndex_(r, (a, i, x) -> {
            a[i] = x;
            return a;
        });
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T[] toArray(T[] t) {
        Vector<A> v = snapshot();
        int size = v.size();
        T[] r = t.length >= size ? t :
                (T[])java.lang.reflect.Array
                .newInstance(t.getClass().getComponentType(), size);
        return v.foldLeftWithIndex_(r, (a, i, x) -> {
            a[i] = (T)x;
            return a;
        });
    }

    @Override
    public String toString() {
        return snapshot().toString();
    }

    private static final long serialVersionUID = -2406628057417424622L;

    /**
     * Saves the state of this {@code MutableVector} instance to a stream
     * (that is, serialize it).
     *
     * @serialData The size of the list (the number of elements it
     *             contains) is emitted (int), followed by all of its
     *             elements (each an Object) in the proper order.
     */
    private void writeObject(java.io.ObjectOutputStream s)
        throws IOException {
        Vector<A> v = snapshot();

        // Write out any hidden serialization magic
        s.defaultWriteObject();

        // Write out size
        s.writeInt(v.size());

        // Write out all elements in the proper order
        IO.forEach(v, s::writeObject);
    }

    /**
     * Reconstitutes this {@code MutableVector} instance from a stream
     * (that is, deserializes it).
     */
    @SuppressWarnings("unchecked")
    private void readObject(java.io.ObjectInputStream s)
        throws IOException, ClassNotFoundException {
        // Read in any hidden serialization magic
        s.defaultReadObject();

        // Read in size
        int size = s.readInt();

        // Read in all elements in the proper order
        try {
            Vector<A> v = Vector.generate(size, () -> {
                try {
                    return (A)s.readObject();
                } catch (Exception ex) {
                    throw new UncheckedException(ex);
                }
            });
            pure = new AtomicReference<>(v);
        } catch (UncheckedException ex) {
            ex.rethrow();
        }
    }

    @SuppressWarnings("serial")
    private static final class UncheckedException extends RuntimeException {
        UncheckedException(Exception cause) {
            super(cause);
        }

        void rethrow() throws IOException, ClassNotFoundException {
            Throwable cause = getCause();
            if (cause instanceof IOException)
                throw (IOException)cause;
            if (cause instanceof ClassNotFoundException)
                throw (ClassNotFoundException)cause;
            if (cause instanceof RuntimeException)
                throw (RuntimeException)cause;
            if (cause instanceof Error)
                throw (Error)cause;
            throw new Error(cause);
        }
    }
}
