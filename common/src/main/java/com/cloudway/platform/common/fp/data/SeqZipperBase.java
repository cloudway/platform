/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.data;

/**
 * Sequence zipper with O(1) get element at cursor, insert at cursor,
 * delete at cursor, move right, and move left operations.
 */
public abstract class SeqZipperBase<T, Z extends SeqZipperBase<T, Z>> {
    protected /*final*/ Seq<T> front, rear;

    protected SeqZipperBase(Seq<T> front, Seq<T> rear) {
        this.front = front;
        this.rear = rear;
    }

    protected abstract Z zip(Seq<T> front, Seq<T> rear);

    @SuppressWarnings("unchecked")
    private Z thiz() { return (Z)this; }

    /**
     * Returns true if the zipper is at the start position.
     */
    public boolean atBegin() {
        return front.isEmpty();
    }

    /**
     * Returns true if the zipper is at the end position. It is not safe to
     * call {@link #get()} on zipper if {@code atEnd} returns true.
     */
    public boolean atEnd() {
        return rear.isEmpty();
    }

    /**
     * Returns true if the zipper is completely empty.
     */
    public boolean isEmpty() {
        return front.isEmpty() && rear.isEmpty();
    }

    /**
     * Returns the targeted element in current position.  This method is not
     * total, but the invariant is that {@code atEnd() == false} means that
     * you can safely call this method.
     */
    public T get() {
        return rear.head();
    }

    /**
     * Like {@link #get()} but total.
     */
    public Maybe<T> peek() {
        return rear.peek();
    }

    /**
     * Returns the zipper with focus moved to start.
     */
    public Z start() {
        return atBegin() ? thiz() : zip(Seq.nil(), front.reverse().append(rear));
    }

    /**
     * Returns the zipper with focus moved to end.
     */
    public Z end() {
        return atEnd() ? thiz() : zip(rear.reverse().append(front), Seq.nil());
    }

    /**
     * Returns the zipper with the focus shifted left one element.
     */
    public Z left() {
        return atBegin() ? thiz() : zip(front.tail(), Seq.cons(front.head(), rear));
    }

    /**
     * Returns the zipper with the focus shifted right one element.
     */
    public Z right() {
        return atEnd() ? thiz() : zip(Seq.cons(rear.head(), front), rear.tail());
    }

    /**
     * Insert element at the cursor.
     */
    public Z insert(T a) {
        return zip(front, Seq.cons(a, rear));
    }

    /**
     * Removes the element at the cursor (if any). Safe to call on an empty
     * zipper.
     */
    public Z delete() {
        return atEnd() ? thiz() : zip(front, rear.tail());
    }

    /**
     * Remove all the elements from the begin to the cursor.
     */
    public Z deleteBefore() {
        return zip(Seq.nil(), rear);
    }

    /**
     * Remove all the elements from the cursor to the end.
     */
    public Z deleteAfter() {
        return zip(front, Seq.nil());
    }

    /**
     * Remove all the element in the zipper.
     */
    public Z clear() {
        return zip(Seq.nil(), Seq.nil());
    }

    /**
     * Changes the current element in the zipper to the given value.  If there
     * is no current element, the zipper is unchanged.  If you want to add the
     * element in that case instead, use {@code z.delete().insert(a)}.
     */
    public Z replace(T a) {
        return atEnd() ? thiz() : zip(front, Seq.cons(a, rear.tail()));
    }

    /**
     * Inserts the element into the zipper, and advances the cursor past it.
     */
    public Z push(T a) {
        return zip(Seq.cons(a, front), rear);
    }

    /**
     * Removes the element before the cursor (if any). Safe to call on an empty
     * zipper.
     */
    public Z pop() {
        return atBegin() ? thiz() : zip(front.tail(), rear);
    }

    /**
     * Returns the zipper with the elements in the reverse order. O(1).  The
     * cursor is moved to the previous element, so if the cursor was at the
     * start, it's now off the right end, and if it was off the right end, it's
     * now at the start of the reversed list.
     */
    public Z reverse() {
        return zip(rear, front);
    }
}
