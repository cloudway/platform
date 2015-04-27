/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.data;

/**
 * Mutable sequence builder.
 *
 * {@inheritDoc}
 */
public final class SeqBuilder<T> extends SeqZipperBase<T, SeqBuilder<T>> {
    private SeqBuilder(Seq<T> front, Seq<T> rear) {
        super(front, rear);
    }

    @Override
    protected SeqBuilder<T> zip(Seq<T> front, Seq<T> rear) {
        this.front = front;
        this.rear = rear;
        return this;
    }

    /**
     * Create an empty builder.
     */
    public static <T> SeqBuilder<T> empty() {
        return new SeqBuilder<>(Seq.nil(), Seq.nil());
    }

    /**
     * Returns a builder containing the elements of list, focused on the first
     * element.
     */
    public static <T> SeqBuilder<T> from(Seq<T> xs) {
        return new SeqBuilder<>(Seq.nil(), xs);
    }

    /**
     * Returns a builder containing the elements of list, focused just off the
     * right end of the list.
     */
    public static <T> SeqBuilder<T> fromEnd(Seq<T> xs) {
        return new SeqBuilder<>(xs.reverse(), Seq.nil());
    }

    /**
     * Reconstruct list from builder.
     */
    public Seq<T> build() {
        return front.reverse().append(rear);
    }
}
