/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.data;

/**
 * Immutable sequence zipper.
 *
 * {@inheritDoc}
 */
public final class SeqZipper<T> extends SeqZipperBase<T, SeqZipper<T>> {
    private SeqZipper(Seq<T> front, Seq<T> rear) {
        super(front, rear);
    }

    @Override
    protected SeqZipper<T> zip(Seq<T> front, Seq<T> rear) {
        return new SeqZipper<>(front, rear);
    }

    /**
     * Create an empty zipper.
     */
    public static <T> SeqZipper<T> empty() {
        return new SeqZipper<>(Seq.nil(), Seq.nil());
    }

    /**
     * Returns a zipper containing the elements of list, focused on the first
     * element.
     */
    public static <T> SeqZipper<T> from(Seq<T> xs) {
        return new SeqZipper<>(Seq.nil(), xs);
    }

    /**
     * Returns a zipper containing the elements of list, focused just off the
     * right end of the list.
     */
    public static <T> SeqZipper<T> fromEnd(Seq<T> xs) {
        return new SeqZipper<>(xs.reverse(), Seq.nil());
    }

    /**
     * Reconstruct list from zipper.
     */
    public Seq<T> toList() {
        return front.reverse().append(rear);
    }

    /**
     * Returns the front list.
     */
    public Seq<T> front() {
        return front.reverse();
    }

    /**
     * Returns the rear list.
     */
    public Seq<T> rear() {
        return rear;
    }
}
