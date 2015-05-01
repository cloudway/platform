/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.data;

import java.util.Arrays;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.function.BiFunction;
import java.util.function.Predicate;
import java.util.function.Supplier;

@SuppressWarnings("EqualsAndHashcode")
final class TSetImpl {
    private TSetImpl() {}

    private static final Empty<?> EMPTY = new Empty<>();

    @SuppressWarnings("unchecked")
    static <K> TSet<K> empty() {
        return (TSet<K>)EMPTY;
    }

    static <K> TSet<K> singleton(K k) {
        return new Singleton<>(k, hash(k));
    }

    static int hash(Object key) {
        int h = key.hashCode();
        return h ^ (h >>> 16);
    }

    interface Traverser<K> {
        Traverser<K> succ();
        K cursor();
    }

    static class SetIterator<K> implements Iterator<K> {
        private Traverser<K> current;

        SetIterator(TSet<K> t) {
            this.current = t.traverser();
        }

        @Override
        public boolean hasNext() {
            return current != null;
        }

        @Override
        public K next() {
            if (current == null)
                throw new NoSuchElementException();
            K result = current.cursor();
            current = current.succ();
            return result;
        }
    }

    static abstract class TSet<K> implements HashPSet<K> {
        abstract boolean contains0(Object key, int hash, int level);
        abstract boolean subsetOf(TSet<K> that, int level);
        abstract TSet<K> add0(K key, int hash, int level);
        abstract TSet<K> remove0(Object key, int hash, int level);
        abstract TSet<K> filter0(Predicate<? super K> p, int level, TSet<K>[] buffer, int offset0);
        abstract TSet<K> union0(TSet<K> that, int level);
        abstract TSet<K> intersect0(TSet<K> that, int level, TSet<K>[] buffer, int offset0);
        abstract TSet<K> diff0(TSet<K> that, int level, TSet<K>[] buffer, int offset0);
        abstract Traverser<K> traverser();

        @Override
        public boolean contains(Object key) {
            return contains0(key, hash(key), 0);
        }

        @Override
        @SuppressWarnings("unchecked")
        public boolean containsAll(PSet<? extends K> that) {
            if (size() < that.size()) {
                return false;
            } else if (that instanceof TSet) {
                return ((TSet<K>)that).subsetOf(this, 0);
            } else {
                return that.allMatch(this::contains);
            }
        }

        @Override
        public PSet<K> add(K key) {
            return add0(key, hash(key), 0);
        }

        @Override
        public PSet<K> remove(Object key) {
            return remove0(key, hash(key), 0);
        }

        @Override
        public PSet<K> clear() {
            return TSetImpl.empty();
        }

        @SuppressWarnings("unchecked")
        private static <K> TSet<K>[] newBuffer(int minSize) {
            int bufferSize = Math.min(minSize + 6, 32 * 7);
            return new TSet[bufferSize];
        }

        @Override
        public PSet<K> filter(Predicate<? super K> p) {
            TSet<K>[] buffer = newBuffer(size());
            return filter0(p, 0, buffer, 0);
        }

        @Override
        public PSet<K> union(PSet<K> that) {
            return union0((TSet<K>)that, 0);
        }

        @Override
        public PSet<K> intersection(PSet<K> that) {
            TSet<K>[] buffer = newBuffer(Math.min(this.size(), that.size()));
            return intersect0((TSet<K>)that, 0, buffer, 0);
        }

        @Override
        public PSet<K> difference(PSet<K> that) {
            TSet<K>[] buffer = newBuffer(this.size());
            return diff0((TSet<K>)that, 0, buffer, 0);
        }

        @Override
        public Iterator<K> iterator() {
            return new SetIterator<>(this);
        }

        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (!(obj instanceof TSet))
                return false;
            @SuppressWarnings("unchecked")
            TSet<K> s = (TSet<K>)obj;
            return size() == s.size() && subsetOf(s, 0);
        }

        public abstract int hashCode();

        public String toString() {
            return show(", ", "{", "}");
        }
    }

    static <K> List<K> cons(K h, List<K> t) {
        return new List<>(h, t);
    }

    static class List<K> implements Traverser<K> {
        final K key;
        final List<K> next;

        List(K key, List<K> next) {
            this.key = key;
            this.next = next;
        }

        int size() {
            int sz = 0;
            List<K> p = this;
            while (p != null) {
                sz++;
                p = p.next;
            }
            return sz;
        }

        List<K> find(Predicate<? super K> pred) {
            List<K> p = this;
            while (p != null) {
                if (pred.test(p.key))
                    return p;
                p = p.next;
            }
            return null;
        }

        boolean contains(Object key) {
            return find(k -> k.equals(key)) != null;
        }

        boolean any(Predicate<? super K> pred) {
            return find(pred) != null;
        }

        boolean all(Predicate<? super K> pred) {
            return find(pred.negate()) == null;
        }

        private List<K> consTo(List<K> end, List<K> res) {
            List<K> p = this;
            while (p != end) {
                res = cons(p.key, res);
                p = p.next;
            }
            return res;
        }

        List<K> delete(Object key) {
            List<K> t = find(k -> k.equals(key));
            return t == null ? this : consTo(t, t.next);
        }

        List<K> filter(Predicate<? super K> pred) {
            List<K> t = find(pred.negate());
            if (t == null) {
                return this;
            } else {
                List<K> r = consTo(t, null);
                while ((t = t.next) != null) {
                    if (pred.test(t.key)) {
                        r = cons(t.key, r);
                    }
                }
                return r;
            }
        }

        <R> R foldl(R z, BiFunction<R, ? super K, R> f) {
            List<K> p = this;
            while (p != null) {
                z = f.apply(z, p.key);
                p = p.next;
            }
            return z;
        }

        <R> R foldr(BiFunction<? super K, Supplier<R>, R> f, Supplier<R> r) {
            if (next == null) {
                return f.apply(key, r);
            } else {
                return f.apply(key, () -> next.foldr(f, r));
            }
        }

        @Override
        public Traverser<K> succ() {
            return next;
        }

        @Override
        public K cursor() {
            return key;
        }

        List<K> force() {
            List<K> p = this;
            while (p != null) {
                Forcible.force(key);
                p = p.next;
            }
            return this;
        }
    }

    static class Empty<K> extends TSet<K> {
        @Override
        public boolean isEmpty() {
            return true;
        }

        @Override
        public int size() {
            return 0;
        }

        @Override
        boolean contains0(Object key, int hash, int level) {
            return false;
        }

        @Override
        public Maybe<K> find(Predicate<? super K> p) {
            return Maybe.empty();
        }

        @Override
        boolean subsetOf(TSet<K> that, int level) {
            return true;
        }

        @Override
        TSet<K> add0(K key, int hash, int level) {
            return new Singleton<>(key, hash);
        }

        @Override
        TSet<K> remove0(Object key, int hash, int level) {
            return this;
        }

        @Override
        TSet<K> filter0(Predicate<? super K> p, int level, TSet<K>[] buffer, int offset0) {
            return this;
        }

        @Override
        TSet<K> union0(TSet<K> that, int level) {
            return that;
        }

        @Override
        TSet<K> intersect0(TSet<K> that, int level, TSet<K>[] buffer, int offset0) {
            return this;
        }

        @Override
        TSet<K> diff0(TSet<K> that, int level, TSet<K>[] buffer, int offset0) {
            return this;
        }

        @Override
        public <R> R foldLeft(R z, BiFunction<R, ? super K, R> f) {
            return z;
        }

        @Override
        public <R> R foldRight(BiFunction<? super K, Supplier<R>, R> f, Supplier<R> r) {
            return r.get();
        }

        @Override
        public Traverser<K> traverser() {
            return null;
        }

        @Override
        public PSet<K> force() {
            return this;
        }

        @Override
        public int hashCode() {
            return 0;
        }
    }

    static class Singleton<K> extends TSet<K> implements Traverser<K> {
        private final K key;
        private final int hash;

        Singleton(K key, int hash) {
            this.key = key;
            this.hash = hash;
        }

        @Override
        public boolean isEmpty() {
            return false;
        }

        @Override
        public int size() {
            return 1;
        }

        @Override
        boolean contains0(Object key, int hash, int level) {
            return this.hash == hash && this.key.equals(key);
        }

        @Override
        public Maybe<K> find(Predicate<? super K> p) {
            return p.test(key) ? Maybe.of(key) : Maybe.empty();
        }

        @Override
        boolean subsetOf(TSet<K> that, int level) {
            return that.contains0(key, hash, level);
        }

        @Override
        TSet<K> add0(K key, int hash, int level) {
            if (this.hash != hash) {
                TSet<K> that = new Singleton<>(key, hash);
                return Trie.make(this.hash, this, hash, that, level, 2);
            } else if (this.key.equals(key)) {
                return this;
            } else {
                return new Collision<>(hash, cons(this.key, cons(key, null)));
            }
        }

        @Override
        TSet<K> remove0(Object key, int hash, int level) {
            return this.hash == hash && this.key.equals(key) ? TSetImpl.empty() : this;
        }

        @Override
        TSet<K> filter0(Predicate<? super K> p, int level, TSet<K>[] buffer, int offset0) {
            return p.test(key) ? this : TSetImpl.empty();
        }

        @Override
        TSet<K> union0(TSet<K> that, int level) {
            return that.add0(key, hash, level);
        }

        @Override
        TSet<K> intersect0(TSet<K> that, int level, TSet<K>[] buffer, int offset0) {
            return that.contains0(key, hash, level) ? this : TSetImpl.empty();
        }

        @Override
        TSet<K> diff0(TSet<K> that, int level, TSet<K>[] buffer, int offset0) {
            return that.contains0(key, hash, level) ? TSetImpl.empty() : this;
        }

        @Override
        public <R> R foldLeft(R z, BiFunction<R, ? super K, R> f) {
            return f.apply(z, key);
        }

        @Override
        public <R> R foldRight(BiFunction<? super K, Supplier<R>, R> f, Supplier<R> r) {
            return f.apply(key, r);
        }

        @Override
        public Traverser<K> traverser() {
            return this;
        }

        @Override
        public Traverser<K> succ() {
            return null;
        }

        @Override
        public K cursor() {
            return key;
        }

        @Override
        public PSet<K> force() {
            Forcible.force(key);
            return this;
        }

        @Override
        public int hashCode() {
            return hash;
        }
    }

    static class Collision<K> extends TSet<K> {
        private final int hash;
        private final List<K> keys;
        private final int size;

        Collision(int hash, List<K> keys) {
            this.hash = hash;
            this.keys = keys;
            this.size = keys.size();
        }

        @Override
        public boolean isEmpty() {
            return false;
        }

        @Override
        public int size() {
            return size;
        }

        @Override
        boolean contains0(Object key, int hash, int level) {
            return hash == this.hash && keys.contains(key);
        }

        @Override
        public Maybe<K> find(Predicate<? super K> p) {
            List<K> t = keys.find(p);
            return t != null ? Maybe.of(t.key) : Maybe.empty();
        }

        @Override
        boolean subsetOf(TSet<K> that, int level) {
            return size() <= that.size() && keys.all(k -> that.contains0(k, hash, level));
        }

        @Override
        TSet<K> add0(K key, int hash, int level) {
            if (hash == this.hash) {
                if (keys.contains(key)) {
                    return this;
                } else {
                    return new Collision<>(hash, cons(key, keys));
                }
            } else {
                TSet<K> that = new Singleton<>(key, hash);
                return Trie.make(this.hash, this, hash, that, level, size + 1);
            }
        }

        @Override
        TSet<K> remove0(Object key, int hash, int level) {
            if (hash == this.hash) {
                return afterRemove(keys.delete(key));
            } else {
                return this;
            }
        }

        @Override
        TSet<K> filter0(Predicate<? super K> p, int level, TSet<K>[] buffer, int offset0) {
            return afterRemove(keys.filter(p));
        }

        @Override
        TSet<K> union0(TSet<K> that, int level) {
            return keys.foldl(that, (s, k) -> s.add0(k, hash, level));
        }

        @Override
        TSet<K> intersect0(TSet<K> that, int level, TSet<K>[] buffer, int offset0) {
            return afterRemove(keys.filter(k -> that.contains0(k, hash, level)));
        }

        @Override
        TSet<K> diff0(TSet<K> that, int level, TSet<K>[] buffer, int offset0) {
            return afterRemove(keys.filter(k -> !that.contains0(k, hash, level)));
        }

        private TSet<K> afterRemove(List<K> keys) {
            if (keys == null) {
                return TSetImpl.empty();
            } else if (keys == this.keys) {
                return this;
            } else if (keys.next == null) {
                return new Singleton<>(keys.key, hash);
            } else {
                return new Collision<>(hash, keys);
            }
        }

        @Override
        public <R> R foldLeft(R z, BiFunction<R, ? super K, R> f) {
            return keys.foldl(z, f);
        }

        @Override
        public <R> R foldRight(BiFunction<? super K, Supplier<R>, R> f, Supplier<R> r) {
            return keys.foldr(f, r);
        }

        @Override
        public Traverser<K> traverser() {
            return keys;
        }

        @Override
        public PSet<K> force() {
            keys.force();
            return this;
        }

        @Override
        public int hashCode() {
            return hash * 31 + size;
        }
    }

    static class Trie<K> extends TSet<K> {
        static <K> Trie<K> make(int hash0, TSet<K> elem0, int hash1, TSet<K> elem1, int level, int size) {
            int index0 = (hash0 >>> level) & 0x1f;
            int index1 = (hash1 >>> level) & 0x1f;
            if (index0 != index1) {
                @SuppressWarnings("unchecked")
                TSet<K> elems[] = new TSet[2];
                int bitmap = (1 << index0) | (1 << index1);
                if (index0 < index1) {
                    elems[0] = elem0;
                    elems[1] = elem1;
                } else {
                    elems[0] = elem1;
                    elems[1] = elem0;
                }
                return new Trie<>(bitmap, elems, size);
            } else {
                @SuppressWarnings("unchecked")
                TSet<K> elems[] = new TSet[1];
                int bitmap = (1 << index0);
                elems[0] = make(hash0, elem0, hash1, elem1, level + 5, size);
                return new Trie<>(bitmap, elems, size);
            }
        }

        private final int bitmap;
        private final TSet<K> elems[];
        private final int size;

        Trie(int bitmap, TSet<K> elems[], int size) {
            this.bitmap = bitmap;
            this.elems = elems;
            this.size = size;
        }

        @Override
        public boolean isEmpty() {
            return false;
        }

        @Override
        public int size() {
            return size;
        }

        @Override
        boolean contains0(Object key, int hash, int level) {
            TSet<K> sub = this;
            do {
                Trie<K> trie = (Trie<K>)sub;
                int bits = trie.bitmap;
                int index = (hash >>> level) & 0x1f;
                int mask = (1 << index);
                if (bits == -1) {
                    sub = trie.elems[index];
                } else if ((bits & mask) != 0) {
                    sub = trie.elems[Integer.bitCount(bits & (mask-1))];
                } else {
                    return false;
                }
                level += 5;
            } while (sub instanceof Trie);
            return sub.contains0(key, hash, level);
        }

        @Override
        public Maybe<K> find(Predicate<? super K> p) {
            for (TSet<K> el : elems) {
                Maybe<K> k = el.find(p);
                if (k.isPresent())
                    return k;
            }
            return Maybe.empty();
        }

        @Override
        @SuppressWarnings("unchecked")
        TSet<K> add0(K key, int hash, int level) {
            int index = (hash >>> level) & 0x1f;
            int mask = (1 << index);
            int offset = Integer.bitCount(bitmap & (mask-1));
            if ((bitmap & mask) != 0) {
                TSet<K> sub = elems[offset];
                TSet<K> subNew = sub.add0(key, hash, level + 5);
                if (subNew == sub) {
                    return this;
                } else {
                    TSet<K> elemsNew[] = elems.clone();
                    elemsNew[offset] = subNew;
                    return new Trie<>(bitmap, elemsNew, size + (subNew.size() - sub.size()));
                }
            } else {
                TSet<K> elemsNew[] = new TSet[elems.length + 1];
                System.arraycopy(elems, 0, elemsNew, 0, offset);
                elemsNew[offset] = new Singleton<>(key, hash);
                System.arraycopy(elems, offset, elemsNew, offset + 1, elems.length - offset);
                return new Trie<>(bitmap | mask, elemsNew, size + 1);
            }
        }

        @Override
        @SuppressWarnings("unchecked")
        TSet<K> remove0(Object key, int hash, int level) {
            int index = (hash >>> level) & 0x1f;
            int mask = (1 << index);
            int offset = Integer.bitCount(bitmap & (mask-1));
            if ((bitmap & mask) != 0) {
                TSet<K> sub = elems[offset];
                TSet<K> subNew = sub.remove0(key, hash, level + 5);
                if (subNew == sub) {
                    return this;
                } else if (subNew.isEmpty()) {
                    int bitmapNew = bitmap ^ mask;
                    if (bitmapNew != 0) {
                        TSet<K> elemsNew[] = new TSet[elems.length - 1];
                        System.arraycopy(elems, 0, elemsNew, 0, offset);
                        System.arraycopy(elems, offset + 1, elemsNew, offset, elems.length - offset - 1);
                        int sizeNew = size - sub.size();
                        if (elemsNew.length == 1 && !(elemsNew[0] instanceof Trie)) {
                            return elemsNew[0];
                        } else {
                            return new Trie<>(bitmapNew, elemsNew, sizeNew);
                        }
                    } else {
                        return TSetImpl.empty();
                    }
                } else if (elems.length == 1 && !(subNew instanceof Trie)) {
                    return subNew;
                } else {
                    TSet<K> elemsNew[] = elems.clone();
                    elemsNew[offset] = subNew;
                    int sizeNew = size + (subNew.size() - sub.size());
                    return new Trie<>(bitmap, elemsNew, sizeNew);
                }
            } else {
                return this;
            }
        }

        @Override
        TSet<K> filter0(Predicate<? super K> p, int level, TSet<K>[] buffer, int offset0) {
            int offset = offset0;
            int rs = 0;
            int bm = bitmap;
            int rbm = 0;

            for (TSet<K> el : elems) {
                TSet<K> result = el.filter0(p, level + 5, buffer, offset);
                int lsb = bm ^ (bm & (bm - 1));
                if (!result.isEmpty()) {
                    buffer[offset++] = result;
                    rs += result.size();
                    rbm |= lsb;
                }
                bm &= ~lsb;
            }

            if (offset == offset0) {
                return TSetImpl.empty();
            } else if (rs == this.size) {
                return this;
            } else if (offset == offset0 + 1 && !(buffer[offset0] instanceof Trie)) {
                return buffer[offset0];
            } else {
                int length = offset - offset0;
                @SuppressWarnings("unchecked")
                TSet<K>[] elems = new TSet[length];
                System.arraycopy(buffer, offset0, elems, 0, length);
                return new Trie<>(rbm, elems, rs);
            }
        }

        @Override
        boolean subsetOf(TSet<K> that, int level) {
            if (this.size() > that.size()) {
                return false;
            } else if (that instanceof Trie) {
                return subset2((Trie<K>)that, level);
            } else {
                return allMatch(k -> that.contains0(k, hash(k), level));
            }
        }

        private boolean subset2(Trie<K> that, int level) {
            TSet<K>[] xs = this.elems;
            TSet<K>[] ys = that.elems;
            int xbm = this.bitmap;
            int ybm = that.bitmap;

            if ((xbm & ybm) != xbm) {
                return false;
            }

            int ix = 0, iy = 0;
            while (ix < xs.length && iy < ys.length) {
                int xlsb = xbm ^ (xbm & (xbm - 1));
                int ylsb = ybm ^ (ybm & (ybm - 1));
                if (xlsb == ylsb) {
                    if (!xs[ix].subsetOf(ys[iy], level + 5))
                        return false;
                    xbm &= ~xlsb;
                    ybm &= ~ylsb;
                    ix++;
                    iy++;
                } else if (unsignedCompare(xlsb - 1, ylsb - 1)) {
                    return false;
                } else {
                    ybm &= ~ylsb;
                    iy++;
                }
            }
            return true;
        }

        @Override
        TSet<K> union0(TSet<K> that, int level) {
            if (that == this) {
                return this;
            } else if (that.isEmpty()) {
                return this;
            } else if (that instanceof Trie) {
                return union2((Trie<K>)that, level);
            } else {
                return that.union0(this, level);
            }
        }

        private TSet<K> union2(Trie<K> that, int level) {
            TSet<K>[] xs = this.elems;
            TSet<K>[] ys = that.elems;
            int xbm = this.bitmap;
            int ybm = that.bitmap;

            // determine the necessary size for the array
            int subcount = Integer.bitCount(xbm | ybm);

            // construct a new array of appropriate size
            @SuppressWarnings("unchecked")
            TSet<K>[] buffer = new TSet[subcount];
            int totalsize = 0;

            // run through both bitmaps and add elements to it
            int ix = 0, iy = 0;
            for (int i = 0; i < subcount; i++) {
                int xlsb = xbm ^ (xbm & (xbm - 1));
                int ylsb = ybm ^ (ybm & (ybm - 1));
                if (xlsb == ylsb) {
                    TSet<K> sub = xs[ix++].union0(ys[iy++], level + 5);
                    buffer[i] = sub;
                    totalsize += sub.size();
                    xbm &= ~xlsb;
                    ybm &= ~ylsb;
                } else if (unsignedCompare(xlsb - 1, ylsb - 1)) {
                    TSet<K> sub = xs[ix++];
                    buffer[i] = sub;
                    totalsize += sub.size();
                    xbm &= ~xlsb;
                } else {
                    TSet<K> sub = ys[iy++];
                    buffer[i] = sub;
                    totalsize += sub.size();
                    ybm &= ~ylsb;
                }
            }
            return new Trie<>(this.bitmap | that.bitmap, buffer, totalsize);
        }

        @Override
        TSet<K> intersect0(TSet<K> that, int level, TSet<K>[] buffer, int offset0) {
            if (that == this) {
                return this;
            } else if (that.isEmpty()) {
                return that;
            } else if (that instanceof Trie) {
                return intersect2((Trie<K>)that, level, buffer, offset0);
            } else {
                return that.intersect0(this, level, buffer, offset0);
            }
        }

        private TSet<K> intersect2(Trie<K> that, int level, TSet<K>[] buffer, int offset0) {
            TSet<K>[] xs = this.elems;
            TSet<K>[] ys = that.elems;
            int xbm = this.bitmap;
            int ybm = that.bitmap;

            // if the bitmasks do not overlap, the result is definitely empty
            if ((xbm & ybm) == 0) {
                return TSetImpl.empty();
            }

            int offset = offset0;
            int rbm = 0, rs = 0;
            int ix = 0, iy = 0;
            while ((xbm & ybm) != 0) {
                int xlsb = xbm ^ (xbm & (xbm - 1));
                int ylsb = ybm ^ (ybm & (ybm - 1));
                if (xlsb == ylsb) {
                    TSet<K> sub = xs[ix++].intersect0(ys[iy++], level + 5, buffer, offset);
                    if (!sub.isEmpty()) {
                        buffer[offset++] = sub;
                        rs += sub.size();
                        rbm |= xlsb;
                    }
                    xbm &= ~xlsb;
                    ybm &= ~ylsb;
                } else if (unsignedCompare(xlsb - 1, ylsb - 1)) {
                    ix++;
                    xbm &= ~xlsb;
                } else {
                    iy++;
                    ybm &= ~ylsb;
                }
            }

            if (rbm == 0) {
                return TSetImpl.empty();
            } else if (rs == this.size) {
                return this;
            } else if (rs == that.size) {
                return that;
            } else if (offset == offset0 + 1 && !(buffer[offset0] instanceof Trie)) {
                return buffer[offset0];
            } else {
                int length = offset - offset0;
                @SuppressWarnings("unchecked")
                TSet<K>[] elems = new TSet[length];
                System.arraycopy(buffer, offset0, elems, 0, length);
                return new Trie<>(rbm, elems, rs);
            }
        }

        @Override
        TSet<K> diff0(TSet<K> that, int level, TSet<K>[] buffer, int offset0) {
            if (that == this) {
                return TSetImpl.empty();
            } else if (that.isEmpty()) {
                return this;
            } else if (that instanceof Trie) {
                return diff2((Trie<K>)that, level, buffer, offset0);
            } else {
                return that.foldLeft((TSet<K>)this, (s, k) -> s.remove0(k, hash(k), level));
            }
        }

        private TSet<K> diff2(Trie<K> that, int level, TSet<K>[] buffer, int offset0) {
            TSet<K>[] xs = this.elems;
            TSet<K>[] ys = that.elems;
            int xbm = this.bitmap;
            int ybm = that.bitmap;

            int offset = offset0;
            int rbm = 0, rs = 0;
            int ix = 0, iy = 0;
            while (xbm != 0) {
                int xlsb = xbm ^ (xbm & (xbm - 1));
                int ylsb = ybm ^ (ybm & (ybm - 1));
                if (xlsb == ylsb) {
                    TSet<K> sub = xs[ix++].diff0(ys[iy++], level + 5, buffer, offset);
                    if (!sub.isEmpty()) {
                        buffer[offset++] = sub;
                        rs += sub.size();
                        rbm |= xlsb;
                    }
                    xbm &= ~xlsb;
                    ybm &= ~ylsb;
                } else if (unsignedCompare(xlsb - 1, ylsb - 1)) {
                    TSet<K> sub = xs[ix++];
                    buffer[offset++] = sub;
                    rs += sub.size();
                    rbm |= xlsb;
                    xbm &= ~xlsb;
                } else {
                    iy++;
                    ybm &= ~ylsb;
                }
            }

            if (rbm == 0) {
                return TSetImpl.empty();
            } else if (rs == this.size) {
                return this;
            } else if (offset == offset0 + 1 && !(buffer[offset0] instanceof Trie)) {
                return buffer[offset0];
            } else {
                int length = offset - offset0;
                @SuppressWarnings("unchecked")
                TSet<K>[] elems = new TSet[length];
                System.arraycopy(buffer, offset0, elems, 0, length);
                return new Trie<>(rbm, elems, rs);
            }
        }

        @Override
        public <R> R foldLeft(R z, BiFunction<R, ? super K, R> f) {
            for (TSet<K> elem : elems) {
                z = elem.foldLeft(z, f);
            }
            return z;
        }

        @Override
        public <R> R foldRight(BiFunction<? super K, Supplier<R>, R> f, Supplier<R> r) {
            return foldr(0, f, r);
        }

        <R> R foldr(int i, BiFunction<? super K, Supplier<R>, R> f, Supplier<R> r) {
            if (i == elems.length - 1) {
                return elems[i].foldRight(f, r);
            } else {
                return elems[i].foldRight(f, () -> foldr(i+1, f, r));
            }
        }

        @Override
        public Traverser<K> traverser() {
            return new TrieTraverser<>(this, null).first();
        }

        static class TrieTraverser<K> implements Traverser<K> {
            private final Trie<K> trie;
            private final TrieTraverser<K> parent;
            private Traverser<K> current;
            private int index;

            TrieTraverser(Trie<K> trie, TrieTraverser<K> parent) {
                this.trie = trie;
                this.parent = parent;
            }

            private Traverser<K> first() {
                return first(trie.elems[0]);
            }

            private Traverser<K> first(TSet<K> t) {
                if (t instanceof Trie) {
                    return new TrieTraverser<>((Trie<K>)t, this).first();
                } else {
                    current = t.traverser();
                    return this;
                }
            }

            @Override
            public Traverser<K> succ() {
                if (current != null && (current = current.succ()) != null) {
                    return this;
                } else if (++index < trie.elems.length) {
                    return first(trie.elems[index]);
                } else if (parent != null) {
                    return parent.succ();
                } else {
                    return null;
                }
            }

            @Override
            public K cursor() {
                return current.cursor();
            }
        }

        @Override
        public PSet<K> force() {
            for (TSet<K> elem : elems) {
                elem.force();
            }
            return this;
        }

        @Override
        public int hashCode() {
            return Arrays.hashCode(elems);
        }

        private static boolean unsignedCompare(int i, int j) {
            return (i < j) ^ (i < 0) ^ (j < 0);
        }
    }
}
