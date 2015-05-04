/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.control.monad.trans;

import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;

import com.cloudway.fp.$;
import com.cloudway.fp.control.ForwardingMonad;
import com.cloudway.fp.control.Monad;
import com.cloudway.fp.control.MonadPlus;
import com.cloudway.fp.data.Fn;
import com.cloudway.fp.data.Maybe;
import com.cloudway.fp.data.Seq;
import com.cloudway.fp.data.Tuple;
import com.cloudway.fp.data.Unit;

/**
 * The {@link ListT} monad typeclass definition.
 */
public abstract class ListTC<T, M extends Monad<M>>
    implements MonadPlus<T>, MonadTrans<T, M>
{
    /**
     * The monadic type of {@code ListT}.
     */
    public static abstract class Monadic<T, M extends Monad<M>, A> implements $<T, A> {
        final Supplier<$<M, MList<M, A>>> run;

        protected Monadic(Supplier<$<M, MList<M, A>>> run) {
            this.run = Fn.lazy(run);
        }
    }

    /**
     * Internal representation of a lazy monadic list.
     */
    public static class MList<M extends Monad<M>, A> {
        public boolean isEmpty() {
            return true;
        }

        public A head() {
            throw new NoSuchElementException();
        }

        public $<M, MList<M, A>> tail() {
            throw new NoSuchElementException();
        }

        public <B> MList<M, B> map(Function<? super A, ? extends B> f) {
            return mnil();
        }
    }

    private final M nm;

    /**
     * Construct a {@code ListT} transformer monad typeclass.
     */
    protected ListTC(M nm) {
        this.nm = nm;
    }

    /**
     * Instantiate a new {@code ListT} monad.
     */
    protected <A> $<T, A> $(Supplier<$<M, MList<M, A>>> run) {
        return new Monadic<T, M, A>(run) {
            @Override @SuppressWarnings("unchecked")
            public T getTypeClass() {
                return (T)ListTC.this;
            }
        };
    }

    /**
     * Unwrap the {@code ListT} computation.
     */
    @SuppressWarnings("unchecked")
    public <A> $<M, MList<M, A>> runList($<T, A> m) {
        return ((Monadic<T,M,A>)m).run.get();
    }

    /**
     * A "lazy" run function, which only calculates the first solution.
     */
    public <A> $<M, Maybe<Tuple<A, $<T, A>>>> runList_($<T, A> m) {
        return nm.map(runList(m), xs -> xs.isEmpty()
                                        ? Maybe.empty()
                                        : Maybe.of(Tuple.of(xs.head(), $(xs::tail))));
    }

    /**
     * Reduce the list using the binary operator, from right to left.
     */
    public <A, R> $<M, R> foldList($<T, A> m, R z, BiFunction<? super A, $<M, R>, R> f) {
        return nm.map(runList(m), xs -> xs.isEmpty()
                                        ? z
                                        : f.apply(xs.head(), foldList($(xs::tail), z, f)));
    }

    /**
     * Promote a list into a {@code ListT} monad.
     */
    public <A> $<T, A> liftList(Seq<A> xs) {
        return $(() -> nm.pure(xs.isEmpty()
                               ? mnil()
                               : mcons(xs.head(), () -> runList(liftList(xs.tail())))));
    }

    /**
     * Construct a pure computation that results in the given value.
     */
    @Override
    public <A> $<T, A> pure(A a) {
        return $(() -> nm.pure(singleton(a)));
    }

    /**
     * Lift a lazy value.
     */
    @Override
    public <A> $<T, A> lazy(Supplier<A> a) {
        Supplier<A> t = Fn.lazy(a);
        return $(() -> nm.pure(singleton(t.get())));
    }

    /**
     * Lift an action with side effect.
     */
    @Override
    public $<T, Unit> action(Runnable a) {
        return $(() -> {
            a.run();
            return nm.pure(singleton(Unit.U));
        });
    }

    /**
     * Fail with a message.
     */
    @Override
    public <A> $<T, A> fail(String msg) {
        return $(() -> nm.pure(mnil()));
    }

    /**
     * Promote an inner monad to a {@code ListT} transformer monad.
     */
    @Override
    public <A> $<T, A> lift($<M, A> m) {
        return $(() -> nm.map(m, this::singleton));
    }

    /**
     * Returns the inner monad typeclass.
     */
    @Override
    public M inner() {
        return nm;
    }

    /**
     * Map between {@code ListT} computations.
     */
    public <A, B> $<T, B> mapList($<T, A> m,
            Function<? super $<M, MList<M, A>>, ? extends $<M, MList<M, B>>> f) {
        return $(() -> f.apply(runList(m)));
    }

    /**
     * Transform a {@code ListT} computation by feeding the value to the given
     * function and wrap the result to a new computation.
     */
    @Override
    public <A, B> $<T, B> map($<T, A> m, Function<? super A, ? extends B> f) {
        return $(() -> nm.map(runList(m), xs -> xs.map(f)));
    }

    /**
     * Transform a {@code ListT} computation by feeding the value to the given
     * function and wrap the result to a new computation.
     */
    @Override
    public <A, B> $<T, B> bind($<T, A> m, Function<? super A, ? extends $<T, B>> k) {
        return joinListT(map(m, k));
    }

    /**
     * Returns the monoid identity.
     */
    @Override
    public <A> $<T, A> mzero() {
        return liftList(Seq.nil());
    }

    /**
     * Combines two computations.
     */
    @Override
    public <A> $<T, A> mplus($<T, A> xs, $<T, A> ys) {
        return $(() -> append(runList(xs), runList(ys)));
    }

    // Internals

    private static final class MCons<M extends Monad<M>, A> extends MList<M, A> {
        private final A head;
        private volatile Supplier<$<M, MList<M, A>>> thunk;
        private volatile $<M, MList<M, A>> tail;

        MCons(A head, Supplier<$<M, MList<M, A>>> thunk) {
            this.head = head;
            this.thunk = thunk;
        }

        @Override
        public boolean isEmpty() {
            return false;
        }

        @Override
        public A head() {
            return head;
        }

        @Override
        public $<M, MList<M, A>> tail() {
            if (tail == null)
                force();
            return tail;
        }

        private synchronized void force() {
            if (tail == null) {
                tail = Objects.requireNonNull(thunk.get());
                thunk = null;
            }
        }

        @Override
        public <B> MList<M, B> map(Function<? super A, ? extends B> f) {
            return mcons(f.apply(head()), () -> {
                $<M, MList<M, A>> t = tail();
                return t.getTypeClass().map(t, xs -> xs.map(f));
            });
        }
    }

    private static final MList<?,?> _NIL = new MList<>();

    @SuppressWarnings("unchecked")
    private static <M extends Monad<M>, A> MList<M, A> mnil() {
        return (MList<M,A>)_NIL;
    }

    private static <M extends Monad<M>, A>
    MList<M, A> mcons(A head, Supplier<$<M, MList<M, A>>> tail) {
        return new MCons<>(head, tail);
    }

    private <A> MList<M, A> singleton(A a) {
        return mcons(a, () -> nm.pure(mnil()));
    }

    private <A> $<M, MList<M, A>> append($<M, MList<M, A>> xs, $<M, MList<M, A>> ys) {
        return nm.bind(xs, x -> append_(x, ys));
    }

    private <A> $<M, MList<M, A>> append_(MList<M, A> xs, $<M, MList<M, A>> ys) {
        return xs.isEmpty() ? ys : nm.pure(mcons(xs.head(), () -> append(xs.tail(), ys)));
    }

    private <A> $<T, A> joinListT($<T, $<T, A>> xss) {
        return $(() -> joinMList(nm.map(runList(xss), xs -> xs.map(this::<A>runList))));
    }

    private <A> $<M, MList<M, A>> joinMList($<M, MList<M, $<M, MList<M, A>>>> xss) {
        return nm.bind(xss, this::joinMList_);
    }

    private <A> $<M, MList<M, A>> joinMList_(MList<M, $<M, MList<M, A>>> xss) {
        return xss.isEmpty() ? nm.pure(mnil()) : append(xss.head(), joinMList(xss.tail()));
    }

    // Lifting other operations

    @Override
    @SuppressWarnings("unchecked")
    public <R> MonadReader<T, R> liftReader() {
        MonadReader<M, R> inner;
        if (nm instanceof MonadReader) {
            inner = (MonadReader<M,R>)nm;
        } else if (nm instanceof MonadTrans) {
            inner = (MonadReader<M,R>)((MonadTrans<T,M>)nm).liftReader();
        } else {
            throw new UnsupportedOperationException("liftReader");
        }
        return new LiftReader<>(inner);
    }

    private class LiftReader<R> implements MonadReader<T, R>, ForwardingMonad<T> {
        private final MonadReader<M, R> inner;

        LiftReader(MonadReader<M, R> inner) {
            this.inner = inner;
        }

        @Override
        public Monad<T> delegate() {
            return ListTC.this;
        }

        @Override
        public <A> $<T, A> reader(Function<? super R, ? extends A> f) {
            return lift(inner.reader(f));
        }

        @Override
        public $<T, R> ask() {
            return lift(inner.ask());
        }

        @Override
        public <A> $<T, A> local(Function<R, R> f, $<T, A> m) {
            return mapList(m, v -> inner.local(f, v));
        }
    }


    @Override
    @SuppressWarnings("unchecked")
    public <E> MonadExcept<T, E> liftExcept() {
        MonadExcept<M, E> inner;
        if (nm instanceof MonadExcept) {
            inner = (MonadExcept<M,E>)nm;
        } else if (nm instanceof MonadTrans) {
            inner = (MonadExcept<M,E>)((MonadTrans<T,M>)nm).liftExcept();
        } else {
            throw new UnsupportedOperationException("liftExcept");
        }
        return new LiftExcept<>(inner);
    }

    private class LiftExcept<E> implements MonadExcept<T, E>, ForwardingMonad<T> {
        private final MonadExcept<M, E> inner;

        LiftExcept(MonadExcept<M, E> inner) {
            this.inner = inner;
        }

        @Override
        public Monad<T> delegate() {
            return ListTC.this;
        }

        @Override
        public <A> $<T, A> throwE(E e) {
            return lift(inner.throwE(e));
        }

        /* This (perhaps more straightforward) implementation has the disadvantage
         * that it only catches errors that occur at the first position of the list.
         *
         *      m `catchError` h = ListT $ runListT m `catchError` \e -> runListT (h e)
         *
         * This is better because errors are caught everywhere in the list.
         */
        @Override
        public <A> $<T, A> catchE(Function<? super E, ? extends $<T, A>> h, $<T, A> m) {
            return $(() -> deepCatch(h, runList(m)));
        }

        private <A> $<M, MList<M, A>> deepCatch(Function<? super E, ? extends $<T, A>> h, $<M, MList<M, A>> ml) {
            return inner.catchE(e -> runList(h.apply(e)), nm.map(ml, xs -> deepCatch_(h, xs)));
        }

        private <A> MList<M, A> deepCatch_(Function<? super E, ? extends $<T, A>> h, MList<M, A> xs) {
            return xs.isEmpty() ? xs : mcons(xs.head(), () -> deepCatch(h, xs.tail()));
        }
    }
}
