/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.io;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Stream;

import com.cloudway.platform.common.fp.control.Conditionals;
import com.cloudway.platform.common.fp.control.trans.MonadIO;
import com.cloudway.platform.common.fp.control.TrampolineIO;
import com.cloudway.platform.common.fp.data.Foldable;
import com.cloudway.platform.common.fp.data.PMap;
import com.cloudway.platform.common.fp.data.Seq;
import com.cloudway.platform.common.fp.data.Traversable;
import com.cloudway.platform.common.fp.data.Unit;
import com.cloudway.platform.common.fp.control.Monad;
import com.cloudway.platform.common.fp.$;

/**
 * A value of type IO is a computation which, when performed, does some I/O
 * before returning a value.
 *
 * <p>{@code IO} is a monad, so IO actions can be combined using either the
 * comprehension or the {@link IO#bind(Function)} operations from the Monad
 * class.</p>
 *
 * @param <A> the type of computation value
 */
@FunctionalInterface
public interface IO<A> extends $<IO.µ, A> {
    /**
     * Run the I/O action.
     *
     * @return the result of I/O action
     * @throws IOException if I/O error occurred
     */
    A runIO() throws IOException;

    /**
     * Run the I/O action, wraps IOException to an UncheckedIOException.
     *
     * @return the result of I/O action
     * @throws UncheckedIOException if I/O error occurred
     */
    default A runUncheckedIO() throws UncheckedIOException {
        try {
            return runIO();
        } catch (IOException ex) {
            throw new UncheckedIOException(ex);
        }
    }

    /**
     * Wraps a pure value to an I/O action.
     *
     * @param a the pure value
     * @return an I/O action that wrapping the pure value
     */
    static <A> IO<A> pure(A a) {
        return () -> a;
    }

    /**
     * The do nothing I/O action.
     */
    IO<Unit> unit = pure(Unit.U);

    /**
     * Maps the I/O action result with the given function.
     *
     * @param f a function that maps the I/O action result
     * @return a new I/O action that return the result of mapping
     */
    default <B> IO<B> map(Function<? super A, ? extends B> f) {
        return () -> f.apply(runIO());
    }

    /**
     * Binds the I/O action result with the given function.
     *
     * @param f a function that bind the I/O action result
     * @return a new I/O action that returns the result of binding
     */
    default <B> IO<B> bind(Function<? super A, ? extends $<µ, B>> f) {
        return () -> narrow(f.apply(runIO())).runIO();
    }

    /**
     * Sequentially perform I/O actions.
     *
     * @param other the another I/O action that performed after this I/O action
     * @return a new I/O action that sequentially perform I/O actions
     */
    default <B> IO<B> then($<µ, B> other) {
        return () -> { runIO(); return narrow(other).runIO(); };
    }

    /**
     * Sequentially perform I/O actions.
     *
     * @param other the another I/O action that performed after this I/O action
     * @return a new I/O action that sequentially perform I/O actions
     */
    default <B> IO<B> then(Supplier<? extends $<µ, B>> other) {
        return () -> { runIO(); return narrow(other.get()).runIO(); };
    }

    // Monad

    interface µ extends Monad<µ>, MonadIO<µ> {
        @Override
        default <A> IO<A> liftIO($<IO.µ, A> m) {
            return narrow(m);
        }

        @Override
        default <A> IO<A> pure(A a) {
            return IO.pure(a);
        }

        @Override
        default <A, B> IO<B> map($<µ, A> a, Function<? super A, ? extends B> f) {
            return narrow(a).map(f);
        }

        @Override
        default <A, B> IO<B> bind($<µ, A> a, Function<? super A, ? extends $<µ, B>> k) {
            return narrow(a).bind(k);
        }

        @Override
        default <A, B> IO<B> seqR($<µ, A> a, $<µ, B> b) {
            return narrow(a).then(b);
        }

        @Override
        default <A, B> IO<B> seqR($<µ, A> a, Supplier<? extends $<µ, B>> b) {
            return narrow(a).then(b);
        }

        @Override
        default <A> IO<Unit> sequence(Foldable<? extends $<µ, A>> ms) {
            return ms.foldRight(TrampolineIO.unit(), (x, r) ->
                TrampolineIO.lift(narrow(x)).then(r)).run();
        }

        @Override
        default <A, B> IO<Unit> mapM_(Foldable<A> xs, Function<? super A, ? extends $<µ, B>> f) {
            return xs.foldRight(TrampolineIO.unit(), (x, r) ->
                TrampolineIO.lift(narrow(f.apply(x))).then(r)).run();
        }

        @Override
        default <A> IO<Seq<A>> filterM(Seq<A> xs, Function<? super A, ? extends $<µ, Boolean>> p) {
            return TrampolineIO.filterM(xs, x ->
                TrampolineIO.lift(narrow(p.apply(x)))).run();
        }

        @Override
        default <A, B> IO<B> foldM(B r0, Foldable<A> xs, BiFunction<B, ? super A, ? extends $<µ, B>> f) {
            return TrampolineIO.foldM(r0, xs, (m, x) ->
                TrampolineIO.lift(narrow(f.apply(m, x)))).run();
        }
    }

    µ tclass = new µ() {};

    @Override
    default µ getTypeClass() {
        return tclass;
    }

    static <A> IO<A> narrow($<µ, A> value) {
        return (IO<A>)value;
    }

    static <A> A runIO($<µ, A> m) throws IOException {
        return narrow(m).runIO();
    }

    static <A> A runUncheckedIO($<µ, A> m) {
        return narrow(m).runUncheckedIO();
    }

    // Convenient static monad methods

    static <T, A> IO<? extends Traversable<T, A>>
    flatM(Traversable<T, ? extends $<µ, A>> ms) {
        return narrow(tclass.flatM(ms));
    }

    @SuppressWarnings("unchecked")
    static <A> IO<Seq<A>> flatM(Seq<? extends $<µ, A>> ms) {
        return (IO<Seq<A>>)tclass.flatM(ms);
    }

    static <T, A, B> IO<? extends Traversable<T, B>>
    mapM(Traversable<T, A> xs, Function<? super A, ? extends $<µ, B>> f) {
        return narrow(tclass.mapM(xs, f));
    }

    @SuppressWarnings("unchecked")
    static <A, B> IO<Seq<B>>
    mapM(Seq<A> xs, Function<? super A, ? extends $<µ, B>> f) {
        return (IO<Seq<B>>)tclass.mapM(xs, f);
    }

    static <A> IO<Unit> sequence(Foldable<? extends $<µ, A>> ms) {
        return narrow(tclass.sequence(ms));
    }

    static <A, B> IO<Unit>
    mapM_(Foldable<A> xs, Function<? super A, ? extends $<µ, B>> f) {
        return narrow(tclass.mapM_(xs, f));
    }

    static <A> IO<Seq<A>>
    filterM(Seq<A> xs, Function<? super A, ? extends $<µ, Boolean>> p) {
        return narrow(tclass.filterM(xs, p));
    }

    static <A, B> IO<B>
    foldM(B r0, Foldable<A> xs, BiFunction<B, ? super A, ? extends $<µ, B>> f) {
        return narrow(tclass.foldM(r0, xs, f));
    }

    static <A> IO<Seq<A>> replicateM(int n, $<µ, A> a) {
        return narrow(tclass.replicateM(n, a));
    }

    static <A> IO<Unit> replicateM_(int n, $<µ, A> a) {
        return narrow(tclass.replicateM_(n, a));
    }

    // The naive methods that work around Java lambda exception handling

    /**
     * Promote a native I/O function to an I/O action function.
     */
    static <A, B> Function<A, IO<B>> lift(IOFunction<? super A, ? extends B> f) {
        return x -> () -> f.evaluate(x);
    }

    /**
     * Promote a native I/O function to an I/O action function.
     */
    static <A> Function<A, IO<Unit>> lift_(IOConsumer<? super A> f) {
        return x -> (VoidIO)() -> f.consume(x);
    }

    static <T> T run(IOSupplier<? extends T> action) throws IOException {
        try {
            return action.produce();
        } catch (UncheckedIOException ex) {
            throw ex.getCause();
        }
    }

    static void run_(IOAction action) throws IOException {
        try {
            action.perform();
        } catch (UncheckedIOException ex) {
            throw ex.getCause();
        }
    }

    static <T> void forEach(Stream<T> stream, IOConsumer<? super T> action)
        throws IOException
    {
        try {
            stream.forEach(IOConsumer.wrap(action));
        } catch (UncheckedIOException ex) {
            throw ex.getCause();
        }
    }

    static <T> void forEach(Iterable<T> collection, IOConsumer<? super T> action)
        throws IOException
    {
        try {
            collection.forEach(IOConsumer.wrap(action));
        } catch (UncheckedIOException ex) {
            throw ex.getCause();
        }
    }

    static <K, V> void forEach(Map<K, V> map, IOBiConsumer<? super K, ? super V> action)
        throws IOException
    {
        try {
            map.forEach(IOBiConsumer.wrap(action));
        } catch (UncheckedIOException ex) {
            throw ex.getCause();
        }
    }

    static <K, V> void forEach(PMap<K, V> map, IOBiConsumer<? super K, ? super V> action)
        throws IOException
    {
        try {
            map.forEach(IOBiConsumer.wrap(action));
        } catch (UncheckedIOException ex) {
            throw ex.getCause();
        }
    }

    static Conditionals.ActionBrancher<IOException> with() {
        return Conditionals.actionBrancher();
    }

    static <T> Conditionals.ActionSwitcher<T, IOException> with(T value) {
        return Conditionals.actionSwitcher(value);
    }

    static <T, U> Conditionals.BiActionSwitcher<T, U, IOException> with(T t, U u) {
        return Conditionals.biActionSwitcher(t, u);
    }
}
