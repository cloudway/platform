/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.fp.control;

import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.Optional;
import java.util.function.BooleanSupplier;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

import com.cloudway.platform.common.fp.function.ExceptionAction;
import com.cloudway.platform.common.fp.function.ExceptionBiConsumer;
import com.cloudway.platform.common.fp.function.ExceptionBiFunction;
import com.cloudway.platform.common.fp.function.ExceptionConsumer;
import com.cloudway.platform.common.fp.function.ExceptionFunction;
import com.cloudway.platform.common.fp.function.ExceptionSupplier;

public final class Conditionals {
    private Conditionals() {}

    /**
     * Create a chained fluent conditional execution.
     */
    public static ActionBrancher<RuntimeException> with() {
        return actionBrancher();
    }

    /**
     * Create a chained fluent conditional execution for evaluating on given value.
     */
    public static <T> ActionSwitcher<T, RuntimeException> with(T value) {
        return actionSwitcher(value);
    }

    /**
     * Create a chained fluent conditional execution for evaluating on given values.
     */
    public static <T,U> BiActionSwitcher<T, U, RuntimeException> with(T t, U u) {
        return biActionSwitcher(t, u);
    }

    /**
     * A shortcut for single condition pattern matching action.
     *
     * @param <T> the type of input argument
     * @param value the input argument
     * @param cond the pattern to be matched
     */
    public static <T> void do__(T value, ConditionCase<? super T, ?, RuntimeException> cond) {
        ExceptionSupplier<?, RuntimeException> sup = cond.evaluate(value);
        if (sup != null) {
            sup.produce();
        }
    }

    /**
     * A shortcut for single condition pattern matching.
     *
     * @param <T> the type of input argument
     * @param <R> the type of result
     * @param value the input argument
     * @param cond the pattern to be matched
     * @return the result from the pattern matching
     */
    public static <T, R> R inCaseOf(T value, ConditionCase<? super T, ? extends R, RuntimeException> cond) {
        ExceptionSupplier<? extends R, RuntimeException> sup = cond.evaluate(value);
        if (sup != null) {
            return sup.produce();
        } else {
            throw new NoSuchElementException("No result present");
        }
    }

    /**
     * A shortcut for single condition pattern matching.
     *
     * @param <T> the type of input argument
     * @param <R> the type of result
     * @param value the input argument
     * @param cond the pattern to be matched
     * @param deflt the default value if pattern doesn't matches input argument
     * @return the result from the pattern matching, or {@code deflt} if pattern
     * doesn't matches input argument
     */
    public static <T, R> R inCaseOf(T value, ConditionCase<? super T, ? extends R, RuntimeException> cond, R deflt) {
        ExceptionSupplier<? extends R, RuntimeException> sup = cond.evaluate(value);
        return sup != null ? sup.produce() : deflt;
    }

    /**
     * A sugar method that surround a constant value to make code readable.
     *
     * @param <T> the type of constant value
     * @param t a constant value
     * @return the constant value
     */
    public static <T> T otherwise(T t) {
        return t;
    }

    /**
     * Promote a pattern matching function to a predicate so it can be used in stream API.
     *
     * <p>For example:</p>
     * <pre>
     *     Stream.of(tuples).filter(on(Tuple((a,b) -> a==b));
     * </pre>
     */
    public static <T> Predicate<T> on(ConditionCase<? super T, Boolean, RuntimeException> cond) {
        return t -> cond.lift(t, false);
    }

    /**
     * Promote a pattern matching function to regular function that can be
     * used to other operations.
     */
    public static <T, R> Function<T, R> as(ConditionCase<? super T, ? extends R, RuntimeException> f) {
        return f::lift;
    }

    /**
     * A placeholder that represents always true predicate.
     */
    public static final Predicate<Object> __ = Predicates.any();

    /**
     * A conditional case that matches any argument and transform the argument
     * into result with the given function.
     *
     * @param mapper transform input argument into result
     */
    public static <T, R, X extends Throwable>
    ConditionCase<T, R, X> Any(ExceptionFunction<? super T, ? extends R, X> mapper) {
        return t -> () -> mapper.evaluate(t);
    }

    /**
     * A conditional case that matches any argument and produce result with the
     * given supplier.
     *
     * @param supplier produces the result
     */
    @SuppressWarnings("unchecked")
    public static <T, R, X extends Throwable>
    ConditionCase<T, R, X> Any(ExceptionSupplier<? extends R, X> supplier) {
        return t -> (ExceptionSupplier<R, X>)supplier;
    }

    /**
     * A conditional case that matches a constant value and produce result with
     * the given supplier.
     *
     * @param value the constant value
     * @param supplier produces the result
     */
    @SuppressWarnings("unchecked")
    public static <T, R, X extends Throwable>
    ConditionCase<T, R, X> $(T value, ExceptionSupplier<? extends R, X> supplier) {
        return t -> Objects.equals(t, value) ? (ExceptionSupplier<R, X>)supplier : null;
    }

    /**
     * Deconstruct nested pattern to get final result.
     *
     * <p>The following example illustrated a typical pattern matching use case
     * that deconstruct the element contained in an {@code Optional} in which
     * the {@code Optional} element is an instance of a {@code Tuple}. The
     * {@code Tuple()} method deconstruct the tuple elements into arguments to
     * a lambda expression.</p>
     *
     * <pre>
     *     void test(Optional&lt;Tuple&gt; obj) {
     *         with(obj)
     *          .when(in(Just(Tuple((x, y) -> ...))));
     *     }
     * </pre>
     *
     * <p>The pseudo code of this method in haskell notation:</p>
     * <pre>
     *  in :: (T -> () -> () -> R) -> (T -> () -> R)
     *  in f t = f t ()
     * </pre>
     */
    public static <T, R, X extends Throwable> ConditionCase<T, R, X>
    in(ConditionCase<T, ExceptionSupplier<R, X>, X> cond) {
        return cond::lift;
    }

    /**
     * A convenient conditional case that cast the argument into desired type.
     * This method primarily used by another conditional case implementation.
     */
    public static <T, U, R, X extends Throwable> ConditionCase<T, R, X>
    cast(Class<U> type, ConditionCase<? super U, R, X> mapper) {
        return t -> type.isInstance(t) ? mapper.evaluate(type.cast(t)) : null;
    }

    /**
     * A sugar method that convert an action to a supplier so it can be used by
     * {@link ActionSwitcher#when(ConditionCase)}.
     */
    public static <X extends Throwable> ExceptionSupplier<?, X>
    doing(ExceptionAction<X> action) {
        return () -> {
            action.perform();
            return null;
        };
    }

    /**
     * A sugar method that convert an action to a supplier so it can be used by
     * {@link ActionSwitcher#when(ConditionCase)}.
     */
    public static <T, X extends Throwable> ExceptionFunction<T, ?, X>
    doing(ExceptionConsumer<? super T, X> action) {
        return t -> {
            action.consume(t);
            return null;
        };
    }

    /**
     * A sugar method that convert an action to a supplier so it can be used by
     * {@link ActionSwitcher#when(ConditionCase)}.
     */
    public static <T, U, X extends Throwable> ExceptionBiFunction<T, U, ?, X>
    doing(ExceptionBiConsumer<? super T, ? super U, X> action) {
        return (t, u) -> {
            action.consume(t, u);
            return null;
        };
    }

    /**
     * Helper class to build conditional execution.
     *
     * @param <A> the action performed by conditional execution
     * @param <X> the exception raised by action
     */
    public interface Brancher<A, X extends Throwable, B extends Brancher<A,X,B>> {
        /**
         * Execute a branch when the given condition is satisfied.
         *
         * @param test perform {@code action} if condition is {@code true},
         * no action is performed if condition is {@code false}
         * @param action the action to perform
         */
        @SuppressWarnings("unchecked")
        default B when(boolean test, A action) throws X {
            return (B)this;
        }

        /**
         * Execute a branch when the given condition is satisfied.
         *
         * @param test a supplier that evaluate to a boolean value
         * @param action the action to perform
         */
        @SuppressWarnings("unchecked")
        default B when(BooleanSupplier test, A action) throws X {
            return (B)this;
        }

        /**
         * Execute a branch when the given condition is satisfied.
         *
         * @param value the value that will be evaluated by the given predicate
         * @param predicate the predicate to evaluate, perform {@code action}
         * if evaluated to {@code true}, no action is performed if evaluated to
         * {@code false}
         * @param action the action to perform
         */
        @SuppressWarnings("unchecked")
        default <T> B when(T value, Predicate<? super T> predicate, A action) throws X {
            return (B)this;
        }
    }

    /**
     * The result of a conditional execution is performing an action.
     *
     * @param <X> the exception raised by action
     */
    public interface ActionBrancher<X extends Throwable>
        extends Brancher<ExceptionAction<X>, X, ActionBrancher<X>>
    {
        /**
         * Allowing actions in the conditional can throw arbitrary exceptions.
         *
         * @param <Y> the exception types that allowed to throw in actions
         * @return the same conditional that just changing throws clause
         */
        @SuppressWarnings("unchecked")
        default <Y extends Throwable> ActionBrancher<Y> throwing() {
            return (ActionBrancher<Y>)this;
        }

        /**
         * Convert an action conditional to a supplier conditional.
         */
        default <R> SupplierBrancher<R,X> get() {
            return supplierBrancher();
        }

        /**
         * Execute fall through action when all conditions are not satisfied.
         *
         * @param action the action to perform when all conditions are not satisfied
         */
        default void otherwise(ExceptionAction<X> action) throws X {
            // no-op
        }

        /**
         * Throws exception if all conditions are not satisfied.
         *
         * @param <Y> type of the exception to be thrown
         * @param exceptionSupplier the supplier which will return the exception to
         * be thrown
         * @throws Y if all conditions are not satisfied
         */
        default <Y extends Throwable> void orElseThrow(Supplier<? extends Y> exceptionSupplier) throws Y {
            // no-op
        }
    }

    /**
     * The result of a conditional execution is getting a value.
     *
     * @param <R> the type of conditional execution result
     * @param <X> the exception raised by supplier
     */
    public interface SupplierBrancher<R, X extends Throwable>
        extends Brancher<ExceptionSupplier<R, X>, X, SupplierBrancher<R, X>>, ConditionalSupplier<R, X>
    {
        /**
         * Allowing actions in the conditional can throw arbitrary exceptions.
         *
         * @param <Y> the exception types that allowed to throw in actions
         * @return the same conditional that just changing throws clause
         */
        @SuppressWarnings("unchecked")
        default <Y extends Throwable> SupplierBrancher<R, Y> throwing() {
            return (SupplierBrancher<R, Y>)this;
        }
    }

    /**
     * A mix-in interface used to get result from conditional execution. The
     * default implementation has no result to return.
     *
     * @param <R> the type of conditional execution result
     * @param <X> the exception raised by supplier
     */
    public interface ConditionalSupplier<R, X extends Throwable> extends Supplier<R> {
        /**
         * Returns the result of the conditional execution.
         *
         * @throws NoSuchElementException if no conditions satisfied.
         */
        @Override
        default R get() {
            throw new NoSuchElementException("No result present");
        }

        /**
         * Returns the result if present, otherwise return {@code other}.
         *
         * @param other the value to be returned if there is no result present,
         * may be null
         * @return the result, if present, otherwise {@code other}
         */
        default R orElse(R other) {
            return other;
        }

        /**
         * Returns the result if present, otherwise invoke {@code other} and
         * return the result of that invocation.
         *
         * @param other a {@code Supplier} whose result is returned if no result
         * is present
         * @return the result if present otherwise the result of {@code other.get()}
         */
        default R orElseGet(ExceptionSupplier<? extends R, X> other) throws X {
            return other.produce();
        }

        /**
         * Returns the result if present, otherwise throw an exception to be
         * created by the provided supplier.
         *
         * @param <Y> Type of the exception to be thrown
         * @param exceptionSupplier The supplier which will return the exception to
         * be thrown
         * @return the present value
         * @throws Y if there is no value present
         */
        default <Y extends Throwable> R orElseThrow(Supplier<? extends Y> exceptionSupplier) throws Y {
            throw exceptionSupplier.get();
        }

        /**
         * Returns an optional to encapsulate result of execution.
         */
        default Optional<R> asOptional() {
            return Optional.empty();
        }

        /**
         * Returns a chained switcher with new value.
         *
         * @param <U> type of new value
         * @param u the new value to be matched
         * @return a new switcher that matching on new value
         */
        default <U> SupplierSwitcher<U, R, X> elseWith(U u) {
            return new SupplierSwitcherImpl<>(u);
        }

        /**
         * Returns a chained switcher with new values.
         *
         * @param <U> type of first new value
         * @param <V> type of second new value
         * @param u the first new value to be matched
         * @param v the second new value to be matched
         * @return a new switcher that matching on new values
         */
        default <U, V> BiSupplierSwitcher<U, V, R, X> elseWith(U u, V v) {
            return new BiSupplierSwitcherImpl<>(u, v);
        }
    }

    /**
     * A mix-in interface that return an actual result from conditional execution.
     *
     * @param <R> the type of conditional result
     * @param <X> the exception raised by supplier
     */
    private interface ResultSupplier<R, X extends Throwable> extends ConditionalSupplier<R, X> {
        @Override R get();

        @Override
        default R orElse(R other) {
            return get();
        }

        @Override
        default R orElseGet(ExceptionSupplier<? extends R, X> other) {
            return get();
        }

        @Override
        default <Y extends Throwable> R orElseThrow(Supplier<? extends Y> exceptionSupplier) {
            return get();
        }

        @Override
        default Optional<R> asOptional() {
            return Optional.of(get());
        }

        @Override
        default <U> SupplierSwitcher<U, R, X> elseWith(U u) {
            return new ResultSwitcher<>(get());
        }

        @Override
        default <U, V> BiSupplierSwitcher<U, V, R, X> elseWith(U u, V v) {
            return new ResultBiSwitcher<>(get());
        }
    }

    /**
     * The singleton implementation of action conditional execution.
     */
    private static class ActionBrancherImpl<X extends Throwable>
        implements ActionBrancher<X>
    {
        @Override
        public ActionBrancher<X> when(boolean test, ExceptionAction<X> action)
            throws X
        {
            if (test) {
                action.perform();
                return shortCircuitBrancher();
            }
            return this;
        }

        @Override
        public ActionBrancher<X> when(BooleanSupplier test, ExceptionAction<X> action) throws X {
            if (test.getAsBoolean()) {
                action.perform();
                return shortCircuitBrancher();
            }
            return this;
        }

        @Override
        public <T> ActionBrancher<X> when(T value, Predicate<? super T> predicate,
                                          ExceptionAction<X> action) throws X {
            if (predicate.test(value)) {
                action.perform();
                return shortCircuitBrancher();
            }
            return this;
        }

        @Override
        public void otherwise(ExceptionAction<X> action) throws X {
            action.perform();
        }

        @Override
        public <Y extends Throwable> void orElseThrow(Supplier<? extends Y> exceptionSupplier) throws Y {
            throw exceptionSupplier.get();
        }
    }

    /**
     * The singleton implementation of supplier conditional execution.
     */
    private static class SupplierBrancherImpl<R, X extends Throwable>
        implements SupplierBrancher<R, X>
    {
        @Override
        public SupplierBrancher<R, X> when(boolean test, ExceptionSupplier<R, X> supplier)
            throws X
        {
            if (test) {
                return new ResultBrancher<>(supplier.produce());
            } else {
                return this;
            }
        }

        @Override
        public SupplierBrancher<R, X> when(BooleanSupplier test, ExceptionSupplier<R, X> supplier)
            throws X
        {
            if (test.getAsBoolean()) {
                return new ResultBrancher<>(supplier.produce());
            } else {
                return this;
            }
        }

        @Override
        public <T> SupplierBrancher<R, X> when(T value, Predicate<? super T> predicate,
                                               ExceptionSupplier<R, X> supplier) throws X {
            if (predicate.test(value)) {
                return new ResultBrancher<>(supplier.produce());
            } else {
                return this;
            }
        }
    }

    /**
     * The supplier conditional execution that returns an concrete result.
     */
    private static class ResultBrancher<R, X extends Throwable>
        implements SupplierBrancher<R, X>, ResultSupplier<R, X>
    {
        private final R result;

        ResultBrancher(R result) {
            this.result = result;
        }

        @Override
        public R get() {
            return result;
        }
    }

    @SuppressWarnings("rawtypes")
    private static final ActionBrancher ACTION_BRANCHER = new ActionBrancherImpl<Throwable>();

    /**
     * Factory method to create action conditional.
     */
    @SuppressWarnings("unchecked")
    public static <X extends Throwable> ActionBrancher<X> actionBrancher() {
        return (ActionBrancher<X>)ACTION_BRANCHER;
    }

    /**
     * A do-nothing conditional execution that short-circuited by previous condition.
     * This is implemented as a singleton.
     */
    @SuppressWarnings("rawtypes")
    private static final ActionBrancher SHORT_CIRCUIT_BRANCHER = new ActionBrancher() {};

    @SuppressWarnings({"unchecked", "MethodOnlyUsedFromInnerClass"})
    private static <X extends Throwable> ActionBrancher<X> shortCircuitBrancher() {
        return (ActionBrancher<X>)SHORT_CIRCUIT_BRANCHER;
    }

    @SuppressWarnings("rawtypes")
    private static final SupplierBrancher SUPPLIER_BRANCHER = new SupplierBrancherImpl<>();

    @SuppressWarnings({"unchecked", "MethodOnlyUsedFromInnerClass"})
    private static <R, X extends Throwable> SupplierBrancher<R, X> supplierBrancher() {
        return (SupplierBrancher<R, X>)SUPPLIER_BRANCHER;
    }

    /**
     * Helper class used to build conditional execution for evaluating on given value.
     *
     * @param <T> the input type of value evaluation
     * @param <A> the action performed by conditional execution
     * @param <X> the exception raised by action
     */
    public interface Switcher<T, A, X extends Throwable, S extends Switcher<T,A,X,S>> {
        /**
         * Execute an action when the given predicate is satisfied.
         *
         * @param p the predicate to evaluate on input value
         * @param action the action to perform
         */
        @SuppressWarnings("unchecked")
        default S when(Predicate<? super T> p, A action) throws X {
            return (S)this;
        }

        /**
         * Convenient condition when tested with a constant value.
         *
         * @param t the value to be compared on input value
         * @param action the action to perform
         */
        @SuppressWarnings("unchecked")
        default S when(T t, A action) throws X {
            return (S)this;
        }
    }

    /**
     * The result of a conditional execution is performing an action.
     *
     * @param <T> the input type of value evaluation
     * @param <X> the exception raised by action
     */
    public interface ActionSwitcher<T, X extends Throwable>
        extends Switcher<T, ExceptionAction<X>, X, ActionSwitcher<T,X>>
    {
        /**
         * Allowing actions in the switcher can throw arbitrary exceptions.
         *
         * @param <Y> the exception types that allowed to throw in actions
         * @return the same action switcher that just changing throws clause
         */
        @SuppressWarnings("unchecked")
        default <Y extends Throwable> ActionSwitcher<T, Y> throwing() {
            return (ActionSwitcher<T, Y>)this;
        }

        /**
         * Convert an action switcher to a supplier switcher.
         */
        default <R> SupplierSwitcher<T, R, X> get() {
            throw new IllegalStateException();
        }

        /**
         * Evaluate the given function to get an action, and perform the action
         * if it's not null.
         *
         * @param branch the function that map the input argument to an action
         */
        default ActionSwitcher<T, X> when(ConditionCase<? super T, ?, X> branch) throws X {
            return this;
        }

        /**
         * There is a common scenario that is to check runtime type for a given value
         * and explicitly cast it to the desired type. This method simplify this use
         * case and make code more readable.
         *
         * <p>The following example illustrate this use case:</p>
         * <pre>
         *     String showType(Number value) {
         *       with(value)
         *         .when(Integer.class,  x -> print("an integer"))
         *         .when(Long.class,     x -> print("a long"))
         *         .when(Double.class,   x -> print("a double"))
         *         .orElse("Unknown");
         *     }
         * </pre>
         *
         * @param type the desired type to check
         * @param action the action that evaluating the value, the value passed
         * to this action will have the desired type
         */
        default <V> ActionSwitcher<T, X> when(Class<V> type, ExceptionConsumer<? super V, X> action) throws X {
            return this;
        }

        /**
         * Execute the fall through action when all predicates are not satisfied.
         */
        default void otherwise(ExceptionAction<X> action) throws X {
            // no-op
        }

        /**
         * Throws exception when all conditions are not satisfied.
         */
        default <Y extends Throwable> void orElseThrow(Supplier<? extends Y> exceptionSupplier) throws Y {
            // no-op
        }
    }

    /**
     * The result of a conditional execution is getting a value.
     *
     * @param <T> the type of input value
     * @param <R> the type of result value
     * @param <X> the exception raised by supplier
     */
    public interface SupplierSwitcher<T, R, X extends Throwable>
        extends Switcher<T, ExceptionSupplier<R, X>, X, SupplierSwitcher<T,R,X>>, ConditionalSupplier<R, X>
    {
        /**
         * Allowing actions in the switcher can throw arbitrary exceptions.
         *
         * @param <Y> the exception types that allowed to throw in actions
         * @return the same action switcher that just changing throws clause
         */
        @SuppressWarnings("unchecked")
        default <Y extends Throwable> SupplierSwitcher<T, R, Y> throwing() {
            return (SupplierSwitcher<T, R, Y>)this;
        }

        /**
         * Evaluate the given function to get a supplier, and evaluate the supplier
         * if it's not null.
         *
         * @param branch the function that map the input argument to a supplier
         */
        default SupplierSwitcher<T, R, X> when(ConditionCase<? super T, ? extends R, X> branch) throws X {
            return this;
        }

        /**
         * There is a common scenario that is to check runtime type for a given value
         * and explicitly cast it to the desired type. This method simplify this use
         * case and make code more readable.
         *
         * <p>The following example illustrate this use case:</p>
         * <pre>
         *     String showType(Number value) {
         *       with(value).&lt;String&gt;get()
         *         .when(Integer.class,  x -> forType(x))
         *         .when(Long.class,     x -> forType(x))
         *         .when(Double.class,   x -> forType(x))
         *         .orElse("Unknown");
         *     }
         *
         *     String forType(int x)    { return "Integer"; }
         *     String forType(long x)   { return "Long";    }
         *     String forType(double x) { return "Double";  }
         *
         *     public void test() {
         *         assertThat(showType(1), is("Integer"));
         *         assertThat(showType(1.0), is("Double"));
         *     }
         * </pre>
         *
         * <p>The original code as follows is tedious and error prone:</p>
         * <pre>
         *     String showType(Number value) {
         *         if (value instanceof Integer) {
         *             return forType((Integer)value);
         *         } else if (value instanceof Long) {
         *             return forType((Long)value);
         *         } else if (value instanceof Double) {
         *             return forType((Double)value);
         *         } else {
         *             return "Unknown";
         *         }
         *     }
         * </pre>
         *
         * @param type the desired type to check
         * @param converter the function that convert value to result, the value passed
         * to this function will have the desired type
         */
        default <V> SupplierSwitcher<T, R, X>
        when(Class<V> type, ExceptionFunction<? super V, ? extends R, X> converter) throws X {
            return this;
        }
    }

    /**
     * Implementation of action switcher.
     */
    private static class ActionSwitcherImpl<T, X extends Throwable>
        implements ActionSwitcher<T, X>
    {
        private final T value;

        ActionSwitcherImpl(T value) {
            this.value = value;
        }

        @Override
        public <R> SupplierSwitcher<T, R, X> get() {
            return new SupplierSwitcherImpl<>(value);
        }

        @Override
        public ActionSwitcher<T, X> when(Predicate<? super T> p, ExceptionAction<X> action) throws X {
            if (p.test(value)) {
                action.perform();
                return shortCircuitSwitcher();
            }
            return this;
        }

        @Override
        public ActionSwitcher<T, X> when(T t, ExceptionAction<X> action) throws X {
            if (Objects.equals(t, value)) {
                action.perform();
                return shortCircuitSwitcher();
            }
            return this;
        }

        @Override
        public ActionSwitcher<T, X> when(ConditionCase<? super T, ?, X> branch) throws X {
            ExceptionSupplier<?, X> action = branch.evaluate(value);
            if (action != null) {
                action.produce();
                return shortCircuitSwitcher();
            }
            return this;
        }

        @Override
        public <V> ActionSwitcher<T, X> when(Class<V> type, ExceptionConsumer<? super V, X> action) throws X {
            if (type.isInstance(value)) {
                action.consume(type.cast(value));
                return shortCircuitSwitcher();
            }
            return this;
        }

        @Override
        public void otherwise(ExceptionAction<X> action) throws X {
            action.perform();
        }

        @Override
        public <Y extends Throwable> void orElseThrow(Supplier<? extends Y> exceptionSupplier) throws Y {
            throw exceptionSupplier.get();
        }
    }

    /**
     * Implementation of supplier switcher.
     */
    private static class SupplierSwitcherImpl<T, R, X extends Throwable>
        implements SupplierSwitcher<T, R, X>
    {
        private final T value;

        SupplierSwitcherImpl(T value) {
            this.value = value;
        }

        @Override
        public SupplierSwitcher<T, R, X> when(Predicate<? super T> p, ExceptionSupplier<R, X> supplier)
            throws X
        {
            if (p.test(value)) {
                return new ResultSwitcher<>(supplier.produce());
            } else {
                return this;
            }
        }

        @Override
        public SupplierSwitcher<T, R, X> when(T t, ExceptionSupplier<R, X> supplier)
            throws X
        {
            if (Objects.equals(t, value)) {
                return new ResultSwitcher<>(supplier.produce());
            } else {
                return this;
            }
        }

        @Override
        public SupplierSwitcher<T, R, X> when(ConditionCase<? super T, ? extends R, X> branch)
            throws X
        {
            // when :: (T -> () -> R) ->  (T -> R)
            // when f t = f t ()
            ExceptionSupplier<? extends R, X> supplier = branch.evaluate(value);
            if (supplier != null) {
                return new ResultSwitcher<>(supplier.produce());
            } else {
                return this;
            }
        }

        @Override
        public <V> SupplierSwitcher<T, R, X>
        when(Class<V> type, ExceptionFunction<? super V, ? extends R, X> converter) throws X {
            if (type.isInstance(value)) {
                return new ResultSwitcher<>(converter.evaluate(type.cast(value)));
            } else {
                return this;
            }
        }
    }

    /**
     * The supplier switcher that returns a concrete result.
     */
    private static class ResultSwitcher<T, R, X extends Throwable>
        implements SupplierSwitcher<T, R, X>, ResultSupplier<R, X>
    {
        private final R result;

        ResultSwitcher(R result) {
            this.result = result;
        }

        @Override
        public R get() {
            return result;
        }
    }

    /**
     * Factory method to create action switcher.
     */
    public static <T, X extends Throwable> ActionSwitcher<T, X> actionSwitcher(T value) {
        return new ActionSwitcherImpl<>(value);
    }

    /**
     * A do-nothing switcher that short-circuited by previous condition.
     */
    @SuppressWarnings("rawtypes")
    private static final ActionSwitcher SHORT_CIRCUIT_SWITCHER = new ActionSwitcher() {};

    @SuppressWarnings({"unchecked", "MethodOnlyUsedFromInnerClass"})
    private static <T, X extends Throwable> ActionSwitcher<T,X> shortCircuitSwitcher() {
        return (ActionSwitcher<T,X>)SHORT_CIRCUIT_SWITCHER;
    }

    /**
     * Helper class used to build conditional execution for evaluating on given pair
     * of values.
     *
     * @param <T> the input type of first value
     * @param <U> the input type of second value
     * @param <A> the action performed by conditional execution
     * @param <X> the exception type raised by action
     */
    public interface BiSwitcher<T, U, A, X extends Throwable, S extends BiSwitcher<T,U,A,X,S>> {
        /**
         * Execute an action when the give predicates are satisfied.
         *
         * @param p1 the predicate to evaluate on first argument
         * @param p2 the predicate to evaluate on second argument
         * @param action the action to perform
         */
        @SuppressWarnings("unchecked")
        default S when(Predicate<? super T> p1, Predicate<? super U> p2, A action) throws X {
            return (S)this;
        }

        /**
         * Execute an action when both arguments are equals to the given
         * constant values.
         *
         * @param v1 the value to be compared to first argument
         * @param v2 the value to be compared to second argument
         * @param action the action to perform
         */
        @SuppressWarnings("unchecked")
        default S when(T v1, U v2, A action) throws X {
            return (S)this;
        }

        /**
         * Execute an action when first argument is satisfied to the given
         * predicate, and the second argument is equals to the given constant
         * value.
         *
         * @param p1 the predicate to evaluate on first argument
         * @param v2 the const value to be compared to second argument
         * @param action the action to perform
         */
        @SuppressWarnings("unchecked")
        default S when(Predicate<? super T> p1, U v2, A action) throws X {
            return (S)this;
        }

        /**
         * Execute an action when first argument is equals to the given constant
         * value, and the second argument is satisfied to the given predicate.
         *
         * @param v1 the const value to be compared to the first argument
         * @param p2 the predicate to evaluate on second argument
         * @param action the action to perform
         */
        @SuppressWarnings("unchecked")
        default S when(T v1, Predicate<? super U> p2, A action) throws X {
            return (S)this;
        }
    }

    /**
     * The result of a conditional execution is performing an action.
     *
     * @param <T> the input type of first argument
     * @param <U> the input type of second argument
     * @param <X> the exception type raised by action
     */
    public interface BiActionSwitcher<T, U, X extends Throwable>
        extends BiSwitcher<T, U, ExceptionAction<X>, X, BiActionSwitcher<T, U, X>>
    {
        /**
         * Allowing actions in the switcher can throw arbitrary exceptions.
         *
         * @param <Y> the exception type that allowed to throw in actions
         * @return the same action switcher that just changing throw clause
         */
        @SuppressWarnings("unchecked")
        default <Y extends Throwable> BiActionSwitcher<T, U, Y> throwing() {
            return (BiActionSwitcher<T, U, Y>)this;
        }

        /**
         * Convert an action switcher to a supplier switcher.
         */
        default <R> BiSupplierSwitcher<T, U, R, X> get() {
            throw new IllegalStateException();
        }

        /**
         * There is a common scenario that is to check runtime types for given
         * arguments and explicitly cast it to the desired type. This method
         * simplify this use case and make code more readable.
         *
         * @param c1 the desired type to check for first argument
         * @param c2 the desired type to check for second argument
         * @param action the action that evaluating the value, the values passed
         * to this action will have the desired types.
         */
        default <T1, U1> BiActionSwitcher<T, U, X>
        when(Class<T1> c1, Class<U1> c2, ExceptionBiConsumer<? super T1, ? super U1, X> action) throws X {
            return this;
        }

        /**
         * If both arguments have the same base type then this method checks on the
         * desired type.
         *
         * @param c the desired type to check for both arguments
         * @param action the action that to perform if both arguments have the desired
         * type. The values passed to this action will have the desired type.
         */
        default <V> BiActionSwitcher<T, U, X>
        when(Class<V> c, ExceptionBiConsumer<? super V, ? super V, X> action) throws X {
            return when(c, c, action);
        }

        /**
         * Evaluate the given function to get an action, and perform the action
         * if it's not null.
         *
         * @param branch the function that map the input argument to an action
         */
        default BiActionSwitcher<T, U, X>
        when(ConditionCase<? super T, ? extends ConditionCase<? super U, ?, X>, X> branch) throws X {
            return this;
        }

        /**
         * Execute the fall through action when all conditions are not satisfied.
         */
        default void otherwise(ExceptionAction<X> action) throws X {
            // no-op
        }

        /**
         * Throws exception when all conditions are not satisfied.
         */
        default <Y extends Throwable> void orElseThrow(Supplier<? extends Y> exceptionSupplier) throws Y {
            // no-op
        }
    }

    /**
     * The result of a conditional execution is getting a value.
     *
     * @param <T> the type of first argument
     * @param <U> the type of second argument
     * @param <R> the type of result value
     * @param <X> the exception type raised by supplier
     */
    public interface BiSupplierSwitcher<T, U, R, X extends Throwable>
        extends BiSwitcher<T, U, ExceptionSupplier<R, X>, X, BiSupplierSwitcher<T,U,R,X>>, ConditionalSupplier<R, X>
    {
        /**
         * Allowing actions in the switcher can throw arbitrary exception.
         *
         * @param <Y> the exception type that allowed to throw in actions
         * @return the same action switcher that just changing throws clause
         */
        @SuppressWarnings("unchecked")
        default <Y extends Throwable> BiSupplierSwitcher<T, U, R, Y> throwing() {
            return (BiSupplierSwitcher<T, U, R, Y>)this;
        }

        /**
         * There is a common scenario that is to check runtime types for given
         * arguments and explicitly cast it to the desired type. This method
         * simplify this use case and make code more readable.
         *
         * @param c1 the desired type to check for first argument
         * @param c2 the desired type to check for second argument
         * @param converter the function that convert values to result, the values passed
         * to this function will have the desired types.
         */
        default <T1, U1> BiSupplierSwitcher<T, U, R, X>
        when(Class<T1> c1, Class<U1> c2, ExceptionBiFunction<? super T1, ? super U1, ? extends R, X> converter) throws X {
            return this;
        }

        /**
         * If both arguments have the same base type then this method checks on the
         * desired type.
         *
         * @param c the desired type to check for both arguments
         * @param converter the function that convert values to result, the values passed
         * to this function will have the desired types.
         */
        default <V> BiSupplierSwitcher<T, U, R, X>
        when(Class<V> c, ExceptionBiFunction<? super V, ? super V, ? extends R, X> converter) throws X {
            return when(c, c, converter);
        }

        /**
         * Evaluate the given function to get a supplier, and evaluate the supplier
         * if it's not null.
         *
         * @param branch the function that map the input argument to a supplier
         */
        default BiSupplierSwitcher<T, U, R, X>
        when(ConditionCase<? super T, ? extends ConditionCase<? super U, ? extends R, X>, X> branch) throws X {
            return this;
        }
    }

    /**
     * Implementation of action switcher on double arguments.
     */
    private static class BiActionSwitcherImpl<T, U, X extends Throwable>
        implements BiActionSwitcher<T, U, X>
    {
        private final T left;
        private final U right;

        BiActionSwitcherImpl(T left, U right) {
            this.left = left;
            this.right = right;
        }

        @Override
        public <R> BiSupplierSwitcher<T, U, R, X> get() {
            return new BiSupplierSwitcherImpl<>(left, right);
        }

        @Override
        public BiActionSwitcher<T, U, X>
        when(Predicate<? super T> p1, Predicate<? super U> p2, ExceptionAction<X> action) throws X {
            if (p1.test(left) && p2.test(right)) {
                action.perform();
                return shortCircuitBiSwitcher();
            }
            return this;
        }

        @Override
        public BiActionSwitcher<T, U, X> when(T v1, U v2, ExceptionAction<X> action) throws X {
            if (Objects.equals(v1, left) && Objects.equals(v2, right)) {
                action.perform();
                return shortCircuitBiSwitcher();
            }
            return this;
        }

        @Override
        public BiActionSwitcher<T, U, X> when(Predicate<? super T> p1, U v2, ExceptionAction<X> action) throws X {
            if (p1.test(left) && Objects.equals(v2, right)) {
                action.perform();
                return shortCircuitBiSwitcher();
            }
            return this;
        }

        @Override
        public BiActionSwitcher<T, U, X> when(T v1, Predicate<? super U> p2, ExceptionAction<X> action) throws X {
            if (Objects.equals(v1, left) && p2.test(right)) {
                action.perform();
                return shortCircuitBiSwitcher();
            }
            return this;
        }

        @Override
        public <T1, U1> BiActionSwitcher<T, U, X>
        when(Class<T1> c1, Class<U1> c2, ExceptionBiConsumer<? super T1, ? super U1, X> action) throws X {
            if (c1.isInstance(left) && c2.isInstance(right)) {
                action.consume(c1.cast(left), c2.cast(right));
                return shortCircuitBiSwitcher();
            }
            return this;
        }

        @Override
        public BiActionSwitcher<T, U, X>
        when(ConditionCase<? super T, ? extends ConditionCase<? super U, ?, X>, X> branch) throws X {
            ExceptionSupplier<? extends ConditionCase<? super U, ?, X>, X> alt = branch.evaluate(left);
            if (alt != null) {
                ExceptionSupplier<?, X> action = alt.produce().evaluate(right);
                if (action != null) {
                    action.produce();
                    return shortCircuitBiSwitcher();
                }
            }
            return this;
        }

        @Override
        public void otherwise(ExceptionAction<X> action) throws X {
            action.perform();
        }

        @Override
        public <Y extends Throwable> void orElseThrow(Supplier<? extends Y> exceptionSupplier) throws Y {
            throw exceptionSupplier.get();
        }
    }

    /**
     * The implementation of supplier switcher on double arguments.
     */
    private static class BiSupplierSwitcherImpl<T, U, R, X extends Throwable>
        implements BiSupplierSwitcher<T, U, R, X>
    {
        private final T left;
        private final U right;

        BiSupplierSwitcherImpl(T left, U right) {
            this.left = left;
            this.right = right;
        }

        @Override
        public BiSupplierSwitcher<T, U, R, X>
        when(Predicate<? super T> p1, Predicate<? super U> p2, ExceptionSupplier<R, X> supplier) throws X {
            if (p1.test(left) && p2.test(right)) {
                return new ResultBiSwitcher<>(supplier.produce());
            }
            return this;
        }

        @Override
        public BiSupplierSwitcher<T, U, R, X>
        when(T v1, U v2, ExceptionSupplier<R, X> supplier) throws X {
            if (Objects.equals(v1, left) && Objects.equals(v2, right)) {
                return new ResultBiSwitcher<>(supplier.produce());
            }
            return this;
        }

        @Override
        public BiSupplierSwitcher<T, U, R, X>
        when(Predicate<? super T> p1, U v2, ExceptionSupplier<R, X> supplier) throws X {
            if (p1.test(left) && Objects.equals(v2, right)) {
                return new ResultBiSwitcher<>(supplier.produce());
            }
            return this;
        }

        @Override
        public BiSupplierSwitcher<T, U, R, X>
        when(T v1, Predicate<? super U> p2, ExceptionSupplier<R, X> supplier) throws X {
            if (Objects.equals(v1, left) && p2.test(right)) {
                return new ResultBiSwitcher<>(supplier.produce());
            }
            return this;
        }

        @Override
        public <T1, U1> BiSupplierSwitcher<T, U, R, X>
        when(Class<T1> c1, Class<U1> c2, ExceptionBiFunction<? super T1, ? super U1, ? extends R, X> converter) throws X {
            if (c1.isInstance(left) && c2.isInstance(right)) {
                return new ResultBiSwitcher<>(converter.evaluate(c1.cast(left), c2.cast(right)));
            }
            return this;
        }

        @Override
        public BiSupplierSwitcher<T, U, R, X>
        when(ConditionCase<? super T, ? extends ConditionCase<? super U, ? extends R, X>, X> branch) throws X {
            // when :: (T -> () -> U -> () -> R) -> (T -> U -> R)
            // when f t u = f t () u ()
            ExceptionSupplier<? extends ConditionCase<? super U, ? extends R, X>, X> alt = branch.evaluate(left);
            if (alt != null) {
                ExceptionSupplier<? extends R, X> sup = alt.produce().evaluate(right);
                if (sup != null) {
                    return new ResultBiSwitcher<>(sup.produce());
                }
            }
            return this;
        }
    }

    /**
     * The supplier switcher that returns a concrete result.
     */
    private static class ResultBiSwitcher<T, U, R, X extends Throwable>
        implements BiSupplierSwitcher<T, U, R, X>, ResultSupplier<R, X>
    {
        private final R result;

        ResultBiSwitcher(R result) {
            this.result = result;
        }

        @Override
        public R get() {
            return result;
        }
    }

    /**
     * Factory method to create an action switcher with two arguments.
     */
    public static <T, U, X extends Throwable> BiActionSwitcher<T, U, X> biActionSwitcher(T t, U u) {
        return new BiActionSwitcherImpl<>(t, u);
    }

    /**
     * A do-nothing switcher that short-circuited by previous condition.
     */
    @SuppressWarnings("rawtypes")
    private static final BiActionSwitcher SHORT_CIRCUIT_BI_SWITCHER = new BiActionSwitcher() {};

    @SuppressWarnings({"unchecked", "MethodOnlyUsedFromInnerClass"})
    private static <T, U, X extends Throwable> BiActionSwitcher<T,U,X> shortCircuitBiSwitcher() {
        return (BiActionSwitcher<T,U,X>)SHORT_CIRCUIT_BI_SWITCHER;
    }
}
