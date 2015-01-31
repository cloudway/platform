/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util;

import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.Optional;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.BooleanSupplier;
import java.util.function.Predicate;
import java.util.function.Supplier;

public final class Conditionals {
    private Conditionals() {}

    /**
     * Create a chained fluent conditional execution.
     */
    public static ActionConditional<RuntimeException> with() {
        return actionConditional();
    }

    /**
     * Create a chained fluent conditional execution for evaluating on given value.
     */
    public static <T> ActionSwitcher<T, RuntimeException> with(T value) {
        return actionSwitcher(value);
    }

    /**
     * A sugar method that convert an action to a supplier so it can be used by
     * {@link ActionSwitcher#when(ConditionCase)}.
     */
    public static <X extends Throwable> ExceptionSupplier<?, X> doing(ExceptionAction<X> action) {
        return () -> {
            action.perform();
            return null;
        };
    }

    /**
     * A sugar method that convert an action to a supplier so it can be used by
     * {@link ActionSwitcher#when(ConditionCase)}.
     */
    public static <T, X extends Throwable> ExceptionFunction<T, ?, X> doing(ExceptionConsumer<? super T, X> action) {
        return t -> {
            action.consume(t);
            return null;
        };
    }

    /**
     * A sugar method that convert an action to a supplier so it can be used by
     * {@link ActionSwitcher#when(ConditionCase)}.
     */
    public static <T, U> BiFunction<T, U, ?> doing(BiConsumer<? super T, ? super U> action) {
        return (t, u) -> {
            action.accept(t, u);
            return null;
        };
    }

    /**
     * Helper class to build conditional execution.
     *
     * @param <A> the action performed by conditional execution
     * @param <X> the exception raised by action
     */
    public interface Conditional<A, X extends Throwable, C extends Conditional<A,X,C>> {
        /**
         * Execute a branch when the given condition is satisfied.
         *
         * @param test perform {@code action} if condition is {@code true},
         * no action is performed if condition is {@code false}
         * @param action the action to perform
         */
        @SuppressWarnings("unchecked")
        default C when(boolean test, A action) throws X {
            return (C)this;
        }

        /**
         * Execute a branch when the given condition is satisfied.
         *
         * @param test a supplier that evaluate to a boolean value
         * @param action the action to perform
         */
        @SuppressWarnings("unchecked")
        default C when(BooleanSupplier test, A action) throws X {
            return (C)this;
        }
    }

    /**
     * The result of a conditional execution is performing an action.
     *
     * @param <X> the exception raised by action
     */
    public interface ActionConditional<X extends Throwable>
        extends Conditional<ExceptionAction<X>, X, ActionConditional<X>>
    {
        /**
         * Allowing actions in the conditional can throw arbitrary exceptions.
         *
         * @param <Y> the exception types that allowed to throw in actions
         * @return the same conditional that just changing throws clause
         */
        @SuppressWarnings("unchecked")
        default <Y extends Throwable> ActionConditional<Y> throwing() {
            return (ActionConditional<Y>)this;
        }

        /**
         * Convert an action conditional to a supplier conditional.
         */
        default <R> SupplierConditional<R,X> get() {
            return supplierConditional();
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
        default <T> ActionConditional<X> when(T value, Predicate<? super T> predicate,
                                              ExceptionAction<X> action) throws X {
            return this;
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
    public interface SupplierConditional<R, X extends Throwable>
        extends Conditional<ExceptionSupplier<? extends R, X>, X, SupplierConditional<R,X>>, ConditionalSupplier<R, X>
    {
        /**
         * Allowing actions in the conditional can throw arbitrary exceptions.
         *
         * @param <Y> the exception types that allowed to throw in actions
         * @return the same conditional that just changing throws clause
         */
        @SuppressWarnings("unchecked")
        default <Y extends Throwable> SupplierConditional<R, Y> throwing() {
            return (SupplierConditional<R, Y>)this;
        }

        /**
         * Execute a branch when the given condition is satisfied.
         *
         * @param value the value that will be evaluated by the given predicate
         * @param predicate the predicate to evaluate, perform {@code action}
         * if evaluated to {@code true}, no action is performed if evaluated to
         * {@code false}
         * @param supplier the action to perform
         */
        default <T> SupplierConditional<R, X> when(T value, Predicate<? super T> predicate,
                                                   ExceptionSupplier<? extends R, X> supplier) throws X {
            return this;
        }

        /**
         * Execute a branch when the given condition is satisfied.
         *
         * @param value the value that will be evaluated by the given predicate
         * @param predicate the predicate to evaluate, perform {@code action}
         * if evaluated to {@code true}, no action is performed if evaluated to
         * {@code false}
         * @param result the result to return
         */
        default <T> SupplierConditional<R, X> when(T value, Predicate<? super T> predicate, R result) {
            return this;
        }
    }

    /**
     * A mix-in interface used to get result from conditional execution. The
     * default implementation has no result to return.
     *
     * @param <T> the type of conditional execution result
     * @param <X> the exception raised by supplier
     */
    public interface ConditionalSupplier<T, X extends Throwable> extends Supplier<T> {
        /**
         * Returns the result of the conditional execution.
         *
         * @throws NoSuchElementException if no conditions satisfied.
         */
        @Override
        default T get() {
            throw new NoSuchElementException("No result present");
        }

        /**
         * Returns the result if present, otherwise return {@code other}.
         *
         * @param other the value to be returned if there is no result present,
         * may be null
         * @return the result, if present, otherwise {@code other}
         */
        default T orElse(T other) {
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
        default T orElseGet(ExceptionSupplier<? extends T, X> other) throws X {
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
        default <Y extends Throwable> T orElseThrow(Supplier<? extends Y> exceptionSupplier) throws Y {
            throw exceptionSupplier.get();
        }

        /**
         * Returns an optional to encapsulate result of execution.
         */
        default Optional<T> asOptional() {
            return Optional.empty();
        }
    }

    /**
     * A mix-in interface that return an actual result from conditional execution.
     *
     * @param <T> the type of conditional result
     * @param <X> the exception raised by supplier
     */
    private interface ResultSupplier<T, X extends Throwable> extends ConditionalSupplier<T, X> {
        @Override T get();

        @Override
        default T orElse(T other) {
            return get();
        }

        @Override
        default T orElseGet(ExceptionSupplier<? extends T, X> other) {
            return get();
        }

        @Override
        default <Y extends Throwable> T orElseThrow(Supplier<? extends Y> exceptionSupplier) {
            return get();
        }

        @Override
        default Optional<T> asOptional() {
            return Optional.of(get());
        }
    }

    /**
     * The singleton implementation of action conditional execution.
     */
    private static class ActionConditionalImpl<X extends Throwable>
        implements ActionConditional<X>
    {
        @Override
        public ActionConditional<X> when(boolean test, ExceptionAction<X> action)
            throws X
        {
            if (test) {
                action.perform();
                return shortCircuitConditional();
            }
            return this;
        }

        @Override
        public ActionConditional<X> when(BooleanSupplier test, ExceptionAction<X> action) throws X {
            if (test.getAsBoolean()) {
                action.perform();
                return shortCircuitConditional();
            }
            return this;
        }

        @Override
        public <T> ActionConditional<X> when(T value, Predicate<? super T> predicate,
                                             ExceptionAction<X> action) throws X {
            if (predicate.test(value)) {
                action.perform();
                return shortCircuitConditional();
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
    private static class SupplierConditionalImpl<R, X extends Throwable>
        implements SupplierConditional<R, X>
    {
        @Override
        public SupplierConditional<R, X> when(boolean test, ExceptionSupplier<? extends R, X> supplier)
            throws X
        {
            if (test) {
                return new ResultConditional<>(supplier.produce());
            } else {
                return this;
            }
        }

        @Override
        public SupplierConditional<R, X> when(BooleanSupplier test, ExceptionSupplier<? extends R, X> supplier)
            throws X
        {
            if (test.getAsBoolean()) {
                return new ResultConditional<>(supplier.produce());
            } else {
                return this;
            }
        }

        @Override
        public <T> SupplierConditional<R, X> when(T value, Predicate<? super T> predicate,
                                                  ExceptionSupplier<? extends R, X> supplier) throws X {
            if (predicate.test(value)) {
                return new ResultConditional<>(supplier.produce());
            } else {
                return this;
            }
        }

        @Override
        public <T> SupplierConditional<R, X> when(T value, Predicate<? super T> predicate, R result) {
            if (predicate.test(value)) {
                return new ResultConditional<>(result);
            } else {
                return this;
            }
        }
    }

    /**
     * The supplier conditional execution that returns an concrete result.
     */
    private static class ResultConditional<R, X extends Throwable>
        implements SupplierConditional<R, X>, ResultSupplier<R, X>
    {
        private final R result;

        ResultConditional(R result) {
            this.result = result;
        }

        @Override
        public R get() {
            return result;
        }
    }

    @SuppressWarnings("rawtypes")
    private static final ActionConditional ACTION_CONDITIONAL = new ActionConditionalImpl<Throwable>();

    /**
     * Package private factory method to create action conditional.
     */
    @SuppressWarnings("unchecked")
    static <X extends Throwable> ActionConditional<X> actionConditional() {
        return (ActionConditional<X>)ACTION_CONDITIONAL;
    }

    /**
     * A do-nothing conditional execution that short-circuited by previous condition.
     * This is implemented as a singleton.
     */
    @SuppressWarnings("rawtypes")
    private static final ActionConditional SHORT_CIRCUIT_CONDITIONAL = new ActionConditional() {};

    @SuppressWarnings({"unchecked", "MethodOnlyUsedFromInnerClass"})
    private static <X extends Throwable> ActionConditional<X> shortCircuitConditional() {
        return (ActionConditional<X>)SHORT_CIRCUIT_CONDITIONAL;
    }

    @SuppressWarnings("rawtypes")
    private static final SupplierConditional SUPPLIER_CONDITIONAL = new SupplierConditionalImpl<>();

    @SuppressWarnings({"unchecked", "MethodOnlyUsedFromInnerClass"})
    private static <R, X extends Throwable> SupplierConditional<R, X> supplierConditional() {
        return (SupplierConditional<R, X>)SUPPLIER_CONDITIONAL;
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
     * The result of a conditional execution is perform an action.
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
        default <V extends T> ActionSwitcher<T, X>
        when(Class<V> type, ExceptionConsumer<? super V, X> action) throws X {
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
        extends Switcher<T, ExceptionSupplier<? extends R, X>, X, SupplierSwitcher<T,R,X>>, ConditionalSupplier<R, X>
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
        default <V extends T> SupplierSwitcher<T, R, X>
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
        public <V extends T> ActionSwitcher<T, X> when(Class<V> type, ExceptionConsumer<? super V, X> action) throws X {
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
        public SupplierSwitcher<T, R, X> when(Predicate<? super T> p, ExceptionSupplier<? extends R, X> supplier)
            throws X
        {
            if (p.test(value)) {
                return new ResultSwitcher<>(supplier.produce());
            } else {
                return this;
            }
        }

        @Override
        public SupplierSwitcher<T, R, X> when(T t, ExceptionSupplier<? extends R, X> supplier)
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
            ExceptionSupplier<? extends R, X> supplier = branch.evaluate(value);
            if (supplier != null) {
                return new ResultSwitcher<>(supplier.produce());
            } else {
                return this;
            }
        }

        @Override
        public <V extends T> SupplierSwitcher<T, R, X>
        when(Class<V> type, ExceptionFunction<? super V, ? extends R, X> converter) throws X {
            if (type.isInstance(value)) {
                return new ResultSwitcher<>(converter.evaluate(type.cast(value)));
            } else {
                return this;
            }
        }
    }

    /**
     * The supplier switcher that returns an concrete result.
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
     * Package private factory method to create action switcher.
     */
    static <T, X extends Throwable> ActionSwitcher<T, X> actionSwitcher(T value) {
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
}
