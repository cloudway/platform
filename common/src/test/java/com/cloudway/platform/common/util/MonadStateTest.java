/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util;

import java.util.Arrays;
import java.util.OptionalInt;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.regex.Pattern;

import org.junit.Test;
import static org.junit.Assert.*;

import com.cloudway.platform.common.fp.$;
import com.cloudway.platform.common.fp.control.ConditionCase;
import com.cloudway.platform.common.fp.control.MonadState;
import com.cloudway.platform.common.fp.control.ReaderT;
import com.cloudway.platform.common.fp.control.StateT;
import com.cloudway.platform.common.fp.data.Identity;
import com.cloudway.platform.common.fp.data.IntSeq;
import com.cloudway.platform.common.fp.data.Maybe;
import com.cloudway.platform.common.fp.data.Seq;
import com.cloudway.platform.common.fp.data.SeqZipper;
import com.cloudway.platform.common.fp.data.Tuple;
import com.cloudway.platform.common.fp.data.Unit;
import com.cloudway.platform.common.fp.function.ExceptionBiFunction;

import static com.cloudway.platform.common.fp.control.Syntax.*;
import static com.cloudway.platform.common.fp.control.Conditionals.*;
import static com.cloudway.platform.common.fp.data.Identity.runIdentity;
import static com.cloudway.platform.common.fp.data.IntSeq.IntCons;
import static com.cloudway.platform.common.fp.data.IntSeq.IntSingle;
import static com.cloudway.platform.common.fp.data.Maybe.Just;

// @formatter:off
public class MonadStateTest {
    private static final Pattern SPACES = Pattern.compile("\\s+");

    static final class RPN {
        private RPN() {}

        public static int solve(String expression) {
            return Seq.of(SPACES.split(expression)).foldLeft(IntSeq.nil(), RPN::scan).head();
        }

        static IntSeq scan(IntSeq stack, String item) {
            return with(stack, item).<IntSeq>get()
                .when(IntCons((x, y, ys) -> $("+", () -> IntSeq.cons(y + x, ys))))
                .when(IntCons((x, y, ys) -> $("-", () -> IntSeq.cons(y - x, ys))))
                .when(IntCons((x, y, ys) -> $("*", () -> IntSeq.cons(y * x, ys))))
                .when(IntCons((x, y, ys) -> $("/", () -> IntSeq.cons(y / x, ys))))
                .orElseGet(() -> IntSeq.cons(Integer.parseInt(item), stack));
        }
    }

    static final class SafeRPN {
        private SafeRPN() {}

        public static OptionalInt solve(String expression) {
            Maybe<IntSeq> stack = Maybe.foldM(IntSeq.nil(), Seq.of(SPACES.split(expression)), SafeRPN::scan);
            return inCaseOf(stack, in(Just(IntSingle(OptionalInt::of))), otherwise(OptionalInt.empty()));
        }

        static Maybe<IntSeq> scan(IntSeq stack, String item) {
            return with(stack, item).<Maybe<IntSeq>>get()
                .when(IntCons((x, y, ys) -> $("+", () -> Maybe.of(IntSeq.cons(y + x, ys)))))
                .when(IntCons((x, y, ys) -> $("-", () -> Maybe.of(IntSeq.cons(y - x, ys)))))
                .when(IntCons((x, y, ys) -> $("*", () -> Maybe.of(IntSeq.cons(y * x, ys)))))
                .when(IntCons((x, y, ys) -> $("/", () -> x == 0 ? Maybe.empty()
                                                                : Maybe.of(IntSeq.cons(y / x, ys)))))
                .orElseGet(() -> parseIntOpt(item).map(n -> IntSeq.cons(n, stack)));
        }
    }

    static Maybe<Integer> parseIntOpt(String str) {
        try {
            return Maybe.of(Integer.parseInt(str));
        } catch (NumberFormatException ex) {
            return Maybe.empty();
        }
    }

    static final class StatefulRPN {
        private StatefulRPN() {}

        public static int solve(String expression) {
            return Seq.of(SPACES.split(expression))
                      .foldRight(pop(), (item, next) -> scan(item).then(next))
                      .eval(IntSeq.nil());
        }

        static MonadState<IntSeq, Integer> scan(String item) {
            switch (item) {
            case "+": return pop().bind(x -> pop().bind(y -> push(y + x)));
            case "-": return pop().bind(x -> pop().bind(y -> push(y - x)));
            case "*": return pop().bind(x -> pop().bind(y -> push(y * x)));
            case "/": return pop().bind(x -> pop().bind(y -> push(y / x)));
            default:  return push(Integer.parseInt(item));
            }
        }

        static MonadState<IntSeq, Integer> push(int a) {
            return MonadState.state(xs -> Tuple.of(0, IntSeq.cons(a, xs)));
        }

        static MonadState<IntSeq, Integer> pop() {
            return MonadState.state(xs -> Tuple.of(xs.head(), xs.tail()));
        }
    }

    static final class StatefulSafeRPN {
        private StatefulSafeRPN() {}

        public static OptionalInt solve(String expression) {
            return Seq.of(SPACES.split(expression))
                      .foldRight(getResult(), (item, next) -> scan(item).then(next))
                      .eval(Maybe.of(IntSeq.nil()));
        }

        static MonadState<Maybe<IntSeq>, Unit> scan(String item) {
            switch (item) {
            case "+": return pop().bind(x -> pop().bind(y -> push(y + x)));
            case "-": return pop().bind(x -> pop().bind(y -> push(y - x)));
            case "*": return pop().bind(x -> pop().bind(y -> push(y * x)));
            case "/": return pop().bind(x -> x == 0 ? push(Maybe.empty())
                                                    : pop().bind(y -> push(y / x)));
            default:  return push(parseIntOpt(item));
            }
        }

        static MonadState<Maybe<IntSeq>, Unit> push(int x) {
            return MonadState.modify((Maybe<IntSeq> stack) ->
                select.from(stack, xs ->
                       yield(Maybe.of(IntSeq.cons(x, xs))))
                      .orElse(Maybe.<IntSeq>empty()));
        }

        static MonadState<Maybe<IntSeq>, Unit> push(Maybe<Integer> item) {
            return MonadState.modify((Maybe<IntSeq> stack) ->
                select.from(stack, xs -> from(item, x ->
                       yield(Maybe.of(IntSeq.cons(x, xs)))))
                      .orElse(Maybe.<IntSeq>empty()));
        }

        static MonadState<Maybe<IntSeq>, Integer> pop() {
            return MonadState.state((Maybe<IntSeq> stack) ->
                select.from(stack, as(IntCons((x, xs) ->
                       yield(Tuple.of(x, Maybe.of(xs))))))
                      .orElseGet(() -> Tuple.of(0, Maybe.empty())));
        }

        static MonadState<Maybe<IntSeq>, OptionalInt> getResult() {
            return pop().mapState((result, stack) ->
                select.from(stack, (IntSeq xs) -> where(xs.isEmpty(),
                       yield(Tuple.of(OptionalInt.of(result), Maybe.<IntSeq>empty()))))
                      .orElseGet(() -> Tuple.of(OptionalInt.empty(), Maybe.<IntSeq>empty())));
        }
    }

    @Test
    public void rpnTest() {
        assertEquals(-4,   RPN.solve("10 4 3 + 2 * -"));
        assertEquals(4037, RPN.solve("90 34 12 33 55 66 + * - + -"));

        assertEquals(-4,   StatefulRPN.solve("10 4 3 + 2 * -"));
        assertEquals(4037, StatefulRPN.solve("90 34 12 33 55 66 + * - + -"));

        assertEquals(OptionalInt.of(6),   SafeRPN.solve("1 2 * 4 +"));
        assertEquals(OptionalInt.of(30),  SafeRPN.solve("1 2 * 4 + 5 *"));
        assertEquals(OptionalInt.empty(), SafeRPN.solve("1 + 2 * 4"));
        assertEquals(OptionalInt.empty(), SafeRPN.solve("1 2 * 4"));
        assertEquals(OptionalInt.empty(), SafeRPN.solve("2 1 1 - /"));
        assertEquals(OptionalInt.empty(), SafeRPN.solve("1 8 whatever"));

        assertEquals(OptionalInt.of(6),   StatefulSafeRPN.solve("1 2 * 4 +"));
        assertEquals(OptionalInt.of(30),  StatefulSafeRPN.solve("1 2 * 4 + 5 *"));
        assertEquals(OptionalInt.empty(), StatefulSafeRPN.solve("1 + 2 * 4"));
        assertEquals(OptionalInt.empty(), StatefulSafeRPN.solve("1 2 * 4"));
        assertEquals(OptionalInt.empty(), StatefulSafeRPN.solve("2 1 1 - /"));
        assertEquals(OptionalInt.empty(), StatefulSafeRPN.solve("1 8 whatever"));
    }

    static class Door {
        enum State { OPENED, CLOSED }
        enum Action { OPEN, CLOSE }

        private final State state;
        private final SeqZipper<Action> actions;

        public Door(State initial) {
            this(initial, SeqZipper.empty());
        }

        private Door(State state, SeqZipper<Action> actions) {
            this.state = state;
            this.actions = actions;
        }

        public State state() {
            return state;
        }

        public Seq<Action> actions() {
            return actions.toList();
        }

        private Door transfer(State state, Action action) {
            return this.state != state ? new Door(state, actions.push(action)) : this;
        }

        private static MonadState<Door, State> action(Action action, State state) {
            return MonadState.narrow(
                do_(MonadState.modify(s -> s.transfer(state, action)),
                do_(MonadState.pure(state))));
        }

        public static MonadState<Door, State> open() {
            return action(Action.OPEN, State.OPENED );
        }

        public static MonadState<Door, State> close() {
            return action(Action.CLOSE, State.CLOSED);
        }
    }

    @Test
    public void doorStateTest() {
        MonadState<Door, Door.State> states =
            Door.open().then(Door.open()).then(Door.close()).then(Door.close());
        Tuple<Door.State, Door> result = states.run(new Door(Door.State.CLOSED));
        assertEquals(Door.State.CLOSED, result.first());
        assertEquals(Door.State.CLOSED, result.second().state());
        assertEquals(Arrays.asList(Door.Action.OPEN, Door.Action.CLOSE), result.second().actions().toList());
    }

    // Example use of State monad
    // Passes a string of dictionary {a,b,c}
    // Game is to produce a number from the string.
    // By default the game is off, a 'c' toggles the
    // game on and off. An 'a' gives +1 and a 'b' gives -1.
    // E.g.
    //   'ab'    = 0
    //   'ca'    = 1
    //   'cabca' = 0
    // State = game is on or off & current score

    @SuppressWarnings("serial")
    static class GameState extends Tuple<Boolean, Integer> {
        public GameState(Boolean left, Integer right) {
            super(left, right);
        }
    }

    static <R, X extends Throwable> ConditionCase<GameState, R, X>
    GameState(ExceptionBiFunction<Boolean, Integer, ? extends R, X> mapper) {
        return t -> () -> mapper.evaluate(t.first(), t.second());
    }

    static final class Game {
        private Game() {}

        public static MonadState<GameState, Integer> play(String input) {
            return MonadState.narrow(
                do_(MonadState.mapM_(Seq.wrap(input), Game::scan),
                do_(MonadState.get(), as(GameState((on, score) ->
                do_(MonadState.pure(score)))))));
        }

        static MonadState<GameState, Unit> scan(char x) {
            return MonadState.modify(as(GameState((on, score) ->
                        x == 'a' && on ? new GameState(on, score + 1) :
                        x == 'b' && on ? new GameState(on, score - 1) :
                        x == 'c'       ? new GameState(!on, score)
                                       : new GameState(on, score)
                   )));
        }
    }

    @Test
    public void gameStateTest() {
        GameState startState = new GameState(false, 0);
        assertEquals(0, (int)Game.play("ab").eval(startState));
        assertEquals(1, (int)Game.play("ca").eval(startState));
        assertEquals(0, (int)Game.play("cabca").eval(startState));
        assertEquals(2, (int)Game.play("abcaaacbbcabbab").eval(startState));
    }

    @Test
    public void flatMTest() {
        MonadState<Integer, Integer> inc =
            MonadState.get(x -> MonadState.put(x+1).then(MonadState.pure(x)));

        Seq<MonadState<Integer, Integer>> states = Seq.replicate(3, inc);
        Tuple<Seq<Integer>, Integer> res = MonadState.flatM(states).run(0);
        Tuple<Unit, Integer> res_ = MonadState.sequence(states).run(0);
        assertSeqEquals(res.first(), 0,1,2);
        assertEquals(3, (int)res.second());
        assertEquals(Unit.U, res_.first());
        assertEquals(3, (int)res.second());
    }

    @Test
    public void stateRunOverflowTest() {
        MonadState<Integer, Integer> inc =
            MonadState.get(x -> MonadState.put(x+1).then(MonadState.pure(x)));

        // ensure no stack overflow
        MonadState.flatM(Seq.replicate(10_000, inc)).run(0).first().count();
        MonadState.sequence(Seq.replicate(10_000, inc)).run(0);
    }

    @Test
    public void mapMTest() {
        Function<Integer, MonadState<Integer, Integer>> add =
            x -> MonadState.get(y -> MonadState.put(x+y).then(MonadState.pure(y)));

        Seq<Integer> xs = Seq.of(1, 2, 3);
        Tuple<Seq<Integer>, Integer> res = MonadState.mapM(xs, add).run(0);
        Tuple<Unit, Integer> res_ = MonadState.mapM_(xs, add).run(0);
        // x  y  z
        //    0  0 <- initial
        // 1  1  0
        // 2  3  1
        // 3  6  3 <- final
        assertSeqEquals(res.first(), 0, 1, 3);
        assertEquals(6, (int)res.second());
        assertEquals(Unit.U, res_.first());
        assertEquals(6, (int)res_.second());
    }

    @Test
    public void filterMTest() {
        Function<Integer, MonadState<Integer, Boolean>> fadd =
            x -> x == 0 ? MonadState.pure(false)
                        : MonadState.get(y -> MonadState.put(x+y).then(MonadState.pure(true)));

        Seq<Integer> xs = Seq.of(1, 2, 0, 3);
        Tuple<Seq<Integer>, Integer> result = MonadState.filterM(xs, fadd).run(0);
        assertSeqEquals(result.first(), 1, 2, 3);
        assertEquals(6, (int)result.second());
    }

    @Test
    public void foldMTest() {
        BiFunction<Integer, Integer, MonadState<Integer, Integer>> add2 =
            (z, x) -> MonadState.get(y -> MonadState.put(x + y + z).then(MonadState.pure(z + x)));

        Seq<Integer> xs = Seq.of(1, 2, 3);
        Tuple<Integer, Integer> result = MonadState.foldM(0, xs, add2).run(0);
        // x  y  z
        //    0  0 <- initial
        // 1  1  1
        // 2  4  3
        // 3  10 6 <- final
        assertEquals(6, (int)result.first());
        assertEquals(10, (int)result.second());
    }

    @SafeVarargs
    private static <T> void assertSeqEquals(Seq<T> xs, T... expected) {
        for (T x : expected) {
            assertEquals(x, xs.head());
            xs = xs.tail();
        }
        assertTrue(xs.isEmpty());
    }

    @Test
    public void monadTransformerTest() {
        StateT.On<Integer, ReaderT.On<Integer, Identity.µ>> sr = StateT.on(ReaderT.on(Identity.tclass));
        $<StateT.On<Integer, ReaderT.On<Integer, Identity.µ>>, Integer> b =
            do_(sr.get(), x ->
            do_(sr.lift(ReaderT::ask), y ->
            do_(sr.pure(x + y))));
        assertEquals(Tuple.of(7, 3), runIdentity(sr.inner().runReader(sr.runState(b, 3), 4)));
    }
}
