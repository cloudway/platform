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
import com.cloudway.platform.common.fp.control.trans.State;
import com.cloudway.platform.common.fp.control.trans.RWST;
import com.cloudway.platform.common.fp.control.trans.ReaderT;
import com.cloudway.platform.common.fp.control.trans.StateT;
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
import static com.cloudway.platform.common.fp.data.IntSeq.IntCons;
import static com.cloudway.platform.common.fp.data.IntSeq.IntSingle;
import static com.cloudway.platform.common.fp.data.Maybe.Just;
import static java.lang.String.format;

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

        static State<IntSeq, Integer> scan(String item) {
            switch (item) {
            case "+": return pop().bind(x -> pop().bind(y -> push(y + x)));
            case "-": return pop().bind(x -> pop().bind(y -> push(y - x)));
            case "*": return pop().bind(x -> pop().bind(y -> push(y * x)));
            case "/": return pop().bind(x -> pop().bind(y -> push(y / x)));
            default:  return push(Integer.parseInt(item));
            }
        }

        static State<IntSeq, Integer> push(int a) {
            return State.state(xs -> Tuple.of(0, IntSeq.cons(a, xs)));
        }

        static State<IntSeq, Integer> pop() {
            return State.state(xs -> Tuple.of(xs.head(), xs.tail()));
        }
    }

    static final class StatefulSafeRPN {
        private StatefulSafeRPN() {}

        public static OptionalInt solve(String expression) {
            return Seq.of(SPACES.split(expression))
                      .foldRight(getResult(), (item, next) -> scan(item).then(next))
                      .eval(Maybe.of(IntSeq.nil()));
        }

        static State<Maybe<IntSeq>, Unit> scan(String item) {
            switch (item) {
            case "+": return pop().bind(x -> pop().bind(y -> push(y + x)));
            case "-": return pop().bind(x -> pop().bind(y -> push(y - x)));
            case "*": return pop().bind(x -> pop().bind(y -> push(y * x)));
            case "/": return pop().bind(x -> x == 0 ? push(Maybe.empty())
                                                    : pop().bind(y -> push(y / x)));
            default:  return push(parseIntOpt(item));
            }
        }

        static State<Maybe<IntSeq>, Unit> push(int x) {
            return State.modify((Maybe<IntSeq> stack) ->
                select.from(stack, xs ->
                       yield(Maybe.of(IntSeq.cons(x, xs))))
                      .orElse(Maybe.<IntSeq>empty()));
        }

        static State<Maybe<IntSeq>, Unit> push(Maybe<Integer> item) {
            return State.modify((Maybe<IntSeq> stack) ->
                select.from(stack, xs -> from(item, x ->
                       yield(Maybe.of(IntSeq.cons(x, xs)))))
                      .orElse(Maybe.<IntSeq>empty()));
        }

        static State<Maybe<IntSeq>, Integer> pop() {
            return State.state((Maybe<IntSeq> stack) ->
                select.from(stack, as(IntCons((x, xs) ->
                       yield(Tuple.of(x, Maybe.of(xs))))))
                      .orElseGet(() -> Tuple.of(0, Maybe.empty())));
        }

        static State<Maybe<IntSeq>, OptionalInt> getResult() {
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
        enum DState { OPENED, CLOSED }
        enum DAction { OPEN, CLOSE }

        private final DState state;
        private final SeqZipper<DAction> actions;

        public Door(DState initial) {
            this(initial, SeqZipper.empty());
        }

        private Door(DState state, SeqZipper<DAction> actions) {
            this.state = state;
            this.actions = actions;
        }

        public DState state() {
            return state;
        }

        public Seq<DAction> actions() {
            return actions.toList();
        }

        private Door transfer(DState state, DAction action) {
            return this.state != state ? new Door(state, actions.push(action)) : this;
        }

        private static State<Door, DState> action(DAction action, DState state) {
            return State.narrow(
                do_(State.modify(s -> s.transfer(state, action)),
                do_(State.pure(state))));
        }

        public static State<Door, DState> open() {
            return action(DAction.OPEN, DState.OPENED );
        }

        public static State<Door, DState> close() {
            return action(DAction.CLOSE, DState.CLOSED);
        }
    }

    @Test
    public void doorStateTest() {
        State<Door, Door.DState> states =
            Door.open().then(Door.open()).then(Door.close()).then(Door.close());
        Tuple<Door.DState, Door> result = states.run(new Door(Door.DState.CLOSED));
        assertEquals(Door.DState.CLOSED, result.first());
        assertEquals(Door.DState.CLOSED, result.second().state());
        assertEquals(Arrays.asList(Door.DAction.OPEN, Door.DAction.CLOSE), result.second().actions().toList());
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

        public static State<GameState, Integer> play(String input) {
            return State.narrow(
                do_(State.mapM_(Seq.wrap(input), Game::scan),
                do_(State.get(), as(GameState((on, score) ->
                do_(State.pure(score)))))));
        }

        static State<GameState, Unit> scan(char x) {
            return State.modify(as(GameState((on, score) ->
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
        State<Integer, Integer> inc =
            State.get(x -> State.put(x+1).then(State.pure(x)));

        Seq<State<Integer, Integer>> states = Seq.replicate(3, inc);
        Tuple<Seq<Integer>, Integer> res = State.flatM(states).run(0);
        Tuple<Unit, Integer> res_ = State.sequence(states).run(0);
        assertSeqEquals(res.first(), 0,1,2);
        assertEquals(3, (int)res.second());
        assertEquals(Unit.U, res_.first());
        assertEquals(3, (int)res.second());
    }

    @Test
    public void stateRunOverflowTest() {
        State<Integer, Integer> inc =
            State.get(x -> State.put(x+1).then(State.pure(x)));

        // ensure no stack overflow
        State.flatM(Seq.replicate(10_000, inc)).run(0).first().count();
        State.sequence(Seq.replicate(10_000, inc)).run(0);
    }

    @Test
    public void mapMTest() {
        Function<Integer, State<Integer, Integer>> add =
            x -> State.get(y -> State.put(x+y).then(State.pure(y)));

        Seq<Integer> xs = Seq.of(1, 2, 3);
        Tuple<Seq<Integer>, Integer> res = State.mapM(xs, add).run(0);
        Tuple<Unit, Integer> res_ = State.mapM_(xs, add).run(0);
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
        Function<Integer, State<Integer, Boolean>> fadd =
            x -> x == 0 ? State.pure(false)
                        : State.get(y -> State.put(x+y).then(State.pure(true)));

        Seq<Integer> xs = Seq.of(1, 2, 0, 3);
        Tuple<Seq<Integer>, Integer> result = State.filterM(xs, fadd).run(0);
        assertSeqEquals(result.first(), 1, 2, 3);
        assertEquals(6, (int)result.second());
    }

    @Test
    public void foldMTest() {
        BiFunction<Integer, Integer, State<Integer, Integer>> add2 =
            (z, x) -> State.get(y -> State.put(x + y + z).then(State.pure(z + x)));

        Seq<Integer> xs = Seq.of(1, 2, 3);
        Tuple<Integer, Integer> result = State.foldM(0, xs, add2).run(0);
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
        StateT<Integer, ReaderT<Integer, Identity.µ>> sr = StateT.on(ReaderT.on(Identity.tclass));
        $<StateT<Integer, ReaderT<Integer, Identity.µ>>, Integer> b =
            do_(sr.get(), x ->
            do_(ReaderT.ask(sr), y ->
            do_(sr.pure(x + y))));
        assertEquals(Tuple.of(7, 3), Identity.run(ReaderT.run(StateT.run(b, 3), 4)));
    }

    // Phrases from The Hitchhiker's Guide to the Galaxy
    private static final String QUESTION =
        "Ultimate Question of Life, the Universe, and Everything";
    private static final String ANSWER =
        "Answer to the Ultimate Question of Life, the Universe, and Everything is 42";

    @Test
    public void deepThought() {
        RWST<String, Seq<String>, Integer, Identity.µ> st = RWST.on(Seq.monoid(), Identity.tclass);

        $<RWST<String, Seq<String>, Integer, Identity.µ>, String> m =
            do_(st.tell(Seq.of("Leaning ...")),
            do_(st.ask(), question ->
            do_(st.tell(Seq.of(format("Question: %s", question))),
            do_(st.tell(Seq.of("Computing ...")),
            do_(st.get(), answer ->
            do_(st.tell(Seq.of("After 7.5 million years")),
            do_(st.tell(Seq.of(format("Answer: %d", answer))),
            do_(st.pure(format("Answer to the %s is %d", question, answer))))))))));

        Tuple<String, Seq<String>> r = Identity.run(RWST.eval(m, QUESTION, 42));
        assertEquals(ANSWER, r.first());
        assertSeqEquals(r.second(), "Leaning ...",
                                    "Question: " + QUESTION,
                                    "Computing ...",
                                    "After 7.5 million years",
                                    "Answer: 42");
    }
}
