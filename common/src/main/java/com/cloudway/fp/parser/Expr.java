/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.parser;

import java.util.function.BinaryOperator;
import java.util.function.UnaryOperator;

import com.cloudway.fp.$;
import com.cloudway.fp.data.Seq;

import static com.cloudway.fp.control.Syntax.choice;
import static com.cloudway.fp.control.Syntax.do_;
import static com.cloudway.fp.data.Seq.cons;
import static com.cloudway.fp.data.Seq.nil;

/**
 * A helper module to parse "expressions". Build a parser given a table
 * of operators and associativities.
 */
public final class Expr {
    private Expr() {}

    /**
     * This enum type specifies the associativity of operators.
     */
    public enum Assoc {
        None, Left, Right
    }

    /**
     * This data type specifies operators that works on values. An operator
     * is either binary infix or unary prefix or postfix. A binary operator
     * has also an associated associativity.
     *
     * @param <P> the parser type
     * @param <A> the value type
     */
    public interface Operator<P, A> {
        /**
         * Returns the operator name.
         */
        String getName();
    }

    /**
     * The binary infix operator.
     */
    private static class Infix<P, A> implements Operator<P, A> {
        final String name;
        final $<P, BinaryOperator<A>> op;
        final Assoc assoc;

        Infix(String name, $<P, BinaryOperator<A>> op, Assoc assoc) {
            this.name  = name;
            this.op    = op;
            this.assoc = assoc;
        }

        @Override
        public String getName() {
            return name;
        }

        public String toString() {
            return assoc + " assoc " + name;
        }
    }

    /**
     * The unary prefix operator.
     */
    private static class Prefix<P, A> implements Operator<P, A> {
        final String name;
        final $<P, UnaryOperator<A>> op;

        Prefix(String name, $<P, UnaryOperator<A>> op) {
            this.name = name;
            this.op = op;
        }

        @Override
        public String getName() {
            return name;
        }

        public String toString() {
            return "Prefix " + name;
        }
    }

    /**
     * The unary postfix operator.
     */
    private static class Postfix<P, A> implements Operator<P, A> {
        final String name;
        final $<P, UnaryOperator<A>> op;

        Postfix(String name, $<P, UnaryOperator<A>> op) {
            this.name = name;
            this.op = op;
        }

        @Override
        public String getName() {
            return name;
        }

        public String toString() {
            return "Postfix " + name;
        }
    }

    /**
     * Construct a binary infix operator with associated associativity.
     */
    public static <P, A> Operator<P, A> infix(String name, $<P, BinaryOperator<A>> op, Assoc assoc) {
        return new Infix<>(name, op, assoc);
    }

    /**
     * Construct a unary prefix operator.
     */
    public static <P, A> Operator<P, A> prefix(String name, $<P, UnaryOperator<A>> op) {
        return new Prefix<>(name, op);
    }

    /**
     * Construct a unary postfix operator.
     */
    public static <P, A> Operator<P, A> postfix(String name, $<P, UnaryOperator<A>> op) {
        return new Postfix<>(name, op);
    }

    /**
     * Builds an expression parser for terms with operators, taking the
     * associativity and precedence specified in the given operator table into
     * account. Prefix and postfix operators of the same precedence can only
     * occur once (i.e. {@code --2} is not allowed if {@code -} is prefix
     * negative). Prefix and postfix operators of the same precedence associate
     * to the left (i.e. if {@code ++} is postfix increment, then {@code -2++}
     * equals {@code -1}, not {@code -3}.
     *
     * <p>This method takes care of all the complexity involved in building
     * expression parser.
     *
     * @param parser the underlying parser used to build the expression parser
     * @param operators the operator table
     * @param simpleExpr a simple parser parses expression term
     */
    public static <P extends ParserT<P,?,?,?>, A>
    $<P, A> buildExpressionParser(P parser, Seq<Seq<Operator<P, A>>> operators, $<P, A> simpleExpr) {
        return operators.foldLeft(simpleExpr, (term, ops) ->
            new Builder<P,A>(parser).build(ops, term));
    }

    // Internal

    private static class Builder<P extends ParserT<P,?,?,?>, A> {
        private final P pt;

        Builder(P pt) {
            this.pt = pt;
        }

        // intermediate variables for parser combinators

        private Seq<$<P, BinaryOperator<A>>> rassoc  = nil();
        private Seq<$<P, BinaryOperator<A>>> lassoc  = nil();
        private Seq<$<P, BinaryOperator<A>>> nassoc  = nil();
        private Seq<$<P, UnaryOperator<A>>>  prefix  = nil();
        private Seq<$<P, UnaryOperator<A>>>  postfix = nil();

        private $<P, BinaryOperator<A>> rassocOp;
        private $<P, BinaryOperator<A>> lassocOp;
        private $<P, BinaryOperator<A>> nassocOp;
        private $<P, UnaryOperator<A>>  prefixOp;
        private $<P, UnaryOperator<A>>  postfixOp;

        private $<P, A> ambiguousRight;
        private $<P, A> ambiguousLeft;
        private $<P, A> ambiguousNone;

        $<P, A> build(Seq<Operator<P, A>> ops, $<P, A> term) {
            ops.forEach(this::splitOp);

            rassocOp  = pt.choice(rassoc);
            lassocOp  = pt.choice(lassoc);
            nassocOp  = pt.choice(nassoc);
            prefixOp  = pt.label("", pt.choice(prefix));
            postfixOp = pt.label("", pt.choice(postfix));

            ambiguousRight = ambiguous("right", rassocOp);
            ambiguousLeft  = ambiguous("left", lassocOp);
            ambiguousNone  = ambiguous("none", nassocOp);

            UnaryOperator<A> id = x -> x;

            $<P, A> termP =
                   do_(choice(prefixOp, pt.pure(id)), pre ->
                   do_(term, x ->
                   do_(choice(postfixOp, pt.pure(id)), post ->
                   do_(pt.pure(post.apply(pre.apply(x)))))));

            return do_(termP, x ->
                   do_(pt.label("operator",
                   choice(
                     rassocP(termP, x)
                   , lassocP(termP, x)
                   , nassocP(termP, x)
                   , pt.pure(x)))));
        }

        @SuppressWarnings("unchecked")
        private void splitOp(Operator<P, A> oper) {
            if (oper instanceof Infix) {
                Infix<P, A> infix = (Infix<P,A>)oper;
                switch (infix.assoc) {
                case None:
                    nassoc = cons(infix.op, nassoc);
                    break;
                case Left:
                    lassoc = cons(infix.op, lassoc);
                    break;
                case Right:
                    rassoc = cons(infix.op, rassoc);
                    break;
                }
            } else if (oper instanceof Prefix) {
                prefix = cons(((Prefix<P,A>)oper).op, prefix);
            } else if (oper instanceof Postfix) {
                postfix = cons(((Postfix<P,A>)oper).op, postfix);
            }
        }

        private $<P, A> ambiguous(String assoc, $<P, BinaryOperator<A>> op) {
            return pt.attempt(
                do_(op,
                do_(pt.fail("ambiguous use of a " + assoc + " associative operator"))));
        }

        private $<P, A> rassocP($<P, A> termP, A x) {
            return choice(
                do_(rassocOp, f ->
                do_(do_(termP, z -> rassocP1(termP, z)), y ->
                do_(pt.pure(f.apply(x, y)))))
              , ambiguousLeft
              , ambiguousNone);
        }

        private $<P, A> rassocP1($<P, A> termP, A x) {
            return choice(rassocP(termP, x), pt.pure(x));
        }

        private $<P, A> lassocP($<P, A> termP, A x) {
            return choice(
                do_(lassocOp, f ->
                do_(termP, y ->
                do_(lassocP1(termP, f.apply(x, y)))))
              , ambiguousRight
              , ambiguousNone
            );
        }

        private $<P, A> lassocP1($<P, A> termP, A x) {
            return choice(lassocP(termP, x), pt.pure(x));
        }

        private $<P, A> nassocP($<P, A> termP, A x) {
            return
                do_(nassocOp, f ->
                do_(termP, y ->
                choice(
                  ambiguousRight
                , ambiguousLeft
                , ambiguousNone
                , pt.pure(f.apply(x, y)))));
        }
    }
}
