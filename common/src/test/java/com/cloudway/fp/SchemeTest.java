/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp;

import com.cloudway.fp.scheme.Evaluator;
import com.cloudway.fp.scheme.LispVal;
import org.junit.Test;
import static org.junit.Assert.*;

import com.cloudway.fp.data.Fn;
import static com.cloudway.fp.scheme.LispVal.*;

public class SchemeTest {
    private final Evaluator evaluator = new Evaluator();

    private LispVal eval(String expr) {
        return evaluator.evaluate(expr).getOrThrow(Fn.id());
    }

    private static void assertEqualsVal(long val, LispVal expr) {
        assertTrue((expr instanceof Num) && ((Num)expr).value.longValue() == val);
    }

    @Test
    public void recursionTest() {
        String factorial =
            "(define (factorial n)\n" +
            "  (if (<= n 0) 1 (* n (factorial (- n 1)))))\n" +
            "(factorial 10)\n";
        assertEqualsVal(3628800, eval(factorial));

        String fibonacci =
            "(define (fibonacci n)\n" +
            "  (define (iter i a b)\n" +
            "     (if (<= i 1) b (iter (- i 1) b (+ a b))))\n" +
            "  (iter n 0 1))\n" +
            "(fibonacci 12)\n";
        assertEqualsVal(144, eval(fibonacci));

        String letrec =
            "(let fac ([n 10])" +
            "  (if (zero? n)" +
            "      1" +
            "      (* n (fac (- n 1)))))";
        assertEqualsVal(3628800, eval(letrec));

        String even_odd =
            "(letrec ([is-even? (lambda (n)\n" +
            "                     (or (zero? n)\n" +
            "                         (is-odd? (- n 1))))]\n" +
            "         [is-odd? (lambda (n)\n" +
            "                     (and (not (zero? n))\n" +
            "                          (is-even? (- n 1))))])\n" +
            "  (is-odd? 11))";
        assertEquals("#t", eval(even_odd).show());

        String do_loop =
            "(do ([i 1 (+ i 1)] \n" +
            "     [s 0 (+ s i)])\n" +
            "    ((> i 10) s))\n";
        assertEqualsVal(55, eval(do_loop));
    }

    @Test
    public void quasiquoteTest() {
        assertEquals("(list a 'a)",
            eval("(let ((name 'a)) `(list ,name ',name))").show());
        assertEquals("(list a 'a)",
            eval("(let ((name 'a)) (quasiquote (list (unquote name) (quote (unquote name)))))").show());

        assertEquals("(a 3 4 5 6 b)",
            eval("`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)").show());
        assertEquals("#(a 3 4 5 6 b)",
            eval("`#(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)").show());

        assertEquals("((foo 7) . cons)",
            eval("`((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))").show());

        assertEquals("(10 5 4 16 9 8)",
            eval("(define (pow x) (* x x)) `(10 5 ,(pow 2) ,@(map pow '(4 3)) 8)").show());
        assertEquals("#(10 5 4 16 9 8)",
            eval("(define (pow x) (* x x)) `#(10 5 ,(pow 2) ,@(map pow '(4 3)) 8)").show());

        assertEquals("(a `(b ,(+ 1 2) ,(foo 4 d) e) f)",
            eval("`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)").show());
        assertEquals("#(a `#(b ,(+ 1 2) ,(foo 4 d) e) f)",
            eval("`#(a `#(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)").show());

        assertEquals("(a `(b ,x ,'y d) e)",
            eval("(let ((name1 'x) (name2 'y)) `(a `(b ,,name1 ,',name2 d) e))").show());
        assertEquals("#(a `#(b ,x ,'y d) e)",
            eval("(let ((name1 'x) (name2 'y)) `#(a `#(b ,,name1 ,',name2 d) e))").show());
    }

    @Test
    public void macroTest() {
        eval("(load \"macro-test.scm\")");
    }

    @Test
    public void sicpTest() {
        eval("(load \"sicp-test.scm\")");
    }
}
