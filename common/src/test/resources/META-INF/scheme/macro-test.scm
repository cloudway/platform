;;; numeric-if test

; This tests how many times everything gets evaluated
(define probe 0)

(define (neg-action) (set! probe (+ probe 1)) "negative!")
(define (zero-action) (set! probe (+ probe 10)) "zero!")
(define (pos-action) (set! probe (+ probe 100)) "positive!")

; This version is careful to evaluate tval only once
(define-macro (numeric-if tval ifn ifz ifp)
  (let ((nit (gensym)))
    `(let ((,nit ,tval))
       (if (number? ,nit)
           (if (negative? ,nit)
               ,ifn
               (if (positive? ,nit)
                   ,ifp
                   ,ifz))
           (error "error: non-numeric test value passed to numeric-if")))))

(set! probe 0)
(numeric-if 1 (neg-action) (zero-action) (pos-action))
(assert (= 100 probe))

(set! probe 0)
(numeric-if -1 (neg-action) (zero-action) (pos-action))
(assert (= 1 probe))

(set! probe 0)
(numeric-if 0 (neg-action) (zero-action) (pos-action))
(assert (= 10 probe))

;;; swap test
(define-macro (swap var1 var2)
  (let ((vs (gensym)))
    `(let ((,vs ,var1))
       (set! ,var1 ,var2)
       (set! ,var2, vs))))

(define val1 0)
(define val2 1)
(swap val1 val2)
(assert (= val1 1))
(assert (= val2 0))
