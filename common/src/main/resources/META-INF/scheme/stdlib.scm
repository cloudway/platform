;;; Macros

(define-macro (with-gensyms syms . body)
  `(let ,(map (lambda (s) `(,s (gensym))) syms) ,@body))

(define-macro (assert some-cond)
  `(if (not ,some-cond)
     (error 'assert "assertion failure" ',some-cond)))

(define-macro (when pred . actions)
  `(if ,pred (begin ,@actions)))

(define-macro (unless pred . actions)
  `(if (not ,pred) (begin ,@actions)))

(define-macro (while some-cond . some-actions)
  (let ((mc (gensym)))
    `(do ((,mc 0 (+ ,mc 1)))
         ((not ,some-cond) ,mc)
       ,@some-actions)))

(define-macro (until some-cond . some-actions)
  (let ((mc (gensym)))
      `(do ((,mc 0 (+ ,mc 1)))
           (,some-cond ,mc)
         ,@some-actions)))

(define-macro (case-lambda . patterns)
  (let ((args (gensym)))
    `(lambda ,args (match ,args ,@patterns))))


(define-macro (case key . clauses)
  (if (not (symbol? key))
      (let ((atom-key (gensym)))
        `(let ((,atom-key ,key)) (case ,atom-key ,@clauses)))
      (match clauses
        [(('else . result))
          `(begin ,@result)]

        [(((atom . atoms) . result) . rest)
          (if (null? rest)
              `(if (memv ,key '(,atom ,@atoms))
                   (begin ,@result))
              `(if (memv ,key '(,atom ,@atoms))
                   (begin ,@result)
                   (case ,key ,@rest)))]

        [((atom . result) . rest) :when (not (eqv? atom 'else))
          (if (null? rest)
              `(if (eqv? ,key ',atom)
                   (begin ,@result))
              `(if (eqv? ,key ',atom)
                   (begin ,@result)
                   (case ,key ,@rest)))]

        [else (error 'case "bad syntax" clauses)])))

(define-macro (:optional . form)
  (let ((x (gensym)))
    (match form
      ((value default)
        `(let ((,x ,value))
           (if (pair? ,x) (car ,x) ,default)))
      ((value default check)
        `(let ((,x ,value))
           (if (pair? ,x)
             (if (not (,check (car ,x)))
                 (error ':optional "value is not satisfy: " (car ,x))
                 (car ,x)))
             ,default))
      (else (error ':optional "bad syntax" form)))))

(define-macro (let-optionals* exp bindings . body)
  (define (expand val)
    (match bindings
      [() `(let () ,@body)]

      [((var default))
       `(let ((,var (if (pair? ,val) (car ,val) ,default))) ,@body)]

      [((var default check))
       `(let ((,var (if (pair? ,val) (car ,val) ,default)))
          (if (not ,check)
              (error 'let-optionals* "value is not satisfy: " ,var)
              (begin ,@body)))]

      [((var default) . rest)
        (let ((more (gensym)))
          `(if (pair? ,val)
               (let ((,var (car ,val)) (,more (cdr ,val)))
                 (let-optionals* ,more ,rest ,@body))
               (let ((,var ,default))
                 (let-optionals* () ,rest ,@body))))]

      [((var default check) . rest)
        (let ((more (gensym)))
          `(if (pair? ,val)
             (let ((,var (car ,val)) (,more (cdr ,val)))
               (if (not ,check)
                   (error 'let-optionals* "value is not satisfy: " ,var)
                   (let-optionals* ,more ,rest ,@body)))
             (let ((,var ,default))
               (let-optionals* () ,rest ,@body))))]

      [(tail)
        `(let ((,tail ,val)) ,@body)]

      [else (error 'let-optional* "bad syntax" bindings)]))

  (define (expand-default)
    (match bindings
      [() `(let () ,@body)]
      [((var default . check))
        `(let ((,var ,default)) ,@body)]
      [((var default . check) . rest)
        `(let ((,var ,default)) (let-optionals* () ,rest ,@body))]
      [(tail)
        `(let ((,tail '())) ,@body)]
      [else (error 'let-optional* "bad syntax" bindings)]))

  (cond ((symbol? exp) (expand exp))
        ((null?   exp) (expand-default))
        (else (let ((tmp (gensym))) `(let ((,tmp ,exp)) ,(expand `,tmp))))))


; SRFI 2: an AND with local bindings, a guarded LET* special form
(define-macro (and-let* claws . body)
  (let* ((new-vars '()) (result (cons 'and '())) (growth-point result))
    (define (andjoin! clause)
      (let ((prev-point growth-point) (clause-cell (cons clause '())))
        (set-cdr! growth-point clause-cell)
        (set! growth-point clause-cell)))

    (if (not (list? claws))
      (error 'and-let* "bindings must be a list " bindings))

    (for-each
      (lambda (claw)
        (match claw
          (var :when (symbol? var)
            (andjoin! var))
          ((expr)
            (andjoin! expr))
          ((var expr) :when (symbol? var)
            (let ((var-cell (cons var '())))
              (if (memq var new-vars)
                (error 'and-let* "duplicate variable " var " in the bindings"))
              (set! new-vars (cons var new-vars))
              (set-cdr! growth-point `((LET ((,var ,expr)) (AND . ,var-cell))))
              (set! growth-point var-cell)))
          (else
            (error 'and-let* "all ill-formed binding in a syntactic form and-let* " claw))))
      claws)

    (if (not (null? body))
      (andjoin! `(begin ,@body)))
    result))


; SRFI 31: A special form rec for recursive evaluation
(define-macro (rec . args)
  (match args
    [((name . variables) . body)
      `(letrec ((,name (lambda ,variables . ,@body))) ,name)]
    [(name expression)
      `(letrec ((,name ,expression)) ,name)]
    [else (error 'rec "bad syntax" args)]))


;;; Utilities

(define true  #t)
(define false #f)

(define (id x)          x)
(define (flip f)        (lambda (a b) (f b a)))
(define (curry f b)     (lambda (a) (f a b)))
(define (compose f g)   (lambda (a) (f (g a))))

(define (foldr op init lst)
  (if (null? lst)
      init
      (op (car lst) (foldr op init (cdr lst)))))

(define (foldl op init lst)
  (do ([result init (op result (car rest))]
       [rest lst (cdr rest)])
      ((null? rest) result)))

(define fold foldl)
(define reduce foldr)

(define (unfold func init pred)
  (if (pred init)
      (cons init '())
      (cons init (unfold func (func init) pred))))

;;; Numerical operations

(define (zero? n)       (= n 0))
(define (positive? n)   (> n 0))
(define (negative? n)   (< n 0))
(define (odd? n)        (= (remainder n 2) 1))
(define (even? n)       (= (remainder n 2) 0))

(define (abs x) (if (< x 0) (- x) x))

(define (max first . rest)
    (fold (lambda (old new) (if (> old new) old new)) first rest))

(define (min first . rest)
    (fold (lambda (old new) (if (< old new) old new)) first rest))

(define (gcd . vals)
  (define (g a b)
    (if (zero? b) a (g b (remainder a b))))
  (abs (fold g 0 vals)))

(define (lcm . vals)
  (/ (abs (apply * vals)) (apply gcd vals)))

;;; Pairs and lists

(define (caar pair) (car (car pair)))
(define (cadr pair) (car (cdr pair)))
(define (cdar pair) (cdr (car pair)))
(define (cddr pair) (cdr (cdr pair)))
(define (caaar pair) (car (car (car pair))))
(define (caadr pair) (car (car (cdr pair))))
(define (cadar pair) (car (cdr (car pair))))
(define (caddr pair) (car (cdr (cdr pair))))
(define (cdaar pair) (cdr (car (car pair))))
(define (cdadr pair) (cdr (car (cdr pair))))
(define (cddar pair) (cdr (cdr (car pair))))
(define (cdddr pair) (cdr (cdr (cdr pair))))
(define (caaaar pair) (car (car (car (car pair)))))
(define (caaadr pair) (car (car (car (cdr pair)))))
(define (caadar pair) (car (car (cdr (car pair)))))
(define (caaddr pair) (car (car (cdr (cdr pair)))))
(define (cadaar pair) (car (cdr (car (car pair)))))
(define (cadadr pair) (car (cdr (car (cdr pair)))))
(define (caddar pair) (car (cdr (cdr (car pair)))))
(define (cadddr pair) (car (cdr (cdr (cdr pair)))))
(define (cdaaar pair) (cdr (car (car (car pair)))))
(define (cdaadr pair) (cdr (car (car (cdr pair)))))
(define (cdadar pair) (cdr (car (cdr (car pair)))))
(define (cdaddr pair) (cdr (car (cdr (cdr pair)))))
(define (cddaar pair) (cdr (cdr (car (car pair)))))
(define (cddadr pair) (cdr (cdr (car (cdr pair)))))
(define (cdddar pair) (cdr (cdr (cdr (car pair)))))
(define (cddddr pair) (cdr (cdr (cdr (cdr pair)))))

(define (list-tail lst k)
  (if (zero? k)
      lst
      (list-tail (cdr lst) (- k 1))))

(define (list-ref lst k)
  (if (zero? k)
      (car lst)
      (list-ref (cdr lst) (- k 1))))

(define (mem-helper item x op)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(define (memq item x)       (mem-helper item x eq?))
(define (memv item x)       (mem-helper item x eqv?))
(define (member item x)     (mem-helper item x equal?))

(define (ass-helper pred op)
    (lambda (acc next)
        (if (and (not acc) (pred (op next))) next acc)))

(define (assq obj alist)    (fold (ass-helper (curry eq? obj) car) #f alist))
(define (assv obj alist)    (fold (ass-helper (curry eqv? obj) car) #f alist))
(define (assoc obj alist)   (fold (ass-helper (curry equal? obj) car) #f alist))

; Generator based on delimited continuations

(define (yield a)
  (shift k (cons a k)))

(define-macro (generator . body)
  (let ((susp (gensym)))
    `(let ((,susp (reset ,@body '())))
      (lambda ()
        (match ,susp
          (() #f)
          ((a . k) (set! ,susp (k '())) a))))))

(define (step proc gen)
  (do ((x (gen) (gen)))
      ((not x) (void))
      (proc x)))

;;; dynamic-wind
;
; This implementation is relatively costly: we have to shadow call/cc
; with a new version that unwinds suspended thunks, but for this to
; happen the return-values of the escaping procedure have to be saved
; temporarily in a list. Since call/cc is very efficient under this
; implementation, and because allocation of memory that is to be
; garbage soon has also quite low overhead, the performance-penalty
; might be acceptable.

(define %%sys%dynamic-winds #f)

(define (dynamic-wind before thunk after)
  (unless %%sys%dynamic-winds
    (letrec ([sys:call/cc call-with-current-continuation]
      [call-cc (lambda (proc)
        (let ((winds %%sys%dynamic-winds))
          (sys:call/cc
            (lambda (cont)
              (proc (lambda results
                      (unless (eq? %%sys%dynamic-winds winds)
                        (dynamic-unwind winds (- (length %%sys%dynamic-winds)
                                                 (length winds))))
                      (apply cont results)))))))]

      [dynamic-unwind (lambda (winds n)
         (cond [(eq? %%sys%dynamic-winds winds)]
               [(< n 0)
                 (dynamic-unwind (cdr winds) (+ n 1))
                 ((car (car winds)))
                 (set! %%sys%dynamic-winds winds)]
               [else
                 (let ([after (cdr (car %%sys%dynamic-winds))])
                   (set! %%sys%dynamic-winds (cdr %%sys%dynamic-winds))
                   (after)
                   (dynamic-unwind winds (- n 1)))]))])

      (set! %%sys%dynamic-winds '())
      (set! call-with-current-continuation call-cc)
      (set! call/cc call-cc)))

  (before)
  (set! %%sys%dynamic-winds (cons (cons before after) %%sys%dynamic-winds))
  (call-with-values
    thunk
    (lambda results
      (set! %%sys%dynamic-winds (cdr %%sys%dynamic-winds))
      (after)
      (apply values results))))

(define call/cc call-with-current-continuation)

; SRFI 8: Binding to multiple values
(define-macro (receive formals expression . body)
  `(call-with-values (lambda () ,expression)
                     (lambda ,formals ,@body)))

; Poor mans multi-values
(define values list)
(define (call-with-values producer consumer)
  (apply consumer (producer)))
