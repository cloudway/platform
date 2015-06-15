(require srfi-35)

(define-condition-type &amb-exhausted &message
  amb-exhausted-condition?)

(define (amb-exhausted)
  (raise (make-condition &amb-exhausted 'message "expression tree exhausted")))

(define-parameter amb-failure-continuation amb-exhausted)

;;

(define-macro (amb . args)
  (if (null? args)
      `((amb-failure-continuation))
      `(amb-thunks (list ,@(map (lambda (expr) `(lambda () ,expr))
                                args)))))

(define-macro (amb-find . form)
  (match form
    [(expr)      `(amb-find-thunk (lambda () ,expr))]
    [(expr fail) `(amb-find-thunk (lambda () ,expr) (lambda () ,fail))]))

(define-macro (amb-collect expr)
  `(amb-collect-thunk (lambda () ,expr)))

(define-macro (amb-assert expr)
  `(unless ,expr ((amb-failure-continuation))))

;;

(define (amb-thunks thunks)
  (let ((afc (amb-failure-continuation)))
    (let/cc return
      (let loop ((tt thunks))
        (cond
          ((null? tt)
            (amb-failure-continuation afc)
            (afc))
          (else
            (amb-failure-continuation (lambda () (loop (cdr tt))))
            (return ((car tt)))))))))

(define* (amb-find-thunk thunk (failure amb-exhausted))
  (let/cc return
    (parameterize ((amb-failure-continuation (lambda () (return (failure)))))
      (thunk))))

(define (amb-collect-thunk thunk)
  (let ((afc #f))
    (dynamic-wind
      (lambda () (set! afc (amb-failure-continuation)))
      (lambda ()
        (let/cc return
          (let* ((root (list #f))
                 (head root))
            (amb-failure-continuation (lambda () (return (cdr root))))
            (set-cdr! head (list (thunk)))
            (set! head (cdr head))
            ((amb-failure-continuation)))))
      (lambda () (amb-failure-continuation afc)))))

;;

(define-macro (choose ls)
  `(amb-thunks (map (lambda (x) (lambda () x)) ,ls)))

(define-macro (one-of expr)
  `(amb-find ,expr))

(define-macro (all-of expr)
  `(amb-collect ,expr))

(define-macro (required expr)
  `(amb-assert ,expr))

(define (distinct? xs)
  (let loop ((xs xs))
    (or (null? xs)
        (and (not (member (car xs) (cdr xs)))
             (loop (cdr xs))))))
