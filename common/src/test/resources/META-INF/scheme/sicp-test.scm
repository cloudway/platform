;;; SECTION 3.3.5

(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
                (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation -- CONNECTOR" request))))
    me))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (has-value? connector)
  (connector 'has-value?))

(define (get-value connector)
  (connector 'value))

(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))

(define (forget-value! connector retractor)
  ((connector 'forget) retractor))

(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

(define (inform-about-value constraint)
    (constraint 'I-have-a-value))

(define (inform-about-no-value constraint)
    (constraint 'I-lost-my-value))

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)  
           (process-new-value))
          ((eq? request 'I-lost-my-value) 
           (process-forget-value))
          (else 
           (error "Unknown request -- ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value)
    (newline))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- PROBE" request))))
  (connect connector me)
  me)

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

(define (constraint-test)
  (define C (make-connector))
  (define F (make-connector))
  (celsius-fahrenheit-converter C F)

  (probe "Celsius temp" C)
  (probe "Fahrenheit temp" F)

  (display ";;; (set-value! C 25 'user)")
  (newline)
  (set-value! C 25 'user)
  (assert (= 25 (get-value C)))
  (assert (= 77 (get-value F)))

  (display ";;; (forget-value! C 'user)")
  (newline)
  (forget-value! C 'user)

  (display ";;; (set-value! F 212 'user)")
  (newline)
  (set-value! F 212 'user)
  (assert (= 212 (get-value F)))
  (assert (= 100 (get-value C)))

  'done)

;;; SECTION 3.5

(define-macro (cons-stream a b)
  `(cons ,a (delay ,b)))

(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))

(define (map-stream f s)
  (cons-stream (f (stream-car s))
               (map-stream f (stream-cdr s))))

(define (filter-stream p s)
  (if (p (stream-car s))
      (cons-stream (stream-car s)
                   (filter-stream p (stream-cdr s)))
      (filter-stream p (stream-cdr s))))

(define (zip-stream f s1 s2)
  (cons-stream (f (stream-car s1) (stream-car s2))
               (zip-stream f (stream-cdr s1) (stream-cdr s2))))

(define (take n s)
  (if (zero? n)
      '()
      (cons (stream-car s) (take (- n 1) (stream-cdr s)))))

(define (print-stream n s)
  (if (zero? n)
      (newline)
      (begin
        (display (stream-car s))
        (display " ")
        (print-stream (- n 1) (stream-cdr s)))))

(define (from n) (cons-stream n (from (+ n 1))))

(define fibs
  (let fibgen ([a 1] [b 1])
    (cons-stream a (fibgen b (+ a b)))))

(define primes (cons 2 (delay (filter-stream prime? (from 3)))))

(define (prime? n)
  (let sieve ((ps primes))
    (let ((x (stream-car ps)))
      (cond ((> (* x x) n) #t)
            ((= 0 (remainder n x)) #f)
            (else (sieve (stream-cdr ps)))))))

(define (stream-test)
  (assert (equal? '(1 1 2 3 5 8 13 21 34 55) (take 10 fibs)))
  (assert (equal? '(2 3 5 7 11 13 17 19 23 29) (take 10 primes))))

;;; T E S T
(constraint-test)
(stream-test)
