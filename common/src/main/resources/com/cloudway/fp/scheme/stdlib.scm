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

(define (list . objs)   objs)
(define (length lst)    (fold (lambda (x y) (+ x 1)) 0 lst))
(define (reverse lst)   (fold (flip cons) '() lst))

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

(define (map func lst)      (foldr (lambda (x y) (cons (func x) y)) '() lst))
(define (flatmap func lst)  (foldr (lambda (x y) (append (func x) y)) '() lst))
(define (filter pred lst)   (foldr (lambda (x y) (if (pred x) (cons x y) y)) '() lst))
(define (for-each proc lst) (foldl (lambda (a x) (proc x)) '() lst))

;;; Macros

(defmacro (assert some-cond)
  `(when (not ,some-cond)
     (error "assertion failure" ',some-cond)))

(defmacro (when pred . actions)
  `(if ,pred (begin ,@actions)))

(defmacro (unless pred . actions)
  `(if (not ,pred) (begin ,@actions)))

(defmacro (while some-cond . some-actions)
  (let ((mc (gensym)))
    `(do ((,mc 0 (+ ,mc 1)))
         ((not ,some-cond) ,mc)
       ,@some-actions)))

(defmacro (until some-cond . some-actions)
  (let ((mc (gensym)))
      `(do ((,mc 0 (+ ,mc 1)))
           (,some-cond ,mc)
         ,@some-actions)))
