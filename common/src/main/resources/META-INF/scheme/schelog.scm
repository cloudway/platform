;Schelog
;An embedding of Prolog in Scheme
;Dorai Sitaram
;last modified 2015-06-02

;logic variables and their manipulation

(define schelog:*ref* "ref")

(define schelog:*unbound* '_)

(define (schelog:make-ref . opt)
  ;;makes a fresh unbound ref;
  ;;unbound refs point to themselves
  (cons schelog:*ref*
    (if (null? opt)
        schelog:*unbound*
        (car opt))))

(define _ schelog:make-ref)

(define (schelog:ref? r)
  (and (pair? r)
       (eq? (car r) schelog:*ref*)))

(define (schelog:deref r)
  (cdr r))

(define (schelog:set-ref! r v)
  (set-cdr! r v))

(define (schelog:unbound-ref? r)
    (eq? (schelog:deref r) schelog:*unbound*))

(define (schelog:unbind-ref! r)
  (schelog:set-ref! r schelog:*unbound*))

;frozen logic vars

(define schelog:*frozen* "frozen")

(define (schelog:freeze-ref r)
  (schelog:make-ref (cons schelog:*frozen* r)))

(define (schelog:thaw-frozen-ref r)
  (cdr (schelog:deref r)))

(define (schelog:frozen-ref? r)
  (let ((r2 (schelog:deref r)))
    (and (pair? r2)
         (eq? (car r2) schelog:*frozen*))))

;deref a structure completely (except the frozen ones, i.e.)

(define (schelog:deref* s)
  (cond ((schelog:ref? s)
         (if (schelog:frozen-ref? s) s
           (schelog:deref* (schelog:deref s))))
        ((pair? s) (cons (schelog:deref* (car s))
                         (schelog:deref* (cdr s))))
        ((vector? s)
         (list->vector (map schelog:deref* (vector->list s))))
        (else s)))

;%let introduces new logic variables

(define-macro (%let x* . e*)
  `(let ,(map (lambda (x) `(,x (schelog:make-ref))) x*)
    ,@e*))

;the unify predicate

(define *schelog-use-occurs-check?* #f)

(define (schelog:occurs-in? var term)
  (and *schelog-use-occurs-check?*
       (let loop ((term term))
         (cond ((eqv? var term) #t)
               ((schelog:ref? term)
                (cond ((schelog:unbound-ref? term) #f)
                      ((schelog:frozen-ref? term) #f)
                      (else (loop (schelog:deref term)))))
               ((pair? term)
                (or (loop (car term)) (loop (cdr term))))
               ((vector? term)
                (loop (vector->list term)))
               (else #f)))))

(define (schelog:unify t1 t2)
  (lambda (fk)
    (letrec
      ((cleanup-n-fail
         (lambda (s)
           (for-each schelog:unbind-ref! s)
           (fk 'fail)))
       (unify1
         (lambda (t1 t2 s)
           ;(printf "unify1 ~s ~s~%" t1 t2)
           (cond ((eqv? t1 t2) s)
                 ((schelog:ref? t1)
                  (cond ((schelog:unbound-ref? t1)
                         (cond ((schelog:occurs-in? t1 t2)
                                (cleanup-n-fail s))
                               (else
                                 (schelog:set-ref! t1 t2)
                                 (cons t1 s))))
                        ((schelog:frozen-ref? t1)
                         (cond ((schelog:ref? t2)
                                (cond ((schelog:unbound-ref? t2)
                                       ;(printf "t2 is unbound~%")
                                       (unify1 t2 t1 s))
                                      ((schelog:frozen-ref? t2)
                                       (cleanup-n-fail s))
                                      (else
                                        (unify1 t1 (schelog:deref t2) s))))
                               (else (cleanup-n-fail s))))
                        (else
                          ;(printf "derefing t1~%")
                          (unify1 (schelog:deref t1) t2 s))))
                 ((schelog:ref? t2) (unify1 t2 t1 s))
                 ((and (pair? t1) (pair? t2))
                  (unify1 (cdr t1) (cdr t2)
                          (unify1 (car t1) (car t2) s)))
                 ((and (string? t1) (string? t2))
                  (if (string=? t1 t2) s
                      (cleanup-n-fail s)))
                 ((and (vector? t1) (vector? t2))
                  (unify1 (vector->list t1)
                          (vector->list t2) s))
                 (else
                   (for-each schelog:unbind-ref! s)
                   (fk 'fail))))))
      (let ((s (unify1 t1 t2 '())))
        (lambda (d)
          (cleanup-n-fail s))))))

(define %= schelog:unify)

;disjunction

(define-macro (%or . gg)
  (with-gensyms (__fk __sk)
    `(lambda (,__fk)
      (call-with-current-continuation
        (lambda (,__sk)
          ,@(map (lambda (g)
                   `(call-with-current-continuation
                      (lambda (,__fk)
                        (,__sk ((schelog:deref* ,g) ,__fk)))))
                 gg)
          (,__fk 'fail))))))

;conjunction

(define-macro (%and . gg)
  (with-gensyms (__fk)
    `(lambda (,__fk)
      (let* ,(map (lambda (g) `(,__fk ((schelog:deref* ,g) ,__fk))) gg)
        ,__fk))))

;cut

(define-macro (%cut-delimiter g)
  (with-gensyms (__fk __fk2)
    `(lambda (,__fk)
      (let ((! (lambda (,__fk2) ,__fk)))
        ((schelog:deref* ,g) ,__fk)))))

;Prolog-like sugar

(define-macro (%rel vv . cc)
  (with-gensyms (__fmls __fk __sk)
    `(lambda ,__fmls
       (lambda (,__fk)
         (call-with-current-continuation
           (lambda (,__sk)
             (let ((! (lambda (fk1) ,__fk)))
               (%let ,vv
                     ,@(map (lambda (c)
                              `(call-with-current-continuation
                                 (lambda (,__fk)
                                   (let* ((,__fk ((%= ,__fmls (list ,@(car c)))
                                                 ,__fk))
                                          ,@(map (lambda (sg)
                                                   `(,__fk ((schelog:deref* ,sg)
                                                           ,__fk)))
                                                 (cdr c)))
                                     (,__sk ,__fk)))))
                            cc)
                     (,__fk 'fail)))))))))

;the fail and true preds

(define (%fail fk) (fk 'fail))

(define (%true fk) fk)

;for structures ("functors"), use Scheme's list and vector
;functions and anything that's built using them.

;arithmetic

(define-macro (%is v e)
  (with-gensyms (__fk)
    (letrec ((%is-help (lambda (e)
                         (cond ((pair? e)
                                (cond ((eq? (car e) 'quote) e)
                                      (else
                                        (map (lambda (e1)
                                               (%is-help e1)) e))))
                               (else
                                 `(if (and (schelog:ref? ,e)
                                           (schelog:unbound-ref? ,e))
                                    (,__fk 'fail) (schelog:deref* ,e)))))))
      `(lambda (,__fk)
         ((%= ,v ,(%is-help e)) ,__fk)))))

;defining arithmetic comparison operators

(define (schelog:make-binary-arithmetic-relation f)
  (lambda (x y)
    (%is #t (f x y))))

(define %=:= (schelog:make-binary-arithmetic-relation =))
(define %> (schelog:make-binary-arithmetic-relation >))
(define %>= (schelog:make-binary-arithmetic-relation >=))
(define %< (schelog:make-binary-arithmetic-relation <))
(define %<= (schelog:make-binary-arithmetic-relation <=))
(define %=/= (schelog:make-binary-arithmetic-relation
               (lambda (m n) (not (= m n)))))

;type predicates

(define (schelog:constant? x)
  (cond ((schelog:ref? x)
         (cond ((schelog:unbound-ref? x) #f)
               ((schelog:frozen-ref? x) #t)
               (else (schelog:constant? (schelog:deref x)))))
        ((pair? x) #f)
        ((vector? x) #f)
        (else #t)))

(define (schelog:compound? x)
  (cond ((schelog:ref? x)
         (cond ((schelog:unbound-ref? x) #f)
                ((schelog:frozen-ref? x) #f)
                (else (schelog:compound? (schelog:deref x)))))
        ((pair? x) #t)
        ((vector? x) #t)
        (else #f)))

(define (%constant x)
  (lambda (fk)
    (if (schelog:constant? x) fk (fk 'fail))))

(define (%compound x)
  (lambda (fk)
    (if (schelog:compound? x) fk (fk 'fail))))

;metalogical type predicates

(define (schelog:var? x)
  (cond ((schelog:ref? x)
         (cond ((schelog:unbound-ref? x) #t)
               ((schelog:frozen-ref? x) #f)
               (else (schelog:var? (schelog:deref x)))))
        ((pair? x) (or (schelog:var? (car x)) (schelog:var? (cdr x))))
        ((vector? x) (schelog:var? (vector->list x)))
        (else #f)))

(define (%var x)
  (lambda (fk) (if (schelog:var? x) fk (fk 'fail))))

(define (%nonvar x)
  (lambda (fk) (if (schelog:var? x) (fk 'fail) fk)))

; negation of unify

(define (schelog:make-negation p) ;basically inlined cut-fail
  (lambda args
    (lambda (fk)
      (if (call-with-current-continuation
            (lambda (k)
              ((apply p args) (lambda (d) (k #f)))))
          (fk 'fail)
          fk))))

(define %/= (schelog:make-negation %=))

;identical

(define (schelog:ident? x y)
  (cond ((schelog:ref? x)
         (cond ((schelog:unbound-ref? x)
                (cond ((schelog:ref? y)
                       (cond ((schelog:unbound-ref? y) (eq? x y))
                             ((schelog:frozen-ref? y) #f)
                             (else (schelog:ident? x (schelog:deref y)))))
                      (else #f)))
               ((schelog:frozen-ref? x)
                (cond ((schelog:ref? y)
                       (cond ((schelog:unbound-ref? y) #f)
                             ((schelog:frozen-ref? y) (eq? x y))
                             (else (schelog:ident? x (schelog:deref y)))))
                      (else #f)))
               (else (schelog:ident? (schelog:deref x) y))))
        ((pair? x)
         (cond ((schelog:ref? y)
                (cond ((schelog:unbound-ref? y) #f)
                      ((schelog:frozen-ref? y) #f)
                      (else (schelog:ident? x (schelog:deref y)))))
               ((pair? y)
                (and (schelog:ident? (car x) (car y))
                     (schelog:ident? (cdr x) (cdr y))))
               (else #f)))
        ((vector? x)
         (cond ((schelog:ref? y)
                (cond ((schelog:unbound-ref? y) #f)
                      ((schelog:frozen-ref? y) #f)
                      (else (schelog:ident? x (schelog:deref y)))))
               ((vector? y)
                (schelog:ident? (vector->list x)
                  (vector->list y)))
               (else #f)))
        (else
          (cond ((schelog:ref? y)
                 (cond ((schelog:unbound-ref? y) #f)
                       ((schelog:frozen-ref? y) #f)
                       (else (schelog:ident? x (schelog:deref y)))))
                ((pair? y) #f)
                ((vector? y) #f)
                (else (eqv? x y))))))

(define (%== x y)
  (lambda (fk) (if (schelog:ident? x y) fk (fk 'fail))))

(define (%/== x y)
  (lambda (fk) (if (schelog:ident? x y) (fk 'fail) fk)))

;variables as objects

(define (schelog:freeze s)
  (let ((dict '()))
    (let loop ((s s))
      (cond ((schelog:ref? s)
             (cond ((or (schelog:unbound-ref? s) (schelog:frozen-ref? s))
                    (let ((x (assq s dict)))
                      (if x (cdr x)
                          (let ((y (schelog:freeze-ref s)))
                            (set! dict (cons (cons s y) dict))
                            y))))
                   ;((schelog:frozen-ref? s) s) ;?
                   (else (loop (schelog:deref s)))))
            ((pair? s) (cons (loop (car s)) (loop (cdr s))))
            ((vector? s)
             (list->vector (map loop (vector->list s))))
            (else s)))))

(define (schelog:melt f)
  (cond ((schelog:ref? f)
         (cond ((schelog:unbound-ref? f) f)
               ((schelog:frozen-ref? f) (schelog:thaw-frozen-ref f))
               (else (schelog:melt (schelog:deref f)))))
        ((pair? f)
         (cons (schelog:melt (car f)) (schelog:melt (cdr f))))
        ((vector? f)
         (list->vector (map schelog:melt (vector->list f))))
        (else f)))

(define (schelog:melt-new f)
  (let ((dict '()))
    (let loop ((f f))
      (cond ((schelog:ref? f)
             (cond ((schelog:unbound-ref? f) f)
                   ((schelog:frozen-ref? f)
                    (let ((x (assq f dict)))
                      (if x (cdr x)
                          (let ((y (schelog:make-ref)))
                            (set! dict (cons (cons f y) dict))
                            y))))
                   (else (loop (schelog:deref f)))))
            ((pair? f) (cons (loop (car f)) (loop (cdr f))))
            ((vector? f)
             (list->vector (map loop (vector->list f))))
            (else f)))))

(define (schelog:copy s)
  (schelog:melt-new (schelog:freeze s)))

(define (%freeze s f)
  (lambda (fk)
    ((%= (schelog:freeze s) f) fk)))

(define (%melt f s)
  (lambda (fk)
    ((%= (schelog:melt f) s) fk)))

(define (%melt-new f s)
  (lambda (fk)
    ((%= (schelog:melt-new f) s) fk)))

(define (%copy s c)
  (lambda (fk)
    ((%= (schelog:copy s) c) fk)))

;negation as failure

(define (%not g)
  (lambda (fk)
    (if (call-with-current-continuation
          (lambda (k)
            ((schelog:deref* g) (lambda (d) (k #f)))))
        (fk 'fail) fk)))

;assert, asserta

(define (%empty-rel . args) %fail)

(define-macro (%assert rel-name vv . cc)
  (with-gensyms (__old-rel __new-addition __fmls)
    `(set! ,rel-name
           (let ((,__old-rel ,rel-name)
                 (,__new-addition (%rel ,vv ,@cc)))
             (lambda ,__fmls
               (%or (apply ,__old-rel ,__fmls)
                    (apply ,__new-addition ,__fmls)))))))

(define-macro (%assert-a rel-name vv . cc)
  (with-gensyms (__old-rel __new-addition __fmls)
    `(set! ,rel-name
       (let ((,__old-rel ,rel-name)
             (,__new-addition (%rel ,vv ,@cc)))
         (lambda ,__fmls
           (%or (apply ,__new-addition ,__fmls)
                (apply ,__old-rel ,__fmls)))))))

;set predicates

(define (schelog:set-cons e s)
  (if (member e s) s (cons e s)))

(define-macro (%free-vars vv g)
  `(cons 'schelog:goal-with-free-vars
         (cons (list ,@vv) ,g)))

(define (schelog:goal-with-free-vars? x)
  (and (pair? x) (eq? (car x) 'schelog:goal-with-free-vars)))

(define (schelog:make-bag-of kons)
  (lambda (lv goal bag)
    (let ((fvv '()))
      (if (schelog:goal-with-free-vars? goal)
        (begin (set! fvv (cadr goal))
          (set! goal (cddr goal)))
        #f)
      (schelog:make-bag-of-aux kons fvv lv goal bag))))

(define (schelog:make-bag-of-aux kons fvv lv goal bag)
  (lambda (fk)
    (call-with-current-continuation
      (lambda (sk)
        (let ((lv2 (cons fvv lv)))
          (let* ((acc '())
                  (fk-final
                    (lambda (d)
                      ;;(set! acc (reverse! acc))
                      (sk ((schelog:separate-bags fvv bag acc) fk))))
                  (fk-retry (goal fk-final)))
            (set! acc (kons (schelog:deref* lv2) acc))
            (fk-retry 'retry)))))))

(define (schelog:separate-bags fvv bag acc)
  ;;(format #t "Accum: ~s~%" acc)
  (let ((bags (let loop ((acc acc)
                          (current-fvv #f) (current-bag '())
                          (bags '()))
                (if (null? acc)
                  (cons (cons current-fvv current-bag) bags)
                  (let ((x (car acc)))
                    (let ((x-fvv (car x)) (x-lv (cdr x)))
                      (if (or (not current-fvv) (equal? x-fvv current-fvv))
                        (loop (cdr acc) x-fvv (cons x-lv current-bag) bags)
                        (loop (cdr acc) x-fvv (list x-lv)
                          (cons (cons current-fvv current-bag) bags)))))))))
    ;;(format #t "Bags: ~a~%" bags)
    (if (null? bags) (%= bag '())
      (let ((fvv-bag (cons fvv bag)))
        (let loop ((bags bags))
          (if (null? bags) %fail
            (%or (%= fvv-bag (car bags))
              (loop (cdr bags)))))))))

(define %bag-of (schelog:make-bag-of cons))
(define %set-of (schelog:make-bag-of schelog:set-cons))

;%bag-of-1, %set-of-1 hold if there's at least one solution

(define (%bag-of-1 x g b)
  (%and (%bag-of x g b)
    (%= b (cons (_) (_)))))

(define (%set-of-1 x g s)
  (%and (%set-of x g s)
    (%= s (cons (_) (_)))))

;user interface

;(%which (v ...) query) returns #f if query fails and instantiations
;of v ... if query succeeds.  In the latter case, type (%more) to
;retry query for more instantiations.

(define schelog:*more-k* 'forward)
(define schelog:*more-fk* 'forward)

(define-macro (%which vv g)
  (with-gensyms (__qk)
    `(%let ,vv
       (call-with-current-continuation
         (lambda (,__qk)
           (set! schelog:*more-k* ,__qk)
           (set! schelog:*more-fk*
             ((schelog:deref* ,g)
              (lambda (d)
                (set! schelog:*more-fk* #f)
                (schelog:*more-k* #f))))
           (schelog:*more-k*
             (map (lambda (nam val) (list nam (schelog:deref* val)))
                  ',vv
                  (list ,@vv))))))))

(define (%more)
  (call-with-current-continuation
    (lambda (k)
      (set! schelog:*more-k* k)
      (if schelog:*more-fk* (schelog:*more-fk* 'more)
        #f))))

;end of embedding code.  The following are
;some utilities, written in Schelog

(define (%member x y)
  (%let (xs z zs)
    (%or
      (%= y (cons x xs))
      (%and (%= y (cons z zs))
        (%member x zs)))))

(define (%if-then-else p q r)
  (%cut-delimiter
    (%or
      (%and p ! q)
      r)))

;the above could also have been written in a more
;Prolog-like fashion, viz.

#;(define %member
  (%rel (x xs y ys)
    ((x (cons x xs)))
    ((x (cons y ys)) (%member x ys))))

#;(define %if-then-else
  (%rel (p q r)
    ((p q r) p ! q)
    ((p q r) r)))

(define %append
  (%rel (x xs ys zs)
    (('() ys ys))
    (((cons x xs) ys (cons x zs))
      (%append xs ys zs))))

(define %repeat
  ;;failure-driven loop
  (%rel ()
    (())
    (() (%repeat))))

;end of file

