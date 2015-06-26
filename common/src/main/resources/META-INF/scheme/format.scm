;;;; Common LISP like text output formatter for R4RS Scheme
;
; Copyright (C) 1992, 1993 by Dirk Lutzebaeck (lutzeb@cs.tu-berlin.de)
;
; Authors of the original version (< 1.4) were Ken Dickey and Aubrey Jaffer.
; Please send error reports to the email address above.
;
; VERSION 2.2
;
; EXPORTS:
;
; (format destination format-string . arguments)               procedure
;
; Returns #t, #f or a string; has side effect of  printing  according to
; <format-string>.  If <destination> is #t, the output is to the current
; output port and #t is returned.  If  <destination>  is #f, a formatted
; string is returned as the result  of the  call.  If <destination> is a
; string, the output is appended to that  string by string-append (note:
; this returns a newly allocated string).  Otherwise  <destination> must
; be an  output port  and  #t is  returned.  <format-string>  must  be a
; string. In case of a  formatting error format  returns #f and prints a
; message  on the current output port.   Characters are output as if the
; string were output by the DISPLAY function with the exception of those
; prefixed by  a   tilde (~).   For  a detailed   description   of   the
; <format-string> syntax please consult a  Common  LISP format reference
; manual.  For  a quick  overview   of implemented,  not  supported  and
; extended control properties of <format-string> see "format.doc". For a
; test suite to verify this format implementation load "formatst.scm".
;
; (obj->string arbitrary-scheme-object . options)              procedure
; (string-upcase string)                                       procedure
; (string-downcase string)                                     procedure
; (string-capitalize string)                                   procedure
; (string-capitalize-first string)                             procedure
;
; For a documentation of these procedures see below.
;
; CHANGELOG:
;
;   Version 2.2:                                                     [dl]
;   * corrected truncation for fixed fields by negative field parameters
;     inserted a '<' or a '>' when field length was equal to object string
;     length
;   * changed #[...] outputs to #<...> outputs to be conform to SCM's
;     display and write functions
;   * changed #[non-printable-object] output to #<unspecified>
;   * ~:s and ~:a print #<...> messages in strings "#<...>" so that the
;     output can always be processed by (read)
;   * changed implementation dependent part: to configure for various scheme
;     systems define the variable format:scheme-system (see below)
;   * format:version is a variable returning the format version in a string
;   * format:custom-types allows to use scheme system dependent predicates
;     to identify the type of a scheme object and its proper textual
;     representation
;   * Tested with scm4a14, Elk 2.0
;
;   Version 2.1:                                                     [dl]
;   * Tested with scm3c11, Elk 1.5, MIT C-Scheme 7.1, UMB Scheme 2.5,
;     and Scheme->C 01nov91 (see "formatst.scm" for error reports)
;   * ~e,~f,~g,~$ fake floating point formatting by number->string;
;     no directive parameters are implemented
;   * replaced ~g by ~y due to ~g CL floating point formatting directive
;   * ~{~} with modifiers implemented (nested iterations allowed)
;   * errors in format-string are showed by a "<--" in the format string
;   * `.' as a directive parameter separator is not supported anymore
;   * ~[ ~; ~] with modifiers implemented (nested conditionals allowed)
;   * ~? expects a list now (as CL does)
;     ~@? works now as ~? in 2.0 did.
;   * ~*, ~n*, ~:*, ~n:*, ~@*, ~n@* implemented
;   * ~:p implemented
;   * don't strip the argument error messages anymore
;   * format returns now #t instead of () if destination is an output port
;
;   Version 2.0:                                                     [dl]
;   * Tested with scm3c11, Elk 1.5, MIT C-Scheme 7.1, UMB Scheme 2.5 and
;     Scheme->C 01nov91. (see "formatst.scm" for error reports)
;   * completely rewritten Version of SLIB format Version 1.4
;   * removed C-style padding support
;
  
(require pprint)

(define format:version "2.2")
  
(define (format . args)
  
  ;;; SCHEME IMPLEMENTATION DEPENDENCIES ---------------------------------------
  
  ;; To configure the format module for your scheme system, set the variable
  ;; format:scheme-system to one of the symbols of (slib elk any). You may add
  ;; appropiate definitions in the cond-construct below for other systems.
  
  (define format:scheme-system 'any)
  
  ;; Default configuration definitions which may be overridden (see below).
  
  ;; The pretty printer (format:pp arg . port).
  (define format:pp pretty-print)
  
  ;; The form feed character.
  (define format:form-feed (integer->char 12))
  
  ;; The tabulator character.
  (define format:tab (integer->char 9))
  
  ;; Flushes an output port (format:force-output output-port).
  (define (format:force-output port) #t)
  
  ;; Aborts the program when a formatting error occures. This is a null
  ;; argument closure to jump to the interpreters toplevel continuation,
  ;; format:abort may return and in this case the format returns properly
  ;; and returns #f.
  (define (format:abort) #t)
  
  ;; Some scheme systems offer more predicates to reveal scheme types than R4RS.
  ;; To use these predicates for format, you have to define a list of pairs of
  ;; a predicate and the textual representation. This representation must be a
  ;; string or it must be a procedure with one argument (which passes the object
  ;; to represent) returning the textual representation as a string. (see below)
  (define format:custom-types '())
  
  (cond
   ((eq? format:scheme-system 'slib)      ; Aubrey Jaffer's SLIB
    (set! format:pp pretty-print)
    (set! format:form-feed slib:form-feed)
    (set! format:tab slib:tab)
    (set! format:force-output force-output)
    (define (format:abort) (slib:error "format: error in argument parsing")))
   ((eq? format:scheme-system 'elk)       ; Oliver Laumann's ELK
    (set! format:force-output flush-output-port)
    (set! format:abort reset)
    (define format:str-port (open-output-string))
    (define (format:real-obj obj)         ; returns the output of (display obj)
      (display obj format:str-port)       ; as a string to get the internal
      (get-output-string format:str-port)); representation of #<...>-objects
    (set! format:custom-types
      `((,control-point? . ,format:real-obj)
        (,environment? . ,format:real-obj)
        (,output-port? . ,format:real-obj)
        (,input-port? . ,format:real-obj)
        (,procedure? . ,format:real-obj)
        ))))
  
  ;; format:char->string converts a character into a slashified string as
  ;; done by `write'. The following procedure is dependent on the integer
  ;; representation of characters and assumes a character number according to
  ;; the ASCII character set.
  
  (define (format:char->string ch)
    (let ((int-rep (char->integer ch)))
      (string-append "#\\"
        (cond                     ; THIS IS IMPLEMENTATION DEPENDENT
         ((char=? ch #\newline) "newline")
         ((and (>= int-rep 0) (<= int-rep 32))
          (vector-ref format:ascii-non-printable-charnames int-rep))
         ((= int-rep 127) "del")
         ((>= int-rep 128) (number->string int-rep 8)) ; octal repr.
         (else (string ch))))))
  
  (define format:ascii-non-printable-charnames
    '#("nul" "soh" "stx" "etx" "eot" "enq" "ack" "bel"
       "bs"  "ht"  "nl"  "vt"  "np"  "cr"  "so"  "si"
       "dle" "dc1" "dc2" "dc3" "dc4" "nak" "syn" "etb"
       "can" "em"  "sub" "esc" "fs"  "gs"  "rs"  "us" "space"))
  
  ;;;--------------------------------------------------------------------------
  
  (define format:destination #f)
  (define format:output-buffer "")
  (define format:flush-output #f)
  (define format:case-conversion #f)
  (define format:error-continuation #f)
  (define format:args #f)
  
  (define (format:out-str str)            ; append to format:output-buffer
    (set! format:output-buffer
          (string-append format:output-buffer
                         (if (procedure? format:case-conversion)
                             (format:case-conversion str)
                             str))))
  
  (define (format:out-char ch)
    (format:out-str (string ch)))
  
  (define (format:error . args)           ; applies format:error-continuation
    (let ((error-continuation format:error-continuation)
          (format-args format:args))
      (format:format #t "FORMAT: ")
      (apply format:format `(#t ,@args))
      (format:format #t ", ARGS: ~a~%" format-args)
      (format:abort)                      ; we might jump to the top level cont.
      (error-continuation #f)))           ; or to the local error continuation
  
  (define (format:format . args)          ; the format wrapper
    (call-with-current-continuation
     (lambda (cont)
       (set! format:error-continuation cont)
       (set! format:args args)
       (if (< (length args) 2)
           (format:error "not enough arguments"))
       (let ((destination (car args))
             (format-string (cadr args))
             (arglist (cddr args)))
         (set! format:destination
               (cond
                ((boolean? destination)
                 (if destination (current-output-port) #f))
                ((output-port? destination) destination)
                ((string? destination) destination)
                (else (format:error "illegal destination `~a'" output-port))))
         (if (not (string? format-string))
             (format:error "illegal format string `~a'" format-string))
         (set! format:output-buffer "")
         (set! format:flush-output #f)
         (set! format:case-conversion #f) ; modifier case conversion procedure
         (let ((arg-pos (format:format-work format-string arglist))
               (arg-len (length arglist)))
           (cond
            ((< arg-pos arg-len)
             (format:error "~a superfluous argument~:p" (- arg-len arg-pos)))
            ((> arg-pos arg-len)
             (format:error "~a missing argument~:p" (- arg-pos arg-len)))
            ((output-port? format:destination)
             (display format:output-buffer format:destination)
             (if format:flush-output (format:force-output format:destination))
             #t)
            ((string? format:destination)
             (string-append format:destination format:output-buffer))
            (else format:output-buffer)))))))
  
  (define format:parameter-characters
    '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\- #\+ #\v #\# #\'))
  
  (define (format:format-work format-string arglist) ; does the formatting work
    (letrec
        ((format-string-len (string-length format-string))
         (pos 0)                          ; input format-string position
         (arg-pos 0)                      ; argument position in arglist
         (arg-len (length arglist))       ; number of arguments
         (modifier #f)                    ; 'colon | 'at | 'colon-at | #f
         (params '())                     ; directive parameter list
         (param-value-found #f)           ; a directive parameter value found
         (conditional-nest 0)             ; conditional nesting level
         (clause-pos 0)                   ; last cond. clause beginning char pos
         (clause-default #f)              ; conditional default clause string
         (clauses '())                    ; conditional clause string list
         (conditional-type #f)            ; reflects the contional modifiers
         (conditional-arg #f)             ; argument to apply the conditional
         (iteration-nest 0)               ; iteration nesting level
         (iteration-pos 0)                ; iteration string beginning char pos
         (iteration-type #f)              ; reflects the iteration modifiers
         (max-iterations #f)              ; maximum number of iterations
  
         (next-char                       ; gets the next char from format-string
          (lambda ()
            (let ((ch (peek-next-char)))
              (set! pos (+ 1 pos))
              ch)))
  
         (peek-next-char
          (lambda ()
            (if (>= pos format-string-len)
                (format:error "illegal format string")
                (string-ref format-string pos))))
  
         (one-positive-integer?
          (lambda (params)
            (cond
             ((null? params) #f)
             ((and (integer? (car params))
                   (>= (car params) 0)
                   (= (length params) 1)) #t)
             (else (fail "one positive integer parameter expected")))))
  
         (next-arg
          (lambda ()
            (if (>= arg-pos arg-len)
                (format:error "missing argument(s)"))
            (add-arg-pos 1)
            (list-ref arglist (- arg-pos 1))))
  
         (prev-arg
          (lambda ()
            (add-arg-pos -1)
            (if (negative? arg-pos)
                (format:error "missing backward argument(s)"))
            (list-ref arglist arg-pos)))
  
         (rest-args
          (lambda ()
            (let loop ((l arglist) (k arg-pos)) ; list-tail definition
              (if (zero? k) l (loop (cdr l) (- k 1))))))
  
         (add-arg-pos
          (lambda (n) (set! arg-pos (+ n arg-pos))))
  
         (fail
          (lambda (fmt . args)
            (apply format:error
                   `(,(string-append fmt ", POS: \"~a\"")
                     ,@args
                     ,(string-append
                       (substring format-string 0 pos)
                       "<--"
                       (substring format-string pos format-string-len))))))
  
         (anychar-dispatch                ; dispatches the format-string
          (lambda ()
            (if (>= pos format-string-len)
                arg-pos                   ; used for ~? continuance
                (let ((char (next-char)))
                  (cond
                   ((char=? char #\~)
                    (set! modifier #f)
                    (set! params '())
                    (set! param-value-found #f)
                    (tilde-dispatch))
                   (else
                    (if (and (zero? conditional-nest)
                             (zero? iteration-nest))
                        (format:out-char char))
                    (anychar-dispatch)))))))
  
         (tilde-dispatch
          (lambda ()
            (cond
             ((>= pos format-string-len)
              (format:out-str "~")        ; tilde at end of string is just output
              arg-pos)                    ; used for ~? continuance
             ((and (or (zero? conditional-nest)
                       (memv (peek-next-char) ; find conditional directives
                             (append '(#\[ #\] #\; #\: #\@ #\^)
                                     format:parameter-characters)))
                   (or (zero? iteration-nest)
                       (memv (peek-next-char) ; find iteration directives
                             (append '(#\{ #\} #\: #\@ #\^)
                                     format:parameter-characters))))
              (case (char-upcase (next-char))
  
                ;; format directives
  
                ((#\A)                    ; Any -- for humans
                 (format:out-str
                  (format:obj->str-padded (memq modifier '(at colon-at))
                                          (next-arg)
                                          (if (eq? modifier 'colon)
                                              '(readproof)
                                              '())
                                          params))
                 (anychar-dispatch))
                ((#\S)                    ; Slashified -- for parsers
                 (format:out-str
                  (format:obj->str-padded (memq modifier '(at colon-at))
                                          (next-arg)
                                          (if (eq? modifier 'colon)
                                              '(readproof slashify)
                                              '(slashify))
                                          params))
                 (anychar-dispatch))
                ((#\D)                    ; Decimal
                 (format:out-str
                  (format:num->str-padded modifier (next-arg) params 10 "#d"))
                 (anychar-dispatch))
                ((#\X)                    ; Hexadecimal
                 (format:out-str
                  (format:num->str-padded modifier (next-arg) params 16 "#x"))
                 (anychar-dispatch))
                ((#\O)                    ; Octal
                 (format:out-str
                  (format:num->str-padded modifier (next-arg) params 8 "#o"))
                 (anychar-dispatch))
                ((#\B)                    ; Binary
                 (format:out-str
                  (format:num->str-padded modifier (next-arg) params 2 "#b"))
                 (anychar-dispatch))
                ((#\R)                    ; any Radix
                 (if (null? params)
                     (fail "~~r not implemented")
                     (format:out-str
                      (format:num->str-padded
                       modifier (next-arg) (cdr params) (car params) "#r")))
                 (anychar-dispatch))
                ((#\E #\F #\G #\$)        ; Floating point (not implemented)
                 (format:out-str (number->string (next-arg)))
                 (anychar-dispatch))
                ((#\C)                    ; Character
                 (let ((ch (if (one-positive-integer? params)
                               (integer->char (car params))
                               (next-arg))))
                   (if (not (char? ch)) (fail "~~c expects a character"))
                   (if (eq? modifier 'at)
                       (format:out-str (obj->string ch 'slashify))
                       (format:out-char ch)))
                 (anychar-dispatch))
                ((#\P)                    ; Plural
                 (if (memq modifier '(colon colon-at))
                     (prev-arg))
                 (let ((arg (next-arg)))
                   (if (not (number? arg))
                       (fail "~~p expects a number argument"))
                   (if (= arg 1)
                       (if (memq modifier '(at colon-at))
                           (format:out-str "y"))
                       (if (memq modifier '(at colon-at))
                           (format:out-str "ies")
                           (format:out-str "s"))))
                 (anychar-dispatch))
                ((#\~)            ; Tilde
                 (if (one-positive-integer? params)
                     (format:out-str (make-string (car params) #\~))
                     (format:out-str "~"))
                 (anychar-dispatch))
                ((#\% #\&)                ; Newline (Freshline is the same)
                 (if (one-positive-integer? params)
                     (format:out-str (make-string (car params) #\newline))
                     (format:out-char #\newline))
                 (anychar-dispatch))
                ((#\_)                    ; Space
                 (if (one-positive-integer? params)
                     (format:out-str (make-string (car params) #\space))
                     (format:out-str " "))
                 (anychar-dispatch))
                ((#\T)                    ; Tab
                 (if (one-positive-integer? params)
                     (format:out-str (make-string (car params) format:tab))
                     (format:out-char format:tab))
                 (anychar-dispatch))
                ((#\|)                    ; Page Seperator
                 (if (one-positive-integer? params)
                     (format:out-str (make-string (car params) format:form-feed))
                     (format:out-char format:form-feed))
                 (anychar-dispatch))
                ((#\Y)                    ; Pretty-print
                 (if (not format:destination)
                     (fail "~~y not supported with string output")
                     (format:pp (next-arg)))
                 (anychar-dispatch))
                ((#\? #\K)                ; Indirection (is "~K" in T)
                 (cond
                  ((memq modifier '(colon colon-at))
                   (fail "illegal modifier in ~~?"))
                  ((eq? modifier 'at)
                   (let* ((frmt (next-arg))
                          (args (rest-args)))
                     (add-arg-pos (format:format-work frmt args))))
                  (else
                   (let* ((frmt (next-arg))
                          (args (next-arg)))
                     (format:format-work frmt args))))
                 (anychar-dispatch))
                ((#\!)                    ; Flush output
                 (set! format:flush-output #t)
                 (anychar-dispatch))
                ((#\newline)              ; Continuation lines
                 (if (eq? modifier 'at)
                     (format:out-char #\newline))
                 (if (< pos format-string-len)
                     (do ((ch (peek-next-char) (peek-next-char)))
                         ((or (not (char-whitespace? ch))
                              (= pos (- format-string-len 1))))
                       (if (eq? modifier 'colon)
                           (format:out-char (next-char))
                           (next-char))))
                 (anychar-dispatch))
                ((#\*)                    ; Argument jumping
                 (case modifier
                   ((colon)               ; jump backwards
                    (if (one-positive-integer? params)
                        (do ((i 0 (+ i 1)))
                            ((= i (car params)))
                          (prev-arg))
                        (prev-arg)))
                   ((at)                  ; jump absolute
                    (set! arg-pos (if (one-positive-integer? params)
                                      (car params) 0)))
                   ((colon-at)
                    (fail "illegal modifier `:@' in ~~* directive"))
                   (else                  ; jump forward
                    (if (one-positive-integer? params)
                        (do ((i 0 (+ i 1)))
                            ((= i (car params)))
                          (next-arg))
                        (next-arg))))
                 (anychar-dispatch))
                ((#\()                    ; Case conversion begin
                 (set! format:case-conversion
                       (case modifier
                         ((at) string-capitalize-first)
                         ((colon) string-capitalize)
                         ((colon-at) string-upcase)
                         (else string-downcase)))
                 (anychar-dispatch))
                ((#\))                    ; Case conversion end
                 (if (not format:case-conversion)
                     (fail "missing ~~("))
                 (set! format:case-conversion #f)
                 (anychar-dispatch))
                ((#\[)                    ; Conditional begin
                 (set! conditional-nest (+ conditional-nest 1))
                 (cond
                  ((= conditional-nest 1)
                   (set! clause-pos pos)
                   (set! clause-default #f)
                   (set! clauses '())
                   (set! conditional-type
                         (case modifier
                           ((at) 'if-then)
                           ((colon) 'if-else-then)
                           ((colon-at) (fail "illegal modifier in ~~["))
                           (else 'num-case)))
                   (set! conditional-arg
                         (if (one-positive-integer? params)
                             (car params)
                             (next-arg)))))
                 (anychar-dispatch))
                ((#\;)                    ; Conditional separator
                 (if (zero? conditional-nest)
                     (fail "~~; not in ~~[~~] conditional"))
                 (if (not (null? params))
                     (fail "no parameter allowed in ~~;"))
                 (if (= conditional-nest 1)
                     (let ((clause-str
                            (cond
                             ((eq? modifier 'colon)
                              (set! clause-default #t)
                              (substring format-string clause-pos (- pos 3)))
                             ((memq modifier '(at colon-at))
                              (fail "illegal modifier in ~~;"))
                             (else
                              (substring format-string clause-pos (- pos 2))))))
                       (set! clauses (append clauses (list clause-str)))
                       (set! clause-pos pos)))
                 (anychar-dispatch))
                ((#\])                    ; Conditional end
                 (if (zero? conditional-nest) (fail "missing ~~["))
                 (set! conditional-nest (- conditional-nest 1))
                 (if modifier
                     (fail "no modifier allowed in ~~]"))
                 (if (not (null? params))
                     (fail "no parameter allowed in ~~]"))
                 (cond
                  ((zero? conditional-nest)
                   (let ((clause-str (substring format-string clause-pos
                                                (- pos 2))))
                     (if clause-default
                         (set! clause-default clause-str)
                         (set! clauses (append clauses (list clause-str)))))
                   (case conditional-type
                     ((if-then)
                      (if conditional-arg
                          (format:format-work (car clauses)
                                              (list conditional-arg))))
                     ((if-else-then)
                      (add-arg-pos
                       (format:format-work (if conditional-arg
                                               (cadr clauses)
                                               (car clauses))
                                           (rest-args))))
                     ((num-case)
                      (if (or (not (integer? conditional-arg))
                              (< conditional-arg 0))
                          (fail "argument not a positive integer"))
                      (if (not (and (>= conditional-arg (length clauses))
                                    (not clause-default)))
                          (add-arg-pos
                           (format:format-work
                            (if (>= conditional-arg (length clauses))
                                clause-default
                                (list-ref clauses conditional-arg))
                            (rest-args))))))))
                 (anychar-dispatch))
                ((#\{)                    ; Iteration begin
                 (set! iteration-nest (+ iteration-nest 1))
                 (cond
                  ((= iteration-nest 1)
                   (set! iteration-pos pos)
                   (set! iteration-type
                         (case modifier
                           ((at) 'rest-args)
                           ((colon) 'sublists)
                           ((colon-at) 'rest-sublists)
                           (else 'list)))
                   (set! max-iterations (if (one-positive-integer? params)
                                           (car params) #f))))
                 (anychar-dispatch))
                ((#\})                    ; Iteration end
                 (if (zero? iteration-nest) (fail "missing ~~{"))
                 (set! iteration-nest (- iteration-nest 1))
                 (case modifier
                   ((colon)
                    (if (not max-iterations) (set! max-iterations 1)))
                   ((colon-at at) (fail "illegal modifier"))
                   (else (if (not max-iterations) (set! max-iterations 100))))
                 (if (not (null? params)) (fail "no parameters allowed in ~~}"))
                 (if (zero? iteration-nest)
                   (let ((iteration-str
                          (substring format-string iteration-pos
                                     (- pos (if modifier 3 2)))))
                     (if (string=? iteration-str "")
                         (set! iteration-str (next-arg)))
                     (case iteration-type
                       ((list)
                        (let ((args (next-arg))
                              (args-len 0))
                          (if (not (list? args))
                              (fail "expected a list argument"))
                          (set! args-len (length args))
                          (do ((arg-pos 0 (+ arg-pos
                                             (format:format-work
                                              iteration-str
                                              (list-tail args arg-pos))))
                               (i 0 (+ i 1)))
                              ((or (>= arg-pos args-len)
                                   (>= i max-iterations))))))
                       ((sublists)
                        (let ((args (next-arg))
                              (args-len 0))
                          (if (not (list? args))
                              (fail "expected a list argument"))
                          (set! args-len (length args))
                          (do ((arg-pos 0 (+ arg-pos 1)))
                              ((or (>= arg-pos args-len)
                                   (>= arg-pos max-iterations)))
                            (let ((sublist (list-ref args arg-pos)))
                              (if (not (list? sublist))
                                  (fail "expected a list of lists argument"))
                              (format:format-work iteration-str sublist)))))
                       ((rest-args)
                        (let* ((args (rest-args))
                               (args-len (length args))
                               (usedup-args
                                (do ((arg-pos 0 (+ arg-pos
                                                   (format:format-work
                                                    iteration-str
                                                    (list-tail args arg-pos))))
                                     (i 0 (+ i 1)))
                                    ((or (>= arg-pos args-len)
                                         (>= i max-iterations))
                                     arg-pos))))
                          (add-arg-pos usedup-args)))
                       ((rest-sublists)
                        (let* ((args (rest-args))
                               (args-len (length args))
                               (usedup-args
                                (do ((arg-pos 0 (+ arg-pos 1)))
                                    ((or (>= arg-pos args-len)
                                         (>= arg-pos max-iterations))
                                     arg-pos)
                                  (let ((sublist (list-ref args arg-pos)))
                                    (if (not (list? sublist))
                                        (fail "expected list arguments"))
                                    (format:format-work iteration-str sublist)))))
                          (add-arg-pos usedup-args)))
                       (else (fail "internal error in ~~}")))))
                 (anychar-dispatch))
                ((#\^)                    ; Up and out
                 (let* ((continue
                         (cond
                          ((not (null? params))
                           (not
                            (case (length params)
                             ((1) (zero? (car params)))
                             ((2) (= (list-ref params 0) (list-ref params 1)))
                             ((3) (<= (list-ref params 0)
                                      (list-ref params 1)
                                      (list-ref params 2)))
                             (else (fail "too much parameters")))))
                          (format:case-conversion ; if conversion stop conversion
                           (set! format:case-conversion string-copy) #t)
                          ((= iteration-nest 1) #t)
                          ((= conditional-nest 1) #t)
                          ((>= arg-pos arg-len)
                           (set! pos format-string-len) #f)
                          (else #t))))
                   (if continue
                       (anychar-dispatch))))
  
                ;; format directive modifiers and parameters
  
                ((#\@)                    ; `@' modifier
                 (if (eq? modifier 'colon-at)
                     (fail "double `@' modifier"))
                 (set! modifier (if (eq? modifier 'colon) 'colon-at 'at))
                 (tilde-dispatch))
                ((#\:)                    ; `:' modifier
                 (if modifier (fail "illegal `:' modifier position"))
                 (set! modifier 'colon)
                 (tilde-dispatch))
                ((#\')                    ; Character parameter
                 (if modifier (fail "misplaced modifier"))
                 (set! params (append params (list (char->integer (next-char)))))
                 (set! param-value-found #t)
                 (tilde-dispatch))
                ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\- #\+) ; num. paramtr
                 (if modifier (fail "misplaced modifier"))
                 (let ((num-str-beg (- pos 1))
                       (num-str-end pos))
                   (do ((ch (peek-next-char) (peek-next-char)))
                       ((not (char-numeric? ch)))
                     (next-char)
                     (set! num-str-end (+ 1 num-str-end)))
                   (set! params
                         (append params
                                 (list (string->number
                                        (substring format-string
                                                   num-str-beg
                                                   num-str-end))))))
                 (set! param-value-found #t)
                 (tilde-dispatch))
                ((#\V)                    ; Variable parameter from next argum.
                 (if modifier (fail "misplaced modifier"))
                 (set! params (append params (list (next-arg))))
                 (set! param-value-found #t)
                 (tilde-dispatch))
                ((#\#)                    ; Parameter is number of remaining args
                 (if modifier (fail "misplaced modifier"))
                 (set! params (append params (list (length (rest-args)))))
                 (set! param-value-found #t)
                 (tilde-dispatch))
                ((#\,)                    ; Parameter separators
                 (if modifier (fail "misplaced modifier"))
                 (if (not param-value-found)
                     (set! params (append params '(#f)))) ; append empty paramtr
                 (set! param-value-found #f)
                 (tilde-dispatch))
                (else                     ; Unknown tilde directive
                 (fail "unknown control character `~c'"
                        (string-ref format-string (- pos 1))))))
             (else (anychar-dispatch)))))) ; in case of conditional
  
      (anychar-dispatch)                  ; start the formatting
      arg-pos))                           ; return the position in the arg. list
  
  
  (define (format:obj->str-padded pad-left obj options params)
    (let ((mincol 0)
          (colinc 1)                      ; sorry I don't understand this CL parm
          (minpad 0)
          (padchar #\space)
          (objstr (apply obj->string (append (list obj) options))))
      (if (null? params)
          objstr
          (begin
            (set! params (append params '(#f #f #f #f)))
            (if (list-ref params 0) (set! mincol (list-ref params 0)))
            (if (list-ref params 1) (set! colinc (list-ref params 1)))
            (if (list-ref params 2) (set! minpad (list-ref params 2)))
            (if (list-ref params 3)
                (set! padchar (integer->char (list-ref params 3))))
            (format:pad-str objstr (negative? mincol) pad-left
                            (abs mincol) minpad padchar)))))
  
  
  (define (format:num->str-padded modifier number params radix-num radix-prefix)
    (if (not (number? number)) (format:error "argument not a number"))
    (let ((mincol 0)
          (padchar #\space)
          (commachar #\,)
          (commawidth 3)                  ; an extension to CL
          (numstr-len 0)
          (numstr (number->string number radix-num)))
  
      (if (and (null? params) (not modifier))
          numstr
          (begin
            (set! params (append params '(#f #f #f #f)))
            (if (list-ref params 0) (set! mincol (list-ref params 0)))
            (if (list-ref params 1)
                (set! padchar (integer->char (list-ref params 1))))
            (if (list-ref params 2)
                (set! commachar (integer->char (list-ref params 2))))
            (if (list-ref params 3) (set! commawidth (list-ref params 3)))
            (set! numstr-len (string-length numstr))
  
            (if (and (memq modifier '(colon colon-at)) ; insert comma character
                     (integer? number))   ; only integers are ,-separated
                (set! numstr
                      (do ((s "")
                           (i (- numstr-len commawidth) (- i commawidth)))
                          ((or (zero? i) (negative? i))
                           (string-append
                            (substring numstr 0 (+ i commawidth )) s))
                        (set! s (string-append
                                 (string commachar)
                                 (substring numstr i (+ i commawidth)) s)))))
  
            (if (memq modifier '(at colon-at))    ; append numerical prefix
                (set! numstr (string-append radix-prefix numstr)))
  
            (format:pad-str numstr (negative? mincol) #t
                            (abs mincol) 0 padchar)))))
  
  
  (define (format:pad-str objstr fixed-field pad-left mincol minpad padchar)
    (let ((objstr-len (string-length objstr)))
      (if fixed-field
          (if (> objstr-len mincol)
              (if pad-left
                  (string-append "<"
                   (substring objstr (- objstr-len (- mincol 1)) objstr-len))
                  (string-append (substring objstr 0 (- mincol 1)) ">"))
              (if pad-left
                  (string-append (make-string (- mincol objstr-len) padchar)
                                 objstr)
                  (string-append objstr
                                 (make-string (- mincol objstr-len) padchar))))
          (if (> objstr-len mincol)
              (if pad-left
                  (string-append (make-string minpad padchar) objstr)
                  (string-append objstr (make-string minpad padchar)))
              (if pad-left
                  (string-append (make-string (- mincol objstr-len) padchar)
                                 objstr)
                  (string-append objstr
                                 (make-string (- mincol objstr-len) padchar)))))
      ))
  
  ;; obj->string converts an arbitrary scheme object to a string.
  ;; `options' is a list of options which may contain the following symbols:
  ;;   slashify:      slashifies output string as (write) does
  ;;   readproof:     prints out #<...> objects as quoted strings "#<...>" so
  ;;                  that the output can always be processed by (read)
  ;; obj->string imports format:char->string which converts a character into
  ;; a slashified string as `write' does and which is implementation dependent.
  
  
  (define (obj->string obj . options)
    (define custom-rep "")
    (define (custom-type? obj pred-l)
      (cond
       ((null? pred-l) #f)
       (((caar pred-l) obj)
        (let ((rep (cdar pred-l)))
          (set! custom-rep (if (procedure? rep) (rep obj) rep))))
       (else (custom-type? obj (cdr pred-l)))))
  
    (let to-str ((obj obj)
                 (slashify (if (memq 'slashify options) #t #f))
                 (readproof (if (memq 'readproof options) #t #f)))
      (cond
       ((custom-type? obj format:custom-types)
        (if readproof (string-append "\"" custom-rep "\"") custom-rep))
       ((string? obj)
        (if slashify
            (let ((obj-len (string-length obj)))
              (string-append
               "\""
               (let loop ((i 0) (j 0))    ; taken from Marc Feeley's pp.scm
                 (if (= j obj-len)
                     (string-append (substring obj i j) "\"")
                     (let ((c (string-ref obj j)))
                       (if (or (char=? c #\\)
                               (char=? c #\"))
                           (string-append (substring obj i j) "\\"
                                          (loop j (+ j 1)))
                           (loop i (+ j 1))))))))
            obj))
  
       ((boolean? obj) (if obj "#t" "#f"))
  
       ((number? obj) (number->string obj))
  
       ((symbol? obj) (symbol->string obj))
  
       ((char? obj)
        (if slashify
            (format:char->string obj)
            (string obj)))
  
       ((null? obj) "()")
  
       ((procedure? obj) (if readproof "\"#<procedure>\"" "#<procedure>"))
  
       ((output-port? obj) (if readproof "\"#<output-port>\"" "#<output-port>"))
  
       ((input-port? obj) (if readproof "\"#<input-port>\"" "#<input-port>"))
  
       ((list? obj)
        (string-append "("
                       (let loop ((obj-list obj))
                         (if (null? (cdr obj-list))
                             (to-str (car obj-list) 'slashify readproof)
                             (string-append
                              (to-str (car obj-list) 'slashify readproof)
                              " "
                              (loop (cdr obj-list)))))
                       ")"))
  
       ((pair? obj)
        (string-append "("
                       (to-str (car obj) 'slashify readproof)
                       " . "
                       (to-str (cdr obj) 'slashify readproof)
                       ")"))
  
       ((eof-object? obj) (if readproof "\"#<eof-object>\"" "#<eof-object>"))
  
       ((vector? obj)
        (string-append "#" (to-str (vector->list obj) 'slashify readproof)))
  
       (else (if readproof "\"#<unspecified>\"" "#<unspecified>")))
      )
  )
  
  ;; string-upcase, string-downcase, string-capitalize, string-capitalize-first
  ;; are obvious string conversion procedures and are non destructive.
  
  (define (string-upcase str)
    (let ((up-str (string-copy str)))
      (do ((i (- (string-length str) 1) (- i 1)))
          ((< i 0) up-str)
        (string-set! up-str i (char-upcase (string-ref str i))))))
  
  (define (string-downcase str)
    (let ((down-str (string-copy str)))
      (do ((i (- (string-length str) 1) (- i 1)))
          ((< i 0) down-str)
        (string-set! down-str i (char-downcase (string-ref str i))))))
  
  (define (string-capitalize str)         ; "hello" -> "Hello"
    (let ((cap-str (string-copy str))     ; "hELLO" -> "Hello"
          (non-first-alpha #f)            ; "*hello" -> "*Hello"
          (str-len (string-length str)))  ; "hello you" -> "Hello You"
      (do ((i 0 (+ i 1)))
          ((= i str-len) cap-str)
        (let ((c (string-ref str i)))
          (if (char-alphabetic? c)
              (if non-first-alpha
                  (string-set! cap-str i (char-downcase c))
                  (begin
                    (set! non-first-alpha #t)
                    (string-set! cap-str i (char-upcase c))))
              (set! non-first-alpha #f))))))
  
  (define (string-capitalize-first str)   ; "hello" -> "Hello"
    (let ((cap-str (string-copy str))     ; "hELLO" -> "Hello"
          (non-first-alpha #f)            ; "*hello" -> "*Hello"
          (str-len (string-length str)))  ; "hello you" -> "Hello you"
      (do ((i 0 (+ i 1)))
          ((= i str-len) cap-str)
        (let ((c (string-ref str i)))
          (if (char-alphabetic? c)
              (if non-first-alpha
                  (string-set! cap-str i (char-downcase c))
                  (begin
                    (set! non-first-alpha #t)
                    (string-set! cap-str i (char-upcase c)))))))))
  
  (apply format:format args))
