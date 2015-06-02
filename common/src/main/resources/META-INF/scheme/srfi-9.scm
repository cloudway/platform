(import-library "com.cloudway.fp.scheme.Record")

; We define the following precedures:
; 
; (make-record-type <type-name> <field-names>)      -> <record-type>
; (record-constructor <record-type> <field-names>)  -> <constructor>
; (record-predicate <record-type>)                  -> <predicate>
; (record-accessor <record-type> <field-name>)      -> <accessor>
; (record-modifier <record-type> <field-name>)      -> <modifier>
;   where
; (<constructor> <initial-value> ...)           -> <record>
; (<predicate> <value>)                         -> <boolean>
; (<accessor> <record>)                         -> <value>
; (<modifier> <record> <value>)                 -> <unspecified>

; Record types are implemented using vector-like records. The first
; slot of each record contains the record's type, which is itself a
; record.

(define (record-type record)
  (record-ref record 0))

;------------------
; Record types are themselves records, so we first define the type for
; them.  Except for problems with circularities, this could be defined as:
;   (define-record-type :record-type
;     (make-record-type name field-tags)
;     record-type?
;     (name record-type-name)
;     (field-tags record-type-field-tags))
; As it is, we need to define everything by hand.

(define :record-type (make-record 3))
(record-set! :record-type 0 :record-type)   ; Its type is itself
(record-set! :record-type 1 ':record-type)
(record-set! :record-type 2 '(name field-tags))

; Now that :record-type exists we can define a procedure for making more
; record types.

(define (make-record-type name field-tags)
  (let ((new (make-record 3)))
    (record-set! new 0 :record-type)
    (record-set! new 1 name)
    (record-set! new 2 field-tags)
    new))

; Accessors for record types.

(define (record-type-name record-type)
  (record-ref record-type 1))

(define (record-type-field-tags record-type)
  (record-ref record-type 2))

;----------------
; A utility for getting the offset of a field within a record.

(define (field-index type tag)
  (let loop ((i 1) (tags (record-type-field-tags type)))
    (cond ((null? tags)
           (error "record type has no such field" type tag))
          ((eq? tag (car tags))
           i)
          (else
           (loop (+ i 1) (cdr tags))))))

;----------------
; Now we are ready to define RECORD-CONSTRUCTOR and the rest of the
; procedures used by the macro expansion of DEFINE-RECORD-TYPE.

(define (record-constructor type tags)
  (let ((size (length (record-type-field-tags type)))
        (arg-count (length tags))
        (indexes (map (lambda (tag)
                        (field-index type tag))
                      tags)))
    (lambda args
      (if (= (length args) arg-count)
        (let ((new (make-record (+ size 1))))
          (record-set! new 0 type)
          (for-each (lambda (arg i)
                      (record-set! new i arg))
                    args
                    indexes)
          new)
        (error "wrong number of arguments to constructor" type args (length args) arg-count)))))

(define (record-predicate type)
  (lambda (thing)
    (and (record? thing)
         (eq? (record-type thing)
              type))))

(define (record-accessor type tag)
  (let ((index (field-index type tag)))
    (lambda (thing)
      (if (and (record? thing)
               (eq? (record-type thing)
                    type))
        (record-ref thing index)
        (error "accessor applied to bad value" type tag thing)))))

(define (record-modifier type tag)
  (let ((index (field-index type tag)))
    (lambda (thing value)
      (if (and (record? thing)
               (eq? (record-type thing)
                    type))
        (record-set! thing index value)
        (error "modifier applied to bad value" type tag thing)))))


; Definition of DEFINE-RECORD-TYPE

(define-macro (define-record-type type (constructor . constructor-tags) predicate . field-specs)
  `(begin
     (define ,type
       (make-record-type ',type ',(map car field-specs)))
     (define ,constructor
       (record-constructor ,type ',constructor-tags))
     (define ,predicate
       (record-predicate ,type))
     ,@(map (case-lambda
              (((field-tag accessor))
                `(define ,accessor (record-accessor ,type ',field-tag)))
              (((field-tag accessor modifier))
                `(begin
                   (define ,accessor (record-accessor ,type ',field-tag))
                   (define ,modifier (record-modifier ,type ',field-tag)))))
            field-specs)))
