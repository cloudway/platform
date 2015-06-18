; MODULE DEFINITION FOR SRFI-27
; =============================
;
; Sebastian.Egner@philips.com, Mar-2002, in Scheme 48 0.57
;
; This file contains the top-level definition for the 54-bit integer-only
; implementation of SRFI-27 for the Scheme 48 0.57 system.
;
; 1. The core generator is implemented in 'mrg32k3a-a.scm'.
; 2. The generic parts of the interface are in 'mrg32k3a.scm'.
; 3. The non-generic parts (record type, time, error) are here.
;
; creating the module:
;   ,config ,load srfi-27-a.scm
;
; loading the module, once created:
;   ,open srfi-27
;
; history of this file:
;   SE, 22-Mar-2002: initial version
;   SE, 27-Mar-2002: checked again

(define-record-type :random-source
  (:random-source-make
   state-ref
   state-set!
   randomize!
   pseudo-randomize!
   make-integers
   make-reals)
  :random-source?
  (state-ref :random-source-state-ref)
  (state-set! :random-source-state-set!)
  (randomize! :random-source-randomize!)
  (pseudo-randomize! :random-source-pseudo-randomize!)
  (make-integers :random-source-make-integers)
  (make-reals :random-source-make-reals))

(define (:random-source-current-time)
  (#!java.lang.System currentTimeMillis:))

(define (mrg32k3a-random-m1 state)
  (let ((x11 (vector-ref state 0))
        (x12 (vector-ref state 1))
        (x13 (vector-ref state 2))
        (x21 (vector-ref state 3))
        (x22 (vector-ref state 4))
        (x23 (vector-ref state 5)))
    (let ((x10 (modulo (- (* 1403580 x12) (* 810728 x13)) 4294967087))
          (x20 (modulo (- (* 527612 x21) (* 1370589 x23)) 4294944443)))
      (vector-set! state 0 x10)
      (vector-set! state 1 x11)
      (vector-set! state 2 x12)
      (vector-set! state 3 x20)
      (vector-set! state 4 x21)
      (vector-set! state 5 x22)
      (modulo (- x10 x20) 4294967087))))

; interface to the generic parts of the generator

(define (mrg32k3a-pack-state unpacked-state)
  unpacked-state)

(define (mrg32k3a-unpack-state state)
  state)

(define (mrg32k3a-random-range) ; m1
  4294967087)

(define (mrg32k3a-random-integer state range) ; rejection method
  (let* ((q (quotient 4294967087 range))
         (qn (* q range)))
    (do ((x (mrg32k3a-random-m1 state) (mrg32k3a-random-m1 state)))
        ((< x qn) (quotient x q)))))

(define (mrg32k3a-random-real state) ; normalization is 1/(m1+1)
  (* 0.0000000002328306549295728 (+ 1.0 (mrg32k3a-random-m1 state))))

(require mrg32k3a)
