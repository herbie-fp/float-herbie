#lang racket

(require herbie/plugin math/bigfloat rival "float.rkt")

(eprintf "Loading generic float support...\n")

(define (bfshl x y)
  (bf* x (bfexpt 2.bf y)))

(define (bfshr x y)
  (bf/ x (bfexpt 2.bf y)))

(define ((comparator test) . args)
  (for/and ([left args] [right (cdr args)])
    (test left right)))

(define (!=-fn . args)
  (not (check-duplicates args =)))

(define (bf!=-fn . args)
  (not (check-duplicates args bf=)))

(define (sym-append . args)
  (string->symbol (apply string-append (map ~s args))))

;; Generator for fixed-point representations
(define (generate-floating-point name)
  (match name
   [(list 'float es nbits)
    #t]
   [_ #f]))

(register-generator! generate-floating-point)