#lang racket

(require herbie/plugin math/bigfloat math/base generic-flonum)

(eprintf "Loading generic float support...\n")

(define ((comparator test) . args)
  (for/and ([left args] [right (cdr args)])
    (test left right)))

(define ((inv-comparator test) . args)
  (for/or ([left args] [right (cdr args)])
    (not (test left right))))

(define (from-bigfloat bff)
  (λ args (bigfloat->flonum (apply bff (map bf args)))))

(define (sym-append . args)
  (string->symbol (apply string-append (map ~s args))))

(define-syntax-rule (gfl-op es nbits fun)
  (λ args
    (parameterize ([gfl-exponent es] [gfl-bits nbits])
      (apply fun args))))

(define-syntax-rule (gfl-const es nbits cnst)
  (parameterize ([gfl-exponent es] [gfl-bits nbits])
    cnst))

;; Generator for fixed-point representations
(define (generate-floating-point name)
  (match name
   [(list 'float es nbits)

    ; Float
    (define (register-fl-constant! cnst fl-impl #:bf [bf-impl #f] #:ival [ival-impl #f])
      (define cnst-name (sym-append cnst '.fl es '- nbits))
      (define base-dict
        (list (cons 'fl fl-impl)
              (cons 'bf bf-impl)
              (cons 'ival ival-impl)))
      (define info-dict (filter cdr base-dict))
      (register-constant! cnst cnst-name name info-dict))

    ; Operator
    (define (register-fl-operator! op op-name argc fl-impl
                                   #:bf [bf-impl #f] #:ival [ival-impl #f]
                                   #:nonffi [nonffi-imlp #f]
                                   #:itype [itype #f] #:otype [otype #f])
      (define base-dict (list (cons 'fl fl-impl) (cons 'bf bf-impl) (cons 'ival ival-impl)
                              (cons 'ival ival-impl) (cons 'itype itype) (cons 'otype otype)))
      (define info-dict (filter cdr base-dict))
      (define op-name* (sym-append op-name '.fl es '- nbits))
      (register-operator! op op-name* (make-list argc name) name info-dict))
  
    ; Representation
    (register-representation! name 'real gfl?
      (gfl-op es nbits bigfloat->gfl)
      gfl->bigfloat
      (gfl-op es nbits ordinal->gfl)
      (gfl-op es nbits gfl->ordinal)
      nbits
      (disjoin gflinfinite? gflnan?))

    (register-fl-constant! 'PI
      (λ () (gfl-const es nbits pi.gfl)))

    (register-fl-constant! 'E
      (λ () (gfl-const es nbits (gflexp 1.gfl))))

    (register-fl-constant! 'INFINITY
      (λ () +inf.gfl))

    (register-fl-constant! 'NAN
      (λ () +nan.gfl))

    (register-fl-operator! '- 'neg 1 (gfl-op es nbits gfl-))
    (register-fl-operator! '+ '+ 2 (gfl-op es nbits gfl+))
    (register-fl-operator! '- '- 2 (gfl-op es nbits gfl-))
    (register-fl-operator! '* '* 2 (gfl-op es nbits gfl*))
    (register-fl-operator! '/ '/ 2 (gfl-op es nbits gfl/))
    (register-fl-operator! 'sqrt 'sqrt 1 (gfl-op es nbits gflsqrt))
    (register-fl-operator! 'cbrt 'cbrt 1 (gfl-op es nbits gflcbrt))
    (register-fl-operator! 'fabs 'fabs 1 (gfl-op es nbits gflabs))

    (register-fl-operator! 'log 'log 1 (gfl-op es nbits gfllog))
    (register-fl-operator! 'log2 'log2 1 (gfl-op es nbits gfllog2))
    (register-fl-operator! 'log10 'log10 1 (gfl-op es nbits gfllog10))
    (register-fl-operator! 'log1p 'log1p 1 (gfl-op es nbits gfllog1p))

    (register-fl-operator! 'exp 'exp 1 (gfl-op es nbits gflexp))
    (register-fl-operator! 'exp2 'exp2 1 (gfl-op es nbits gflexp2))
    (register-fl-operator! 'expm1 'expm1 1 (gfl-op es nbits gflexpm1))
    (register-fl-operator! 'pow 'pow 2 (gfl-op es nbits gflexpt))

    (register-fl-operator! 'sin 'sin 1 (gfl-op es nbits gflsin))
    (register-fl-operator! 'cos 'cos 1 (gfl-op es nbits gflcos))
    (register-fl-operator! 'tan 'tan 1 (gfl-op es nbits gfltan))
    (register-fl-operator! 'asin 'asin 1 (gfl-op es nbits gflasin))
    (register-fl-operator! 'acos 'acos 1 (gfl-op es nbits gflacos))
    (register-fl-operator! 'atan 'atan 1 (gfl-op es nbits gflatan))

    (register-fl-operator! 'sinh 'sinh 1 (gfl-op es nbits gflsinh))
    (register-fl-operator! 'cosh 'cosh 1 (gfl-op es nbits gflcosh))
    (register-fl-operator! 'tanh 'tanh 1 (gfl-op es nbits gfltanh))
    (register-fl-operator! 'asinh 'asinh 1 (gfl-op es nbits gflasinh))
    (register-fl-operator! 'acosh 'acosh 1 (gfl-op es nbits gflacosh))
    (register-fl-operator! 'atanh 'atanh 1 (gfl-op es nbits gflatanh))

    (register-fl-operator! '== '== 2 (comparator gfl=) #:itype name #:otype 'bool) ; override number of arguments
    (register-fl-operator! '!= '!= 2 (inv-comparator gfl=) #:itype name #:otype 'bool) ; override number of arguments
    (register-fl-operator! '< '< 2 (comparator gfl<) #:itype name #:otype 'bool) ; override number of arguments
    (register-fl-operator! '> '> 2 (comparator gfl>) #:itype name #:otype 'bool) ; override number of arguments
    (register-fl-operator! '<= '<= 2 (comparator gfl<=) #:itype name #:otype 'bool) ; override number of arguments
    (register-fl-operator! '>= '>= 2 (comparator gfl>=) #:itype name #:otype 'bool) ; override number of arguments

    #t]
   [_ #f]))

(register-generator! generate-floating-point)