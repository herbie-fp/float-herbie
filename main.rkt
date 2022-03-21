#lang racket

(require herbie/plugin math/bigfloat math/base generic-flonum)

(eprintf "Loading generic float support...\n")

(define ((comparator test) . args)
  (for/and ([left args] [right (cdr args)])
    (test left right)))

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
(define (generate-floating-point* name)
  (match name
   [(list 'float es nbits)
  
    ; Representation
    (register-representation! name 'real gfl?
      (gfl-op es nbits bigfloat->gfl)
      gfl->bigfloat
      (gfl-op es nbits ordinal->gfl)
      (gfl-op es nbits gfl->ordinal)
      nbits
      (disjoin gflinfinite? gflnan?))

    (define repr (get-representation name))

    ; Constant registration
    (define (register-fl-constant! cnst fl-impl #:bf [bf-impl #f] #:ival [ival-impl #f])
      (define cnst-name (sym-append cnst '.fl es '- nbits))
      (define base-dict
        (list (cons 'fl fl-impl)
              (cons 'bf bf-impl)
              (cons 'ival ival-impl)))
      (define info-dict (filter cdr base-dict))
      (register-operator-impl! cnst cnst-name (list) repr info-dict))

    ; Operator registration
    (define (register-fl-operator! op op-name argc fl-impl
                                   #:bf [bf-impl #f] #:ival [ival-impl #f]
                                   #:nonffi [nonffi-imlp #f] #:otype [otype #f])
      (define base-dict (list (cons 'fl fl-impl) (cons 'bf bf-impl) (cons 'ival ival-impl)))
      (define info-dict (filter cdr base-dict))
      (define op-name* (sym-append op-name '.fl es '- nbits))
      (define orepr (if otype (get-representation otype) repr))
      (register-operator-impl! op op-name* (make-list argc repr) orepr info-dict))

    (register-fl-constant! 'PI
      (λ () (gfl-const es nbits pi.gfl)))

    (register-fl-constant! 'E
      (λ () (gfl-const es nbits (gflexp 1.gfl))))

    (register-fl-constant! 'INFINITY
      (λ () +inf.gfl))

    (register-fl-constant! 'NAN
      (λ () +nan.gfl))

    (register-fl-operator! 'neg 'neg 1 (gfl-op es nbits gfl-))
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

    (register-fl-operator! '== '== 2 (comparator gfl=) #:otype 'bool)
    (register-fl-operator! '!= '!= 2 (negate (comparator gfl=)) #:otype 'bool)
    (register-fl-operator! '< '< 2 (comparator gfl<) #:otype 'bool)
    (register-fl-operator! '> '> 2 (comparator gfl>) #:otype 'bool)
    (register-fl-operator! '<= '<= 2 (comparator gfl<=) #:otype 'bool)
    (register-fl-operator! '>= '>= 2 (comparator gfl>=) #:otype 'bool)

    repr]
   [_ #f]))

(define (generate-floating-point name)
  (match name
   ; This plugin is slow!! Use Herbie's built-in equivalents if possible.
   [(list 'float 11 64)
    (define repr (get-representation 'binary64))
    (register-representation-alias! name repr)
    repr]
   [(list 'float 8 32)
    (define repr (get-representation 'binary32))
    (register-representation-alias! name repr)
    repr]
   ; Helpful aliases
   ['binary256
    (define repr (generate-floating-point* '(float 19 256)))
    (register-representation-alias! name repr)
    repr]
   ['binary128
    (define repr (generate-floating-point* '(float 15 128)))
    (register-representation-alias! name repr)
    repr]
   ['binary80
    (define repr (generate-floating-point* '(float 15 80)))
    (register-representation-alias! name repr)
    repr]
   ['pxr24
    (define repr (generate-floating-point* '(float 8 24)))
    (register-representation-alias! name repr)
    repr]
   ['fp24
    (define repr (generate-floating-point* '(float 7 24)))
    (register-representation-alias! name repr)
    repr]
   ['tensorfloat
    (define repr (generate-floating-point* '(float 8 19)))
    (register-representation-alias! name repr)
    repr]
   ['bfloat16
    (define repr (generate-floating-point* '(float 8 16)))
    (register-representation-alias! name repr)
    repr]
   ['binary16
    (define repr (generate-floating-point* '(float 5 16)))
    (register-representation-alias! name repr)
    repr]
   ; Default
   [_ (generate-floating-point* name)]))

(register-generator! generate-floating-point)
