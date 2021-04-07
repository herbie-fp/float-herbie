#lang racket

(require herbie/plugin math/bigfloat math/base rival generic-flonum)

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
    (define (register-fl-constant! cnst fl-impl bf-impl ival-impl)
      (define cnst-name (sym-append cnst '.fl es '- nbits))
      (define info-dict
        (list (cons 'fl fl-impl)
              (cons 'bf bf-impl)
              (cons 'ival ival-impl)))
      (register-constant! cnst cnst-name name info-dict))

    ; Operator
    (define (register-fl-operator! op op-name argc fl-impl bf-impl ival-impl
                                  [nonffi-imlp #f] #:itype [itype #f] #:otype [otype #f])
      (define nonffi* (if nonffi-imlp nonffi-imlp fl-impl))
      (define op-name* (sym-append op-name '.fl es '- nbits))
      (define info-dict
        (let ([base-dict (list (cons 'fl fl-impl) (cons 'bf bf-impl)
                               (cons 'ival ival-impl) (cons 'nonffi nonffi*))])
          (cond
            [(and itype otype) (dict-set* base-dict 'itype itype 'otype otype)]
            [itype (dict-set base-dict 'itype itype)]
            [otype (dict-set base-dict 'otype otype)]
            [else base-dict])))
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
      (λ () (gfl-const es nbits pi.gfl))
      (λ () pi.bf)
      ival-pi)

    (register-fl-constant! 'E
      (λ () (gfl-const es nbits (gflexp 1.gfl)))
      (λ () (bfexp 1.bf))
      ival-e)

    (register-fl-constant! 'INFINITY
      (λ () +inf.gfl)
      (λ () +inf.bf)
      (λ () (mk-ival +inf.bf)))

    (register-fl-constant! 'NAN
      (λ () +nan.gfl)
      (λ () +nan.bf)
      (λ () (mk-ival +nan.bf)))

    (register-fl-operator! '- 'neg 1 (gfl-op es nbits gfl-) bf- ival-neg -)
    (register-fl-operator! '+ '+ 2 (gfl-op es nbits gfl+) bf+ ival-add +)
    (register-fl-operator! '- '- 2 (gfl-op es nbits gfl-) bf- ival-sub -)
    (register-fl-operator! '* '* 2 (gfl-op es nbits gfl*) bf* ival-mult *)
    (register-fl-operator! '/ '/ 2 (gfl-op es nbits gfl/) bf/ ival-div /)
    (register-fl-operator! 'sqrt 'sqrt 1 (gfl-op es nbits gflsqrt) bfsqrt ival-sqrt sqrt)
    (register-fl-operator! 'cbrt 'cbrt 1 (gfl-op es nbits gflcbrt) bfcbrt ival-cbrt (curryr expt 1/3))
    (register-fl-operator! 'abs 'abs 1 (gfl-op es nbits gflabs) bfabs ival-fabs abs)

    (register-fl-operator! 'log 'log 1 (gfl-op es nbits gfllog) bflog ival-log log)
    (register-fl-operator! 'log2 'log2 1 (gfl-op es nbits gfllog2) bflog2 ival-log2 (curryr log 2))
    (register-fl-operator! 'log10 'log10 1 (gfl-op es nbits gfllog10) bflog10 ival-log10 (curry log 10))
    (register-fl-operator! 'log1p 'log1p 1 (gfl-op es nbits gfllog1p) bflog1p
                           ival-log1p (from-bigfloat bflog1p))

    (register-fl-operator! 'exp 'exp 1 (gfl-op es nbits gflexp) bfexp ival-exp exp)
    (register-fl-operator! 'exp2 'exp2 1 (gfl-op es nbits gflexp2) bfexp2 ival-exp2 (curry expt 2))
    (register-fl-operator! 'expm1 'expm1 1 (gfl-op es nbits gflexpm1) bfexpm1
                           ival-expm1 (from-bigfloat bfexpm1))
    (register-fl-operator! 'pow 'pow 2 (gfl-op es nbits gflexpt) bfexpt ival-pow expt)

    (register-fl-operator! 'sin 'sin 1 (gfl-op es nbits gflsin) bfsin ival-sin sin)
    (register-fl-operator! 'cos 'cos 1 (gfl-op es nbits gflcos) bfcos ival-cos cos)
    (register-fl-operator! 'tan 'tan 1 (gfl-op es nbits gfltan) bftan ival-tan tan)
    (register-fl-operator! 'asin 'asin 1 (gfl-op es nbits gflasin) bfasin ival-asin asin)
    (register-fl-operator! 'acos 'acos 1 (gfl-op es nbits gflacos) bfacos ival-acos acos)
    (register-fl-operator! 'atan 'atan 1 (gfl-op es nbits gflatan) bfatan ival-atan atan)

    (register-fl-operator! 'sinh 'sinh 1 (gfl-op es nbits gflsinh) bfsinh ival-sinh sinh)
    (register-fl-operator! 'cosh 'cosh 1 (gfl-op es nbits gflcosh) bfcosh ival-cosh cosh)
    (register-fl-operator! 'tanh 'tanh 1 (gfl-op es nbits gfltanh) bftanh ival-tanh tanh)
    (register-fl-operator! 'asinh 'asinh 1 (gfl-op es nbits gflasinh) bfasinh ival-asinh asinh)
    (register-fl-operator! 'acosh 'acosh 1 (gfl-op es nbits gflacosh) bfacosh ival-acosh acosh)
    (register-fl-operator! 'atanh 'atanh 1 (gfl-op es nbits gflatanh) bfatanh ival-atanh atanh)

    (register-fl-operator! '== '== 2 (comparator gfl=) (comparator bf=) (comparator ival-==)
                           (comparator =) #:itype name #:otype 'bool) ; override number of arguments
    (register-fl-operator! '!= '!= 2 (inv-comparator gfl=) (inv-comparator bf=) (comparator ival-!=)
                           (inv-comparator =) #:itype name #:otype 'bool) ; override number of arguments
    (register-fl-operator! '< '< 2 (comparator gfl<) (comparator bf<) (comparator ival-<)
                           (comparator <) #:itype name #:otype 'bool) ; override number of arguments
    (register-fl-operator! '> '> 2 (comparator gfl>) (comparator bf>) (comparator ival->)
                           (comparator >) #:itype name #:otype 'bool) ; override number of arguments
    (register-fl-operator! '<= '<= 2 (comparator gfl<=) (comparator bf<=) (comparator ival-<=)
                           (comparator <=) #:itype name #:otype 'bool) ; override number of arguments
    (register-fl-operator! '>= '>= 2 (comparator gfl>=) (comparator bf>=) (comparator ival->=)
                           (comparator >=) #:itype name #:otype 'bool) ; override number of arguments

    #t]
   [_ #f]))

(register-generator! generate-floating-point)