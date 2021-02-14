#lang racket

(require herbie/plugin math/bigfloat rival "float.rkt")

(eprintf "Loading generic float support...\n")

(define ((comparator test) . args)
  (for/and ([left args] [right (cdr args)])
    (test left right)))

(define (!=-fn . args)
  (not (check-duplicates args float-=)))

(define (bf!=-fn . args)
  (not (check-duplicates args bf=)))

(define (sym-append . args)
  (string->symbol (apply string-append (map ~s args))))

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
                #:nonffi [nonffi-imlp #f] #:itype [itype #f] #:otype [otype #f])
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
    (register-representation! name 'real float?
      (curryr bf->float es nbits)
      float->bf
      (curryr ordinal->float es nbits)
      float->ordinal
      nbits
      (disjoin float-infinite? float-nan?))

    (register-fl-constant! 'PI
      (λ () ((float-mul es nbits) (real->float 2 es nbits)
                                  ((float-acos es nbits) (real->float 0 es nbits))))
      (λ () pi.bf)
      ival-pi)

    (register-fl-constant! 'E
      (λ () ((float-exp es nbits) (real->float 1 es nbits)))
      (λ () (bfexp 1.bf))
      ival-e)

    (register-fl-constant! 'INFINITY
      (λ () (real->float +inf.0 es nbits))
      (λ () +inf.bf)
      (λ () (mk-ival +inf.bf)))

    (register-fl-constant! 'NAN
      (λ () (real->float +nan.0 es nbits))
      (λ () +nan.bf)
      (λ () (mk-ival +nan.bf)))

    (register-fl-operator! '- 'neg 1 (float-neg es nbits) bf- ival-neg)
    (register-fl-operator! '+ '+ 2 (float-add es nbits) bf+ ival-add)
    (register-fl-operator! '- '- 2 (float-sub es nbits) bf- ival-sub)
    (register-fl-operator! '* '* 2 (float-mul es nbits) bf* ival-mult)
    (register-fl-operator! '/ '/ 2 (float-div es nbits) bf/ ival-div)
    (register-fl-operator! 'sqrt 'sqrt 1 (float-sqrt es nbits) bfsqrt ival-sqrt)
    (register-fl-operator! 'cbrt 'cbrt 1 (float-cbrt es nbits) bfcbrt ival-cbrt)
    (register-fl-operator! 'abs 'abs 1 (float-abs es nbits) bfabs ival-fabs)

    (register-fl-operator! 'log 'log 1 (float-log es nbits) bflog ival-log)
    (register-fl-operator! 'log2 'log2 1 (float-log2 es nbits) bflog2 ival-log2)
    (register-fl-operator! 'log10 'log10 1 (float-log10 es nbits) bflog10 ival-log10)
    (register-fl-operator! 'log1p 'log1p 1 (float-log1p es nbits) bflog1p ival-log1p)

    (register-fl-operator! 'exp 'exp 1 (float-exp es nbits) bfexp ival-exp)
    (register-fl-operator! 'exp2 'exp2 1 (float-exp2 es nbits) bfexp2 ival-exp2)
    (register-fl-operator! 'expm1 'expm1 1 (float-expm1 es nbits) bfexpm1 ival-expm1)
    (register-fl-operator! 'pow 'pow 2 (float-pow es nbits) bfexpt ival-pow)

    (register-fl-operator! 'sin 'sin 1 (float-sin es nbits) bfsin ival-sin)
    (register-fl-operator! 'cos 'cos 1 (float-cos es nbits) bfcos ival-cos)
    (register-fl-operator! 'tan 'tan 1 (float-tan es nbits) bftan ival-tan)
    (register-fl-operator! 'asin 'asin 1 (float-asin es nbits) bfasin ival-asin)
    (register-fl-operator! 'acos 'acos 1 (float-acos es nbits) bfacos ival-acos)
    (register-fl-operator! 'atan 'atan 1 (float-atan es nbits) bfatan ival-atan)

    (register-fl-operator! 'sinh 'sinh 1 (float-sinh es nbits) bfsinh ival-sinh)
    (register-fl-operator! 'cosh 'cosh 1 (float-cosh es nbits) bfcosh ival-cosh)
    (register-fl-operator! 'tanh 'tanh 1 (float-tanh es nbits) bftanh ival-tanh)
    (register-fl-operator! 'asinh 'asinh 1 (float-asinh es nbits) bfasinh ival-asinh)
    (register-fl-operator! 'acosh 'acosh 1 (float-acosh es nbits) bfacosh ival-acosh)
    (register-fl-operator! 'atanh 'atanh 1 (float-atanh es nbits) bfatanh ival-atanh)

    (register-fl-operator! '== '== 2 (comparator float-=) (comparator bf=) (comparator ival-==)
                           #:itype name #:otype 'bool) ; override number of arguments
    (register-fl-operator! '!= '!= 2 !=-fn bf!=-fn ival-!=
                           #:itype name #:otype 'bool) ; override number of arguments
    (register-fl-operator! '< '< 2 (comparator float-<) (comparator bf<) (comparator ival-<)
                           #:itype name #:otype 'bool) ; override number of arguments
    (register-fl-operator! '> '> 2 (comparator float->) (comparator bf>) (comparator ival->)
                           #:itype name #:otype 'bool) ; override number of arguments
    (register-fl-operator! '<= '<= 2 (comparator float-<=) (comparator bf<=) (comparator ival-<=)
                           #:itype name #:otype 'bool) ; override number of arguments
    (register-fl-operator! '>= '>= 2 (comparator float->=) (comparator bf>=) (comparator ival->=)
                           #:itype name #:otype 'bool) ; override number of arguments

    #t]
   [_ #f]))

(register-generator! generate-floating-point)