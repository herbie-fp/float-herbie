#lang racket

(require math/bigfloat)

(provide (struct-out float)
         (contract-out
          [real->float (real? exact-positive-integer? exact-positive-integer? . -> . float?)]
          [float->real (float? . -> . real?)]
          [bf->float (bigfloat? exact-positive-integer? exact-positive-integer? . -> . float?)]
          [float->bf (float? . -> . bigfloat?)]
          [ordinal->float (exact-integer? exact-positive-integer? exact-positive-integer? . -> . float?)]
          [float->ordinal (float? . -> . exact-integer?)]
          [float-infinite? (float? . -> . boolean?)]
          [float-nan? (float? . -> . boolean?)]

          [float-= (float? float? . -> . boolean?)]
          [float-> (float? float? . -> . boolean?)]
          [float-< (float? float? . -> . boolean?)]
          [float->= (float? float? . -> . boolean?)]
          [float-<= (float? float? . -> . boolean?)]

          [float-sqrt (exact-positive-integer? exact-positive-integer? . -> . (float? . -> . float?))]
          [float-cbrt (exact-positive-integer? exact-positive-integer? . -> . (float? . -> . float?))]
          [float-neg (exact-positive-integer? exact-positive-integer? . -> . (float? . -> . float?))]
          [float-abs (exact-positive-integer? exact-positive-integer? . -> . (float? . -> . float?))]
          [float-log (exact-positive-integer? exact-positive-integer? . -> . (float? . -> . float?))]
          [float-log2 (exact-positive-integer? exact-positive-integer? . -> . (float? . -> . float?))]
          [float-log10 (exact-positive-integer? exact-positive-integer? . -> . (float? . -> . float?))]
          [float-log1p (exact-positive-integer? exact-positive-integer? . -> . (float? . -> . float?))]
          [float-exp (exact-positive-integer? exact-positive-integer? . -> . (float? . -> . float?))]
          [float-exp2 (exact-positive-integer? exact-positive-integer? . -> . (float? . -> . float?))]
       ;  [float-exp10 (exact-positive-integer? exact-positive-integer? . -> . (float? . -> . float?))]
          [float-expm1 (exact-positive-integer? exact-positive-integer? . -> . (float? . -> . float?))]
          [float-sin (exact-positive-integer? exact-positive-integer? . -> . (float? . -> . float?))]
          [float-cos (exact-positive-integer? exact-positive-integer? . -> . (float? . -> . float?))]
          [float-tan (exact-positive-integer? exact-positive-integer? . -> . (float? . -> . float?))]
          [float-asin (exact-positive-integer? exact-positive-integer? . -> . (float? . -> . float?))]
          [float-acos (exact-positive-integer? exact-positive-integer? . -> . (float? . -> . float?))]
          [float-atan (exact-positive-integer? exact-positive-integer? . -> . (float? . -> . float?))]
          [float-cosh (exact-positive-integer? exact-positive-integer? . -> . (float? . -> . float?))]
          [float-sinh (exact-positive-integer? exact-positive-integer? . -> . (float? . -> . float?))]
          [float-tanh (exact-positive-integer? exact-positive-integer? . -> . (float? . -> . float?))]
          [float-sech (exact-positive-integer? exact-positive-integer? . -> . (float? . -> . float?))]
          [float-csch (exact-positive-integer? exact-positive-integer? . -> . (float? . -> . float?))]
          [float-coth (exact-positive-integer? exact-positive-integer? . -> . (float? . -> . float?))]
          [float-acosh (exact-positive-integer? exact-positive-integer? . -> . (float? . -> . float?))]
          [float-asinh (exact-positive-integer? exact-positive-integer? . -> . (float? . -> . float?))]
          [float-atanh (exact-positive-integer? exact-positive-integer? . -> . (float? . -> . float?))]

          [float-add (exact-positive-integer? exact-positive-integer? . -> . (float? float? . -> . float?))]
          [float-sub (exact-positive-integer? exact-positive-integer? . -> . (float? float? . -> . float?))]
          [float-mul (exact-positive-integer? exact-positive-integer? . -> . (float? float? . -> . float?))]
          [float-div (exact-positive-integer? exact-positive-integer? . -> . (float? float? . -> . float?))]
          [float-pow (exact-positive-integer? exact-positive-integer? . -> . (float? float? . -> . float?))]
          [float-atan2 (exact-positive-integer? exact-positive-integer? . -> . (float? float? . -> . float?))]

          [float-fma (exact-positive-integer? exact-positive-integer? . -> . (float? float? float? . -> . float?))]))

(struct float (val es nbits)
        #:transparent
        #:methods gen:custom-write
        [(define (write-proc x port mode)
          (fprintf port "#<fl[~a, ~a]: ~a>" (float-es x) (float-nbits x) 
                        (bigfloat->string (float-val x))))])

;;;;;;;;;;;;;;;;; FFI for MPFR ;;;;;;;;;;;;;;;;;;;;;;

(module hairy racket/base
  (require ffi/unsafe math/private/bigfloat/mpfr)
  (provide (all-defined-out))

  ;; MPFR extra functions
  (define mpfr_get_emin (get-mpfr-fun 'mpfr_get_emin (_fun -> _exp_t)))
  (define mpfr_get_emax (get-mpfr-fun 'mpfr_get_emax (_fun -> _exp_t)))
  (define mpfr_set_emin (get-mpfr-fun 'mpfr_set_emin (_fun _exp_t -> _int)))
  (define mpfr_set_emax (get-mpfr-fun 'mpfr_set_emax (_fun _exp_t -> _int)))
  (define mpfr_subnormalize (get-mpfr-fun 'mpfr_subnormalize (_fun _mpfr-pointer _int _rnd_t -> _int)))
  (define mpfr_check_range (get-mpfr-fun 'mpfr_check_range (_fun _mpfr-pointer _int _rnd_t -> _int)))

  ;;; 1-ary functions

  (define-syntax-rule (mpfr-1ary-fun name mpfr-name)
    (begin
      (define fun (get-mpfr-fun mpfr-name (_fun _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))
      (define (name x)
        (define r (bf 0))
        (define t (fun r x (bf-rounding-mode)))
        (mpfr_subnormalize r t (bf-rounding-mode))
        r)))

  (define-syntax-rule (mpfr-1ary-funs [name mpfr-name] ...)
    (begin (mpfr-1ary-fun name mpfr-name) ...))

  (mpfr-1ary-funs
   [mpfr-sqrt 'mpfr_sqrt]
   [mpfr-cbrt 'mpfr_cbrt]
   [mpfr-neg 'mpfr_neg]
   [mpfr-abs 'mpfr_abs]
   [mpfr-log 'mpfr_log]
   [mpfr-log2 'mpfr_log2]
   [mpfr-log10 'mpfr_log10]
   [mpfr-log1p 'mpfr_log1p]
   [mpfr-exp 'mpfr_exp]
   [mpfr-exp2 'mpfr_exp2]
   [mpfr-exp10 'mpfr_exp10]
   [mpfr-expm1 'mpfr_expm1]
   [mpfr-cos 'mpfr_cos]
   [mpfr-sin 'mpfr_sin]
   [mpfr-tan 'mpfr_tan]
   [mpfr-sec 'mpfr_sec]
   [mpfr-csc 'mpfr_csc]
   [mpfr-cot 'mpfr_cot]
   [mpfr-acos 'mpfr_acos]
   [mpfr-asin 'mpfr_asin]
   [mpfr-atan 'mpfr_atan]
   [mpfr-cosh 'mpfr_cosh]
   [mpfr-sinh 'mpfr_sinh]
   [mpfr-tanh 'mpfr_tanh]
   [mpfr-sech 'mpfr_sech]
   [mpfr-csch 'mpfr_csch]
   [mpfr-coth 'mpfr_coth]
   [mpfr-acosh 'mpfr_acosh]
   [mpfr-asinh 'mpfr_asinh]
   [mpfr-atanh 'mpfr_atanh])

  ;;; 2-ary functions

  (define-syntax-rule (mpfr-2ary-fun name mpfr-name)
    (begin
      (define fun (get-mpfr-fun mpfr-name (_fun _mpfr-pointer _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))
      (define (name x y)
        (define r (bf 0))
        (define t (fun r x y (bf-rounding-mode)))
        (mpfr_subnormalize r t (bf-rounding-mode))
        r)))

  (define-syntax-rule (mpfr-2ary-funs [name mpfr-name] ...)
    (begin (mpfr-2ary-fun name mpfr-name) ...))

  (mpfr-2ary-funs
   [mpfr-add 'mpfr_add]
   [mpfr-sub 'mpfr_sub]
   [mpfr-mul 'mpfr_mul]
   [mpfr-div 'mpfr_div]
   [mpfr-pow 'mpfr_pow]
   [mpfr-atan2 'mpfr_atan2])

  (define (mpfr-fma x y z)
    (define fun (get-mpfr-fun 'mpfr_fma (_fun _mpfr-pointer _mpfr-pointer _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))
    (define r (bf 0))
    (define t (fun r x y z (bf-rounding-mode)))
    (mpfr_subnormalize r t (bf-rounding-mode))
    r)
)
(require (submod "." hairy))

(define (set-mpfr-exp es nbits)
  (let ([emin (mpfr_get_emin)] [emax (mpfr_get_emax)]
        [p2 (expt 2 (- es 1))])
    (mpfr_set_emin (- 4 (+ p2 (- nbits es))))
    (mpfr_set_emax p2)
    (values emin emax)))

(define (set-mpfr-exp2 emin emax)
  (let ([emin* (mpfr_get_emin)] [emax* (mpfr_get_emax)])
    (mpfr_set_emin emin)
    (mpfr_set_emax emax)
    (values emin* emax*)))

;;;;;;;;;;;;;; Conversions ;;;;;;;;;;;;;;;;;;

(define (real->float x es nbits)
  (parameterize ([bf-precision (- nbits es)])
    (define-values (emin emax) (set-mpfr-exp es nbits))
    (define x* (bf x))
    (mpfr_check_range x* 0 (bf-rounding-mode))
    (mpfr_subnormalize x* 0 (bf-rounding-mode))
    (define val (float x* es nbits))
    (set-mpfr-exp2 emin emax)
    val))

(define (float->real x)
  (bigfloat->real (float-val x)))

(define (bf->float x es nbits)
  (parameterize ([bf-precision (- nbits es)])
    (define-values (emin emax) (set-mpfr-exp es nbits))
    (define x* (bfcopy x))
    (mpfr_check_range x* 0 (bf-rounding-mode))
    (mpfr_subnormalize x* 0 (bf-rounding-mode))
    (define val (float x* es nbits))
    (set-mpfr-exp2 emin emax)
    val))

(define (float->bf x)
  (bfcopy (float-val x)))

(define (floor-pow2 x)
  (for/fold ([l 0]) ([i (in-naturals 1)])
            #:break (> (expt 2 i) x)
    i))

(define (ordinal->float x es nbits)
  (define sig (- nbits es))
  (parameterize ([bf-precision (- nbits es)])
    (define-values (emin emax) (set-mpfr-exp es nbits))
    (define x* (abs x))
    (define infty (* (expt 2 (- sig 1)) (- (expt 2 es) 1)))
    (define val
      (cond
       [(> x* infty) +nan.bf]
       [(= x* infty) +inf.bf]
       [(zero? x*) 0.bf]
       [(< x* (expt 2 (- sig 1)))
        (define unit (ordinal->bigfloat 1))
        (define exp-offset (floor-pow2 x*))
        (define base (bfshift unit exp-offset))
        (define diff (- x* (expt 2 exp-offset)))
        (bf+ base (bf* unit (bf diff)))]
       [else
        (ordinal->bigfloat (+ x* (* (- sig 2) (expt 2 (- sig 1))) 1))]))
    (define val* (float (if (negative? x) (bf- val) val) es nbits))
    (set-mpfr-exp2 emin emax)
    val*))

(define (float->ordinal x)
  (define sig (- (float-nbits x) (float-es x)))
  (parameterize ([bf-precision sig])
    (define-values (emin emax) (set-mpfr-exp (float-es x) (float-nbits x)))
    (define x* (bfabs (float-val x)))
    (define enorm (- 2 (mpfr_get_emax)))
    (define ex (+ (bigfloat-exponent x*) (- sig 1)))
    (define val
      (cond
       [(bfnan? x*)
        (+ (* (expt 2 (- sig 1)) (- (expt 2 (float-es x)) 1)) 1)]
       [(bf= x* +inf.bf)
        (* (expt 2 (- sig 1)) (- (expt 2 (float-es x)) 1))]
       [(bfzero? x*) 0]
       [(< ex enorm)
        (define unit (ordinal->bigfloat 1))
        (define exp-offset (+ 1 (- ex (mpfr_get_emin))))
        (define base (bfshift unit exp-offset))
        (define accum (expt 2 exp-offset))
        (define diff (bf- x* base))
        (+ accum (bigfloat->real (bfround (bf/ diff unit))))]
       [else
        (- (bigfloat->ordinal x*) (* (- sig 2) (expt 2 (- sig 1))) 1)]))
    (define val* (if (bfnegative? (float-val x)) (- val) val))
    (set-mpfr-exp2 emin emax)
    val*))

;;;;;;;;;;;;; Predicates ;;;;;;;;;;;;;;;;;;

(define (float-infinite? x)
  (bfinfinite? (float-val x)))

(define (float-nan? x)
  (bfnan? (float-val x)))

;;;;;;;;;;;;;; Operators ;;;;;;;;;;;;;;;;;;

(define-syntax-rule (float-1ary-fun name mpfr-fun)
  (define (name es nbits)
    (λ (x)
      (parameterize ([bf-precision (- nbits es)])
        (define-values (emin emax) (set-mpfr-exp es nbits))
        (define r (mpfr-fun (float-val x)))
        (set-mpfr-exp2 emin emax)
        (float r es nbits)))))

(define-syntax-rule (float-1ary-funs [name mpfr-fun] ...)
  (begin (float-1ary-fun name mpfr-fun) ...))

(float-1ary-funs
 [float-sqrt mpfr-sqrt]
 [float-cbrt mpfr-cbrt]
 [float-neg mpfr-neg]
 [float-abs mpfr-abs]
 [float-log mpfr-log]
 [float-log2 mpfr-log2]
 [float-log10 mpfr-log10]
 [float-log1p mpfr-log1p]
 [float-exp mpfr-exp]
 [float-exp2 mpfr-exp2]
 [float-exp10 mpfr-exp10]
 [float-expm1 mpfr-expm1]
 [float-cos mpfr-cos]
 [float-sin mpfr-sin]
 [float-tan mpfr-tan]
 [float-sec mpfr-sec]
 [float-csc mpfr-csc]
 [float-cot mpfr-cot]
 [float-acos mpfr-acos]
 [float-asin mpfr-asin]
 [float-atan mpfr-atan]
 [float-cosh mpfr-cosh]
 [float-sinh mpfr-sinh]
 [float-tanh mpfr-tanh]
 [float-sech mpfr-sech]
 [float-csch mpfr-csch]
 [float-coth mpfr-coth]
 [float-acosh mpfr-acosh]
 [float-asinh mpfr-asinh]
 [float-atanh mpfr-atanh])

(define-syntax-rule (float-2ary-fun name mpfr-fun)
  (define (name es nbits)
    (λ (x y)
      (parameterize ([bf-precision (- nbits es)])
        (define-values (emin emax) (set-mpfr-exp es nbits))
        (define r (mpfr-fun (float-val x) (float-val y)))
        (set-mpfr-exp2 emin emax)
        (float r es nbits)))))

(define-syntax-rule (float-2ary-funs [name mpfr-fun] ...)
  (begin (float-2ary-fun name mpfr-fun) ...))

(float-2ary-funs
 [float-add       mpfr-add]
 [float-sub       mpfr-sub]
 [float-mul       mpfr-mul]
 [float-div       mpfr-div]
 [float-pow       mpfr-pow]
 [float-atan2     mpfr-atan2])

(define (float-fma es nbits)
  (λ (x y z)
    (parameterize ([bf-precision (- nbits es)])
      (define-values (emin emax) (set-mpfr-exp es nbits))
      (define r (mpfr-fma (float-val x) (float-val y) (float-val z)))
      (set-mpfr-exp2 emin emax)
      (float r es nbits))))

(define-syntax-rule (float-comparator name bf-cmp)
  (define (name x y)
    (bf-cmp (float-val x) (float-val y))))

(define-syntax-rule (float-comparators [name bf-cmp] ...)
  (begin (float-comparator name bf-cmp) ...))

(float-comparators
 [float-= bf=]
 [float-> bf>]
 [float-< bf<]
 [float->= bf>=]
 [float-<= bf<=])

(module+ test
  (require rackunit)
  (define es 8)
  (define nbits 32)

  (define rs (append (for/list ([i (in-range 100)]) (random)) (list +inf.0 -inf.0 +nan.0)))
  (define re (append (for/list ([i (in-range 100)]) (- (random 0 632) 324)) (list 1 1 1)))
  (define rd (map (λ (s e) (* s (expt 10 e))) rs re))
  (define rf (map (curryr real->float es nbits) rd))
  (for ([d rd] [f rf])
    (check-equal? (real->float (float->real f) es nbits) f)
    (check-equal? (bf->float (float->bf f) es nbits) f)
    (check-equal? (ordinal->float (float->ordinal f) es nbits) f)))