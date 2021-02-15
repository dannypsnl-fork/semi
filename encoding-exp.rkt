#lang racket

(require syntax/parse/define
         (for-syntax "core.rkt"))

(begin-for-syntax
  (require racket/string
           racket/match)

  (struct tt (val typ)
    #:methods gen:custom-write
    [(define (write-proc tt port mode)
       (fprintf port "~a" (tt-val tt)))]
    #:transparent)

  (define (freevar? sym)
    (and (symbol? sym)
         (string-prefix? (symbol->string sym) "?")))

  (define (typeof t)
    (match t
      ['Type 'Type1]
      [(? tt?) (tt-typ t)]
      [_ (error 'unknown "what? `~a`" t)])))

(define-for-syntax Nat (tt 'Nat 'Type))
(define-for-syntax zero (tt 'zero Nat))
(define zero 'zero)
(define-for-syntax (suc n)
  (unify Nat (typeof n)
         n n)
  (tt `(suc ,n) Nat))
(define (suc n)
  `(suc ,n))

(define-for-syntax Bool (tt 'Bool 'Type))
(define Bool 'Bool)
(define-for-syntax true (tt 'true Bool))
(define true 'true)
(define-for-syntax false (tt 'false Bool))
(define false 'false)

(define-syntax-parser define-
  #:datum-literals (:)
  [(_ name:id : ty exp:expr)
   (define unified-ty
     (unify (eval #'ty) (typeof (eval #'exp))
            this-syntax
            #'name))
   #`(begin
       (define-for-syntax name (tt exp #,unified-ty))
       (define name exp))])

zero
(define- a : Nat
  (suc (suc zero)))
a

; error case: `true` is not `Nat`, however, this experiment shows this approach cannot provide where error occurred. Thus, the experiment was failed. 
#;(define- b : Nat
  (suc true))
