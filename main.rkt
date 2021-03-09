#lang racket

(require (for-syntax syntax/parse
                     "core.rkt"))

(begin-for-syntax
  (define (typeof stx)
    (eval (syntax-property stx 'type)))

  (define U (syntax-property #''U
                             'type
                             'U)))

(define-for-syntax Nat (syntax-property #''Nat
                                        'type U))
(define-for-syntax zero (syntax-property #''zero
                                         'type Nat))
(define-syntax (zero stx)
  (syntax-parse stx
    [x zero]))
(define-for-syntax (suc n) (syntax-property #`'(suc #,n)
                                            'type Nat))
(define-syntax (suc stx)
  (syntax-parse stx
    [(_ n)
     (let ([new-n (local-expand #'n 'expression null)])
       (unify (eval Nat) (typeof new-n)
              stx #'n)
       (suc (eval new-n)))]))

zero
(suc zero)
