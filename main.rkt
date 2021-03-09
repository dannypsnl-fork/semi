#lang racket

(require (for-syntax syntax/parse
                     "core.rkt"))

(begin-for-syntax
  (define (typeof stx)
    (eval (syntax-property stx 'type)))

  (define U (syntax-property #''U
                             'type
                             'U))

  (define Nat (syntax-property #''Nat
                               'type
                               U))

  (define zero (syntax-property #''zero
                                'type
                                Nat))

  (define (suc n) (syntax-property #`'(suc #,n)
                                   'type
                                   Nat)))

(define-syntax (zero stx)
  (syntax-parse stx
    [x zero]))

zero

(define-syntax (suc stx)
  (syntax-parse stx
    [(_ n)
     (define new-n (local-expand #'n 'expression null))
     (unify (eval Nat) (typeof new-n)
            stx #'n)
     (suc (eval new-n))]))

(suc zero)
