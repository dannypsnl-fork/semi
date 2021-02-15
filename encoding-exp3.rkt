#lang racket

(module dt racket
  (define (Nat? val)
    (match val
      ['zero #t]
      [`(suc ,n) (Nat? n)]
      [_ #f]))
  (define zero 'zero)
  (define (suc n) `(suc ,n))

  (define (Bool? v)
    (match v
      ['false #t]
      ['true #t]
      [_ #f]))
  (define true 'true)
  (define false 'false)

  (define (List? E?)
    (λ (v)
      (match v
        ['nil #t]
        [`(:: ,e ,@e*)
         (and (E? e)
              (andmap E? e*))]
        [_ #f])))
  (define nil 'nil)
  (define (:: e e*)
    `(:: ,e ,@e*))

  (provide (contract-out
            [zero Nat?]
            [suc (Nat? . -> . Nat?)]

            [true Bool?]
            [false Bool?]

            #:∃ E?
            [nil (List? E?)]
            [:: (E? (List? E?) . -> . (List? E?))])))

(require 'dt)

true
false
zero
(suc zero)
nil
;; unable to semulate free variable
;(:: zero nil)
