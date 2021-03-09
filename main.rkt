#lang racket

(require (for-syntax "core.rkt")
         syntax/parse/define)

(begin-for-syntax
  (define (typeof stx)
    (eval (syntax-property stx 'type)))

  (define U (syntax-property #''U
                             'type 'U))

  (define-syntax-class data-clause
    #:datum-literals (:)
    (pattern (name:id : typ)
             #:attr def-for-syn
             #'(define-for-syntax name (syntax-property #''name
                                                        'type typ))
             #:attr def-syn
             #'(define-syntax (name stx)
                 (syntax-parse stx
                   [x name])))

    (pattern (name:id [p*:id : p-ty*] ... : typ)
             #:attr def-for-syn
             #'(define-for-syntax (name p* ...) (syntax-property #`'(name #,p* ...)
                                                                 'type typ))
             #:attr def-syn
             #'(define-syntax (name stx)
                 (syntax-parse stx
                   [(_ p* ...)
                    (with-syntax ([new-p* (generate-temporaries #'(p* ...))])
                      (let ([new-p* (local-expand #'p* 'expression null)]
                            ...)
                        (unify (eval p-ty*) (typeof new-p*)
                               stx #'p*)
                        ...
                        (name (eval new-p*))))])))))

(define-syntax-parser data
  [(_ name:id
      c*:data-clause ...)
   #'(begin
       (define-for-syntax name (syntax-property #''name
                                                'type U))
       c*.def-for-syn ...
       c*.def-syn ...
       )])

(data Bool
      [true : Bool]
      [false : Bool])

true
false

(data Nat
      [zero : Nat]
      [suc [n : Nat] : Nat])

zero
(suc (suc zero))
