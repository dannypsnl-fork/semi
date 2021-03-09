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
             (with-syntax ([(new-p* ...) (generate-temporaries #'(p* ...))])
               #'(define-syntax (name stx)
                   (syntax-parse stx
                     [(_ p* ...)
                      (let ([new-p* (local-expand #'p* 'expression null)]
                            ...)
                        (define subst (make-subst))
                        (unify (eval p-ty*) (typeof new-p*)
                               stx #'p*
                               #:subst subst)
                        ...
                        (name (eval new-p*) ...))]))))))

(define-syntax-parser data
  [(_ name:id
      c*:data-clause ...)
   #'(begin
       (define-for-syntax name (syntax-property #''name
                                                'type U))
       (define-syntax (name stx)
         (syntax-parse stx
           [x name]))
       c*.def-for-syn ...
       c*.def-syn ...
       )]
  [(_ (name:id [d*:id : d-ty*] ...)
      c*:data-clause ...)
   #'(begin
       (define-for-syntax d* (syntax-property #''?freevar
                                              'type d-ty*))
       ...
       (define-for-syntax (name d* ...)
         (define subst (make-subst))
         (unify (eval d-ty*) (typeof d*)
                #'this-syntax #'c*
                #:subst subst)
         ...
         (syntax-property #`'(name d* ...)
                          'type U))
       c*.def-for-syn ...
       c*.def-syn ...)])

(data Zero)
(data One
      [one : One])
(data Bool
      [true : Bool]
      [false : Bool])
(data Nat
      [zero : Nat]
      [suc [n : Nat] : Nat])
(data (Vec [A : U] [len : Nat])
      [vecnil : (Vec A zero)]
      [vec:: [a : A] [l : (Vec A len)] : (Vec A (suc len))])

true
false
zero
(suc (suc zero))
vecnil
(vec:: zero vecnil)
(vec:: true (vec:: zero vecnil))
