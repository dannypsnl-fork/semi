#lang racket

(require (for-syntax "core.rkt"
                     "subst.rkt")
         syntax/parse/define)

(begin-for-syntax
  (define (typeof e)
    (cond
      [(freevar? e)
       (eval (freevar-typ e))]
      [(syntax? e)
       (eval (syntax-property e 'type))]
      [else
       (error 'unknown "~a" e)]))

  (define Type 'Type)

  (define-syntax-class data-clause
    #:datum-literals (:)
    (pattern (name:id : typ)
             #:attr def-for-syn
             #'(define-for-syntax name (syntax-property #''name
                                                        'type (eval typ)))
             #:attr def-syn
             #'(define-syntax (name stx)
                 (syntax-parse stx
                   [x name])))

    (pattern (name:id [p*:id : p-ty*] ... : typ)
             #:attr def-for-syn
             #'(define-for-syntax (name p* ...) (syntax-property #`'(name #,p* ...)
                                                                 'type (eval typ)))
             #:attr def-syn
             (with-syntax ([(new-p* ...) (generate-temporaries #'(p* ...))])
               #'(define-syntax (name stx)
                   (syntax-parse stx
                     [(_ p* ...)
                      (let ([new-p* (local-expand #'p* 'expression null)]
                            ...)
                        (define subst (make-subst))
                        (unify (eval p-ty*) (eval (typeof new-p*))
                               stx #'p*
                               #:subst subst
                               #:solve? #f)
                        ...
                        (name (eval new-p*) ...))]))))))

(define-syntax-parser data
  [(_ name:id
      c*:data-clause ...)
   #'(begin
       (define-for-syntax name 'name)
       (define-syntax (name stx)
         (syntax-parse stx
           [x name]))
       c*.def-for-syn ...
       c*.def-syn ...
       )]
  [(_ (name:id [d*:id : d-ty*] ...)
      c*:data-clause ...)
   #'(begin
       (define-for-syntax d* (syntax-property #`#,(freevar 'd* (eval d-ty*))
                                              'type d-ty*))
       ...
       (define-for-syntax (name d* ...)
         (list 'name d* ...))
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
(data (Vec [A : Type] [len : Nat])
      [vecnil : (Vec A zero)]
      [vec:: [a : A] [l : (Vec A len)] : (Vec A (suc len))])

(suc (suc (suc zero)))
vecnil
(vec:: zero vecnil)
(vec:: true (vec:: zero vecnil))
