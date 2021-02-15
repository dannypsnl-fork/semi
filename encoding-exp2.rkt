#lang racket

(require syntax/parse/define
         (for-syntax "core.rkt"))

(begin-for-syntax
  (require racket/string)

  (struct the (typ val)
    #:methods gen:custom-write
    [(define (write-proc t port mode)
       (fprintf port "~a" (the-val t)))]
    #:transparent)

  (define (freevar? sym)
    (and (symbol? sym)
         (string-prefix? (symbol->string sym) "?")))

  (define (typeof t)
    (the-typ t))

  (define-syntax-class type
    #:datum-literals (->)
    (pattern name:id)
    (pattern (name:id e*:type ...))
    (pattern (-> param*:type ... ret:type)))

  (define-syntax-class clause
    #:datum-literals (:)
    (pattern (name:id : typ:type)
             #:attr val
             (syntax-parse #'typ
               [(-> param*:type ... ret:type) #'(λ (arg*) `(name ,@arg*))]
               [ty #''name]))))

(define-syntax-parser data
  #:datum-literals (:)
  [(_ name:id c*:clause ...)
   #'(begin
       (define-for-syntax name (the 'Type 'name))
       (define name 'name)

       (define-for-syntax c*.name (the name 'c*.name)) ...
       (define c*.name c*.val) ...)])

(define-syntax-parser define-
  #:datum-literals (:)
  [(_ name:id : ty exp:expr)
   (define unified-ty
       (unify (eval #'ty) (typeof (eval #'exp))
              this-syntax
              #'name))
   #`(begin
       (define-for-syntax name
         (the #,unified-ty exp))
       (define name
         exp))])

(data Bool
      [true : Bool]
      [false : Bool])
(data Nat
      [zero : Nat]
      [suc : (Nat . -> . Nat)])

Bool
Nat

false

zero
(suc zero)
(suc (suc zero))
