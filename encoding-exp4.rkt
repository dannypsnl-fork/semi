#lang racket

(require syntax/parse/define)

(define (-> . any)
  `(-> ,@any))

(begin-for-syntax
  (define-syntax-class data-clause
    #:datum-literals (:)
    (pattern (name:id : typ)
             #:attr val
             (syntax-parse #'typ
               #:datum-literals (->)
               [(-> param* ... ret)
                (with-syntax ([(id* ...)
                               (generate-temporaries #'(param* ...))])
                  #'(λ (id* ...) `(name ,id* ...)))]
               [ty #''name])
             #:attr def
             (with-syntax ([(tmp) (generate-temporaries (list #'name))])
               #'(begin
                   ; generate to draw usage arrow of type
                   (define tmp typ)
                   ; generate definitions
                   (define name val)
                   (define-for-syntax name val))))))

(define-syntax-parser data
  #:datum-literals (:)
  [(_ name:id clause*:data-clause ...)
   #'(begin
       (define name 'name)
       (define-for-syntax name 'name)

       clause*.def ...)]

  [(_ (name:id [free*:id : typ*] ...)
      clause*:data-clause ...)
   (with-syntax ([(id* ...) (generate-temporaries #'(free* ...))])
     #'(begin
         (define (name id* ...) `(name ,id* ...))
         (define-for-syntax (name id* ...) `(name ,id* ...))

         ; generate to draw usage arrow of freevar type
         (define id* typ*) ...
         ; generate freevar for reference
         ; NOTE: this trick make different data type in the same module cannot reuse the same freevar name
         (define free* id*) ...

         clause*.def ...))])

(define Type 'Type)

(data (× [L : Type] [R : Type])
      [cons : (L R . -> . (× L R))])

(data Nat
      [zero : Nat]
      [suc : (Nat . -> . Nat)])

(data (Vec [A : Type] [N : Nat])
      [vecnil : (Vec A N)]
      [vec:: : (A (Vec A N) . -> . (Vec A (suc N)))])

Nat
zero
(suc (suc zero))

(cons zero zero)

vecnil
(vec:: (suc zero) (vec:: zero vecnil))
