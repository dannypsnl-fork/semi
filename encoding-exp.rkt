#lang racket

(require syntax/parse/define)

(begin-for-syntax
  (require racket/string)

  (struct tt (val typ)
    #:methods gen:custom-write
    [(define (write-proc tt port mode)
       (fprintf port "~a" (tt-val tt)))]
    #:transparent)

  (define (freevar? sym)
    (and (symbol? sym)
         (string-prefix? (symbol->string sym) "?")))

  (define (typeof t)
    (syntax-property t 'type))

  (define (unify t1 t2 stx)
    (unless (equal? t1 t2)
      (raise-syntax-error
       'type-eq
       (format "mismatching ~a ~a" t1 t2)
       stx))))

(define-for-syntax Nat (tt 'Nat 'Type))
(define-syntax zero
  (syntax-property #''zero 'type Nat))
(define-for-syntax (suc n)
  (unify Nat (typeof n)
         ; FIXME: n is not syntax
         ; cannot reference to target syntax for reporting
         n)
  (syntax-property #'`(suc ,n) 'type Nat))
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
     (unify (eval #'ty) (typeof #'exp)
            #'name))
   #`(begin
       (define-syntax name
         (syntax-property exp 'type #,unified-ty)))])

#;(define- a : Nat zero
  #;(suc (suc zero)))
(begin-for-syntax
  (displayln zero))
