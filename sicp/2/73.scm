#lang sicp
(#%require racket/base)

; a
; We use the operator as the tag of an expression, but numbers and variables
; don't have operators.

; b
; There are a few tricky parts to this problem.

; 1
; In the process of finding the derivative of a product, we must create a
; product. We may choose to use the internal make-product and then tag the
; result (this is the strategy that Ben and Alyssa use to implement their
; complex number constructors). However, in the process of finding the
; derivative of a product, we must also create a sum. Within the product
; package, however, we have no idea how to make or tag a sum, so we cannot use
; the same strategy. Therefore, in order to preserve a bare minimum of
; separation between the packages, in addition to exporting a derivative
; procedure, we must also export a procedure that creates the particular
; expression type. For example, the product package must export a procedure that
; can construct a product.

; 2
; We must export constructors, but these constructors will not always return
; tagged data. For example, ((get 'make '+) 1 0) should return a bare 1, not '(+
; 1 0). However, the definitions of internal constructors should not include
; this special-case code, nor any code related to the external representation.
; For example, in Section 2.3.2, make-sum is first defined as (lambda (a1 a2)
; (list '+ a1 a2)). But in our sum package, '+ is the tag; in other words, it is
; a part of the external representation. Therefore, it has no part in our
; internal constructor. Similarly, the second definition of make-sum in Section
; 2.3.2 includes special case code stipulating that, for example, (make-sum 1 0)
; should return 1. Again, this special case code should not be included in the
; internal constructor.

; 3
; We must recursively call the generic procedure deriv whenever we take the
; derivative of any type of expression. This restricts our naming of the
; internal derivative procedure within each package; namely, we cannot use the
; name deriv, because then we would shadow the generic procedure. This would be
; easily remedied if we had a way to "assimilate the predicates number? and
; variable? into the data-directed dispatch".

; put and get
(define ht (make-hash))
(define (put op type item) (hash-set! ht (list op type) item))
(define (get op type) (hash-ref ht (list op type) false))

; tags
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))
(define operator type-tag)
(define operands contents)

; generally useful procedures
(define (=number? exp num) (and (number? exp) (= exp num)))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (install-sum-package)
  ; internal
  (define (make-sum a1 a2) (list a1 a2))
  (define addend car)
  (define augend cadr)
  (define (deriv-sum s var)
    ((get 'make '+) (deriv (addend s) var)
                    (deriv (augend s) var)))

  ; external
  (define (tag x) (attach-tag '+ x))
  (put 'make '+
       (lambda (a1 a2)
         (cond ((=number? a1 0) a2)
               ((=number? a2 0) a1)
               ((and (number? a1) (number? a2))
                (+ a1 a2))
               (else (tag (make-sum a1 a2))))))
  (put 'deriv '+ deriv-sum)
  'done)

(define (install-product-package)
  ; internal
  (define (make-product m1 m2) (list m1 m2))
  (define multiplier car)
  (define multiplicand cadr)
  (define (deriv-product p var)
    ((get 'make '+)
     ((get 'make '*) (multiplier p)
                     (deriv (multiplicand p) var))
     ((get 'make '*) (deriv (multiplier p) var)
                     (multiplicand p))))

  ; external
  (define (tag x) (attach-tag '* x))
  (put 'make '*
       (lambda (m1 m2)
         (cond ((or (=number? m1 0) (=number? m2 0)) 0)
               ((=number? m1 1) m2)
               ((=number? m2 1) m1)
               ((and (number? m1) (number? m2)) (* m1 m2))
               (else (tag (make-product m1 m2))))))
  (put 'deriv '* deriv-product)
  'done)

; c
(define (install-exponentiation-package)
  ; internal
  (define (make-exponentiation b e) (list b e))
  (define base car)
  (define exponent cadr)
  (define (deriv-exponentiation e var)
    ((get 'make '*)
     (exponent e)
     ((get 'make '*)
      ((get 'make '**) (base e) ((get 'make '+) (exponent e) (- 1)))
      (deriv (base e) var))))

  ; external
  (define (tag x) (attach-tag '** x))
  (put 'make '**
       (lambda (b e)
         (cond ((=number? e 0) 1)
               ((=number? e 1) b)
               ((and (number? b) (number? e))
                (expt b e))
               (else (tag (make-exponentiation b e))))))
 (put 'deriv '** deriv-exponentiation)
 'done)

(install-sum-package)
(install-product-package)
(install-exponentiation-package)

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))

; d
; All calls to put would have to swap their first 2 arguments.
