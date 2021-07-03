#lang sicp
(#%require racket/base)

(define (square x) (* x x))
(define (repeated f n)
  (lambda (x) (if (= n 0) x ((repeated f (- n 1)) (f x)))))

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

; Returns x if x cannot be raised
(define (raise x)
  (let ((r (get 'raise (list (type-tag x)))))
    (if r (r (contents x)) x)))

(define (depth x)
  (let ((raised (raise x)))
    (if (eq? (type-tag x) (type-tag raised))
        0
        (+ 1 (depth raised)))))

; Returnx x if x cannot be projected
; This pattern should be codified in a higher-order procedure
(define (project x)
  (let ((p (get 'project (list (type-tag x)))))
    (if p (p (contents x)) x)))

(define (drop x)
  (define (can-drop? x)
    (let ((p (project x)))
      (if (eq? (type-tag x) (type-tag p))
          #f
          (let ((r ((repeated raise (- (depth p) (depth x))) p)))
            (equ? r x)))))
  (if (can-drop? x)
      (drop (project x))
      x))

; Drawbacks:
; Assumes that all types in the system are part of a single, non-branching
; hierarchy.
; Only raises arguments to the "lowest" common type; i.e. does the least amount
; of work possible.
; Will not try mixed-type operations, even if they are installed in the table.
; See Exercise 2.82.
;
; Generic procedures which return untagged data cannot use apply-generic, which
; uses drop. Thus, equ? and all complex selectors must use ag. We could
; alternatively check for tagged data with pair?, but that would make procedures
; returning lists ambiguous.
(define (ag op . args)
  (define (smallest sequence)
    (cond ((null? sequence) (error "Null sequence: SMALLEST"))
          ((null? (cdr sequence)) (car sequence))
          (else (let ((s (smallest (cdr sequence))))
                  (if (< (car sequence) s)
                      (car sequence)
                      s)))))
  (define (help raised-args)
    (let ((type-tags (map type-tag raised-args)))
      (let ((proc (get op type-tags)))
        (if proc
            (apply proc (map contents raised-args))
            (if (= (depth (car raised-args)) 0)
                (error "No method for these types -- APPLY-GENERIC"
                       (list op (map type-tag args)))
                (help (map raise raised-args)))))))
  (let ((depths (map depth args)))
    (let ((s (smallest depths)))
      (let ((raised-args
             (map (lambda (a d)
                    ((repeated raise (- d s)) a))
                  args
                  depths)))
        (help raised-args)))))
(define (apply-generic op . args)
  (drop (apply ag (cons op args))))

(define (install-integer-package)
  ; internal
  (define (raise x)
    ((get 'make 'rational) x 1))

  ; external
  (define (tag x)
    (attach-tag 'integer x))
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (tag (quotient x y))))
  (put 'equ? '(integer integer) =)
  (put 'raise '(integer) raise)
  (put 'make 'integer
       (lambda (x) (tag (truncate x))))
  'done)

(define (make-integer n)
  ((get 'make 'integer) n))

(define (install-rational-package)
  ; internal
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ?-rat x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))
  (define (raise-rat x)
    ((get 'make 'real) (/ (* (numer x) 1.0) (denom x))))
  (define (project-rat x)
    ((get 'make 'integer) (truncate (/ (numer x) (denom x)))))

  ; external
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational) equ?-rat)
  (put 'raise '(rational) raise-rat)
  (put 'project '(rational) project-rat)

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-real-package)
  ; internal
  (define (raise x)
    ((get 'make-from-real-imag 'complex) x 0))
  (define (project x)
    ((get 'make 'integer) (truncate x)))

  ; external
  (define (tag x)
    (attach-tag 'real x))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'sqroot '(real)
       (lambda (x) (tag (sqrt x))))
  (put 'equ? '(real real) =)
  (put 'raise '(real) raise)
  (put 'project '(real) project)
  (put 'make 'real
       (lambda (x) (tag (* 1.0 x))))
  'done)

(define (make-real n)
  ((get 'make 'real) n))

(define (install-rectangular-package)
  ; internal
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ; external
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ; internal
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  ; external
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (real-part z) (ag 'real-part z))
(define (imag-part z) (ag 'imag-part z))
(define (magnitude z) (ag 'magnitude z))
(define (angle z) (ag 'angle z))

(define (install-complex-package)
  ; imported
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ; internal
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (equ?-complex z1 z2)
    (and (= (real-part z1) (real-part z2))
         (= (imag-part z1) (imag-part z2))))
  (define (project-complex z)
    ((get 'make 'real) (real-part z)))

  ; external
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'equ? '(complex complex) equ?-complex)
  (put 'project '(complex) project-complex)
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (sqroot x) (apply-generic 'sqroot x))
(define (equ? x y) (ag 'equ? x y))

(install-integer-package)
(install-rational-package)
(install-real-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
