#lang sicp

; phi is the solution to the equation x^2 = x + 1, which is equivalent to
; x = 1 / x + 1. Solving this equation is equivalent to finding the fixed
; point of f(x) = 1 / x + 1.

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point (lambda (x) (+ (/ 1 x) 1)) 1.0)
