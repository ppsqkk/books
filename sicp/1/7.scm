#lang sicp

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (square x)
  (* x x))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

; (define (good-enough? guess x)
;   (< (abs (- (square guess) x)) 0.001))

; (sqrt (square 3e-2)) returns 0.04 instead of 0.03 because we use a fixed
; tolerance of 0.001.
; (sqrt (square (+ 1e+20 1))) returns 1e+20 instead of 1e+20 + 1.

(define (good-enough? guess x)
  (< (/ (abs (- guess (improve guess x)))
        guess)
     0.001))

; (sqrt (square 3e-2)) returns 0.03, as expected.
; (sqrt (square (+ 1e+20 1))) seems closer to 1e+20 + 1.
