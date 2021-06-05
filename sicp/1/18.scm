#lang sicp

(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2))

(define (mul a b)
  (define (iter x a b)
    (cond ((= b 0) x)
          ((even? b) (iter x (double a) (halve b)))
          (else (iter (+ x a) a (- b 1)))))
  (iter 0 a b))
