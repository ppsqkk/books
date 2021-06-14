#lang sicp

(define (make-interval a b) (cons a b))
(define (lower-bound c) (car c))
(define (upper-bound c) (cdr c))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (make-center-width c (* c (/ p 100))))
(define (percent i)
  (* (/ (width i) (center i)) 100))
