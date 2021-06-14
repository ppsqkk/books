#lang sicp

(define (make-interval a b) (cons a b))
(define (lower-bound c) (car c))
(define (upper-bound c) (cdr c))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

; (define (sub-interval x y)
;   (add-interval x
;                 (make-interval (* (- 1) (upper-bound y))
;                                (* (- 1) (lower-bound y)))))
