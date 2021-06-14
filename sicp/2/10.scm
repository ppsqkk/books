#lang sicp

(define (make-interval a b) (cons a b))
(define (lower-bound c) (car c))
(define (upper-bound c) (cdr c))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (define (between? a b c)
    (and (not (> a c)) (not (< a b))))
  (if (between? 0 (lower-bound y) (upper-bound y))
      (error "Divisor could be zero")
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))
