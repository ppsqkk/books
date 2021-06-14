#lang sicp

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car c)
  (define (iter c result)
    (if (not (= (remainder c 2) 0))
        result
        (iter (/ c 2) (+ result 1))))
  (iter c 0))

(define (cdr c)
  (define (iter c result)
    (if (not (= (remainder c 3) 0))
        result
        (iter (/ c 3) (+ result 1))))
  (iter c 0))
