#lang sicp

(define (sum term a inc b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (inc a) inc b))))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k)
    (f (+ a (* k h))))
  (define (inc x)
    (+ x 1))
  (define (term x)
    (* (y x)
       (cond ((or (= x 0) (= x n)) 1)
             ((odd? x) 4)
             (else 2))))
  (* (/ h 3)
     (sum term 0 inc n)))
