#lang sicp

; (define (product term a next b)
;   (if (> a b)
;       1
;       (* (term a) (product term (next a) next b))))

 (define (product term a next b)
   (define (iter a result)
     (if (> a b)
         result
         (iter (next a) (* (term a) result))))
   (iter a 1))

(define (factorial n)
  (define (fact-term x) x)
  (define (fact-next x) (+ x 1))
  (product fact-term 1 fact-next n))

(define (pi-approx n)
  (define (pi-term x)
    (define y (+ (* (/ (if (odd? x) (+ x 1) x) 2) 2) 1))
    (/ (+ y (if (odd? x) (- 1) 1)) y))
  (define (pi-next x)
    (+ x 1))
  (* 4 (product pi-term 1.0 pi-next n)))
