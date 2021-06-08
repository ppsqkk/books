#lang sicp

; (define (cont-frac n d k)
;   (define (rec x)
;     (if (> x k)
;         0
;         (/ (n x) (+ (d x) (rec (+ x 1))))))
;   (rec 1))

; k must be at least 11 to get an approximation that is accurate to 4 decimal
; places.

(define (cont-frac n d k)
  (define (iter k result)
    (if (= k 0)
        result
        (iter (- k 1) (/ (n k) (+ (d k) result)))))
  (iter k 0))
