#lang sicp

; Recursive
; (define (f n)
;   (if (< n 3)
;       n
;       (+ (f (- n 1))
;          (* 2 (f (- n 2)))
;          (* 3 (f (- n 3))))))

; Iterative
(define (f n)
  (define (iter x y z n)
    (if (= n 0)
        x
        (iter y
              z
              (+ z
                 (* 2 y)
                 (* 3 x))
              (- n 1))))
   (if (< n 3) n (iter 0 1 2 n)))
