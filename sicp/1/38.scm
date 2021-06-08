#lang sicp

(define (cont-frac n d k)
  (define (iter k result)
    (if (= k 0)
        result
        (iter (- k 1) (/ (n k) (+ (d k) result)))))
  (iter k 0))

(define (approx-e k)
  (+ 2
     (cont-frac (lambda (i) 1.0)
                (lambda (i)
                  (let ((x (+ i 1)))
                    (if (= (remainder x 3) 0)
                        (* 2 (/ x 3))
                        1)))
                k)))
