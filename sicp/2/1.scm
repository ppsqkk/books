#lang sicp

(define (make-rat n d)
  (let ((sign (if (or (and (> n 0) (> d 0))
                      (and (< n 0) (< d 0)))
                  1
                  -1))
        (an (abs n))
        (ad (abs d)))
    (let ((g (gcd an ad)))
      (cons (* sign (/ an g)) (/ ad g)))))
