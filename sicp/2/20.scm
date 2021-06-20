#lang sicp

(define (same-parity x . items)
  (define (parity x) (remainder x 2))
  (define (help items)
    (cond ((null? items) nil)
          ((= (parity x) (parity (car items)))
           (cons (car items) (help (cdr items))))
          (else (help (cdr items)))))
  (cons x (help items)))
