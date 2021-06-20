#lang sicp

(define (deep-reverse items)
  (define (iter items result)
    (cond ((null? items) result)
          ((pair? (car items))
           (iter (cdr items)
                 (cons (deep-reverse (car items)) result)))
          (else (iter (cdr items) (cons (car items) result)))))
  (iter items nil))
