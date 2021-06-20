#lang sicp

; (define (reverse items)
;   (if (null? items)
;       nil
;       (append (reverse (cdr items))
;               (list (car items)))))

(define (reverse items)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items) (cons (car items) result))))
  (iter items nil))
