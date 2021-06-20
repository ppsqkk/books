#lang sicp

(define (square x) (* x x))

; (define (square-tree tree)
;   (cond ((null? tree) nil)
;         ((not (pair? tree)) (square tree))
;         (else (cons (square-tree (car tree))
;                     (square-tree (cdr tree))))))

(define (square-tree tree)
  (map (lambda (x)
         (if (pair? x)
             (square-tree x)
             (square x)))
       tree))
