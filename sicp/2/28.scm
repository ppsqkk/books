#lang sicp

(define (fringe tree)
  (cond ((null? tree) nil)
        ((pair? (car tree))
         (append (fringe (car tree)) (fringe (cdr tree))))
        (else (cons (car tree) (fringe (cdr tree))))))
