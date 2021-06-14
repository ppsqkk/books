#lang sicp

(define (make-interval a b) (cons a b))
(define (lower-bound c) (car c))
(define (upper-bound c) (cdr c))
