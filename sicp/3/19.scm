#lang sicp

(define (has-cycle? x)
  (cond ((not (pair? x)) false)
        ((memq (car x) (cdr x)) true)
        (else (has-cycle? (cdr x)))))
