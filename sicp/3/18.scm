#lang sicp

(define (has-cycle? x)
  ; A set would be faster here
  (let ((seen '()))
    (define (f x)
      (cond ((not (pair? x)) false)
            ((memq x seen) true)
            (else (set! seen (cons x seen))
                  (f (cdr x)))))
    (f x)))
