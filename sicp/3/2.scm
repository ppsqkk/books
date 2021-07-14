#lang sicp

(define (make-monitored f)
  (let ((calls 0))
    (define (mf m)
      (cond ((eq? m 'how-many-calls?) calls)
            ((eq? m 'reset-count) (set! calls 0))
            (else (set! calls (+ calls 1)) (f m))))
    mf))
