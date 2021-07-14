#lang sicp

(define random-init 1)
(define (rand-update x) (+ x 1))

(define rand
  (let ((x random-init))
    (lambda (m)
      (cond ((eq? m 'generate) (set! x (rand-update x)) x)
            ((eq? m 'reset) (lambda (new-value) (set! x new-value)))))))
