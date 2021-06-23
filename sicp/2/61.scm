#lang sicp

(define (adjoin-set x set)
  (if (null? set)
      (cons x set)
      (let ((c (car set)))
        (cond ((= c x) set)
              ((< c x) (cons c (adjoin-set x (cdr set))))
              (else (cons x set))))))
