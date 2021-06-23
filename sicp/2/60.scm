#lang sicp

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define adjoin-set cons)

(define union-set append)

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1 set2))))
        (else (intersection-set (cdr set1) set2))))

; The performances of adjoin-set and union-set are faster by an order of
; magnitude. However, if duplicate elements would be added to the set,
; element-of-set? (and consequentially intersection-set) are slower.

; This representation of sets is useful when there are few duplicate
; elements to be added to the set, and/or the number of calls to adjoin-set and
; union-set is high, and the number of calls to element-of-set? and
; intersection-set is low. In the extreme case, if there are zero duplicate
; elements to be added to the set, this representation is always better.
