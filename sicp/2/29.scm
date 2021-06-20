#lang sicp

; (define (make-mobile left right)
;   (list left right))

; (define (make-branch length structure)
;   (list length structure))

; a
; (define (left-branch m) (car m))
; (define (right-branch m) (cadr m))
; (define mobile? pair?)

; (define (branch-length b) (car b))
; (define (branch-structure b) (cadr b))

; b
(define (total-weight m)
  (if (not (mobile? m))
      m
      (+ (total-weight (branch-structure (left-branch m)))
         (total-weight (branch-structure (right-branch m))))))

; c
(define (torque b) (* (branch-length b)
                      (total-weight (branch-structure b))))
(define (balanced? m)
  (if (not (mobile? m))
      true
      (and (= (torque (left-branch m)) (torque (right-branch m)))
           (balanced? (branch-structure (left-branch m)))
           (balanced? (branch-structure (right-branch m))))))

; d
(define (make-mobile left right) (cons left right))
(define (make-branch length structure)
  (cons length structure))

(define (left-branch m) (car m))
(define (right-branch m) (cdr m))
(define mobile? pair?)

(define (branch-length b) (car b))
(define (branch-structure b) (cdr b))
