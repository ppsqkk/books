#lang sicp
(#%require racket)
(#%require sicp-pict)

; Redefinitions for consistency
(define xcor-vect vector-xcor)
(define ycor-vect vector-ycor)
(define start-segment segment-start)
(define end-segment segment-end)

(define (rotate sequence) (append (cdr sequence) (list (car sequence))))

; a
(define corners
  (list (make-vect 0 0)
        (make-vect 1 0)
        (make-vect 1 1)
        (make-vect 0 1)))
(define edges
  (map make-segment corners (rotate corners)))
(define outline (segments->painter edges))

; b
(define x
  (segments->painter (list (make-segment (make-vect 0 0) (make-vect 1 1))
                           (make-segment (make-vect 1 0) (make-vect 0 1)))))

; c
(define (average x y) (/ (+ x y) 2))
(define (midpoint-segment s)
  (let ((start (start-segment s))
        (end (end-segment s)))
    (make-vect (average (xcor-vect start) (xcor-vect end))
               (average (ycor-vect start) (ycor-vect end)))))
(define diamond
  (let ((midpoints (map midpoint-segment edges)))
    (segments->painter (map make-segment midpoints (rotate midpoints)))))

; d
; I don't want to do this.

(define (save image-snip)
  (send (send image-snip get-bitmap) save-file "test.png" 'png))
