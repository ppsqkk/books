#lang sicp
(#%require racket)
(#%require sicp-pict)

; Redefinitions for consistency
(define xcor-vect vector-xcor)
(define ycor-vect vector-ycor)
(define start-segment segment-start)
(define end-segment segment-end)

(define (rotate sequence) (append (cdr sequence) (list (car sequence))))

(define corners
  (list (make-vect 0 0)
        (make-vect 1 0)
        (make-vect 1 1)
        (make-vect 0 1)))
(define edges (map make-segment corners (rotate corners)))

(define (average x y) (/ (+ x y) 2))
(define (midpoint-segment s)
  (let ((start (start-segment s))
        (end (end-segment s)))
    (make-vect (average (xcor-vect start) (xcor-vect end))
               (average (ycor-vect start) (ycor-vect end)))))

(define midpoints
  (map midpoint-segment edges))
(define segment-list
  (map make-segment midpoints (rotate midpoints)))
(define diamond (segments->painter segment-list))

; a
(define segment-list2
  (map make-segment midpoints (rotate (rotate midpoints))))
(define diamond2 (segments->painter (append segment-list segment-list2)))

; b
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))
(define (corner-split painter n)
  (if (= n 0)
      painter
      (beside (below painter (up-split painter (- n 1)))
              (below (right-split painter (- n 1))
                     (corner-split painter (- n 1))))))

; c
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))
(define (square-limit painter n)
  (let ((combine4 (square-of-four identity flip-horiz
                                  flip-vert rotate180)))
    (combine4 (corner-split painter n))))

(define (save image-snip)
  (send (send image-snip get-bitmap) save-file "test.png" 'png))
