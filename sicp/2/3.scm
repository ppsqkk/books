#lang sicp

(define (average x y) (/ (+ x y) 2))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment start end) (cons start end))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (midpoint-segment s)
  (let ((start (start-segment s))
        (end (end-segment s)))
    (make-point (average (x-point start) (x-point end))
                (average (y-point start) (y-point end)))))

; (define (make-rect tl br) (cons tl br))
; (define (tl r) (car r))
; (define (br r) (cdr r))

; (define (length r)
;   (- (x-point (br r)) (x-point (tl r))))
; (define (height r)
;   (- (y-point (tl r)) (y-point (br r))))

(define (make-rect tl length height) (cons tl (cons length height)))
(define (length r) (car (cdr r)))
(define (height r) (cdr (cdr r)))

(define (area r)
  (* (length r) (height r)))
(define (perimeter r)
  (* 2 (+ (length r) (height r))))
