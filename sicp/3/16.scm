#lang sicp

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

; 3
(define w '(a b c))

; 4
(define x
  (let ((p '((a) b)))
    (set-cdr! (car p) (cdr p))
    p))

; 7
(define y
  (let ((p (cons 'a 'b)))
    (let ((q (cons p p)))
      (cons q q))))

; Never returns
(define z
  (let ((p '(a b c)))
    (set-cdr! (cddr p) p)
    p))
