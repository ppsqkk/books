#lang sicp
(#%require racket)
(#%require sicp-pict)

(define (split f g)
  (lambda (p n)
    (if (= n 0)
        p
        (let ((q ((split f g) p (- n 1))))
          (f p (g q q))))))

(define (save image-snip)
  (send (send image-snip get-bitmap) save-file "test.png" 'png))
