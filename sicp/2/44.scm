#lang sicp
(#%require racket)
(#%require sicp-pict)

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (save image-snip)
  (send (send image-snip get-bitmap) save-file "test.png" 'png))
