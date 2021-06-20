#lang sicp
(#%require racket)
(#%require sicp-pict)

(define (flip-horiz painter)
  (transform-painter
   painter
   (make-vect 1.0 0.0)
   (make-vect 0.0 0.0)
   (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter
   painter
   (make-vect 1.0 1.0)
   (make-vect 0.0 1.0)
   (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter
   painter
   (make-vect 0.0 1.0)
   (make-vect 0.0 0.0)
   (make-vect 1.0 1.0)))

(define (save image-snip)
  (send (send image-snip get-bitmap) save-file "test.png" 'png))
