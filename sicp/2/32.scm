#lang sicp

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

; All subsets of a set S is equivalent to the combination of A and B, where A is
; all subsets of S excluding its first element, and B is all subsets of S
; including its first element. But adding the excluded element back to each
; subset in A produces B. This is what the code implements.
