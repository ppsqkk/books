#lang sicp

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
     (if (null? bits)
         '()
         (let ((next-branch
                (choose-branch (car bits) current-branch)))
           (if (leaf? next-branch)
               (cons (symbol-leaf next-branch)
                     (decode-1 (cdr bits) tree))
               (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
(define (encode-symbol symbol tree)
  (define (element? x sequence)
    (cond ((null? sequence) false)
          ((eq? (car sequence) x) true)
          (else (element? x (cdr sequence)))))
  (define (help symbol tree)
    (cond ((leaf? tree) '())
          ((element? symbol (symbols (left-branch tree)))
           (cons 0 (help symbol (left-branch tree))))
          (else (cons 1 (help symbol (right-branch tree))))))
  (if (element? symbol (symbols tree))
      (help symbol tree)
      (error "symbol not in tree: ENCODE" symbol)))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(define (successive-merge set)
  (if (null? (cdr set))
      (car set)
      (successive-merge (adjoin-set
                         (make-code-tree (car set) (cadr set))
                         (cddr set)))))

(define tree
  (generate-huffman-tree
   '((A 2) (GET 2) (SHA 3) (WAH 1) (BOOM 1) (JOB 2) (NA 16) (YIP 9))))
(define message
  '(GET A JOB
    SHA NA NA NA NA NA NA NA NA
    GET A JOB
    SHA NA NA NA NA NA NA NA NA
    WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
    SHA BOOM))

(display (encode message tree))
(display (length (encode message tree)))
; If we used a fixed-length code, we would need 3 bits per symbol.
; (length message) is 36, and 36 * 3 = 108, so we would need 108 bits in total.
