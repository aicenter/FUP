#lang racket

(define (insert xs tree)
  (if (empty? xs)
      tree
      (let* ((k (car xs))
             (v (if (dict-has-key? tree k) (dict-ref tree k) #hash()))
             (newv (insert (cdr xs) v)))
        (dict-set tree k newv))))

(define (split file) (string-split file "/"))

(define (parse files)
  (foldl (lambda (file tree) (insert (split file) tree))
         #hash()
         files))

(define (exists file tree)
  (define (helper names tree)
  (cond [(empty? names) #t]
        [(empty? tree) #f]
        [(dict-has-key? tree (car names))
         (helper (cdr names) (dict-ref tree (car names)))]
        [else #f]))
  (helper (split file) tree))


;;;;;;;;;;;;;;;;;;;;;;

(define files
  (list "src/tree.hs"
        "src/complex.hs"
        "scripts/ex1/test.ss"
        "scripts/ex1/eval.ss"
        "scripts/emptydir"
        "scripts/ex2/test.ss"
        "tests/test_tree.hs"))

; (define a (insert '("asdf" "asdf") #hash()))
; (insert '("asdf" "sss") a)

(define tree (parse files))
(exists "src" tree)
(exists "src/complex.hs" tree)