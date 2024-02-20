#lang racket

(provide add-edge build-tree)

(struct node (val kids) #:transparent)
(struct leaf (val) #:transparent)

(define (value t)
  (match t
    [(leaf x) x]
    [(node x kids) x]))

(define (sortnodes ns)
  (sort ns (lambda (p q) (< (value p) (value q)))))
  

(define (add-edge edge tree)
  (define s (car edge))
  (define t (cadr edge))
  (match tree
   [(leaf x)
    (if (= x s)
        (node x (list (leaf t)))
        (leaf x))]
   [(node x kids)
    (if (= x s)
        (node x (sortnodes (cons (leaf t) kids)))
        (node x (map (curry add-edge edge) kids)))]
   ))


(define (build-tree init edges) (foldl add-edge init edges))

; (define (build-tree init edges))
; (add-edge '(2 4) (node 1 (list (le))))
(add-edge '(2 4) (leaf 2))
(add-edge '(2 4) (node 1 (list (leaf 2) (leaf 3))))


(build-tree (leaf 1) '((1 2) (1 3) (2 4) (4 5) (3 6)))
(node 1 (list (node 2 (list (node 4 (list (leaf 5))))) (node 3 (list (leaf 6)))))
