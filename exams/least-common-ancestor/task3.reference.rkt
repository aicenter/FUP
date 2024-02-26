#lang racket
(provide find-path
         common-ancestor
         (struct-out node)
         (struct-out leaf))

(struct node (val left right) #:transparent)
(struct leaf (val) #:transparent)

(define tree (node 1 (node 2 (leaf 5) (leaf 6)) (node 3 (leaf 4) (leaf 7))))

(define (find-path x tree)
  (match tree
    [(leaf y) (if (equal? x y)
                  (list x)
                  '())]
    [(node y l r) (if (equal? x y)
                      (list x)
                      (let* ([pl (find-path x l)]
                             [pr (find-path x r)]
                             [both (append pl pr)])
                        (if (null? both)
                            '()
                            (cons y both))))]))

(define (common-ancestor x y tree)
  (define px (find-path x tree))
  (define py (find-path y tree))
  (define common (take-common-prefix px py))
  (if (null? common)
      #f
      (last common)))