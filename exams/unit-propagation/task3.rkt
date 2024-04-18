#lang racket
(require racket/trace)
(provide node node-v node-left node-right is-leaf? build-heap)

(struct node (v left right) #:transparent)

(define (is-leaf? nd)
  (eq? 'leaf nd))

(define (show-tree tree [depth 0])
  (define (offset d)
    (if (= d 0)
        ""
        (string-append "---" (offset (- d 1)))))
  (if (is-leaf? tree)
      tree
      (begin
      (show-tree (node-left tree) (+ depth 1))
      (displayln (string-append (offset depth) (number->string (node-v tree))))
      (show-tree (node-right tree) (+ depth 1))
       tree)
      ))
  

(define (min-depth tree)
  (cond ((is-leaf? tree) 0)
        (#t (+ 1 (min (min-depth (node-left tree)) (min-depth (node-right tree) ))))
        ))

(define (enforce-heap tree)
  (cond ((is-leaf? tree) 'leaf)
        ((and (is-leaf? (node-left tree)) (is-leaf? (node-right tree))) tree)
        ((is-leaf? (node-right tree))
         (let* ([x (node-v tree)]
                [left (enforce-heap (node-left tree))]
                [ll (node-left left)]
                [lr (node-right left)]
                [lv (node-v left)]
                [nlv (min x lv)]
                [nv (max x lv)])
           (node nv (node nlv ll lr) 'leaf) 
         ))
        (#t
         (let* ([x (node-v tree)]
                [left (enforce-heap (node-left tree))]
                [ll (node-left left)]
                [lr (node-right left)]
                [lv (node-v left)]
                [right (enforce-heap (node-right tree))]
                [rl (node-left right)]
                [rr (node-right right)]
                [rv (node-v right)]
                [nlv (min x lv)]
                [nrv (min x rv)]
                [nv (max x lv rv)])
           (node nv (node nlv ll lr) (node nrv rl rr)) 
         ))
        ))
;(trace enforce-heap)

(define (heap-insert val tree)
  (define (dfs stree)
    (cond ((is-leaf? stree) (node val 'leaf 'leaf))
          ((< (min-depth (node-right stree)) (min-depth (node-left stree))) (node (node-v stree) (node-left stree) (dfs (node-right stree))))
          (#t (node (node-v stree) (dfs (node-left stree)) (node-right stree)))
          ))
  (cond ((is-leaf? tree) (node val 'leaf 'leaf))
        (#t (dfs tree))
  ))


(define (build-heap ls)
  (if (null? ls)
      'leaf
      (enforce-heap (heap-insert (car ls) (build-heap (cdr ls))))
  ))

