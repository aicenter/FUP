---
title: "Exam Task: Heap: Balanced Binary Tree"
subtitle: "From: Exam 3 - 2022"
date: "2024-01-12"
outline: deep
---

# Heap: Balanced Binary Tree

A *Heap* is a tree that satisfies the heap property: the node's value is greater or equal to the
value of any of its children.  As a result, the heap's root carries the greatest value in the heap.
Hence, heaps are commonly used to implement queue-like data structures.  Further, let us consider
the heap as a balanced binary tree, where a binary tree is balanced if the heights of the left and
right subtree of any node differ by not more than one.

<div style="width: 100%; display: table;">
  <div style="display: table-row">
    <div style="width: 50%; display: table-cell;">
      <img src="/img/balanced-tree-valid_01.svg" style="width: 100%; margin: auto;">
      Balanced binary tree fulfilling the heap property that is returned by the described algorithm.
    </div>
    <div style="display: table-cell;">
      <img src="/img/balanced-tree-valid_02.svg" style="width: 100%; margin: auto;">
      Another valid balanced binary tree fulfilling the heap property.
    </div>
  </div>
</div>

<div style="width: 100%; display: table;">
  <div style="display: table-row">
    <div style="width: 50%; display: table-cell;">
      <img src="/img/balanced-tree-not_heap_03.svg" style="width: 100%; margin: auto;">
      Balanced binary tree violating the heap property (violating notes marked in red).
    </div>
    <div style="display: table-cell;">
      <img src="/img/balanced-tree-not_balanced_04.svg" style="width: 100%; margin: auto;">
Binary tree fulfilling the heap property that is not balanced (one pair of empty subtree-endpoints violating the depth difference limit in red; note, there are additional violations that are not marked).
    </div>
  </div>
</div>


Your task is to implement a function that builds a heap from a given list of elements 
(see Algorithm 1). This function starts with the empty leaf, inserts elements one by one,
and fixes the heap property after each insertion. 

The *insert* operation can be implemented recursively as shown in Algorithm 1 
(`insertAtEmpty`). This function recursively inserts the new element into its left or right subtree
depending on their minimum depth. Recall that the minimum depth of a tree is the length of its shortest branch.

After each insertion the heap property must be enforced, see Algorithm 1 (`enforceHeap`).
This function recursively processes the tree in a bottom-up manner, and if the heap property is violated,
it swaps the parent node with its child.

To implement these functions in Scheme and Haskell (especially the swapping), it is good to use pattern matching.

<img src="/img/balanced-tree-algo.svg" style="width: 100%; margin: auto;" class="inverting-image">


## Racket

In Scheme, implement a function `(build-heap lst)` that accepts a list of values and returns a heap in the form of a balanced binary tree.
In the tree, non-empty nodes are represented as
```scheme
(struct node (v left right) #:transparent)
```
while the empty nodes are denotes as `'leaf`.
Note that the exact shape of the values in the heap is not enforced.
Instead, the results returned by your function need to be a binary balanced tree that satisfies the heap property. 

For your convenience, you are provided with the functions
```scheme
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
```
where `is-leaf?` determines whether a node is empty, and `show-tree` prints tree in a human readable format.

Your task is to be called `balanced-tree.rkt` and must provide the `build-heap` and `is-leaf?` functions and 
the `node` structure.  Hence, the head of your file should read
```scheme
#lang racket
(provide node node-v node-left node-right is-leaf? build-heap)
(struct node (v left right) #:transparent)
(define (is-leaf? nd)
  (eq? 'leaf nd))
```

## Examples
The following shows the behaviour of the `build-heap` function if implemented exactly as proposed.
Note that other correct heaps may exist.

```scheme
(build-heap '())
'leaf 
```

```scheme
(show-tree (build-heap '(8 9 4 29)))
------4
---8
29
---9
(node 29 (node 8 (node 4 'leaf 'leaf) 'leaf) (node 9 'leaf 'leaf))
```

```scheme
(show-tree (build-heap 
  '(1 1 2 3 5 8 13 21 34 55)))
  ---------2
  ------13
  ---------1
  ---34
  ---------1
  ------8
  55
  ------5
  ---21
  ------3
  (node
   55
   (node 34 (node 13 (node 2 'leaf 'leaf) (node 1 'leaf 'leaf)) 
            (node 8 (node 1 'leaf 'leaf) 'leaf))
   (node 21 (node 5 'leaf 'leaf) (node 3 'leaf 'leaf)))
```

::: details
```scheme
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
```
:::

# Haskell

In Haskell, implement a function
```haskell
buildHeap :: (Eq a, Ord a) => [a] -> Tree a
```
that accepts a list of comparable and orderable values and returns a heap in the form of a balanced binary tree.
The tree is represented as the type
```haskell
data Tree a = Leaf | Node a (Tree a) (Tree a)
```
and for your convenience, you are provided with the instance of `Show`
```haskell
tostr :: (Show a) => Tree a -> Int -> String
tostr Leaf d = ""
tostr (Node x l r) d = tostr l (d+1) ++ concat (replicate d "---") ++ show x 
                       ++ "\n" ++  (tostr r (d+1))
instance (Show a) => Show (Tree a) where
    show tree = tostr tree 0
```

Your task is to be called `BalancedTree.hs` and must export the `buildHeap` function and the `Tree` data type.
Hence, the head of your file should read

```haskell
module BalancedTree ( Tree (..), buildHeap) where
data Tree a = Leaf | Node a (Tree a) (Tree a)
```

## Examples
The following shows the behaviour of the `buildHeap` function if implemented exactly as proposed.
Note that other correct heaps may exist.

```haskell
buildHeap []

```

```haskell
buildHeap [8,9,4,29]
------4
---8
29
---9
```

```haskell
buildHeap [1,1,2,3,5,8,13,21,34,55]
---------2
------13
---------1
---34
---------1
------8
55
------5
---21
------3
```

::: details
```haskell
module BalancedTree ( Tree (..), build_heap) where

data Tree a = Leaf | Node a (Tree a) (Tree a)

tostr :: (Show a) => Tree a -> Int -> String
tostr Leaf d = ""  
tostr (Node x l r) d = tostr l (d+1) ++ concat (replicate d "---") ++ show x ++ "\n" ++  (tostr r (d+1))
instance (Show a) => Show (Tree a) where
    show tree = tostr tree 0

min_depth :: Tree a -> Int
min_depth Leaf = 0
min_depth (Node _ left right) = 1+(min (min_depth left) (min_depth right))

enforce_heap :: (Eq a, Ord a) => Tree a -> Tree a
enforce_heap Leaf = Leaf
enforce_heap tr@(Node x Leaf Leaf) = tr
enforce_heap (Node x left Leaf) = Node nv (Node nlv ll lr) Leaf 
    where (Node lv ll lr) = enforce_heap left
          nlv = (min x lv)
          nv = (max lv x)
enforce_heap (Node x left right) = Node nv (Node nlv ll lr) (Node nrv rl rr)  
    where (Node lv ll lr) = enforce_heap left
          (Node rv rl rr) = enforce_heap right
          nlv = (min x lv)
          nrv = (min x rv)
          nv = (max (max lv rv) x)

heap_insert :: (Eq a, Ord a) => a -> Tree a -> Tree a
heap_insert x Leaf = Node x Leaf Leaf
heap_insert x tree@(Node y left right) = dfs tree
    where dfs Leaf = Node x Leaf Leaf 
          dfs tr@(Node z lf rh) = if min_depth rh < min_depth lf then Node z lf (dfs rh)  
                                  else Node z (dfs lf) rh  

build_heap :: (Eq a, Ord a) => [a] -> Tree a
build_heap [] = Leaf
build_heap (x:xs) = enforce_heap ( heap_insert x (build_heap xs) ) 
```
:::
