---
outline: deep
title: "Exam Task: Building Trees"
subtitle: "From: Exam 2 - 2023"
---

# Building Trees


In this task, you will build a tree from an intial tree and a sequence of edges. Your task is to
iterate through the edge sequence from left to right and expand successively the initial tree.  Each
edge is a pair $(s,t)$. To expand the tree by the edge $(s,t)$, you must traverse through the tree
and locate the node $s$. Next, you add $t$ among its children. If $s$ does not occur in the tree,
the edge is ignored.

For example, suppose that the initial tree is just a single node (a leaf) $1$ and the edge sequence
equals $(1,2),(1,3),(5,7),(2,4),(4,5),(3,6)$. By expanding the initial tree by the edges, we obtain
the following sequence of trees:

<img src="/img/building-trees-sequence.svg" style="width: 95%; margin: auto;" class="inverting-image">

Note that the third edge $(5,7)$ was ignored because there is no node $5$ in the currently
constructed tree. So the currently constructed tree remains unchanged.  

## Racket

Implement a function `(build-tree init edges)` that takes an initial 
tree `init` and a list of edges `edges`, and returns the tree created by expanding 
the initial tree by the edges.

To represent trees in Racket, use the following structures:
```scheme
(struct node (val kids) #:transparent)
(struct leaf (val) #:transparent)    
```
Thus the leaves (nodes without children) are represented as, for instance, `(leaf 6)`.
The nodes with children are represented as, for instance, `(node 1 (list (leaf 2) (leaf 3)))`.

To implement the function `build-tree`, implement first a function `(add-edge edge tree)`
expanding a given tree `tree` by a single edge `edge`. For example,
```scheme
> (add-edge '(2 4) (node 1 (list (leaf 2) (leaf 3))))
(node 1 (list (node 2 (list (leaf 4))) (leaf 3)))  
```

When you add a new leaf to a list of children, prepend it at the front. For example,
```scheme
> (add-edge '(1 3) (node 1 (list (leaf 2))))
(node 1 (list (leaf 3) (leaf 2)))
```

To make the output of the function `build-tree` unique, sort the children of every node 
in the resulting tree based on their values. You may assume, that the node values are numbers.
For example, 
```scheme
(node 1 (list (node 2 (list (leaf 4))) (leaf 3))) ; correct
(node 1 (list (leaf 3) (node 2 (list (leaf 4))))) ; not correct
```

Your file should be called `building-trees.rkt` and should export the `add-edge` and 
`build-tree` functions and the structures `node` and `leaf`.
```scheme
#lang racket

(provide add-edge
         build-tree
         (struct-out node)
         (struct-out leaf))

(struct node (val kids) #:transparent)
(struct leaf (val) #:transparent)

(define (add-edge edge tree)
  ; Implement me!
  )

(define (build-tree init edges)
  ; Implement me!
  )
```

### Example

```scheme
> (build-tree (leaf 1) '((1 2) (1 3) (5 7) (2 4) (4 5) (3 6)))
(node 1 (list (node 2 (list (node 4 (list (leaf 5))))) 
              (node 3 (list (leaf 6)))))
```

### Hints

To sort the children, use the function `(sort lst less-than?)` sorting a list `lst`
comparing its elements by a function `less-than?`. For example,
```scheme
> (sort '((1 3) (2 2) (3 1)) (lambda (p q) (< (cadr p) (cadr q))))
'((3 1) (2 2) (1 3))
```

::: details Solution
```scheme
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
```
:::

## Haskell

In Haskell, implement a function 
`buildTree :: Ord a => Tree a -> [Edge a] -> Tree a` that takes an initial 
tree and a list of edges, and returns the tree created by expanding 
the initial tree by the edges.

To represent trees and edges, use the following data types:
```haskell
data Tree a = Leaf { val :: a } 
            | Node { val :: a,
                     kids :: [Tree a] } deriving (Eq,Show) 

type Edge a = (a,a)
```
Thus the leaves (nodes without children) are represented as, for instance, `Leaf {val = 6}`.
The nodes with children are represented as, for instance,
```haskell
Node {val = 1, kids = [Leaf {val = 2,Leaf {val = 3}]}
```

To implement the function `buildTree`, implement first a function
```haskell
addEdge :: Eq a => Tree a -> Edge a -> Tree a
```
expanding a given tree by a single edge. For example,
```haskell
> addEdge (Node {val = 1, kids = [Leaf {val = 2}, Leaf {val = 3}]}) (2,4)
Node {val = 1, kids = [Node {val = 2, kids = [Leaf {val = 4}]},
                       Leaf {val = 3}]}
```

When you add a new leaf to a list of children, prepend it at the front. For example,
```haskell
> addEdge (Node {val = 1, kids = [Leaf {val = 2}]}) (1,3)
Node {val = 1, kids = [Leaf {val = 3}, Leaf {val = 2}]}
```

To make the output of the function `buildTree` unique, sort the children of every node 
in the resulting tree based on their values. You may assume, that the node values belongs to 
the typeclass `Ord`. For example, 

```haskell
  Node {val = 1, kids = [Node {val = 2, kids = [Leaf {val = 4}]},
                         Leaf {val = 3}]} -- correct
  Node {val = 1, kids = [Leaf {val = 3}, 
                         Node {val = 2, kids = [Leaf {val = 4}]}]} -- not correct
```

Your file should be called `BuildingTrees.hs` and should export the `buildTree`, 
`addEdge` functions and the type `Tree`.

```haskell
module BuildingTrees (addEdge, buildTree, Tree(..)) where
import Data.List

data Tree a = Leaf { val :: a } 
            | Node { val :: a,
                     kids :: [Tree a] } deriving (Eq,Show) 

type Edge a = (a,a)

addEdge :: Eq a => Tree a -> Edge a -> Tree a
-- implement me!

buildTree :: Ord a => Tree a -> [Edge a] -> Tree a
-- implement me!
```

### Example

```haskell
> buildTree (Leaf {val=1}) [(1,2),(1,3),(5,7),(2,4),(4,5),(3,6)]
Node {val = 1, kids = [Node {val = 2, kids = [Node {val = 4, 
                                                    kids = [Leaf {val = 5}]}]},
                       Node {val = 3, kids = [Leaf {val = 6}]}]}
```

### Hints

To sort the children, use the function `sortOn :: Ord b => (a -> b) -> [a] -> [a]` 
from the library `Data.List` sorting a list by converting them into a values of 
a type inside `Ord`. For example, 
```haskell
> sortOn snd [(1,3),(2,2),(3,1)]
[(3,1),(2,2),(1,3)]
```

::: details Solution
```haskell
import Data.List

data Tree a = Leaf { val :: a }
            | Node { val :: a,
                     kids :: [Tree a] } deriving Show
type Edge a = (a,a)

tr = Node {val = 1, kids = [Leaf {val = 2},Leaf {val = 3}]}

addEdge (Leaf x) (s,t) | x == s = Node x [Leaf t]
                       | otherwise = Leaf x
addEdge (Node x kids) (s,t)
  | x == s    = Node x (sortOn val ((Leaf t):kids))
  | otherwise = Node x [addEdge n (s,t) | n <- kids]


buildTree tree edges = foldl addEdge tree edges
```
:::
