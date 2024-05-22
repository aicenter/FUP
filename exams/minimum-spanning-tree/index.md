---
title: "Exam Task: Minimum Spanning Tree"
subtitle: "From: Exam 1 - 2022"
date: "2024-01-12"
outline: deep
---

# Minimum Spanning Tree

Given a connected weighted graph $G=(V,E)$ with a weight function $w\colon E\to\mathbf{N}$ assigning
to each edge $e$ its weight $w(e)$, its minimum spanning tree is a graph $T=(V,E')$ such that
$E'\subseteq E$, $T$ is a tree (i.e., a connected graph without cycles) and $\sum_{e\in E'}w(e)$ is
minimum possible among such trees. The figure shows an example of a connected weighted
graph and its minimum spanning tree.

![Left: A connected, weighted graph. Right: Its minimum spanning tree of weight 16.](/img/minimum-spanning-tree-graph.svg){class="inverting-image"}

Your task is to implement an algorithm computing the minimum spanning tree, i.e.,
a function returning for a given connected weighted graph $(V,E)$ the subset $E'$
of edges in the minimum spanning tree. There are various greedy algorithms computing
the minimum spanning tree. You can use, for instance, use Jarnik's algorithm, whose pseudocode 
is below.

```
vertices = [list of graph vertices]
edges = [list of graph edges] 

covered = [v0] # select an arbitrary vertex as the initial one
tree_edges = [] 

until covered == vertices:
  find the minimum-weight edge e=(u,v) connecting a vertex u in covered 
    with a vertex v not in covered
  add v to covered
  add e to tree_edges

return tree_edges
```

# Racket

In Scheme, implement a function `(minimum-spanning-tree gr)` 
that accepts a connected weighted graph `gr` and
returns a list of edges forming the minimum spanning tree. The graph and weighted
edges are represented by the following structures:

```scheme
(struct edge (u v weight) #:transparent)
(struct graph (nodes edges) #:transparent)
```

Your file has to be called `task3.rkt` and must provide the function
`minimum-spanning-tree` and the above structures so 
it should start like this:
```scheme
#lang racket
(provide minimum-spanning-tree (struct-out edge) graph)
(struct edge (u v weight) #:transparent)
(struct graph (nodes edges) #:transparent)

; your code goes here
```

## Example

The graph from the figure is represented as follows:
```scheme
  (define gr (graph '(A B C D E F)
                     (list (edge 'A 'B 1)
                           (edge 'D 'E 4)
                           (edge 'E 'F 7)
                           (edge 'A 'D 5)
                           (edge 'B 'E 2)
                           (edge 'C 'F 5)
                           (edge 'D 'B 6)
                           (edge 'E 'C 4)
                           (edge 'A 'E 3))))

> (minimum-spanning-tree gr)
(list (edge 'C 'F 5) (edge 'E 'D 4) (edge 'E 'C 4) 
      (edge 'B 'E 2) (edge 'A 'B 1))
```

Note that the structure `(edge x y w)` represents a bidirectional 
edge so it can be used in Jarn\'ik's algorithm as `(x,y)` and also as 
`(y,x)`.

The returned list of edges might be ordered arbitrarily. Each edge might be ordered 
arbitrarily as well. For instance, it does not matter if your output contains 
`(edge 'C 'F 5)` or `(edge 'F 'C 5)`. However,
do not include both variants in your output.

## Hint

To find the minimum-weight edge, you may want to sort a list of edges by their
weight. This can be done by the function `sort` 
allowing sorting w.r.t. a given comparing function, e.g., 
```scheme
> (sort (list (edge 'a 'b 3) (edge 'b 'c 1) (edge 'c 'a 2)) 
        (lambda (e1 e2) (< (edge-weight e1) (edge-weight e2))))
(list (edge 'b 'c 1) (edge 'c 'a 2) (edge 'a 'b 3))
```
::: details Solution
```scheme
#lang racket
(provide minimum-spanning-tree graph edge)

(struct edge (u v weight) #:transparent)
(struct graph (nodes edges) #:transparent)

(define gr (graph '(1 2 3 4) (list (edge 1 2 10) (edge 1 3 20) (edge 1 4 30) (edge 2 4 20) (edge 3 4 20))))

(define gr2 (graph '(A B C D E F)
                   (list (edge 'A 'B 1)
                         (edge 'D 'E 4)
                         (edge 'E 'F 7)
                         (edge 'A 'D 5)
                         (edge 'B 'E 2)
                         (edge 'C 'F 5)
                         (edge 'D 'B 6)
                         (edge 'E 'C 4)
                         (edge 'A 'E 3))))


(define (reverse-edge e)
  (match e
    [(edge u v w) (edge v u w)]))

(define (reverse-edges gr)
  (graph (graph-nodes gr)
         (append (graph-edges gr) (map reverse-edge (graph-edges gr)))))

(define (find-edge edges covered uncovered)
  (car (sort (filter (lambda (e)
                       (and (member (edge-u e) covered)
                            (member (edge-v e) uncovered)))
                     edges)
             (lambda (e1 e2) (< (edge-weight e1) (edge-weight e2))))))

(define (iter gr covered uncovered tree-edges)
  (if (null? uncovered)
      tree-edges
      (let* ([e (find-edge (graph-edges gr) covered uncovered)]
             [v (edge-v e)])
        (iter gr (cons v covered) (remove v uncovered) (cons e tree-edges)))))

(define (minimum-spanning-tree gr)
  (let* ([enriched-gr (reverse-edges gr)]
         [nodes (graph-nodes gr)]
         [covered (list (car nodes))]
         [uncovered (cdr nodes)])
    (iter enriched-gr covered uncovered '())))
```
:::


# Haskell

In Haskell, we represent the weighted graph and edges by the following
types:
```haskell
data Edge a b = Edge { u :: a,
                       v :: a,
                       weight :: b } deriving (Eq,Show)

data Graph a b = Graph { nodes :: [a],
                         edges :: [Edge a b] } deriving Show  
```

Implement a function 
`minSpanningTree :: (Eq a, Ord b) => Graph a b -> [Edge a b]`
that accepts a connected weighed graph and returns a list of edges from the 
minimum spanning tree.

## Example

```haskell
gr :: Graph Char Int
gr = Graph{ nodes = ['A'..'F'],
            edges = [Edge 'A' 'B' 1,
                     Edge 'D' 'E' 4,
                     Edge 'E' 'F' 7,
                     Edge 'A' 'D' 5,
                     Edge 'B' 'E' 2,
                     Edge 'C' 'F' 5,
                     Edge 'D' 'B' 6,
                     Edge 'E' 'C' 4,
                     Edge 'A' 'E' 3] }

> minSpanningTree gr
[Edge {u = 'C', v = 'F', weight = 5},Edge {u = 'E', v = 'D', weight = 4},
 Edge {u = 'E', v = 'C', weight = 4},Edge {u = 'B', v = 'E', weight = 2},
 Edge {u = 'A', v = 'B', weight = 1}]
```

The returned list of edges might be ordered arbitrarily.  Each edge might be ordered arbitrarily as
well.  For instance, it does not matter if your output contains `Edge {u='C', v='F', weight=5}` or
`Edge {u='F', v='C', weight=5}`. However, do not include both variants in your output.

Your file has to be called `Task4.hs` and must export the function
`minSpanningTree` and the data types `Graph a b`,
`Edge a b` so it should start like this:
```haskell
module Task4 (minSpanningTree, Graph (..), Edge (..)) where
import Data.List -- for sortOn

data Edge a b = Edge { u :: a,
                       v :: a,
                       weight :: b } deriving (Eq,Show)

data Graph a b = Graph { nodes :: [a],
                         edges :: [Edge a b] } deriving Show  

-- your code goes here
```

## Hint
To find the minimum-weight edge, you may want to sort a list of edges by their
weight. This can be done by the function
`sortOn :: :: Ord b => (a -> b) -> [a] -> [a]` 
provided by `Data.List`. Below is an example.
```haskell
import Data.List

> sortOn weight [Edge 'A' 'B' 4, Edge 'B' 'C' 3, Edge 'C' 'A' 1]
[Edge {u = 'C', v = 'A', weight = 1},Edge {u = 'B', v = 'C', weight = 3},
 Edge {u = 'A', v = 'B', weight = 4}]
```

::: details Solution
```haskell
module Task4 (minSpanningTree, Graph (..), Edge (..)) where

import Data.List

data Edge a b = Edge { u :: a,
                       v :: a,
                       weight :: b } deriving (Eq,Show)
data Graph a b = Graph { nodes :: [a],
                         edges :: [Edge a b] } deriving Show

gr2 :: Graph Char Int
gr2 = Graph{ nodes = ['A'..'F'],
             edges = [Edge 'A' 'B' 1,
                      Edge 'D' 'E' 4,
                      Edge 'E' 'F' 7,
                      Edge 'A' 'D' 5,
                      Edge 'B' 'E' 2,
                      Edge 'C' 'F' 5,
                      Edge 'D' 'B' 6,
                      Edge 'E' 'C' 4,
                      Edge 'A' 'E' 3] }

reverseEdge :: Edge a b -> Edge a b
reverseEdge e = e{ u=v e, v=u e}

reverseEdges :: Graph a b -> Graph a b
reverseEdges g@Graph{ edges=es } = g{ edges=nes }
  where nes = es ++ map reverseEdge es

findEdge :: (Eq a, Ord b) => [Edge a b] -> [a] -> [a] -> Edge a b
findEdge es covered uncovered = head active
  where active = sortOn weight [ e | e <- es, u e `elem` covered, v e `elem` uncovered ]

jarnik :: (Eq a, Ord b) => Graph a b -> [a] -> [a] -> [Edge a b] -> [Edge a b]
jarnik _ _ [] es = es
jarnik g covered uncovered es = jarnik g (x:covered) (delete x uncovered) (ne:es)
  where ne = findEdge (edges g) covered uncovered
        x = v ne

minSpanningTree :: (Eq a, Ord b) => Graph a b -> [Edge a b]
minSpanningTree g = jarnik g' [x] xs []
  where g' = reverseEdges g 
        (x:xs) = nodes g 
```
:::
