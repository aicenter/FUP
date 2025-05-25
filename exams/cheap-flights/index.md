---
outline: deep
title: "Exam Task: Cheap Flights"
subtitle: "From: Exam 1 - 2021"
---

# Cheap Flights


In this task your goal is to find the cheapest flights from one airport to another. You are given an
undirected graph that is represented by a list of nodes (airports) and a list of edges (connections
from airport to airport). Each edge contains the two nodes that it connects as well as the cost of
traveling along the edge.

Below you can see an exemplary graph with 5 nodes. The cost to travel along an edge is written next to the
corresponding edge.

<img src="/img/cheap-flights-graph.svg" style="width: 50%; margin-left: auto; margin-right: auto;" class="inverting-image"/>


## Racket

In Racket, the exmeplary graph is represented as shown in the code snippet below.

```racket
; list of nodes
(define ns '(1 2 3 4 5 6)) ; listofnodes
; list of edges where each edge contains (start end cost)
(define es '((1 2 0.5) (1 3 1.0) (2 3 2.0) (2 5 1.0) (3 4 4.0) (4 5 1.0)))
; the graph; a list of nodes and edges
(define gr (list ns es))

; some convenience functions
(define (nodes gr) (car gr))
(define (edges gr) (cadr gr))
(define (cost edge) (caddr edge))
```

A `Path` represents your travel plan. For example, there are three paths from node `2` to node `3`:
```racket
(2,3)     ; path #1 cost: 2
(2,1,3)   ; path #2 cost: 1.5
(2,5,4,3) ; path #3 cost: 6
```

Write a function `(cheapflight a b gr)` which takes a starting node `a`, a destination node `b`, a
graph `gr`, and returns a list containing the cheapest path from start to destination, as well as the
total cost. If there is no path from `a` to `b` return `#f`

Your solution must be in a file called in `cheapflights.rkt` and has to `provide` the `cheapflight` function.
Your file should therefore start like this:
```racket
#lang racket

(provide cheap-flight)

; ... your code goes here ...
```

### Example

```racket
; with the graph defined above:
> (cheap-flight 2 3 gr)
'((2 1 3) 1.5)

> (cheap-flight 2 6 gr)
#f
```

### Hints

In order to find the cheapest path you can take inspiration from the Scheme lectures. We used a
queue of partial solutions and extended the shortest path until we found a solution. Here you are
not looking for the shortest but the cheapest path. To achieve this you can sort the queue of
partial solutions to extend the cheapest path.

In Scheme you can `sort` a list with the help of a predicate function taking two arguments. In case
of a list of lists (containing cost and path) you could do the following:
```racket
> (define (cheaper? x y) (< (car x) (car y)))
> (sort '(((2 3) 2.0) ((2 1 3) 1.5)) cheaper?)
'(((2 1 3) 1.5) ((2 3) 2.0))
```

::: details Exam Solution
```racket
#lang racket

(provide cheap-flight)

(define (nodes gr) (car gr))
(define (edges gr) (cadr gr))
(define (cost edge) (caddr edge))

; (display gr)

(define (neighbors n gr)
  (define (iter n es ns)
    (if (empty? es) ns
      (let* ([e (car es)]
             [a (car e)]
             [b (cadr e)]
             [rest-es (cdr es)])
        (cond
          ((= n a) (iter n rest-es (cons b ns)))
          ((= n b) (iter n rest-es (cons a ns)))
          (else (iter n rest-es ns))
          )))
    )
  (iter n (edges gr) '())
  )

; (display (neighbors 2 gr))

(define (extend path gr)
  (if (empty? path) '()
    (map (lambda (n) (cons n path)) (neighbors (car path) gr))
    )
  )

(define (unvisited path gr)
  (define (f ns) (not (member (car ns) (cdr ns))))
  (filter f (extend path gr))
  )

; (display (unvisited '(2 5) gr))

(define (split f lst)
  (define (iter f lst r1 r2)
    (if (empty? lst) (list r1 r2)
      (if (f (car lst))
        (iter f (cdr lst) (cons (car lst) r1) r2)
        (iter f (cdr lst) r1 (cons (car lst) r2))
        )
      ))
  (iter f lst '() '())
  )

; (display (split (lambda (x) (< x 3)) '(1 2 3 4 1 1 5 3 6)))

(define (search paths goal gr res)
  (if (empty? paths) res
    (let* ([p (car paths)]
           [ps (cdr paths)]
           [rps-eps (split (lambda (x) (= goal (car x))) (unvisited p gr))]
           [rps (car rps-eps)]
           [eps (cadr rps-eps)])
      (search (append ps eps) goal gr (append res (map reverse rps)))
      )
    )
  )

; (display (search '((2)) 3 gr '()))

(define (find-edge xy gr)
  (define (f edge)
    (let ([a (car edge)]
          [b (cadr edge)]
          [c (cost edge)]
          [x (car xy)]
          [y (cadr xy)])
      (or (and (= x a) (= y b))
          (and (= x b) (= y a)))
      )
    )
  (filter f (edges gr))
  )

; (display (find-edge '(1 2) gr))

(define (zip l1 l2) (map list l1 l2))
(define (sum xs) (foldl + 0 xs))
(define (init-list lst) (reverse (cdr (reverse lst))))

(define (total-cost path gr)
  (define es (zip (init-list path) (cdr path)))
  (define cs (map (lambda (e) (cost (car (find-edge e gr)))) es))
  (sum cs))

; (display (total-cost '(2 1 3) gr))

(define (connections a b gr)
  (define ps (search (list (list a)) b gr '()))
  (define cs (map (lambda (p) (total-cost p gr)) ps))
  (zip ps cs)
  )

; (connections 2 3 gr)

(define (cheap-flight a b gr)
  (define (cheaper? x y) (< (cadr x) (cadr y)))
  (define fs (sort (connections a b gr) cheaper?))
  (if (> (length fs) 0) (car fs) #f)
  )
```
:::

## Haskell

In Haskell, the exmeplary graph is represented as shown in the code snippet below.

```haskell
type Node = Int
type Cost = Float
type Edge = (Node,Node,Cost) type Graph = ([Node],[Edge]) type Path = [Node]

nodes :: [Node]
nodes = [1..6]

edges :: [Edge]
edges = [(1,2,0.5), (1,3,1.0), (2,3,2.0), (2,5,1.0), (3,4,4.0), (4,5,1.0)]

graph :: Graph
graph = (nodes,edges)
```

A `Path` represents your travel plan. For example, there are three paths from node `2` to node `3`:

```haskell
p1 = [2,3]     -- cost: 2
p2 = [2,1,3]   -- cost: 1.5
p3 = [2,5,4,3] -- cost: 6
```

Write a function `cheapflight :: Node -> Node -> Graph -> Maybe (Path,Cost)` which takes a starting
`Node`, a destination `Node`, a `Graph`, and returns the cheapest path from start to destination, as
well as the total cost.
Your solution must be in a module called `CheapFlights.hs` and export the defined
types as well was the `cheapflight` function. Your file should therefore start like this:

```haskell
module CheapFlights (cheapflight,Node,Cost,Edge,Graph,Path) where
import Data.List -- needed for sorting (see hints)

-- ... your code goes here ...
```


### Example

```haskell
 -- with the graph defined above:
> cheapflight 2 3 gr
Just ([2,1,3],1.5)

> cheapflight 2 6 gr
Nothing
```


### Hints

In order to find the cheapest path you can modify the breadth-first-search (BFS) algorithm that was
discussed in the labs. In the lab we used a queue of partial solutions and extended the shortest
path until we found a solution. Here you are not looking for the shortest but the cheapest path. To
achieve this you can sort the queue of partial solutions to extend the cheapest path.

For sorting your paths according to the cost you can use the function `sortBy` which is provided by
the package `Data.List`:

```haskell
import Data.List

lowcost (_,x) (_,y) | x < y = LT
                    | otherwise = GT

sortBy lowcost [([1,3],2.0) ([1,2,3],0.3)]
-- [([1,2,3],0.3) ([1,3],2.0)]
```

::: details Exam Solution
```haskell
module CheapFlights (cheapflight,Node,Cost,Edge,Graph,Path) where
import Data.List

type Node = Int
type Cost = Float
type Edge = (Node,Node,Cost)
type Graph = ([Node],[Edge])
type Path = [Node]

nextPos :: Node -> Graph -> [(Node,Cost)]
nextPos n g = [(y,z) | (x,y,z) <- snd g, x == n] ++ [(x,z) | (x,y,z) <- snd g, y == n]

extend :: (Path,Cost) -> Graph -> [(Path,Cost)]
extend ([],_) _ = []
extend (path@(p:_),c) m = map (\(n,c') -> (n:path,c+c')) $ nextPos p m

lowcost :: Ord b => (a,b) -> (a,b) -> Ordering
lowcost (_,x) (_,y) | x < y = LT
                    | otherwise = GT

bfs :: [Node] -> [(Path,Cost)] -> Node -> Graph -> Maybe (Path, Cost)
bfs _ [] _ _ = Nothing
bfs visited upaths q m
    -- is path a solution? If yes, return the reversed solution
    | p == q = Just (reverse path, c)
    -- does path end in an already visited position? If yes, disregard it
    | p `elem` visited = bfs visited paths q m
    | otherwise = bfs (p:visited) (paths ++ extend (path,c) m) q m
    -- consider the first path in the queue and its head p
    where ((path@(p:_),c):paths) = sortBy lowcost upaths

cheapflight :: Node -> Node -> Graph -> Maybe (Path,Cost)
cheapflight s t g = bfs [] [([s],0)] t g


nodes :: [Node]
nodes = [1..6]

edges :: [Edge]
edges = [(1,2,0.5), (1,3,1.0), (2,3,2.0), (2,5,1.0), (3,4,4.0), (4,5,1.0)]

graph :: Graph
graph = (nodes,edges)

-- cheapflight 2 5 graph
-- > Just ([2,1,3], 1.5)
```
:::

