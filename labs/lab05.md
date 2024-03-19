---
outline: deep
---

# Lab 5: Streams and graphs

## Exercise 1

Define a function `(stream-add s1 s2)` adding two infinite streams together component-wise. For instance,
```scheme
  0 1 2 3 4 5 6 ....
+ 1 1 1 1 1 1 1 ....
--------------------
  1 2 3 4 5 6 7 ....
```
Using `stream-add`, define the infinite stream `fib-stream` of all Fibonacci numbers. We can test it, for instance, as follows:
```scheme
(stream->list (stream-take fib-stream 10)) =>
  (0 1 1 2 3 5 8 13 21 34)
```

Adding two infinite streams can be done recursively. Since the streams are infinite, we do not have to check the emptiness of any input streams. 
::: details Solution: `stream-add`
```scheme
(define (stream-add s1 s2)
  (stream-cons (+ (stream-first s1) (stream-first s2))
               (stream-add (stream-rest s1) (stream-rest s2))))
```
:::

The definition of the Fibonacci sequence 
$F(0)=0$, $F(1)=1$, and $F(n)=F(n-1) + F(n-2)$ for $n>1$ can be reformulated as follows:
```scheme
      0 1 1 2 3  5  8 13 ...   ; F(n-2) - Fibonacci sequence
+     1 1 2 3 5  8 13 21 ...   ; F(n-1) - shifted Fibonacci sequence to the left by one element
----------------------------
  0 1 1 2 3 5 8 13 21 34 ...   ; Fibonacci sequence starts with 0,1 followed by the sum of the above sequences
```

This directly leads to the following code:
::: details Solution: `fib-stream`
```scheme
(define fib-stream
  (stream-cons 0
               (stream-cons 1
                            (stream-add fib-stream
                                        (stream-rest fib-stream)))))
```
Alternatively one can use `stream*` as follows:
```scheme
(define fib-stream
  (stream* 0 1 (stream-add fib-stream
                           (stream-rest fib-stream))))
```
:::

## Exercise 2

Apart from streams, this lab is also focused on graphs. A graph $G=(V,E)$ is a tuple consisting of a set of vertices $V$ (also called nodes) and a set of edges
$E\subseteq\{\{u,v\}\mid u,v\in V, u\neq v\}$. We will represent a graph in Scheme as a struct with two fields. The first is a list of vertices, and the second is a list of edges. An edge $\{u,v\}$ is represented as a list `(u v)`. We define a structure for a graph:
```scheme
(struct graph (nodes edges))
```

The following graph
![](/img/6n-graph.svg){ style="width: 50%; margin: auto;" }

is represented as follows:
```scheme
(define gr
  (graph
    '(1 2 3 4 5 6)
    '((1 2) (1 5) (2 3) (2 5) (3 4) (4 5) (4 6))))
```


Given a graph $G$, a [Hamiltonian path](https://en.wikipedia.org/wiki/Hamiltonian_path) is a path visiting each vertex of $G$ exactly once. We will represent a path as a list of consecutive nodes in the path. The above graph `gr` has a Hamiltonian path `(3 2 1 5 4 6)`. 

Write a function `(find-hamiltonian-path g)` which takes a graph as its input and returns a Hamiltonian path, if it exists, and `#f` otherwise.
E.g. 
```scheme
(find-hamiltonian-path gr) => (3 2 1 5 4 6)
(find-hamiltonian-path (graph '(a b c d) '((a b) (a c) (a d)))) => #f
```

As a Hamiltonian path traverses each node exactly once, if it exists, it has to be represented by a permutation of the nodes. Thus, we can apply the function `permutations` from the previous lab to generate all node permutations and check whether each of them forms a Hamiltonian path. We start with a definition of a function checking if a given list of nodes is a path. For that, we need a function testing whether a pair of nodes is connected.
::: details Solution: `edge?`
```scheme
; test whether a pair of nodes is connected
(define (edge? g)
  (define edges (graph-edges g))
  (lambda (p)
     (or (member p edges) (member (reverse p) edges))))
```
:::
Given a list, we create a list of pairs of consecutive nodes. E.g. `(1 2 3 4)` is
transformed to `((1 2) (2 3) (3 4))`. This is done
by taking `(1 2 3)`
and `(2 3 4)` and joining them by mapping `list` element-wise. Finally, we test whether all these pairs are connected.
To do so, we use the function `(andmap f lst)`. This function is implemented in Racket. It behaves like `map` but aggregates the results of `f` by `and` function, i.e., once any of the results is `#f`, it returns `#f` and the last result otherwise. 
::: details Solution: `check-path`
```scheme
(define (check-path g)
  (lambda (lst)
    (define but-last (take lst (- (length lst) 1)))
    (if (andmap (edge? g) (map list but-last (cdr lst)))
        lst
        #f)))
```
:::
Now we can apply the above function to all permutations. The function `(check-path g)` for a graph `g` either returns `lst` if `lst` forms a path or `#f` otherwise. Thus we can map it over all permutations of nodes and filter those which form a path. If there is a permutation being a path simultaneously, we have a Hamiltonian path. Otherwise, we return `#f`.
::: details Solution: `find-hamiltonian-path`
```scheme
(define (find-hamiltonian-path g)
  (define perms (permutations (graph-nodes g)))
  (let ([paths (filter identity (map (check-path g) perms))])
    (if (null? paths)
        #f
        (car paths))))
```

If you are curious, try to use the function `in-permutations` to compute the `perms` lazily and
compare the perfromance of the two implementations on a larger graph.
:::

## Task 1
Write a function `(stream-mul s1 s2)` taking two infinite streams and multiplying them elements-wise. Using this function, define an infinite stream `factorial-stream` of factorials $0!, 1!, 2!, 3!,\ldots$. 

::: tip
The recursive definition of factorial $f(0)=1$ and $f(n)=n\cdot f(n-1)$ for $n>0$ gives us
```scheme
    1 2 3  4   5 ...  ; n
*   1 1 2  6  24 ...  ; f(n-1)
--------------------
  1 1 2 6 24 120 ...  ; f(n)
```
In your definition, you can use the function `(in-naturals n)` implemented in Racket to define the stream of natural numbers starting from n.
:::
 
Once you have the stream of factorials `factorial-stream`, the function `stream-mul` and the stream of natural numbers `(in-natural 0)` (or even simply `(in-naturals)`), you can define a function `(exp-stream x)` taking a number `x` and returning the power series representing $e^x$, i.e., $e^x = 1 + x + \frac{x^2}{2!} + \frac{x^3}{3!} + \cdots$. Then you can approximate the value $e^x$ by summing an initial segment of this stream. E.g., to approximate $e$, we can sum the first 100 elements:
```scheme
(apply + (stream->list (stream-take (exp-stream 1) 100)))
```

<!--
::: details Solution
```scheme
(define (stream-mul s1 s2)
  (stream-cons (* (stream-first s1) (stream-first s2))
               (stream-mul (stream-rest s1) (stream-rest s2))))

(define factorial-stream (stream-cons 1 (stream-mul (in-naturals 1) factorial-stream)))

(define (exp-stream x)
  (define recipr (stream-map ((curry /) 1) factorial-stream))
  (define powers (stream-map ((curry expt) x) (in-naturals)))
  (stream-map exact->inexact (stream-mul powers recipr)))
```
:::
-->

::: tip Note
Some of you noticed that the stream elements returned by `exp-stream` having a
larger index might overflow if we apply `exp-stream` to a float number, e.g.,
`(exp-stream 3.0)`. That is true as the nominators in the power series
(and the denominators) grow extremely fast. In particular, the nominator might
become infinite if evaluated on a float number. Compare the following calls:
```scheme
(expt 3.0 1000) => +inf.0
(expt 3 1000) =>
1322070819480806636890455259752144365965422032752148167664920368226828597346704899540778313850608061963909777696872582355950954582100618911865342725257953674027620225198320803878014774228964841274390400117588618041128947815623094438061566173054086674490506178125480344405547054397038895817465368254916136220830268563778582290228416398307887896918556404084898937609373242171846359938695516765018940588109060426089671438864102814350385648747165832010614366132173102768902855220001
```
So the above solution avoids this problem by working with precise arithmetic and finally translating the result into the decimal representation. To use it, one must call `exp-stream` with an exact number e.g. `3` or `(/ 35 10)`.
:::


## Task 2
Given a graph $G=(V,E)$, a subset of nodes $S\subseteq V$ is called a vertex cover, if for every edge $\{u,v\}\in E$, we have
$u\in S$ or $v\in S$. If $S$ is smallest possible, it is called minimum vertex cover (see [wikipedia](https://en.wikipedia.org/wiki/Vertex_cover)).

Write a function `(min-vertex-cover g)` taking a graph `g` and returning a minimum vertex cover of `g`. E.g.
```scheme
(min-vertex-cover gr) => (1 2 4)
```

::: tip Hint
The idea is similar to Exercise 2. Instead of all permutations, we can take all subsets of nodes. Subsets can be generated by the function `sub-seq` from the previous lab. We can sort them by cardinality and starting from the smallest ones, we can check which of them form a vertex cover. In fact, it is computationally more efficient, if we create a stream of subsets so that subsets are computed lazily as they are needed.

Thus we can test smaller subsets first without computing large ones. To create
such a lazy stream, we can modify the function `sub-seq`. It is basically the
same code where list functions are replaced by their stream equivalents.
However, our original function `sub-seq` does not generate subsets ordered by
their cardinality. To fix this, we have to modify the function merging together
the result of the recursive call and the newly created subsets. Thus we define a
function `(stream-merge s1 s2 cmp)` which takes two streams and a function
`cmp` comparing two elements of these streams and returns a stream where the
values are merged so that the smaller elements come first.  
:::
```scheme
; lazy subsequences
(define (stream-merge s1 s2 cmp)
  (cond
    ([stream-empty? s1] s2)
    ([stream-empty? s2] s1)
    ([cmp (stream-first s1) (stream-first s2)]
     (stream-cons (stream-first s1) (stream-merge (stream-rest s1) s2 cmp)))
    (else (stream-cons (stream-first s2) (stream-merge s1 (stream-rest s2) cmp)))))
      
(define (sub-seq lst)
  (if (null? lst)
      (stream '())
      (let ([el (car lst)]
            [rest-sub-seq (sub-seq (cdr lst))])
        (stream-merge rest-sub-seq
                (stream-map ((curry cons) el) rest-sub-seq)
                (lambda (x y) (< (length x) (length y)))))))
```

<!--
::: details Solution
```scheme
; minimum vertex cover = smallest subset of nodes such that each edge has one of its nodes in it
(define (check-cover g)
  (lambda (lst)
    (if (andmap (lambda (e) (or (member (car e) lst) (member (cadr e) lst))) (graph-edges g))
        lst
        #f)))

(define (min-vertex-cover g)
  (stream-first (stream-filter identity (stream-map (check-cover g) (sub-seq (graph-nodes g))))))
```
:::
-->
