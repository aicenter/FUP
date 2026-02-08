# Example Exam B

::: tip
We suggest not looking at the assignments before you have time to solve them.
:::

## 1. Entry Requirement
**Sum integers using map and apply in Racket**

The user will enter a single line of numbers separated by spaces to the standard input. Your task is parse them and print out their sum.

Hint: `map`, `read-line`, `string-split`, `apply`

Example:
```
1 2 3
6
```

::: details Exam Solution
```racket
(let* ((line (read-line))
       (words (string-split line))
       (nums (map string->number words))
       (sum (apply + nums)))
  (displayln sum))
```
:::

## 2. Racket Main Task (15 points)
**[Spiral Matrix](/exams/tasklist.md#spiral-matrix)**

A *Spiral Matrix* is a square $n\times n$-matrix filled with natural numbers,
starting from $1$ in the top-left corner, increasing in inward, clockwise spiral order, like these examples:
$$
\mathbf{S}_3=\left(\begin{array}{rrr}
  1 & 2 & 3\\
  8 & 9 & 4\\
  7 & 6 & 5
\end{array}\right)
\qquad
\mathbf{S}_5=\left(\begin{array}{rrrrr}
  1 & 2 & 3 & 4 & 5\\
  16 & 17 & 18 &19 & 6\\
  15 & 24 & 25 & 20 & 7\\
  14 & 23 & 22 & 21 & 8\\
  13 & 12 & 11 & 10 & 9
\end{array}\right)
$$

Even though one can define a spiral matrix for each size $n$, your task is to implement a function generating
a spiral $n\times n$-matrix only for odd $n$. Note that such matrices can be generated recursively because
the spiral matrix $\mathbf{S}_n$ of size $n\times n$ can be constructed from the spiral matrix $\mathbf{S}_{n-2}$
of size $(n-2)\times(n-2)$ as follows:

$$
\mathbf{S}_n=\left(
  \begin{array}{rrrrr}
    1 & 2 & \cdots & n-1 & n \\
    4n-4 &  &  &  & n+1 \\
    \vdots & &\mathbf{B} & &\vdots \\
    3n-1 &  &  &   & 2n-2\\
    3n-2 & 3n-3 & \cdots & 2n & 2n-1
  \end{array}
\right)
$$
where $\mathbf{B}$ is basically the matrix $\mathbf{S}_{n-2}$ whose all elements are increased by $4n-4$.

### Implementation

In Racket, implement a function `(spiral-matrix n)` that accepts a positive odd integer
and returns the spiral matrix $\mathbf{S}_n$ of size `n`.
A matrix is represented as a list of its rows, e.g. $\mathbf{S}_3$ is represented as
```racket
'((1 2 3)
  (8 9 4)
  (7 6 5))
```

Your file is to be called `spiral-matrix.rkt` and must provide the function `spiral-matrix`.
Hence, the head of your file should start with
```racket
#lang racket
(provide spiral-matrix)

; your code here
```

### Hint
To generate a sequences of numbers, you may use the function `(range start end step)` generating
a sequence of numbers starting at `start` and whose successive elements are computed by adding
`step` until `end`. Note that the element `end` is excluded. E.g.
```racket
> (range 5 0 -1)
'(5 4 3 2 1)
```

### Examples
The following shows the behaviour of the `spiral-matrix` function.

```racket
> (spiral-matrix 3)
'((1 2 3)
  (8 9 4)
  (7 6 5))
```

```racket
> (spiral-matrix 1)
'((1))
```

::: details Exam Solution
```racket
#lang racket

(provide spiral-matrix)


(define (mat-add xss n)
	(map (curry map (curry + n)) xss))


(define (wrap x ys z)
  (cons x (append ys (list z))))


(define (spiral-matrix n)
  (define (extendH x)
    (map wrap
         (range (- (* 4 n) 4) (- (* 3 n) 2) -1)
         x
         (range (+ n 1) (- (* 2 n) 1))))

  (define (extendV x)
    (wrap (range 1 (+ 1 n))
          x
          (range (- (* 3 n) 2) (- (* 2 n) 2) -1)))

  (if (equal? 1 n)
    '((1))
    (let ((smaller (spiral-matrix (- n 2))))
	(extendV (extendH (mat-add smaller (- (* 4 n) 4)))))))
```
:::


## 3. Haskell Main Task (15 points)
**Photographing [Skyscrapers](/exams/tasklist.md#photographing-skyscrapers)**

#

You are an avid photographer that is obsessed with regular structures and you want to take pictures
of cities that are built on regular grids. It turns out that you are also really into roof tops so
you want to see as many of them in your pictures as possible. Naturally, you wonder from which side
of a given city (North/South/East/West) you should be taking the picture. Luckily a befriended
architect gave you maps of the cities you want to photograph. The maps are very simplified and can
be represented as lists of lists of integers, so for example in Scheme:

```racket
;    north
(define city
  '((3 0 3 7 3)
    (2 5 5 1 2)
    (6 5 3 3 2)
    (3 3 5 4 9)
    (3 5 3 9 0)))
;    south
```
Every number represents the height of a building.

A roof is *visible* if all other roofs between it and the edge of the grid are *smaller* than it.
Only consider roofs in the same row or column. The first roof at the edge of a grid is always
visible.

### Implementation

In Haskell, write a function `bestView city` that outputs the direction with the most roofs visible,
along with the number of roofs visible from that direction. The direction should be one of four
characters: `'N'`, `'S'`, `'E'`, or `'W'`. The result should be a pair in the format `(direction,
number)`.

```haskell
city = [[3, 3, 3],
        [1, 2, 3],
        [1, 2, 3]]

-- 'N' has 3 roofs, 'S' has 5, 'E' has 3, and 'W' is the best with 7
bestView city -- ('W', 7)
```

Your file should be called `Skyscrapers.hs` and should export the `bestView` function.
```haskell
module Skyscrapers (bestView) where

bestView :: [[Int]] -> (Char, Int)
bestView city = ... -- Implement me!
```

::: details Exam Solution
```haskell
 module Skyscrapers (bestView) where

import Data.List

roofs xss = sum $ inner <$> xss
  where
    inner xs = length (group $ scanl1 max xs)

morph 'N' = transpose
morph 'S' = fmap reverse . transpose
morph 'E' = fmap reverse
morph _ = id

bestView :: [[Int]] -> (Char, Int)
bestView city =
  let dirs = "NSEW"
      views = roofs . (`morph` city) <$> dirs
      opts = zip dirs views
  in last $ sortOn snd opts
```
:::