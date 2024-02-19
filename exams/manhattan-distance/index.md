---
title: "Exam Task: Manhattan Distance"
subtitle: "From: Exam 5 - 2023"
date: "2024-01-09"
author: "Niklas Heim"
author-url: ""
return-url: '../../'
return-text: '← Return home'
---

# Manhattan Distance

Suppose we have a list of named coordinates:

```scheme
(define points
 '((#\A 1 1)
   (#\B 1 6)
   (#\C 8 3)
   (#\D 3 4)
   (#\E 5 5)
   (#\F 8 9)))
```

They can be printed on a 2D grid (with dimensions according to the largest x and y coordinates,
and `(0, 0)` on the top left)
```
.........
.A.......
.........
........C
...D.....
.....E...
.B.......
.........
.........
........F
```

Your task is to fill each `.` with the lower case letter of the closest coordinate according to the
Manhattan distance. We call the resulting grid the nearest-neighbour grid.
```
aaaaa.ccc
aAaaa.ccc
aaaddeccc
aadddeccC
..dDdeecc
bb.deEeec
bBb.eeee.
bbb.eeeff
bbb.eefff
bbb.ffffF
```

The grid points which still contain a `.` represent tied points, i.e. points which have at least two
equally close neighbours.  The Manhattan distance between two points is defined by

$$
d = |x_1 - x_2| + |y_1 - y_2|
$$


## Racket implementation

In Racket, write a function (grid points) that accepts a list of named points and computes the
nearest neighbour grid. The output should be a list of strings where each string represents one row
in the grid.

```scheme
#lang racket
(provide grid)

(define (grid points)
    ; Implement me!
)

(define points
 '((#\A 1 1)
   (#\B 1 6)
   (#\C 8 3)
   (#\D 3 4)
   (#\E 5 5)
   (#\F 8 9)))

> (grid points)
'("aaaaa.ccc"
  "aAaaa.ccc"
  "aaaddeccc"
  "aadddeccC"
  "..dDdeecc"
  "bb.deEeec"
  "bBb.eeee."
  "bbb.eeeff"
  "bbb.eefff"
  "bbb.ffffF")
```

### Hints

The following functions might be helpful.

To get lower case characters/strings: `char-downcase` and `string-downcase`.
```scheme
> (char-downcase #\A)
#\a
```

To construct a string from a list of characters `list->string`.
```scheme
> (list->string '(#\A #\B))
"AB"
```

::: details Solution
```scheme
scheme solution
```
:::




## Haskell Implementation
In Haskell, write a function `grid :: [(Char,Int,Int)] -> [[Char]]` that computes the nearest
neighbour grid. The output should be a list of strings where each string represents one row in the
grid.

```haskell
module Task4 (grid) where
import Data.Char
import Data.List

grid :: [(Char,Int,Int)] -> [[Char]]
-- grid = Implement me!

points :: [(Char, Int, Int)]
points = [
       ('A', 1, 1),
       ('B', 1, 6),
       ('C', 8, 3),
       ('D', 3, 4),
       ('E', 5, 5),
       ('F', 8, 9)]


> grid points
["aaaaa.ccc"
,"aAaaa.ccc"
,"aaaddeccc"
,"aadddeccC"
,"..dDdeecc"
,"bb.deEeec"
,"bBb.eeee."
,"bbb.eeeff"
,"bbb.eefff"
,"bbb.ffffF"]
```

### Hints

The following functions might be helpful.

`toLower` from `Data.Char` to convert an upper case character to a lower case character
```haskell
ghci> toLower 'A'
'a'
```

From the library `Data.List` you can use your favourite sorting function like `sortBy` or `sortOn`.


::: details Solution
```haskell
module Task4 (grid) where
import Data.List
import Data.Char (toLower)

sortDists :: [(a, Int)] -> [(a, Int)]
sortDists = sortBy (\(_,x) (_,y) -> compare x y)

manhattan (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

dists points (x,y) = map (\(c,a,b) -> (c, manhattan (x,y) (a,b))) points

character ((cx,0):_) = cx
character ((cx,_):[]) = toLower cx
character ((cx,dx):(cy,dy):_) = if dx==dy then '.' else toLower cx

grid points = [[closest (i,j) | i <- [0..w]] | j<-[0..h]] where
  closest = character . sortDists . dists points
  w = maximum (map second points)
  h = maximum (map third points)

first (x,_,_) = x
second (_,x,_) = x
third (_,_,x) = x

points :: [(Char, Int, Int)]
points = [
       ('A', 1, 1),
       ('B', 1, 6),
       ('C', 8, 3),
       ('D', 3, 4),
       ('E', 5, 5),
       ('F', 8, 9)]

-- grid points
-- > ["aaaaa.ccc"
--   ,"aAaaa.ccc"
--   ,"aaaddeccc"
--   ,"aadddeccC"
--   ,"..dDdeecc"
--   ,"bb.deEeec"
--   ,"bBb.eeee."
--   ,"bbb.eeeff"
--   ,"bbb.eefff"
--   ,"bbb.ffffF"]

```
:::

