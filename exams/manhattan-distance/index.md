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
