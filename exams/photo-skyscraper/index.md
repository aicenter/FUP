---
outline: deep
title: "Exam Task: Photographing Skyscrapers"
subtitle: "From: Exam 1 - 2023"
---

# Photographing Skyscrapers

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


## Racket

In Racket, write a function `(best-view city)` that outputs the direction with the most roofs
visible, along with the number of roofs visible from that direction. The direction should be one of
four symbols: `'N`, `'S`, `'E`, and `'W`. The result should be a pair of the format
`'(direction . number)`.

```racket
(define city
  '((3 3 3)
    (1 2 3)
    (1 2 3)))

; 'N has 3 roofs, 'S has 5, 'E has 3, and 'W is the best with 7
> (best-view city)
'(W . 7)
```

Your file should be called `photo-skyscraper.rkt` and should `provide` the `best-view` function.
```racket
#lang racket

(provide best-view)

(define (best-view city)
  ; Implement me!
  )
```

::: details Exam Solution
```racket
#lang racket
(provide best-view)

(define (visible-roofs-row row [height 0] [n 0])
  (if (empty? row)
      n
      (if (< height (car row))
          (visible-roofs-row (cdr row) (car row) (+ n 1))
          (visible-roofs-row (cdr row) height n))))

(define (transpose mat) (apply map list mat))


(define (visible-roofs city dir)
  (define m
    (match dir
      ['W city]
      ['E (map reverse city)]
      ['N (transpose city)]
      ['S ((compose (curry map reverse) transpose) city)]))
  (apply + (map visible-roofs-row m)))


(define (best-view city)
  (define views (list (list 'N (visible-roofs city 'N))
                      (list 'S (visible-roofs city 'S))
                      (list 'E (visible-roofs city 'E))
                      (list 'W (visible-roofs city 'W))))
  (define (inner m v) (if (< (cadr m) (cadr v)) v m))
  (define sol (foldl inner (list 'None 0) views))
  (cons (car sol) (cadr sol)))
```
:::



## Haskell

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

Your file should be called `PhotoSkyscraper.hs` and should export the `bestView` function.
```haskell
module PhotoSkyscraper (bestView) where

bestView :: [[Int]] -> (Char, Int)
bestView city = ... -- Implement me!
```

::: details Exam Solution
```haskell
 module Task4 (bestView) where

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
