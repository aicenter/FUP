---
outline: deep
title: "Exam Task: $N^2$-Knights"
subtitle: ""
---

# $N^2$-Knights

In chess, a knight can move to any square that is two squares away horizontally and one square away
vertically, or two squares vertically and one square horizontally. Therefore, its complete move
looks like the letter "L".

<img src="/img/n2-knights.png" style="max-width: 50%; display: block; margin: 0 auto;">

The $N^2$-knights puzzle concerns placing $O(N^2)$ knights on an $n \times n$ chessboard so that no
two knights can attack each other.  Below, you can see a valid configuration for a 8x8 board.

<img class="inverting-image" src="/img/n2-knights-max.png" style="max-width: 50%; display: block; margin: 0 auto;">

Determine the validity of $N^2$-knights board configurations.

### Hints


A possible brute force solution generates a list of all knight coordinates on the board, and then
checks their pairwise consistency.
Unlike for $N$-queens, there can be $O(N^2)$ knights in a valid $N^2$-knights configuration, see the
second figure.


## Racket

In Racket, implement the function

```racket
(valid? board)
```

where `board` is a list of lists representing an arbitrarily sized board containing binary values,
where 1 denotes a knight and 0 an empty cell; The function returns `#t` if and only if no knight
threatens another one. Your file should be called `knights.rkt` and `provide` the `valid?`
function.

### Examples
```racket
(valid? ‘((1 0 0 0)
          (0 0 0 1)
          (1 0 0 0)
          (0 1 0 0)))
#t

(valid? '((0 1 0 0)
          (0 0 0 1)
          (1 0 0 0)
          (0 0 1 0)))
#f
```


::: details Exam Solution
```racket
#lang racket

(provide valid?)

(define (generate-coords-row row offset row-id)
  (if (null? row)
      '()
      (if (eq? (car row) 1)
          (cons (cons row-id offset) (generate-coords-row (cdr row) (+ offset 1) row-id))
          (generate-coords-row (cdr row) (+ offset 1) row-id))))

(define (generate-coords-board board offset)
  (if (null? board)
      '()
      (append (generate-coords-row (car board) 0 offset) (generate-coords-board (cdr board) (+ offset 1)) )))

(define (valid-pair? coord1 coord2)
  (let ((absx (abs (- (car coord1) (car coord2))))
        (absy (abs (- (cdr coord1) (cdr coord2)))))
       (cond ((and (eq? absx 1) (eq? absy 2)) #f)
             ((and (eq? absx 2) (eq? absy 1)) #f)
             (#t #t))))

(define (valid-coord? coord coords)
  (cond ((null? coords) #t)
        ((valid-pair? coord (car coords)) (valid-coord? coord (cdr coords)))
        (#t #f)))

(define (valid-coords? coords)
  (cond ((null? coords) #t)
        ((valid-coord? (car coords) (cdr coords)) (valid-coords? (cdr coords)))
        (#t #f)))

(define (valid? board)
  (let ((coords (generate-coords-board board 0)))
    (valid-coords? coords)))
```
:::



## Haskell

In Haskell, implement the function

```haskell
isValid :: [[Piece]] -> Bool
```

where

* `data Piece = Nil | Knight`, with `Knight` denoting a knight and `Nil` an empty cell with no
  piece; Note, the `Piece` type must be defined in your code.
* The function input represents an arbitrarily sized board containing `Nil` and `Knight` pieces.

and the function returns `True` if and only if no knight threatens another one.

Your file should be called `Knights.hs`, contain a module of the same name, and export the `isValid` function.

### Examples

```haskell
> isValid [[Knight, Nil, Nil ,Nil],
           [Nil, Nil, Nil, Knight],
           [Knight, Nil, Nil, Nil],
           [Nil, Knight, Nil, Nil]]
True

> isValid [[Nil, Knight, Nil, Nil],
           [Nil, Nil, Nil, Knight],
           [Knight, Nil, Nil, Nil],
           [Nil, Nil, Knight, Nil]]
False
```

::: details Exam Solution
```haskell
module Knights (Piece (..), isValid) where

data Piece = Nil | Knight deriving (Show)

enumerate :: [a] -> [(Int, a)]
enumerate xs = zip [0 ..] xs

isKnight :: Piece -> Bool
isKnight Nil = False
isKnight Knight = True

knightCoords :: [[Piece]] -> [(Int, Int)]
knightCoords board = map (\(i, j, _) -> (i, j)) ij_ks
  where
    ij_ks = filter (\(_, _, k) -> isKnight k) (concat ij_xs)
    ij_xs = map (\(i, jxs) -> insert' i jxs) (enumerate (map enumerate board))

    insert' :: a -> [(b, c)] -> [(a, b, c)]
    insert' x = map (\(y, z) -> (x, y, z))

isValidPair :: (Int, Int) -> (Int, Int) -> Bool
isValidPair (x, y) (u, v)
  | ((abs (x - u)) == 2 && (abs (y - v)) == 1) = False
  | ((abs (x - u)) == 1 && (abs (y - v)) == 2) = False
  | otherwise = True

isValid :: [[Piece]] -> Bool
isValid board = and [isValidPair x y | x <- cs, y <- cs]
  where
    cs = knightCoords board
```
:::
