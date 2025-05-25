---
title: "Exam Task: Minesweeper"
subtitle: "From: Exam 2 - 2022"
outline: deep
---

# Minesweeper

Implement a program to mark the number of mines directy adjacent (horizontally, vertically and
diagonally) to squares on a rectangular Minesweeper field.


Fields with `.` denote an empty field and `*` denote mines:
```
.*.*.
..*..
..*..
.....
```
Your program should output the following
```
1*3*1
13*31
.2*2.
.111.
```

You can break this task down into multiple sub-problems. Instead of solving the whole board, solve
the problem for a specific square $(x,y)$:

1. Get all the neighbours of $(x,y)$ (staying in-bounds!).

$$
\text{neighbours}(\text{board}, (x,y)) = [
    \text{board}_{(x-1,y-1)},
    \text{board}_{(x-1,y)},
    \text{board}_{(x-1,y+1)}, \dots]]
$$

2. Count the number of mines among the neighbours.
3. Create a helper function capturing the printing rules: If the current square is a mine, return a
   star; otherwise, return the count of neighboring mines, or a dot if there are none.
4. Now apply the procedure to every index.

## Racket

Your file should be called `minesweeper.rkt`. You may assume the input is rectangular and non-empty.
The functions `string->list` and `string-split` might be useful to parse the output of the function
`port-lines` which reads from stdin. You can use the following skeleton.
```racket
; for converting ints to chars.
(define (int->digit i) (integer->char (+ 48 i)))

(let* ((input-string (port->lines))
        ; implement parsing of board/sweep for mines
        ; assuming counted-board contins a list of list of chars
       (sn (map list->string counted-board)))
  (for ((l sn))
    (display l)
    (newline)))
```

### Example

```racket
(define board (map string->list (string-split "·*·*·\n··*··\n··*··\n·····")))
```

::: details Exam Solution
```racket
#lang racket

(define (int->digit i)
  (integer->char (+ 48 i)))

(define (finalizer nu st)
  (match (cons nu st)
    ((cons _ #\*) #\*)
    ((cons 0 _) #\.)
    ((cons n _) (int->digit n))))

(define (neighbours bss mx my x y)
  (for*/list ((i (range (max 0 (- y 1)) (min my (+ y 2))))
	      (j (range (max 0 (- x 1)) (min mx (+ x 2)))))
    (list-ref (list-ref bss i) j)))

(define (num nei y x m)
  (let* ((neis (nei x y))
	 (fnei (filter (curry equal? #\*) neis))
	 (countMines (length fnei)))
  (finalizer countMines m)))

(define (sweep board)
  (define mx (length (car board)))
  (define my (length board))
  (define nei (curry neighbours board mx my))
  (define (inner y xs) (map (curry num nei y) (range 0 mx) xs))
  (map inner (range 0 my) board))

(let* ((ss (port->lines))
       (ls (map string->list ss))
       (ln (sweep ls))
       (sn (map list->string ln)))
  (for ((l sn))
    (display l)
    (newline)))
```
:::

## Haskell

Write a Haskell program called `Minesweeper.hs`, which reads the board from standard input. To get all data from standard input as a string, you can call `getContents`. The resulting string can then be split with the `lines` function.

```haskell
-- for converting ints to chars
import Data.Char (intToDigit)

sweep :: [String] -> [String]
sweep board = board -- implement me !

main = do
  input <- getContents
  mapM_ putStrLn (sweep (lines input))
```

### Example

```haskell
-- for testing
test_board = ["..."
             ,".**"
             ,"..."]
```

::: details Exam Solution
```haskell
import Control.Monad (replicateM)
import Data.Char (intToDigit)

slice :: Int -> Int -> [a] -> [a]
slice f t xs = drop f $ take t xs

neighbours :: Int -> Int -> [String] -> String
neighbours x y board = slice (y-1) (y+2) board >>= slice (x-1) (x+2)


sweep :: [String] -> [String]
sweep board = mapi2d pretty board where

  -- fill coordinates with numbers/*/.
  mapi2d f = zipWith (\y-> zipWith (\x e-> f e x y) [0..]) [0..]
  pretty e x y = finalizer (mines x y) e
  finalizer _ '*' = '*'
  finalizer 0 _ = '.'
  finalizer n _ = intToDigit n

  -- compute number of mines at coords
  mines x y = count (neighbours x y board)
  count xs = length $ filter (=='*') xs

main = do
  input <- getContents
  mapM_ putStrLn (sweep (lines input))
```
:::
