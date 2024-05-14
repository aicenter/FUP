---
outline: deep
title: "Exam Task: Minesweeper"
subtitle: "From: Exam 2 - 2022"
---

# Minesweeper

Implement a program to mark the number of mines directy adjacent (horizontally, vertically and
diagonally) to squares on a Minesweeper field.


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

Your file should have the extension `.rkt`. You may assume the input is rectangular and non-empty.
The functions `string->list` and `string-split` might be useful to parse the output of the function
`port-lines` which reads from stdin. You can use the following skeleton. 
```scheme
; for testing
(define test-board
  (map string->list (string-split ".*..\n..*.\n**..\n...*\n*...")))

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


## Haskell

To read multiple lines of input in plain Haskell, we have to tell it how many lines to read.
Assume that this will be done by first reading a number and then calling `getLine` the desired
number of times. Implement a function `readInput :: IO [String]` with the following behaviour
```haskell
ghci> readInput
3
...
...
...
["...","...","..."]  <-- this is the function output
```

Your file should have the extension `.hs`.

```haskell
-- for converting ints to chars
import Data.Char (intToDigit)

-- for testing
test_board = ["..."
             ,".**"
             ,"..."]

main = do
  lines <- readInput
  putStrLn "\nSweep Result:"
  let sw = sweep lines
  mapM_ putStrLn sw
```
