---
outline: deep
title: "Exam Task: $N^2$-Knights"
subtitle: ""
---

# $N^2$-Knights

## Problem Definition

In chess, a knight can move to any square that is two squares away horizontally and one square away
vertically, or two squares vertically and one square horizontally. Therefore, its complete move
looks like the letter "L".

<img src="/img/n2-knights.png" style="max-width: 50%; display: block; margin: 0 auto;">

The $N^2$-knights puzzle concerns placing $O(N^2)$ knights on an $n \times n$ chessboard so that no
two knights can attack each other.  Below, you can see a valid configuration for a 8x8 board.

<img src="/img/n2-knights-max.png" style="max-width: 50%; display: block; margin: 0 auto;">

Determine the validity of $N^2$-knights board configurations.

### Hints


A possible brute force solution generates a list of all knight coordinates on the board, and then
checks their pairwise consistency.
Unlike for $N$-queens, there can be $O(N^2)$ knights in a valid $N^2$-knights configuration, see the
second figure.


## Racket

In Racket, implement the function

```scheme
(is_valid? board)
```

where `board` is a list of lists representing an arbitrarily sized board containing binary values,
where 1 denotes a knight and 0 an empty cell; The function returns `#t` if and only if no knight
threatens another one. Your file should be called `knights.rkt` and `provide` the `is_valid?`
function.

### Examples
```scheme
(is_valid? ‘((1 0 0 0)
             (0 0 0 1)
             (1 0 0 0)
             (0 1 0 0)))
#t

(is_valid? '((0 1 0 0)
             (0 0 0 1)
             (1 0 0 0)
             (0 0 1 0)))
#f
```





## Haskell

In Haskell, implement the function

```haskell
is_valid :: [[Piece]] -> Bool
```

where

* `data Piece = Nil | Knight`, with `Knight` denoting a knight and `Nil` an empty cell with no
  piece; Note, the `Piece` type must be defined in your code.
* The function input represents an arbitrarily sized board containing `Nil` and `Knight` pieces.

and the function returns `True` if and only if no knight threatens another one.

Your file should be called `Knights.hs`, contain a module of the same name, and export the `is_valid` function.

### Examples

```haskell
> is_valid [[Knight, Nil, Nil ,Nil],
            [Nil, Nil, Nil, Knight],
            [Knight, Nil, Nil, Nil],
            [Nil, Knight, Nil, Nil]]
True

> is_valid [[Nil, Knight, Nil, Nil],
            [Nil, Nil, Nil, Knight],
            [Knight, Nil, Nil, Nil],
            [Nil, Nil, Knight, Nil]]
False
```
