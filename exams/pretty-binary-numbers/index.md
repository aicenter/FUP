---
title: "Exam Task: Pretty Printing of Binary Numbers"
subtitle: "From: Exam 1 - 2022"
date: "2024-01-12"
outline: deep
---

# Pretty Printing of Binary Numbers


The goal of this assignment is to implement a program that reads a positive integer from
standard input, converts it into its binary representation, and displays this representation as a
text image. Each digit (i.e., $0$ and $1$) is represented as a $4 \times 4$ grid of characters.

Implement a program that works as follows:

1. Display a message `Enter integer:`.
2. Let the user enter an integer $n$ (you may assume only valid inputs).
3. Convert $n$ into its binary representation.
4. Display the binary representation using the above text-images. The particular digits must be
   separated by a column consisting of the character `'.'`.

Below you can see an example run were the user enters the number 12.
```sh
Enter integer:
12
...#....#..##...##.
..##...##.#..#.#..#
...#....#.#..#.#..#
...#....#..##...##.
```

## Racket

Your program should be called `binary.rkt`. The text images of digits $0$ and $1$ are rendered in Racket like below:

```racket
(define img-zero
  '(".##."
    "#..#"
    "#..#"
    ".##."))

(define img-one
  '("...#"
    "..##"
    "...#"
    "...#"))
```

You can display lines by `displayln`.

::: details Exam Solution
```racket
#lang racket

(define img-zero
  '(".##."
    "#..#"
    "#..#"
    ".##."))

(define img-one
  '("...#"
    "..##"
    "...#"
    "...#"))

(define (concat-imgs imgs)
  (define (inner img1 img2)
    (map (lambda (s t) (string-append s "." t)) img1 img2))
  (foldl inner (first imgs) (rest imgs)))

(define (number->digits n radix)
  (define (iter num acc)
    (if (< num radix)
        (reverse (cons num acc))
        (iter (quotient num radix) (cons (remainder num radix) acc))))
  (iter n '()))

(define/match (digit->picture digit)
  ((0) img-zero)
  ((1) img-one))

(define (main)
  (displayln "Enter integer:")
  (define input (read))
  (define digits (number->digits input 2))
  (define imgs (map digit->picture digits))
  (displayln (string-join (concat-imgs imgs) "\n")))

(main)
```
:::

## Haskell

Your program should be called `Binary.hs`. The text images of digits $0$ and $1$ are rendered in Haskell like below:

```haskell
    type Img = [String]

    zero :: Img
    zero = [".##.",
            "#..#",
            "#..#",
            ".##."]

    one :: Img
    one =  ["...#",
            "..##",
            "...#",
            "...#"]
```

You can display lines by `putStrLn`.

::: details Exam Solution
```haskell
type Img = [String]

empty :: Img
empty = replicate 4 ""

zero :: Img
zero = [".##.",
        "#..#",
        "#..#",
        ".##."]

one :: Img
one =  ["...#",
        "..##",
        "...#",
        "...#"]

concat2Img :: Img -> Img -> Img
concat2Img = zipWith (\s t -> s ++ "." ++ t)

concatImgs :: [Img] -> Img
concatImgs (im:ims) = foldl concat2Img im ims
concatImgs _ = empty

numToStr :: Int -> Int -> String
numToStr n radix = if  n < radix then [chars !! n]
                   else numToStr d radix ++ [chars !! r]
                 where chars = ['0'..'9'] ++ ['A'..'F']
                       d = n `div` radix
                       r = n `mod` radix

binToImgs :: String -> [Img]
binToImgs = map (\c -> case c of
                         '0' -> zero
                         '1' -> one)

genSol :: Int -> String
genSol n = unlines strs
        where strs = concatImgs $ binToImgs $ numToStr n 2

main :: IO ()
main = do putStrLn "Enter integer:"
          n <- read <$> getLine
          let bin = numToStr n 2
          let ims = binToImgs bin
          mapM_ putStrLn $ concatImgs ims
```
:::
