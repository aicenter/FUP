# Example Exam A

::: tip
We suggest not looking at the assignments before you have time to solve them.
:::

## 1. Entry Requirement
**Check if a word from standard input is a palindrome in Haskell**

The user will enter a single word to the standard input. Your task is to check if the word is exactly the same when read forwards and backwards, output the answer and exit.
If it is, print "palindrome" to standard output, otherwise print "not a palindrome".

Example:
```
hey
not a palindrome
```

::: details Exam Solution
```haskell
main = do
    word <- getLine
    if word == reverse word then
        putStrLn "palindrome"
    else
        putStrLn "not a palindrome"
```
:::

## 2. Racket Main Task (15 points)
**Check if an [$N^2$-Knights](/exams/tasklist.md#knights) board is valid**

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


### Implementation

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


## 3. Haskell Main Task (15 points)
**Pretty-print [Binary Numbers](/exams/tasklist.md#pretty-printing-of-binary-numbers)**

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

### Implementation

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