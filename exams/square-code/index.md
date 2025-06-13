---
title: "Exam Task: Square Code"
subtitle: "From: Exam 6 - 2022"
outline: deep
---

# Square Code

One classic method for composing secret messages is called a *square code*.  First, the input is
normalized: the spaces and punctuation are removed from the English text, and the message is
downcased.  Then, the normalized characters are broken into rows.  These rows can be regarded as
forming a rectangle. For example, the sentence
```
If man was meant to stay on the ground, god would have given us roots.
```
is normalized to:
```
ifmanwasmeanttostayonthegroundgodwouldhavegivenusroots
```
The normalized string is 54 characters long, so it is written into a rectangle with 7 rows and 8 columns.
```
  ifmanwas
  meanttos
  tayonthe
  groundgo
  dwouldha
  vegivenu
  sroots
```
Note that the last row is padded with spaces at the end to make it 8 characters long.

The coded message is obtained by reading down the columns going left to right.
For example, the message above is coded as:
```
imtgdvs fearwer mayoogo anouuio ntnnlvt wttddes aohghn  sseoau
```

Given the length $n\in\mathbb{N}$ of the normalized text, the number of columns $c\in\mathbb{N}$ in the rectangle
is computed by $c = \lceil\sqrt{n}\rceil$, i.e.,
$c$ is the smallest natural number greater than or equal to $\sqrt{n}$.

## Racket

In Racket, implement a function `(encode str)` that accepts a string
and returns the encoded string as described above.
Your file is to be called `square-code.rkt` and must provide the function `encode`.
Hence, the head of your file should start with
```racket
#lang racket
(provide encode)

; your code here
```

### Hint
To make the string downcased, you may use the function `(string-downcase str)`.
To remove spaces and punctuation, you may use the predicate `(char-alphabetic? ch)`.
To get the least integer above a real number $x$, use the function `(exact-ceiling x)`.

### Examples
The following shows the behaviour of the `encode` function.

```racket
> (encode "If man was meant to stay on the ground, god would have given us roots.")
"imtgdvs fearwer mayoogo anouuio ntnnlvt wttddes aohghn  sseoau "
```

```racket
> (encode "Have a nice day!")
"hae and via ecy"
```

::: details Exam Solution
```racket
#lang racket

(provide encode)

(define (normalize str)
  (filter char-alphabetic? (string->list (string-downcase str))))

(define (get-num-cols n)
  (exact-ceiling (sqrt n)))

(define (make-rows lst n)
  (let ([len (length lst)])
    (cond
      ([null? lst] '())
      ([< len n] (list (append lst (make-list (- n len) #\space))))
      (else (cons (take lst n) (make-rows (drop lst n) n))))))

(define (transpose lst)
  (apply map list lst))

(define (encode str)
  (let* ([lst (normalize str)]
         [n (get-num-cols (length lst))]
         [rows (transpose (make-rows lst n))])
    (string-join (map list->string rows))))
```
:::

## Haskell

In Haskell, implement a function `encode :: String -> String` that accepts a string
and returns the encoded string as described above.
Your task is to be called `SquareCode.hs` and must export the `encode` function.
Hence, the head of your file should read

```haskell
module SquareCode ( encode ) where
import Data.Char

-- your code goes here
```

### Hint
To make the string downcased, you may use the function `toLower :: Char -> Char`.
To remove spaces and punctuation, you may use the predicate `isAlpha :: Char -> Bool`.
Both functions are located at the module `Data.Char`.
To get the least integer above a real number, use the function `ceiling :: Integral b => a -> b`.

### Examples
The following shows the behaviour of the `encode` function.

```haskell
> encode "If man was meant to stay on the ground, god would have given us roots."
"imtgdvs fearwer mayoogo anouuio ntnnlvt wttddes aohghn  sseoau "
```

```haskell
> encode "haveaniceday"
"hae and via ecy"
```

::: details Exam Solution
```haskell
module SquareCode ( encode ) where
import Data.Char
import Data.List

normalize :: String -> String
normalize s = [toLower c | c <- s, isAlpha c]

getNumCols :: Int -> Int
getNumCols n = ceiling $ sqrt $ fromIntegral n

makeRows :: String -> Int -> [String]
makeRows s n | s == "" = []
             | len < n = [s ++ replicate (n-len) ' ']
             | otherwise = take n s:makeRows (drop n s) n
        where len = length s

encode :: String -> String
encode s = unwords rows'
        where ns = normalize s
              n = getNumCols $ length ns
              rows = makeRows ns n
              rows' = transpose rows
```
:::
