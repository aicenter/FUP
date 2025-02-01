---
outline: deep
---

# Lab 1: Introduction to Racket

In this lab you will familiarize yourself with the IDE we will use to write Racket programs.

## DrRacket IDE

You can use [the free DrRacket IDE](https://racket-lang.org/) (for Linux, Windows, and MAC) that is pre-installed on the lab computers, or you can install it on your own laptop.
If you use a package manager to install Racket, the IDE usually comes bundled in. Don't hesitate to ask for help!

The documentation for Racket and DrRacket is accessible directly via the **Help / Help-Desk** menu; Use it To get familiar with the software.

DrRacket supports multiple languages. To select *Racket*, your first line of code should be:
```scheme
#lang racket
```

## Exercises - Racket basics
Start with an interactive session in the REPL. Keep in mind that Racket uses prefix notation for all functions, and try to compute a
simple formula, such as $2+3/5$.

### Exercise 1
Write a recursive function `my-even?` that decides whether a number is even using
only functions `+`, `-`, `=` (without mutual recursion).

::: details Solution of `my-even?`
```scheme
(define (my-even? n)
  (cond
    [(< n 0) (my-even? (- n))]
    [(= n 0) #t]
    [(= n 1) #f]
    [else (my-even? (- n 2))]))
```
:::



### Exercise 2
Using the function `string-append`, create a function `(string-repeat n str)` which takes as arguments an
integer `n`, a string `str`, and returns a string consisting of `n` repetitions of `str`. For example
`(string-repeat 3 "abc") => "abcabcabc"`.

::: details Solution of `string-repeat`
```scheme
(define (string-repeat n str)
  (if (<= n 0)
      ""
      (string-append str (string-repeat (- n 1) str))))
```
:::


### Exercise 3
Rewrite the function from Exercise 2 so that it uses tail recursion.

::: details Solution for a tail recursive `string-repeat`
```scheme
(define (string-repeat n str [acc ""])
  (if (<= n 0)
      acc
      (string-repeat (- n 1) str (string-append acc str))))
```
:::

### Exercise 4
Write a function `(char-inclusive-range fst lst)` which takes two characters and returns a string
consisting of a sequence of consecutive characters starting with `fst`, ending with `lst`, and
following the order in the ASCII table.
For example `(char-inclusive-range #\A #\D) => "ABCD"` or  `(char-inclusive-range #\z #\u) => "zyxwvu"`.
For converting characters into positions in the ASCII table, use functions
`char->integer` and `integer->char`. To convert a character into a
string, apply the function `string`.

::: details Solution: `char-inclusive-range`
::: code-group
```scheme [basic]
(define (integer->string i)
  (string (integer->char i)))

(define (char-inclusive-range fst lst)
  (define first-index (char->integer fst))
  (define last-index (char->integer lst))
  (define step (if (< first-index last-index) 1 -1))
  (define (iter k acc)
    (if (= k last-index)
        (string-append acc (integer->string k))
        (iter (+ k step)
              (string-append acc (integer->string k)))))
  (iter first-index ""))
```

```scheme [helpers]
; Alternatively, and maybe slightly more elegantly, you can define two helper
; functions `char+1` and `char-1` and use an accumulator:

(define (char+1 c) (integer->char (add1 (char->integer c))))
(define (char-1 c) (integer->char (sub1 (char->integer c))))

(define (char-inclusive-range fst lst [acc ""])
  (cond
    [(char=? fst lst) (string-append acc (string fst))]
    [(char<? fst lst)
     (char-inclusive-range (char+1 fst) lst (string-append acc (string fst)))]
    [(char>? fst lst)
     (char-inclusive-range (char-1 fst) lst (string-append acc (string fst)))]))
```
:::

## Tasks

Try to solve the following individual tasks.

### Task 1

Write a function `num-of-digits` which takes an integer `n` and computes the number of digits `n`
has in the standard decimal representation. For example `(num-of-digits 123) => 3` or
`(num-of-digits -3456) => 4`.

::: tip
The number of digits can be computed by successive dividing the input number by 10. For
the integer division, you can use the function `quotient`.
:::

::: details Solution
```scheme
(define (num-of-digits n [acc 1])
  (define q (quotient n 10))
  (if (= q 0) acc (num-of-digits q (+ acc 1))))
```
Thank you for the improved solution <u>@Denis Pak</u>!
:::

### Task 2
Write a function `(num->str n [radix 10])` taking as input an integer `n` together
with `radix` denoting the number of symbols used to represent the number `n` (for example 2,10,16
for binary, decimal, hexadecimal representation respectively). This function returns a string
containing the representation of `n` in the corresponding numerical system. For the representation,
use the standard symbols 0123456789ABCDEF.

Examples:
  * `(num->str 52) => "52"`,
  * `(num->str 5 2) => "101"`,
  * `(num->str 255 16) => "FF"`.

::: tip
The representation can be obtained by consecutive division of `n` by `radix` and collecting the
remainders. The remainder after integer division can be computed by the function `remainder`.
:::


::: details Solution
```scheme
(define (num->str n [radix 10])
  (define rem (remainder n radix))
  (define initial (if (< rem 10)
                      (char->integer #\0)
                      (- (char->integer #\A) 10)))
  (define rem-str
    (string (integer->char (+ initial rem))))

  (if (< n radix)
      rem-str
      (string-append (num->str (quotient n radix) radix)
                     rem-str)))
```

Alternatively, you can use `string-ref` to pick out the correct character from `numeric-alphabet`
given an index, and instead of `string-append`ing you can construct a list of characters which is
converted to a string in the end:
```scheme
(define numeric-alphabet "0123456789ABCDEF")

(define (num->char n [radix 10])
  (string-ref numeric-alphabet (remainder n radix)))

(define (num->str n [radix 10] [acc '()])
  (cond
    [(negative? n) (num->str (- n) radix '(#\-))]
    [(< n radix) (list->string (cons (num->char n radix) acc))]
    [else (num->str (quotient n radix) radix (cons (num->char n radix) acc))]))
```
:::
