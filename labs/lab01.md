---
outline: deep
---

# Lab 1: Introduction to Racket

This lab aims to familiarize the students with the IDE we will use for Racket and help them write
simple programs.

## Dr. Racket IDE

The IDE can be downloaded for free for Linux, Windows, and MAC from:
https://racket-lang.org/

The students can use the one installed in the lab computers. The teacher may help the students (to a
reasonable degree) to get the IDE running on students’ laptops.

Get familiar with the definition window and REPL in DrRacket. The documentation of implemented
functions is accessible via Help Desk in the menu.

DrRacket allows writing programs in several languages. We focus on Racket, so your first line in the
code should be:
```scheme
#lang racket
```

## Exercises - Racket basics
Start interaction in REPL. Racket uses prefix notation for all functions. Let students compute
simple formulas, e.g.,  $2+3/5$.

### Exercise 1
Write a recursive function `my-even?` that decides whether a number is even using
only functions `+`, `-`, `=` (without mutual recursion).

::: details Solution
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

Using the function `string-append`, create a function `(copy-str n str)` taking as arguments an
integer `n`, a string `str` and returns a string consisting of `n`-many copies of `str`. For example
`(copy-str 3 "abc") => "abcabcabc"`.

::: details Solution
```scheme
(define (copy-str n str)
  (if (<= n 0)
      ""
      (string-append str (copy-str (- n 1) str))))
```
:::


### Exercise 3

Rewrite the function from Exercise 2 so that it uses tail recursion.

::: details Solution
```scheme
(define (copy-str n str [acc ""])
  (if (<= n 0)
      acc
      (copy-str (- n 1) str (string-append acc str))))
```
:::

### Exercise 4
Write a function `(consecutive-chars fst lst)` which takes two characters and returns a string
consisting of a sequence of consecutive characters starting with `fst`, ending with `lst`, and
following the order in the ASCII table.
For example `(consecutive-chars #\A #\D) => "ABCD"` or  `(consecutive-chars #\z #\u) => "zyxwvu"`.
For converting characters into positions in the ASCII table, use functions
`<nowiki>char->integer</nowiki>` and `<nowiki>integer->char</nowiki>`. To convert a character into a
string, apply the function `string`.

::: details Solution
```scheme
(define (integer->string i)
  (string (integer->char i)))

(define (consecutive-chars fst lst)
  (define first-index (char->integer fst))
  (define last-index (char->integer lst))
  (define step (if (< first-index last-index) 1 -1))
  (define (iter k acc)
    (if (= k last-index)
        (string-append acc (integer->string k))
        (iter (+ k step)
              (string-append acc (integer->string k)))))
  (iter first-index ""))

; alternatively:
#|
(define (char+1 c) (integer->char (add1 (char->integer c))))
(define (char-1 c) (integer->char (sub1 (char->integer c))))

(define (consecutive-chars fst lst [acc ""])
  (cond
    [(char=? fst lst) (string-append acc (string fst))]
    [(char<? fst lst)
     (consecutive-chars (char+1 fst) lst (string-append acc (string fst)))]
    [(char>? fst lst)
     (consecutive-chars (char-1 fst) lst (string-append acc (string fst)))]))
|#

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

<!--
::: details Solution
```scheme
(define (num-of-digits n [acc 1])
  (cond [(< n 0) (num-of-digits (- n))]
        [(< n 10) acc]
        [else (num-of-digits (quotient n 10) (+ acc 1))]))
```
:::
-->

## Task 2
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


<!--
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

; Alternative
#|
(define numeric-alphabet "0123456789ABCDEF")
(define (num->char n [radix 10])
  (string-ref numeric-alphabet (remainder n radix)))
(define (num->str n [radix 10] [acc '()])
  (cond
    [(negative? n) (num->str (- n) radix '(#\-))]
    [(< n radix) (list->string (cons (num->char n radix) acc))]
    [else (num->str (quotient n radix) radix (cons (num->char n radix) acc))]))
|#
```
:::-->

