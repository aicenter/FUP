---
outline: deep
---
<SolutionHider/>

# Lab 1: Introduction to Racket

In this lab, you will familiarize yourself with the IDE that we will use to write Racket programs.

## DrRacket IDE

You can use [the free DrRacket IDE](https://racket-lang.org/) (available for Linux, Windows, and Mac) that is pre-installed on the lab computers, or you can install it on your own laptop. If you use a package manager to install Racket, the IDE usually comes bundled with it. *Don't hesitate to ask for help!*

Documentation for Racket and DrRacket is accessible directly via the **Help / Help-Desk** menu; Use it To get familiar with the software.

::: tip
DrRacket supports multiple languages. To select *Racket*, your first line of code should be:
```racket
#lang racket
```
:::

## Exercises - Racket Basics
Start with an interactive session in the REPL. Keep in mind that Racket uses prefix notation for all functions, and try to compute a simple formula, such as $2 + \frac{3}{5}$.

### Exercise 1
Write a recursive function `my-even?` that decides whether a number is even, using only the functions `+`, `-`, and `=`. The function should not use mutual recursion.

::: details Solution for `my-even?`
```racket
(define (my-even? n)
  (cond
    [(< n 0) (my-even? (- n))]
    [(= n 0) #t]
    [(= n 1) #f]
    [else (my-even? (- n 2))]))
```
:::

### Exercise 2
Using the function `string-append`, create a function `(string-repeat n str)` which takes two arguments: an integer `n` and a string `str`. The function should return a string consisting of `n` repetitions of `str`. For example, `(string-repeat 3 "abc")` should return `"abcabcabc"`.

::: details Solution for `string-repeat`
```racket
(define (string-repeat n str)
  (if (<= n 0)
      ""
      (string-append str (string-repeat (- n 1) str))))
```
:::

### Exercise 3
Rewrite the function from Exercise 2 so that it uses tail recursion.

::: details Solution for a tail-recursive `string-repeat`
```racket
(define (string-repeat n str [acc ""])
  (if (<= n 0)
      acc
      (string-repeat (- n 1) str (string-append acc str))))
```
:::

### Exercise 4
Write a function `(char-inclusive-range fst lst)` that takes two characters and returns a string consisting of a sequence of consecutive characters, starting with `fst`, ending with `lst`, and following the order in the ASCII table. For example, `(char-inclusive-range #\A #\D)` should produce `"ABCD"`, while `(char-inclusive-range #\z #\u)` should yield `"zyxwvu"`. To convert characters into their positions in the ASCII table, use the functions `char->integer` and `integer->char`. To convert a character into a string, apply the `string` function.

::: details Solution: `char-inclusive-range`
::: code-group
```racket [basic]
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

```racket [helpers]
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

Write a function `num-of-digits` that takes an integer `n` and computes the number of digits in its standard decimal representation. For example, `(num-of-digits 123)` is 3 and `(num-of-digits -3456)` is 4.

::: tip
The number of digits can be computed by successively dividing the input number by 10. For integer division, you can use the function `quotient`.
:::

::: details Solution { hideme }
```racket
(define (num-of-digits n [acc 1])
  (define q (quotient n 10))
  (if (= q 0) acc (num-of-digits q (+ acc 1))))
```
Thank you for the improved solution, <u>@Denis Pak</u>!
:::

### Task 2
Write a function `(num->str n [radix 10])` that takes an integer `n` as input, along with an optional `radix` parameter specifying the base in which to represent `n` (e.g., 2 for binary, 10 for decimal, or 16 for hexadecimal). The function should return a string representing `n` in the specified numerical system. Use the standard symbols `0123456789ABCDEF` for the representation. Try the following examples:
  * `(num->str 52)` $\rightarrow$ `"52"`,
  * `(num->str 5 2)` $\rightarrow$ `"101"`,
  * `(num->str 255 16)` $\rightarrow$ `"FF"`.

::: tip
You can obtain the representation by consecutively dividing `n` by the `radix` and collecting the remainders. The remainder after integer division can be computed using the function `remainder`.
:::


::: details Solution { hideme }
::: code-group
```racket [direct]
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
```racket [string-ref]
; Alternatively, you can use `string-ref` to pick out the correct character from
; `numeric-alphabet` given an index, and instead of `string-append`ing you can
; construct a list of characters which is converted to a string at the end
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
