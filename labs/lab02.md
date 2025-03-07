<SolutionHider/>

# Lab 02: Lists & Trees

The main purpose of this lab is to practice elementary recursive manipulation with lists. Lists can be
decomposed by functions `car` and `cdr`. On the other hand, lists can be built by functions
`cons`, `list` or `append`. Also, higher-order functions `filter` and `map` can be used as
they were introduced in the second lecture (`map` only applied to a single list).

## Exercise 1
Write the function `(my-reverse lst)` taking a list `lst` and returning a list consisting of
elements from `lst` in the reverse order. E.g. `(my-reverse '(a b c)) => (c b a)`. The function
should use the tail recursion.

::: tip
The idea is to use an accumulator `acc` storing the intermediate result. We start with an empty
accumulator and recursively deconstruct the list `lst` element by element by means of `car,cdr` and
join them to the accumulator by `cons`. The computation for `lst = '(a b c)` and `acc = '()` go as
follows:

| `(cdr lst)` | `(cons (car lst) acc)` |
|-------------|------------------------|
| '(b c)      | '(a)                   |
| '(c)        | '(b a)                 |
| '()         | '(c b a)               |
:::


::: details Solution
```racket
(define (my-reverse lst [acc '()])
  (if (null? lst)
      acc
      (my-reverse (cdr lst) (cons (car lst) acc))))
```
Note that using accumulators is a general concept.
:::

## Exercise 2
Write the function `group-same` that scans an input list and returns a list consisting of lists of the same
consecutive characters, for example `(group-same '(b b a a a c))` should return `'((b b) (a a a) (c))`.

The recursive function `group-same` should keep a partially
built group of the same character as an intermediate result. If the new character `(car l)` coming
from the list is the same as the current character in the group, the partial group is extended by
this character. Once the new character `(car l)` differs from the current character in the group,
the partial group is closed, joined to the output, and a new group is created.

::: details Solution
```racket
(define (group-same lst)
  (define (iter l gr)
    (cond
      [(null? l) (list gr)]
      [(eqv? (car gr) (car l)) (iter (cdr l) (cons (car gr) gr))]
      [else (cons gr (iter (cdr l) (list (car l))))]))
  (if (null? lst)
      '()
      (iter (cdr lst) (list (car lst)))))
```
:::

## Exercise 3

Write a function `(letter-frequencies str)` which takes a string `str` and returns a histogram of
letters occurring in `str` so that the most frequent characters come first. The histogram is just a
list of pairs `(char . num)` where `char` is a character and `num` is the number of its occurrences
in `str`. E.g. `(letter-frequencies "good") => ((#\o . 2) (#\d . 1) (#\g . 1))`.
The string `str` should be first converted into lowercase characters, so that `#\A` and `#\a`
represent the same character. Non-alphabetic characters should be removed.

**Idea:** The function `letter-frequencies` is just a composition of several functions.
```racket
string-downcase -> string->list -> filter-alphabetic -> sort
  -> group-same -> join-lengths -> sort
```
- The function `string-downcase` translates all characters into lowercase letters, and it is
  implemented in Racket.
- The function `string->list` is implemented as well, and it decomposes a given string into a list
  of its characters.
- Then non-alphabetic characters can be filter out by `filter` function using the predicate
  `char-alphabetic?`.
- To compute the number of occurrences of characters, we apply the `sort` function, which groups
  together the same characters, e.g., `(sort '(#\c #\z #\c) char<?) => (#\c #\c #\z)`. The function
  `sort` takes a binary function as its second argument, taking two arguments and comparing them.
- The function `join-lengths` creates for each group of the same character a pair of the form `(char .
  num)` where the number of occurrences num is computed by function `length`.
- Finally, the output is sorted by the number of occurrences.

::: details Solution
::: code-group
```racket [nested]
(define (join-lengths grs)
  (map (lambda (g) (cons (car g) (length g))) grs))

(define (letter-frequencies str)
  (sort
   (join-lengths
    (group-same
     (sort
      (filter char-alphabetic? (string->list (string-downcase str)))
      char<?)))
   >
   #:key cdr))
```
```racket [let*]
(define (group->count group)
  (cons (car group) (length group)))

(define (letter-frequencies str)
  (let* ((downed (string-downcase str))
         (listed (string->list downed))
         (filtered (filter char-alphabetic? listed))
         (sorted (sort filtered char<?))
         (grouped (group-consecutive sorted))
         (counted (map group->count grouped))
         (result (sort counted > #:key cdr)) )
    result))
```
:::

Try reading text from a file with `file->string` or from the standard input with `port->string`.
Compare the letter frequencies in [Shakespeare's Sonnets](/extra/shakespeare.txt) to their
[relative frequencies](https://en.wikipedia.org/wiki/Letter_frequency) in the English language.

## Task 1
Write a function `(list-average lst)` taking a list of numbers `lst` and returning their
arithmetical average. E.g. `(list-average '(1 2 3)) => 2`. The function should be tail-recursive.

**Hint:** As the function should be tail-recursive, it has to use an accumulator storing a partial
sum of elements from the list. Finally, the resulting sum is divided by the number of all elements
in the list. For the number of elements in `lst`, you can use the function `length`.  Depending on
your implementation function can return precise rational numbers like `(list-average '(0 1)) =>
1/2`. If you want to have the usual floating-point representation, use the function
`exact->inexact`, transforming the result into the imprecise floating-point representation.

::: details Solution { hideme }
```racket
(define (list-average lst)
  (define (iter l acc)
    (if (null? l)
        acc
        (iter (cdr l) (+ acc (car l)))))
  (exact->inexact (/ (iter lst 0) (length lst))))
```
:::


## Task 2
Taking an inspiration from the `group-same` function, write a function `(list-split n lst)` which
takes a natural number `n` and a list `lst` and returns a list of lists consisting of `n`-tuples of
consecutive elements from `lst`, e.g.:
```racket
(list-split 2 '(a b 1 2 3 4)) => ((a b) (1 2) (3 4))
```
In case the number of elements is not divisible by `n`, make the last list in the output shorter,
e.g.:
```racket
(list-split 3 '(a b 1 2)) => ((a b 1) (2))
```

Using functions `list-split` and `list-average` from the previous task, write a function
`(n-block-average n lst)` which splits a given list of numbers `lst` into `n`-tuples of consecutive
numbers and returns a list of averages of these `n`-tuples. E.g. `(n-block-average 2 '(1 3 1 5)) =>
(2 3)`.

**Hint:** The function `list-split` needs two accumulators. The first accumulator keeps a partially
built segment of consecutive elements. The second tracks how many elements we must read from the
list to complete the `n`-tuple of consecutive elements.

::: details Solution { hideme }
```racket
(define (list-split n lst)
  (define (iter l k segment)
    (cond
      [(null? l) (list segment)]
      [(zero? k) (cons segment (iter l n '()))]
      [else (iter (cdr l) (- k 1) (append segment (list (car l))))]))
  (iter lst n '()))

(define (n-block-average n lst)
  (map list-average (list-split n lst)))
```
:::
