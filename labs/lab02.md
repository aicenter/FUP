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
```scheme
(define (my-reverse lst [acc '()])
  (if (null? lst)
      acc
      (my-reverse (cdr lst) (cons (car lst) acc))))
```
Note that using accumulators is a general concept.
:::

## Exercise 2

Write a function `(letter-frequencies str)` which takes a string `str` and returns a histogram of
letters occurring in `str` so that the most frequent characters come first. The histogram is just a
list of pairs `(char . num)` where `char` is a character and `num` is the number of its occurrences
in `str`. E.g. `(letter-frequencies "good") => ((#\o . 2) (#\d . 1) (#\g . 1))`.
The string `str` should be first converted into lowercase characters, so that `#\A` and `#\a`
represent the same character. Non-alphabetic characters should be removed.

**Idea:** The function `letter-frequencies` is just a composition of several functions.
```scheme
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
  `sort` takes a boolean function as its second argument, taking two arguments and comparing them.
- The function `group-same` scans the input list and returns a list consisting of lists of the same
  consecutive characters, e.g., `(group-same '(#\c #\c #\z)) => ((#\c #\c) (#\z))`. 
- The function `join-lengths` creates for each group of the same character a pair of the for `(char .
  num)` where the number of occurrences num is computed by function `length`.
- Finally, the output is sorted by the number of occurrences. 

The function `group-same` is the only recursive function in our program. It has to keep a partially
built group of the same character as an intermediate result. If the new character `(car l)` coming
from the list is the same as the current character in the group, the partial group is extended by
this character. Once the new character `(car l)` differs from the current character in the group,
the partial group is closed, joined to the output, and a new group is created.

::: details Solution
```scheme
(define (group-same lst)
  (define (iter l gr)
    (cond
      [(null? l) (list gr)]
      [(eqv? (car gr) (car l)) (iter (cdr l) (cons (car gr) gr))]
      [else (cons gr (iter (cdr l) (list (car l))))]))
  (if (null? lst)
      '()
      (iter (cdr lst) (list (car lst)))))

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
   
; Might be more readable:
#|
(define (letter-frequencies-2 str)
  (let* [(lowercase (string-downcase str))
         (listified (string->list lowercase))
         (alphabetic (filter char-alphabetic? listified))
         (sorted-chars (sort alphabetic char<?))
         (grouped (group-same sorted-chars))
         (joined (join-lengths grouped))
         (sorted-occurs (sort joined > #:key cdr))]
    sorted-occurs))
|#

```
:::


If you wish, you can use function `file->string` to check letter frequencies in any file, for
instance, in Shakespeare's
[Sonnets](https://drive.google.com/file/d/1fFrMtcTdlt3GHHFkDnuxM7igCVs_JmtZ/view?usp=sharing) by
calling `(letter-frequencies (file->string "sonnets.txt"))` and comparing the result with the letter
frequencies in English alphabet [Wikipedia](https://en.wikipedia.org/wiki/Letter_frequency).

## Task 1
Write a function `(average-list lst)` taking a list of numbers `lst` and returning their
arithmetical average. E.g. `(average-lst '(1 2 3)) => 2`. The function should be tail-recursive. 

**Hint:** As the function should be tail-recursive, it has to use an accumulator storing a partial
sum of elements from the list. Finally, the resulting sum is divided by the number of all elements
in the list. For the number of elements in `lst`, you can use the function `length`.  Depending on
your implementation function can return precise rational numbers like `(average-list '(0 1)) =>
1/2`. If you want to have the usual floating-point representation, use the function
`exact->inexact`, transforming the result into the imprecise floating-point representation.

::: details Solution
```scheme
(define (average-list lst)
  (define (iter l acc)
    (if (null? l)
        acc
        (iter (cdr l) (+ acc (car l)))))
  (exact->inexact (/ (iter lst 0) (length lst))))      
```
:::


## Task 2
Taking an inspiration from the `group-same` function, write a function `(split-list n lst)` which
takes a natural number `n` and a list `lst` and returns a list of lists consisting of `n`-tuples of
consecutive elements from `lst`, e.g.:
```scheme
(split-list 2 '(a b 1 2 3 4)) => ((a b) (1 2) (3 4))
```
In case the number of elements is not divisible by `n`, make the last list in the output shorter,
e.g.:
```scheme
(split-list 3 '(a b 1 2)) => ((a b 1) (2))
```

Using functions `split-list` and `average-list` from the previous task, write a function
`(n-block-average n lst)` which splits a given list of numbers `lst` into `n`-tuples of consecutive
numbers and returns a list of averages of these `n`-tuples. E.g. `(n-block-average 2 '(1 3 1 5)) =>
(2 3)`.

**Hint:** The function `split-list` needs two accumulators. The first accumulator keeps a partially
built segment of consecutive elements. The second tracks how many elements we must read from the
list to complete the `n`-tuple of consecutive elements.

::: details Solution
```scheme
(define (split-list n lst)
  (define (iter l k segment)
    (cond
      [(null? l) (list segment)]
      [(zero? k) (cons segment (iter l n '()))]
      [else (iter (cdr l) (- k 1) (append segment (list (car l))))]))
  (iter lst n '()))
  
(define (n-block-average n lst)
  (map average-list (split-list n lst)))
```
:::
