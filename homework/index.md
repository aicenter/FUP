---
outline: deep
---

# Homework

We will publish four homework assignments on [BRUTE](https://cw.felk.cvut.cz/brute) for **50 points** in total:

| Assignment | Language | Opening | Deadline | Points |
| - | -------- | ------- | -------- | ------:|
| [ASCII Art](/homework/hw01.md) | Racket   | 05.03.  | 25.03.   |     10 |
| [SVGen Interpreter](/homework/hw02.md) | Racket   | 19.03.  | 08.04.   |     15 |
| [λ-Calculus Evaluator](/homework/hw03.md) | Haskell  | 09.04.  | 02.05.   |     12 |
| [Parser of λ-Programs](/homework/hw04.md) | Haskell  | 30.04.  | 20.05.   |     13 |

## Requirements

You have to reach at least **25 points** in total, and You need **at least one point** from every homework. For each day that a homework assignment is submitted past the deadline, a penalty of one point will be deducted, until only one point is left on your score.

Keep in mind our academic principles — [no plagiarism](https://cw.fel.cvut.cz/wiki/help/common/plagiaty_opisovani), avoid sharing code snippets directly, and try to come up with a solution by yourself first.

## Note on the private test data

A part of any programming assignment is generating test cases and testing the produced algorithm. It
is a very important skill each programmer has to master, regardless of the programming paradigm.
Please, take the result of the automatic evaluation as a hint that you should continue this process.
Do not ask for the private test data and do not ask what is wrong with your output in the evaluation
system. In the real world, no one will give you even this feedback and if you release a product with
serious mistakes, you will suffer losses in money and reputation.

However, if you spend a long time (at least 4 hours) without any progress and you have
created a decent set of your own test cases that pass without problems - I encourage you to write
to your lab teachers to help you design additional test cases or possibly help to identify the
mistakes in your code.

For writing your test cases, you can use the unit testing framework provided by Racket. To write
unit tests, it suffices to import the rackunit module into your code. Then it is possible to test
your functions by the function ''check-equal?'' comparing the expected output with the actual
output. Consult also your local documentation in DrRacket to get more details on rackunit.

```scheme
#lang racket
(module+ test
  (require rackunit))

(define (only-numbers lst) (filter number? lst))

(module+ test
  (check-equal? (only-numbers '(1 a 2 3)) '(1 2) "You made an error!"))
```
