---
outline: deep
---

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
