---
outline: deep
---

# Homework assignments

## Requirements

We will publish 4 homework assignments for **50 points** in total.
* In total, from all homeworks, you have to reach at least **25 points**
* You need at least **one point from every homework**
* Late submissions: We deduct one point per day until +1 point is left on your score.


## 01: ASCII Art (Racket)

::: danger Important
Submit your solution as a zip archive containing a single file with the name **`hw1.rkt`**.
:::

The description of the assignment can be found [here](hw01).

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


<!--
/*
To fulfill your wish from the lecture, I provide an extra test case for the first homework assignment with Arnold Schwarzenegger. The input image is
{{https://drive.google.com/file/d/11lT2PHHRaa_DKIEl0VOylh0T9VQ1o5E1/view?usp=sharing|here}}. The output for the block size 8x16 and ''chars=" .,:;ox%#@"'' is {{https://drive.google.com/file/d/1hZUSwl__bxdBsJrbFtTBtPeyepZlnhzK/view?usp=sharing|here}}. 
*/

/*
==== Homework assignment 2 ====

<note important>Submit your solution as a zip archive containing a single file named **hw2.rkt**.</note>
<note important>Using the built-in function ''eval'' is not allowed!</note>

The assignment description can be found [[https://drive.google.com/file/d/1eA9_4UfsAW_ueexL47_CZnW1pD5vMuFm/view?usp=share_link|here]].

===== Haskell =====

==== Homework assignment 3 ====

<note important>Submit your solution as a zip archive containing a single file named **Hw3.hs**.</note>

The description of the assignment can be found [[https://drive.google.com/file/d/1kAKrg4dRf6Z5BDXk7-O1B5Inhh-cIaNS/view?usp=share_link|here]].

==== Homework assignment 4 ====

<note important>Submit your solution as a zip archive containing a single file with the name **Hw4.hs**.</note>

The description of the assignment can be found [[https://drive.google.com/file/d/1rXAKtlkkRFhzfrKPY0LkbYQIXG6LApuM/view?usp=sharing|here]].
The module ''Parser.hs'' can be downloaded [[https://drive.google.com/file/d/1zVbVpSl0UphPmglpGa94P0TQI3OVMPAL/view?usp=sharing|here]].

*-->
