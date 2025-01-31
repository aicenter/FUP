---
outline: deep
---
# Lecture Notes

The following lecture notes cover most of the course material.

## Racket

| # | Title | Content | Files |
|-|-|-|-|
|1. | [Introduction](lecture01) | Discusses the key ideas behind functional programming. It further introduces the programming language [Racket](https://racket-lang.org/).  | [Slides](/slides/lecture01.pdf).  [Log](/code/lecture01.rkt).  |
| 2. | [Lists & Trees](lecture02) | Focuses on Racket lists and trees. Further, it introduces the unit testing library [Rackunit](https://docs.racket-lang.org/rackunit/index.html). | [Slides](/slides/lecture02.pdf).  [Log](/code/lecture02.rkt).|
| 3. | [Higher Order Functions](lecture03) | Deals with higher-order functions like `map`, `filter`, `foldl`, function closures and Racket structures.| [Slides](/slides/lecture03.pdf).  [Log](/code/lecture03.rkt).|
| 4. | [Lazy Evaluation](lecture04) | Introduces pattern matching, and explains how to implement lazy evaluation and streams in Racket. | [Slides](/slides/lecture04.pdf).  [Log](/code/lecture04.rkt).|
| 5. | [Macros & Interpreters](lecture05) | Briefly introduces syntactic macros, and shows how to implement interpreters. | [Slides](/slides/lecture05.pdf).  [Log](/code/lecture05.rkt).  [Brainf*ck.rkt](/code/lecture05-brainfuck.rkt). |

## Lambda Calculus

| # | Title | Content | Files |
|-|-|-|-|
| 6. | [Lambda Calculus](lecture06) | Describes the basics of lambda calculus to show you were most of the initial ideas for functional programming came from. | [Slides](/slides/lecture06.pdf).  [Lambda-calculus.rkt](/code/lambda-calculus.rkt). |

## Haskell

| # | Title | Content | Files |
|-|-|-|-|
|  7. | [Introduction to Haskell](lecture07) | Introduces Haskell as a compiled, statically-typed, and lazy language. | [Slides](/slides/lecture07.pdf).  [Log](/code/lecture07.hs). |
|  8. | [Haskell Types](lecture08) | Discusses the strongly-typed, static, and inferred type system of Haskell including: parametric polymorphism, ad-hoc polymorphism (typeclasses), and algebraic datatypes. | [Slides](/slides/lecture08.pdf).  [Log](/code/lecture08.hs). |
|  9. | [Type Classes](lecture09) | We discuss some more examples of type classes, most importantly `Functor`s. | [Slides](/slides/lecture09.pdf).  [Log](/code/lecture09.hs).|
| 10. | [Haskell's IO & Monads](lecture10) | Introduces Haskell's `IO` and the typeclass `Monad`. | [Slides](/slides/lecture10.pdf).  [Log](/code/lecture10.hs).|
| 11. | [Monadic Parsing](lecture11) | Uses `Functor` and `Monad` instances from the previous lecture to demonstrate the elegance of monadic parsing. | [Slides](/slides/lecture11.pdf).  [Log](/code/lecture11.hs).  [`Parser.hs`](/code/Parser.hs). |
| 12. | [State Monad](lecture12) | Make repetitive, stateful boilerplate disappear via the `State` monad. | [Slides](/slides/lecture12.pdf).  [Log](/code/lecture12.hs).  [`State.hs`](/code/State.hs).  [`StateIO.hs`](/code/StateIO.hs). |
| 13. | [Lecture 13](lecture13) | Dissecting `foldr` into `Monoid`s and `Foldable`s. | [Slides](/slides/lecture13.pdf).  [Log](/code/lecture13.hs).  [Dataset](/code/FUP-hw.csv). |

## Bonus

| # | Title | Content | Files |
|-|-|-|-|
| 1. | [Immutable datastructures](bonus_immutable) | For the interested reader there is another lecture on immutable datastructures like random access lists. |
| 2. | [Parallel Haskell](bonus_parallel) | Introduces Haskell's spark system and demonstrates how to use `Strategy` types for simple parallelization of existing Haskell programs. | [`pfold.hs`](/code/pfold.hs).  [`parmaze.hs`](/code/parmaze.hs).|


# Old Recorded Lectures

Old recorded lectures from 2021 can be found [here](https://cw.fel.cvut.cz/b202/courses/fup/lectures/start).


