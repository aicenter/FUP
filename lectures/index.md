---
outline: deep
---
# Lectures

As we proceed through the lectures, I will try to write down lecture notes so that you have most of
the course material in a single place. However, I am unsure if I will manage to be on time with them
due to other obligations.

## Racket

| # | Title | Content | Files |
|-|-|-|-|
|1. | [Introduction](lecture01) | Discusses the key ideas behind functional programming. It further introduces the programming language [Racket](https://racket-lang.org/).  | [Slides](https://github.com/aicenter/FUP/blob/main/lectures/lecture01.pdf).  [Log](https://github.com/aicenter/FUP/blob/main/code/lecture01.rkt).  |
| 2. | [Lists & Trees](lecture02) | Focuses on Racket lists and trees. Further, it introduces the unit testing library [Rackunit](https://docs.racket-lang.org/rackunit/index.html). | [Slides](https://github.com/aicenter/FUP/blob/main/lectures/lecture02.pdf).  [Log](https://github.com/aicenter/FUP/blob/main/code/lecture02.rkt).|
| 3. | [Higher Order Functions](lecture03) | Deals with higher-order functions like `map`, `filter`, `foldl`, function closures and Racket structures.| [Slides](https://github.com/aicenter/FUP/blob/main/lectures/lecture03.pdf).  [Log](https://github.com/aicenter/FUP/blob/main/code/lecture03.rkt).|
| 4. | [Lazy Evaluation](lecture04) | Introduces pattern matching, and explains how to implement lazy evaluation and streams in Racket. | [Slides](https://github.com/aicenter/FUP/blob/main/lectures/lecture04.pdf).  [Log](https://github.com/aicenter/FUP/blob/main/code/lecture04.rkt).|
| 5. | [Macros & Interpreters](lecture05) | Briefly introduces syntactic macros, and shows how to implement interpreters (the latter is remains to be written). | [Slides](https://github.com/aicenter/FUP/blob/main/lectures/lecture05.pdf).  [Log](https://github.com/aicenter/FUP/blob/main/code/lecture05.rkt).  [Brainf*ck.rkt](https://github.com/aicenter/FUP/blob/main/code/lecture05-brainfuck.rkt). |
| 6. | [Lambda Calculus](lecture06) | Describes the basics of lambda calculus to show you were most of the initial ideas for functional programming came from. | [Slides](https://github.com/aicenter/FUP/blob/main/lectures/lecture06.pdf).  [Lambda-calculus.rkt](https://github.com/aicenter/FUP/blob/main/code/lambda-calculus.rkt). |
| Bonus | [Immutable datastructures](bonus) | For the interested reader there is another lecture on immutable datastructures like random access lists. |

## Haskell

| # | Title | Content | Files |
|-|-|-|-|
| 7. | [Introduction to Haskell](lecture07) | Introduces Haskell as a compiled, statically-typed, and lazy language. | [Slides](https://github.com/aicenter/FUP/blob/main/lectures/lecture07.pdf).  [Log](https://github.com/aicenter/FUP/blob/main/code/lecture07.hs). |
| 8. | [Haskell Types](lecture08) | Discusses the strongly-typed, static, and inferred type system of Haskell including: parametric polymorphism, ad-hoc polymorphism (typeclasses), and algebraic datatypes. | [Slides](https://github.com/aicenter/FUP/blob/main/lectures/lecture08.pdf).  [Log](https://github.com/aicenter/FUP/blob/main/code/lecture08.hs). |
| 9. | [Type Classes](lecture09) | We discuss some more examples of type classes, most importantly `Functor`s. | [Slides](https://github.com/aicenter/FUP/blob/main/lectures/lecture09.pdf).  [Log](https://github.com/aicenter/FUP/blob/main/code/lecture09.hs).|
| 10. | [Haskell's IO & Monads](lecture10) | Introduces Haskell's `IO` and the typeclass `Monad`. | [Slides](https://github.com/aicenter/FUP/blob/main/lectures/lecture10.pdf).  [Log](https://github.com/aicenter/FUP/blob/main/code/lecture10.hs).|
| 11. | [Monadic Parsing](lecture11) | Uses `Functor` and `Monad` instances from the previous lecture to demonstrate the elegance of monadic parsing. | [Slides](https://github.com/aicenter/FUP/blob/main/lectures/lecture11.pdf).  [Log](https://github.com/aicenter/FUP/blob/main/code/lecture11.hs).  [`Parser.hs`](https://github.com/aicenter/FUP/blob/main/code/Parser.hs). |
| 12. | [State Monad](lecture12) | Make repetitive, stateful boilerplate disappear via the `State` monad. | [Slides](https://github.com/aicenter/FUP/blob/main/lectures/lecture12.pdf).  [Log](https://github.com/aicenter/FUP/blob/main/code/lecture12.hs).  [`State.hs`](https://github.com/aicenter/FUP/blob/main/code/State.hs).  [`StateIO.hs`](https://github.com/aicenter/FUP/blob/main/code/StateIO.hs). |
| 13. | [Lecture 13](lecture13) | Dissecting `foldr` into `Monoid`s and `Foldable`s. | [Slides](https://github.com/aicenter/FUP/blob/main/lectures/lecture13.pdf).  [Log](https://github.com/aicenter/FUP/blob/main/code/lecture13.hs).  [Dataset](https://github.com/aicenter/FUP/blob/main/code/FUP-hw.csv). |
| Bonus | [Parallel Haskell](lecture14) | Introduces Haskell's spark system and demonstrates how to use `Strategy` types for simple parallelization of existing Haskell programs. | [`pfold.hs`](https://github.com/aicenter/FUP/blob/main/code/pfold.hs).  [`parmaze.hs`](https://github.com/aicenter/FUP/blob/main/code/parmaze.hs). |

## Old recorded lectures

Old recorded lectures from 2021 can be found [here](https://cw.fel.cvut.cz/b202/courses/fup/lectures/start).


