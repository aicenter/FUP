---
outline: deep
---
# Organization

This course introduces students to the techniques of _**functional programming**_, the advantages
and disadvantages of this programming paradigm, and its use in practice.
This approach is **declarative** in the sense that the programmer symbolically describes the problem to be solved
rather than specifying the exact sequence of operations required to solve it. It allows focusing on
the essence of the solved problem and implementing even more complex algorithms compactly.
Functional programming has notable advantages for **parallelization** and **automated verification**
of algorithms, and the most useful functional programming concepts are increasingly often introduced
to standard programming languages. Because of the focus of functional programming on symbols rather
than numbers, functional programming has been heavily used in artificial intelligence fields, such
as agent systems or symbolic machine learning.

The course consists of weekly [lectures](lectures/) and [labs](labs/) which will be published
as we go along the course.

## Course outline

* **Lisp/Scheme/Racket**
    * simple syntax (directly matches $\lambda$-calculus)
    * dynamically typed
    * code-as-data (easy to write interpreters, ...)
    * allows mutable data
* **$\lambda$-calculus**
* **Haskell**
    * pure functional language
    * statically typed
    * rich type system
    * strictly separates the pure core from the mutable shell

## Homework

You will have to solve four homework assignments (for **50 points** in total):
* 2 assignments in Racket
* 2 assignments in Haskell
More on the details of the homeworks [here](homeworks/).

## Exam
The final *programming* exam has **30 points**.
Final, optional *oral* exam for **20 points**.
More on the details of the exam [here](exams/).

## Grading
The grading is the standard grading scale:
|  A   |  B  |  C  |  D  |  E  |  F  |
|------|-----|-----|-----|-----|-----|
|91-100|81-90|71-80|61-70|51-60| 0-50|


## Teachers

| Name             | Consulting hours     | E-mail                 | Room    | Role                 |
|------------------|----------------------|------------------------|---------|----------------------|
| Rostislav Horčík | appointment by email | <xhorcik@fel.cvut.cz>  | KN:E-322| Lecturer             |
| Niklas Heim      | appointment by email | <heimnikl@fel.cvut.cz> | KN:E-406| Lecturer & Instructor|
| Tomáš Votroubek  | appointment by email | <votroto1@fel.cvut.cz> |         | Instructor           |
| Matěj Zorek      | appointment by email | <zorekmat@fel.cvut.cz> |         | Instructor           |
| Jiří Němeček     | appointment by email | <nemecj38@fel.cvut.cz> |         | Instructor           |


## Other resources

* Harold Abelson and Gerald Jay Sussman and Julie Sussman: [Structure and Interpretation of Computer
  Programs](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book.html), MIT Press,
  1996. 

* R. Kent Dybvig: [The Scheme Programming Language](https://www.scheme.com/tspl4/), Fourth Edition, MIT Press, 2009.

* Raul Rojas: [A Tutorial Introduction to the Lambda Calculus](http://www.inf.fu-berlin.de/lehre/WS03/alpi/lambda.pdf)

* Greg Michaelson: An Introduction to Functional Programming Through Lambda Calculus, Dover edition, 2011. 

* Graham Hutton: Programming in Haskell, Cambridge University Press, 2016.


* Scheme/Racket:
    * [Beautiful Racket](https://beautifulracket.com/)
    * [Lexical scopes](https://docs.racket-lang.org/guide/eval.html)
    * [Lazy evaluation](https://sites.ualberta.ca/~jhoover/325/CourseNotes/section/Scheme_3.htm )
    * [Side effects in scheme](https://courses.cs.washington.edu/courses/cse341/05au/lectures/scheme-side-effects.html)
    * [*Objects* in scheme](http://sarabander.github.io/sicp/html/3_002e1.xhtml)

* Haskell:
    * https://haskell.org/documentation/
    * Lecture: [Introduction to Haskell](https://www.cis.upenn.edu/~cis1940/spring13/) - including
        lecutre notes and nice homework exercises
    * [Creating types](http://learnyouahaskell.com/making-our-own-types-and-typeclasses)
    * [Pattern matching](https://www.haskell.org/tutorial/patterns.html)
    * [Modules](https://www.haskell.org/tutorial/modules.html)
    * [Functors, Applicatives, and Monads In Pictures](https://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html)
    * [Introduction to `IO`](https://wiki.haskell.org/Introduction_to_IO)
    * [`IO` inside](https://wiki.haskell.org/IO_inside)
    * [Monads](https://www.schoolofhaskell.com/user/bartosz/basics-of-haskell/10_Error_Handling)
    * [Functors](http://learnyouahaskell.com/functors-applicative-functors-and-monoids)



<!--
broken links
* Streams: https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-24.html
-->

