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

## Course Outline

This course explores:

* **Racket** which features a simple syntax that aligns with λ-calculus, is dynamically typed, supports mutable data, and can treat code as data, which makes it easy to write interpreters;
* **$\lambda$-calculus** as the theoretical foundation of functional programming;
* and **Haskell** a pure functional language with static typing, a rich type system, and a design that strictly separates its pure core from its mutable shell.

## Homework

We will publish four homework assignments for **50 points** in total. You have to reach at least **25 points** in total, and You need at least one point from every homework. For each day that a homework assignment is submitted past the deadline, a penalty of one point will be deducted, until only one point is left on your score.

Dates for opening of the assignment / deadlines are:

| # | Language | Opening | Deadline | Points |
| - | -------- | ------- | -------- | ------:|
| 1 | Racket   | 05.03.  | 25.03.   |     10 |
| 2 | Racket   | 19.03.  | 08.04.   |     15 |
| 3 | Haskell  | 09.04.  | 02.05.   |     12 |
| 4 | Haskell  | 30.04.  | 20.05.   |     13 |


## Exam

The final exam consists of two parts: The *programming* exam for **30 points**, followed by an optional *oral* exam for **20 points**:

- In the *programming* exam, you will have 3 hours to solve one Racket assignment (15 points) and one Haskell assignment (15 points). You have to reach at least 16 points in total to pass the programming exam. You will have access to all course materials on this webpage, as well as the Racket documentation and Hoogle, but you will not have access to the internet.
- If you pass the programming exam, You can take an optional 15-minute *oral exam*. We will ask you about the theoretical underpinnings of functional programming, Racket, and Haskell. You can earn up to 20 extra points, and you cannot lose any points.

To be eligible for the exam, your homework solutions must meet at least the minimum requirements.


## Grading
Our grading scale is as follows:

|  A   |  B  |  C  |  D  |  E  |  F  |
|------|-----|-----|-----|-----|-----|
|91-100|81-90|71-80|61-70|51-60| 0-50|


## Teachers

| Name             | E-mail                 | Room     | Role                  |
|------------------|------------------------|----------|-----------------------|
| Rostislav Horčík | <xhorcik@fel.cvut.cz>  | KN:E-322 | Lecturer              |
| Tomáš Votroubek  | <votroto1@fel.cvut.cz> |          | Lecturer & Instructor |
| Jiří Němeček     | <nemecj38@fel.cvut.cz> |          | Instructor            |

Consulting hours are not explicitly scheduled; please contact us via email to make an appointment.


## Where to get Help

If you struggle with an exercise or an assignment, feel free to contact your tutor via email. For questions which could help others, we recommend posting in the course [discussion forum](https://cw.felk.cvut.cz/forum/forum-1867.html), where we will prioritize responses. There are also online communities where you can get in touch with your peers:
* The [Faculty Discord](https://discord.gg/cvutfel) is primarily in Czech. You can select which course's channels you want to see in `#volba-předmětů` (check `#fup`) after authenticating with a faculty account.
* The [Functional Programming Discord](https://discord.gg/7C3RPWZcYg) is primarily in English, and includes channels not only for Racket (`#racket`) and Haskell (`#haskell-beginners`, `#haskell-beginners-more`, `#haskell`, `#haskell-tooling`), but also for all kinds of FP and math topics.

Keep in mind our academic principles — [no plagiarism](https://cw.fel.cvut.cz/wiki/help/common/plagiaty_opisovani), avoid sharing code snippets directly, and try to come up with a solution by yourself first.

## Other Resources

* Harold Abelson and Gerald Jay Sussman and Julie Sussman: [Structure and Interpretation of Computer Programs](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book.html), MIT Press, 1996.
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
    * [Real World Haskell](https://book.realworldhaskell.org/)
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
