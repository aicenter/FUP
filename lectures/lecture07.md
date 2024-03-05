# Introduction to Haskell

Haskell is a *pure*, *lazy* ,and *statically-typed* functional programming language (as opposed to
Racket which is *eager* and *dynamically-typed*).

**Pure**, in the context of Haskell, means that it is impossible to write mutating code. (Except for
`IO` actions, which let us print to screen, but more on that in due time.)

**Statically-typed**: Haskell programs are checked and types derived at compile time. The type
system is very powerful in Haskell and we will spend a lot of time learning about its different
features/implications.

**Lazy** means that expressions *are not evaluated until the are actually needed*. This has far
reaching consequences, for example we can easily deal with infinite data structure which
enables programming in a very different style (*wholemeal programming*)[^wholemeal].

[^wholemeal]: [Wholemeal programming](https://www.cis.upenn.edu/~cis1940/spring13/lectures/01-intro.html):
    A quote from Ralf Hinze: *Functional languages excel at wholemeal programming, a term coined by
    Geraint Jones. Wholemeal programming means to think big: work with an entire list, rather than a
    sequence of elements; develop a solution space, rather than an individual solution; imagine a
    graph, rather than a single path. The wholemeal approach often offers new insights or provides
    new perspectives on a given problem. It is nicely complemented by the idea of projective
    programming: first solve a more general problem, then extract the interesting bits and pieces by
    transforming the general program into more specialised ones.*
    For example, consider this pseudocode in a C/Java-ish sort of language:
    ```c
    int acc = 0;
    for ( int i = 0; i < lst.length; i++ ) {
      acc = acc + 3 * lst[i];
    }
    ```
    This code suffers from what Richard Bird refers to as *indexitis*: it has to worry about the
    low-level details of iterating over an array by keeping track of a current index. It also mixes
    together what can more usefully be thought of as two separate operations: multiplying every item
    in a list by 3, and summing the results.
    In Haskell, we can just write
    ```haskell
    sum (map (3*) lst)
    ```
    This semester we’ll explore the shift in thinking represented by this way of programming, and
    examine how and why Haskell makes this possible.

The leading implementation of Haskell is the [Glasgow Haskell
Compiler](https://www.haskell.org/ghc/) (GHC). It includes both a compiler and an interpreter. GHC
is written in Haskell (apart from small parts of the runtime in
C/[C--](https://en.wikipedia.org/wiki/C--)).


