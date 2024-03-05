---
outline: deep
---
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

## Basics

### The interpreter - GHCi

The leading implementation of Haskell is the [Glasgow Haskell
Compiler](https://www.haskell.org/ghc/) (GHC). It includes both a compiler and an interpreter. GHC
is written in Haskell (apart from small parts of the runtime in
C/[C--](https://en.wikipedia.org/wiki/C--)).

We will mostly be working with the interpreter *GHCi*:
```bash
$ ghci
GHCi, version 9.8.1: https://www.haskell.org/ghc/  :? for help
𝝺>
```

Like in the Racket REPL, you can evaluate Haskell expressions in this interpreter
```scheme
𝝺> fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
𝝺> take 20 fibs
[0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181]
```

The most important commands you can use in the REPL are
- `:?` for help
- `:load <filename>`
- `:reload`
- `:type <expr>` displays the type of and `expr`
- `:info <name>` displays info on a function or type
- `:quit` or `ctrl-d`


### Basic Syntax

```haskell
-- this is a comment

{-
this is a block comment
-}

x :: Int
x = 3
```
Above we defined a variable `x` with the `Int` and assigned the value `3` to it.

*Every* well-formed expression `e` has a well-formed type `t` written like `e :: t`:
```haskell
𝝺> "a" ++ "b"
"ab" :: String
```

::: details Custom REPL
To get the lambda prompt instead of `Prelude>` and also output type information automatically, you 
and use the following config:
```bash
$ cat ~/.ghc/ghci.conf
-- Enables type display
:set +t
-- Sets the prompt to a lambda
:set prompt "𝝺> "
```
:::

Haskell has a number of basic types, including:
- `Bool`: logical values `True`, `False`
- `Char`: single characters `'a'`
- `String`s of characters `"abc"`
- `Int`: fixed-precision integers
- `Integer`: arbitrary-precision integers
- `Float`: single-precision floating-point numbers
- `Double`: double-precision floating-point numbers


#### Functions

Function definitions have to start with a **lower-case letter** (like `myFun`, `fun1`, `h'`, `g_2`,
etc.) and we can do pattern matching on inputs (even integers):

```haskell:line-numbers
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)
```

In the first line above we are defining the function type signature. `factorial` is a function that
accepts an `Int` and outputs an `Int`. Then we have two clauses (which are checked in the order they
are written in and the first clause that matches a given input is used).  Calling `factorial 2` does
not match the first clause, so the second one is used, because a variable (in this case `n`) matches
anything. Hence, we end up with `2 * factorial (2-1)`, which can be further evaluated until we
arrive at `2 * 1 * factorial 0`, where we match the base case and end up with the final expression
that is evaluated as soon as we want to print it.

We can define *infix operators* consisting only of special symbols, e.g. `+/+` can be defined in
infix notation:
```haskell
x +/+ y = 2*x + y
```
A prefix function turns infix by ` ` and infix turns prefix by `( )`:
- `` `mod` ``, `` `elem` ``
- `(+)`, `(+/+)`

#### `let` / `where`

Like in Racket we can use `let`:
```haskell
discr :: Float -> Float -> Float -> Float
discr a b c =
    let x = b*b
        y = 4*a*c
    in x - y
```
or alternatively, we can use Haskell's `where`:

```haskell
discr a b c = x - y
    where x = b*b
          y = 4*a*c
```


#### Layout rule
