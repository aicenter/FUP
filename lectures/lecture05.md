---
outline: deep
---

# Macros & Interpreters

## Syntactic macros

The following section should only briefly introduce syntactic macros. At the
same time, I will explain a related topic on how Racket processes source files.
What happens if we press the `Run` button in DrRacket or execute `racket
source.rkt` in a shell? 

1. Racket must first parse our source file consisting of a sequence of
   characters. In other words, the sequence of characters is converted into a data
   structure. This data structure is called [Abstract Syntax
   Tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree) (AST) and contains all
   the information extracted from the source file, i.e., our expressions,
   definitions, etc. Due to the homoiconicity of Racket, Racket's syntax directly
   corresponds to regular lists. Thus AST is represented through lists. For
   example, if our source file contains `(+ (* 2 3) 4)`, AST is a list whose first
   element is `+`, followed by a list `(* 2 3)`, and `4`. Moreover, each part of
   AST is enriched by further syntactic information, e.g., the corresponding line
   and column in the source file.

2. Once AST is created, Racket executes various transformations on AST. In fact,
   some Racket syntactic constructions are implemented as transformations on AST.
   For example, the expression `(and exp1 exp2 exp3)` is transformed into 

```scheme
(if exp1
    (if exp2
        (if exp3
            #f)
        #f)
    #f)
```

3. When AST consists only of basic syntactic constructions, a compiler
   translates AST into a machine code that can be executed.

The workflow is depicted below:

![](../img/ast.png){ style="width: 100%; margin: auto;" }

If we create a Racket program, its expressions get evaluated in the runtime,
i.e., after the compilation phase. On the other hand, syntactic
[macros](https://docs.racket-lang.org/guide/macros.html) allow us to add some
extra transformations into the second phase before the compilation.  Macros are
basically functions operating on AST. Thanks to the homoiconicity, AST is
represented as usual data in Racket. Thus macros can be defined as usual Racket
functions. 

Implementing macros is a complex topic. The simplest way to introduce a macro is
based on pattern matching. We specify a syntax rule consisting of a pattern and
a template on how the pattern should be rewritten. Racket tries to match the
pattern against pieces of our code. Once there is a match, the corresponding
portion of the code is rewritten according to the template.

Recall the function [`my-if`](/lectures/lecture04#lazy-evaluation), whose
parameters must be passed as thunks.  We cannot eliminate those thunks because
the parameters get evaluated before the function body. However, injecting them
before the compilation by a macro is possible.

```scheme
(define-syntax-rule (macro-if c a b)
  (my-if c (thunk a) (thunk b)))
```
The macro defines a syntax rule whose pattern is `(macro-if c a b)`. Racket
searches for a portion of AST being a list of length $4$ starting with a symbol
`macro-if`. If such a list is found, it is rewritten into `(my-if c (thunk a)
(thunk b))`.
Thus we can use in our code `macro-if` instead of `my-if`. Racket automatically translates all occurrences of `macro-if` to `my-if` with the thunks injected.

A macro can be composed of several syntax rules. We can specify several patterns
to be matched against the AST and templates for each pattern. If any pattern
matches, the corresponding template defines how to rewrite the code. For
example, let us implement a macro introducing comprehension terms into Racket. A
comprehension term allows us to define a new list from a list by means of an
expression and predicate. They are common in Python. For instance,
```python
python> [x**2 for x in range(5)]
[0, 1, 4, 9, 16]

python> [x for x in range(20) if x % 2 == 0]
[0, 2, 4, 6, 8, 10, 12, 14, 16, 18]
```
We want to define a similar syntax for Racket via a macro. We will express the above Python examples in Racket as follows:
```scheme
> (list-comp (* x x) : x <- (range 5))
'(0 1 4 9 16)

> (list-comp x : x <- (range 20) if (even? x))
'(0 2 4 6 8 10 12 14 16 18)
```

We must define a macro with two syntax rules to make the new syntax work—one for the comprehension term without and one for the comprehension term with an if clause.
```scheme:line-numbers
(define-syntax list-comp
  (syntax-rules (: <- if)
    [(list-comp <expr> : <id> <- <lst>)
     (map (lambda (<id>) <expr>) <lst>)]

    [(list-comp <expr> : <id> <- <lst> if <cond>)
     (map (lambda (<id>) <expr>)
          (filter (lambda (<id>) <cond>) <lst>))]))
```
[Line 3](#cb55-3) defines a pattern for the comprehension term without an if clause. [Line 4](#cb55-4) specifies how to rewrite the comprehension term. The symbol `<id>` together with the expression `<exp>` define a function to be applied to each element of the list `<lst>`. [Line 6](#cb55-6) represents the second pattern containing the if clause. In that case, we must first filter the list `<lst>` based on the if clause ([Line 8](#cb55-8)) and then apply the function to each element of the filtered list ([Line 7](#cb55-7)).


## Interpreters

The next homework with be about interpreters, so lets talk a little bit about
what they are/how they work.

Generally, programming lanuages are composed of two parts:

- **Syntax**: Tells you what kind of expressions you can write to obtain a valid program.
- **Semantics**: Assigns *meaning* of certain primitives. For example, it
  defines what the operation `+` does.

```
.
└── Programming Languages
    ├── Syntax
    │   └── Grammar
    └── Semantics
        └── Meaning of primitives
```

Interpreters usually work in a two major phases. After you provide the course code,
the interpreter first **parses** the text into an AST (this can include an initial
lexing phase which generates tokens from your code without creating a tree
structure, and only then outputting an AST).
Then the AST is **evaluated** to produce the final output.

In the case of LISP-like languages we already wrote our code in terms of
S-expressions, which means that *we don't have to worry about parsing our code*
at all! Parsing is provided for free by the racket language, because it is *homoiconic*[^homoiconic].
Hence, we will only have to worry about the evaluation of given S-expressions.[^monadic-parsing]

[^homoiconic]: [Homoiconicity](https://en.wikipedia.org/wiki/Homoiconicity)
  essentially means `code == data`, so that programs that are written in
  homoiconic languages can immediately be used as datastructures (in our case:
  lists).

[^monadic-parsing]: If you have been looking forward to a lecture on parsing and
  are now disappointed, do not despair, we will have a full lecture on [*monadic
  parsing*](https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf) in a few weeks
  during tha Haskell part of this course.


### Brainf*ck

Interpreting a full-fledged programming language is quite tricky, so we will choose a very simple
language to interpret, called [Brainf*ck](https://en.wikipedia.org/wiki/Brainfuck).
Here is an example of a valid Brainf*ck program:
```
,>,[-<+>]<.
```
Most of the characters above represent operations on a *tape* of numbers (which typically is filled
with zeros at the start of the program) and a *pointer* to the current position on the tape:
```
  ↓
0 0 0 0 0 0 0 ...
  ↑
```
Additionally, the user can also provide data to a program via a list of inputs
(not the similarity to the Turing Machine). The full list of operations can be
found in the table below:

| Character  | Meaning                                                                      |
| ---------- | ---------------------------------------------------------------------------- |
| >          | Increment the data pointer by one (to point to the next cell to the right).  |
| <          | Decrement the data pointer by one (to point to the next cell to the left).   |
| +          | Increment the byte at the data pointer by one.                               |
| -          | Decrement the byte at the data pointer by one.                               |
| .          | Output the byte at the data pointer.                                         |
| ,          | Accept one byte of input, storing its value in the byte at the data pointer. |
| [ `code` ] | While the number at the data pointer is not zero, execute `code`.            |

With the table above we can decipher the first example program `,>,[-<+>]<.` and realize that
it adds two numbers from a user provided input.

More formally, Brainf*ck is a minimalistic, esoteric programming language that
defines computations over a fixed-size tape of numbers. The syntax grammar of
the language is given by
```
<program> -> <term>*
<term>    -> <cmd> | <cycle>
<cycle>   -> [<program>]
<cmd>     -> + | - | < | > | . | ,
```

which means that a `<program>` is a sequence of `<term>`s. Each term is either a
command (`<cmd>`) or a `<cycle>`. We already listed the six possible commands
above. Inside cycles we can nest whole programs which gives us the ability to
write arbitrary loops. Thus, we can write a well-formed expression simply with
arbitrary sequence of commands. The only things we have to take care of is to
match parentheses appropriately.

We will represent Brainf\*ck programs simply as lists of terms. Cycles will form
nested lists.  To make things more convenient for us we will slightly alter the
syntax of Brainf\*ck, because `.` and `,` are already taken in Racket (for pairs
and quoting). We will substitute them by `@` and `*`, respectively:

| Character  | Substitue | Meaning                                                                      |
| ---------- | --------- | ---------------------------------------------------------------------------- |
| >          | >         | Increment the data pointer by one (to point to the next cell to the right).  |
| <          | <         | Decrement the data pointer by one (to point to the next cell to the left).   |
| +          | +         | Increment the byte at the data pointer by one.                               |
| -          | -         | Decrement the byte at the data pointer by one.                               |
| .          | @         | Output the byte at the data pointer.                                         |
| ,          | *         | Accept one byte of input, storing its value in the byte at the data pointer. |
| [ `code` ] | [`code`]  | While the number at the data pointer is not zero, execute `code`.            |

With the substitutions we can define our addition program as
```scheme
(define add-prg '(@ > @ [- < + >] < *))
```

Our final implementation will define a function `run-prg` which accepts a
program and some input, for example:
```scheme
> (run-prg add-prg '(12 34))
46
```

### Mutable Tape

During the lecture we will use a *gobal*, *mutable* tape to implement our
interpreter (you will modify this implementation to use an immutable tape during
the labs).

Our tape will be represented by a mutable vector of numbers which we can initialize with
```scheme
> (define SIZE 10)
> (define t (make-tape SIZE 0))
'#(0 0 0 0 0 0 0 0 0 0)
```
And mutate via the `vector-set!` function.
```
> (vector-set! t 2 5)
> t
'#(0 0 5 0 0 0 0 0 0 0)
```

We will implement the tape and the possible operations on the tape by defining a
closure.  The closure will hold the tape itself, a pointer `ptr` to the current
position, and will accept a number of messages `msg` that trigger operations on
the tape:

```scheme
(define (make-tape size)
  (define tape (make-vector size 0))
  (define ptr 0)

  (lambda (msg)
    (cond
      [(eqv? msg 'tape) (list tape ptr)]
      [(eqv? msg 'plus) (vector-set! tape ptr (+ 1 (vector-ref tape ptr)))])))
```

The tape can then be used like this:
```scheme
> (define tp (make-tape SIZE))
> (tp 'tape)
'(#(0 0 0 0 0 0 0 0 0 0) 0)

> (tp 'plus)

> (tp 'tape)
'(#(1 0 0 0 0 0 0 0 0 0) 0)
```

### Command Implementation

Implementing the operations for the commands `<`, `>`, `+`, `-`, `@`, and `.` is now straightforward:

<<< @/lectures/lecture05-brainfuck.rkt#make-tape{scheme}


### Program evaluation
