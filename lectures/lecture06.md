---
outline: deep
---

# Lambda Calculus

Lambda calculus is a model of computation. It was introduced in the 1930s by Alonzo Church. He was
trying to develop formal foundations for mathematics. His attempts were unsuccessful, as several of
his systems turned out to be inconsistent. Nevertheless, he invented lambda calculus as a fragment
of his formal system. Using this calculus, he solved the famous Hilbert's
[Entscheidungsproblem](https://en.wikipedia.org/wiki/Entscheidungsproblem) by showing that there are
questions that cannot be solved algorithmically. Later, lambda calculus became a formal tool for
analyzing computability and functional programming languages.[^church]

[^church]: If you are interested in the history, check the entry on [Alonzo
    Church](https://plato.stanford.edu/entries/church/) in the Stanford Encyclopedia of Philosophy.

Lambda calculus is probably the *simplest universal programming language*. Many of the key concepts
that make functional programming so powerful today were invented for, or are inspired by lambda
calculus. This includes for example *currying*, *recursion*, and *closures*. You will also see were
all this parenthesis in Racket come from!

Lambda calculus can be viewed as a minimal programming language in which computations can be
described and as a mathematical object about which rigorous statements can be proved. It serves as a
core language that allows one to specify and study programming language features by translating them
into the core language. We will discuss only the *untyped* variant of lambda calculus. There is also
a *typed* version used in the study of type systems.[^pierce]

[^pierce]: Benjamin C. Pierce: *Types and programming languages.* MIT Press 2002, ISBN
    978-0-262-16209-8, pp. I-XXI, 1-623.

Even though the syntax and semantics of lambda calculus are extremely simple, lambda calculus is
*Turing complete*. In other words, it is possible to simulate any Turing machine with a program in
lambda calculus and vice versa.

## Syntax

The syntax of lambda calculus has only three types of terms: a *variable* (denoted by lowercase
letters $x,y,z,\ldots$), the *abstraction* of a variable $x$ from a term $t$ defining a function
(denoted $\lambda x.t$), and the *application* of a term $t_1$ to a term $t_2$. Formally, we define
lambda terms by the following grammar:

$$
\begin{align*}
    \mathtt{term} &\to \mathtt{var}\mid \mathtt{func} \mid \mathtt{app}\\
    \mathtt{func} &\to (\lambda\,\mathtt{var}.\mathtt{term})\\
    \mathtt{app} &\to (\mathtt{term}\ \mathtt{term})
\end{align*}
$$

The abstraction $\lambda x.t$ defines an anonymous function as in Racket. The variable $x$ is its
parameter, and $t$ is its body. The application represents a function call.

Note that each abstraction and application introduce parentheses. To simplify the notation, we use
several conventions to remove some of the parentheses. 

- We often leave the outermost parentheses.
- The application is left-associative, e.g. $e_1e_2e_3e_4$ is $(((e_1e_2)e_3)e_4)$
- The bodies of functions extends to the right as far as possible. 

Using the above conventions, we can simplify the following term 

$$\Big(\lambda x.\Big(\textcolor{red}{(}\lambda y.((xy)x)\textcolor{red}{)}z\Big)\Big)$$

as follows:

$$\lambda x.\textcolor{red}{(}\lambda y.xyx\textcolor{red}{)}z$$

The red parentheses are the only ones that remain because they determine the body of the anonymous
function defined by $\lambda y$.

The parentheses are necessary if we want to represent lambda terms as sequences of characters.
Alternatively, we avoid parentheses if we understand lambda terms as abstract syntax trees. The
above lambda term $\lambda x.\textcolor{black}{(}\lambda y.xyx\textcolor{black}{)}z$ corresponds to
the following tree (the application nodes are denoted by the symbol @):

![](../img/lambda-tree.png){ style="width: 30%; margin: auto;" }

Before focusing on semantics, we need to introduce the scopes of variables. An occurrence of a
variable $x$ is said to be *bound* if it occurs in the body $t$ of an abstraction $\lambda x.t$. The
term $t$ is called *scope* of the *binder* $\lambda x$. An occurrence of $x$ is *free* if it is not
bound. To find out whether an occurrence of $x$ is bound or free, we can also track the path in the
abstract syntax tree from the occurrence of $x$ to the root. The occurrence is bound if we find the
node $\lambda x$ on the path. If not, it is free. In the above term $\lambda x.(\lambda y.xyx)z$,
all the occurrences are bound except for the only occurrence of $z$.[^fol] A lambda term is called
*closed* (aka *combinator*) if all its variable occurrences are bound. Otherwise, it is called
*open*.

[^fol]: Note that the terminology on bound and free occurrences is completely analogous to the
    terminology used in first-order logic, where variables can be bound by quantifiers instead of
    $\lambda$.

## Semantics

In lambda calculus, programs (i.e., lambda terms) consist of anonymous functions created by
abstraction and function calls. The computation of such a program is the simplification process
reducing the program to a lambda term that cannot be reduced anymore. This term is the value
computed by the program. 

A lambda term $\lambda x.t$ represents an anonymous function with an argument $x$ and body $t$. The
term $\lambda x.t$ corresponds to the following expression in Racket:

```scheme
(lambda (x) t)
```

We can call the function $\lambda x.t$ by applying it to another term $e$: $(\lambda x.t)e$. A
lambda term of the form $(\lambda x.t)e$ is called a *redex* because it is reducible by a
substitution rule known as *$\beta$-reduction*. When we reduce a redex $(\lambda x.t)e$, we
substitute $e$ for all *free* occurrences of $x$ in $t$. The resulting term is denoted $t[x:=e]$.

The $\beta$-reduction can be applied to any redex occurring in a lambda term. It represents a single
step in the computation. We denote the relation that a term is $\beta$-reducible to another term by
$\to^\beta$.[^abuse] For example, $(\lambda x.t)e \to^\beta t[x:=e]$

[^abuse]: We will abuse the notation and write $t\to^\beta s$ even if several $\beta$-reductions are
    needed to get from $t$ to $s$.

Let us see some examples. We denote the equality on lambda terms by $\equiv$.

$$(\lambda x.x)(\lambda y.y) \to^\beta x[x:=(\lambda y.y)] \equiv (\lambda y.y)$$

$$
\begin{align*}
        (\lambda x.xx)(\lambda y.y) &\to^\beta (xx)[x:=(\lambda y.y)] \equiv (\lambda y.y)(\lambda y.y) \\
        &\to^\beta y[y:=(\lambda y.y)] \equiv (\lambda y.y)
    \end{align*}    
$$

$$(\lambda x.x(\lambda x.x))y \to^\beta (x(\lambda x.x))[x:=y] \equiv y(\lambda x.x)$$

Note that we substitute only for the free occurrence of $x$ in the last example.

The substitution rule has one more caveat regarding the variable names. Consider a redex $(\lambda x.t)e$. 
A free occurrence of a variable in $e$ may become bound after the $\beta$-reduction. For example,

$$(\lambda x.(\lambda y.xy))y \to^\beta \lambda y.xy[x:=y]\equiv \lambda y.\textcolor{red}{y}y$$

The above reduction step is not valid. The reason is that functions do not depend on the names of their arguments. Consider the following Racket program:
```scheme
(define y 5)
(define (cadd x) (lambda (y) (+ x y)))
```
If we now call 
```scheme
(cadd y)
``` 
the resulting function cannot be 
```scheme
(lambda (y) (+ y y))
``` 
which would be doubling its argument. Instead, it should be the function adding $y=5$ to its argument. To see that, we can simple rename the argument $y$, for instance, to $z$:
```scheme
> (define (cadd x) (lambda (z) (+ x z)))
> (cadd y) => (lambda (z) (+ y z))
(lambda (z) (+ 5 z))
```

Thus the correct reduction of the above lambda term should proceed as follows:

$$(\lambda x.(\lambda y.xy))y \equiv (\lambda x.(\lambda \textcolor{blue}{z}.x\textcolor{blue}{z}))y 
\to^\beta \lambda \textcolor{blue}{z}.x\textcolor{blue}{z}[x:=y]\equiv \lambda z.yz$$

Whenever we want to substitute a term $e$ for a variable in a term $t$, we must first check if any
free occurrence in $e$ becomes bound.  If so, we must rename the clashing bound variable in $t$ to a
new name.  The renaming of the function arguments is known as *$\alpha$-conversion*.  We will
consider two lambda terms that are the same up to renaming a bound variable equivalent. For example,

$$\lambda x.xz \equiv \lambda y.yz$$

Formally, we can define the substitution by induction as follows:
$$
\begin{align*}
x[x:=e] &= e\\
y[x:=e] &= y \quad\text{if $y\neq x$}\\
(t_1\,t_2)[x:=e] &= (t_1[x:=e]\,t_2[x:=e])\\
(\lambda x.t)[x:=e] &= (\lambda x.t)\\
(\lambda y.t)[x:=e] &= (\lambda y.t[x:=e]) \quad\text{if $y\neq x$ and $y$ is not free in $e$}\\
(\lambda y.t)[x:=e] &= (\lambda z.t[y:=z][x:=e]) \quad\text{if $y\neq x$ and $y$ is free in $e$; $z$ is a fresh variable}
\end{align*}
$$
The last case is the one where we need to do the $\alpha$-conversion and rename the function
argument to a fresh name $\lambda z.t[y:=z]$ before we proceed with the substitution $[x:=e]$. 

## Church-Rosser Theorems

The computation in lambda calculus is performed by $\beta$-reduction. Given a lambda term, we reduce
it as much as possible by repetitive applications of $\beta$-reduction. The resulting irreducible
lambda term is the computed value. However, a lambda term can contain several redexes, for example:

$$\underbrace{(\lambda x.x)(\overbrace{(\lambda y.y)z}^{\text{redex}})}_{\text{redex}}$$

This leads to natural questions: Does it matter which redex we reduce first? Can different reduction
orders lead to different final values? Can some reduction orders terminate, whereas some do not? The
answers to these questions are provided by the Church-Rosser theorems. 

Before we state the Church-Rosser theorems, let us discuss a few examples showing what might happen.
First of all, it may happen that the reduction process does not terminate no matter which reduction
order is used! Consider the following term containing only a single redex:

$$(\lambda x.xx)(\lambda x.xx) \to^\beta (\lambda x.xx)(\lambda x.xx)$$

The above lambda term corresponds, in fact, to an infinite loop.

For some expressions, the reduction process terminates for some reduction orders and diverges for
others. For example, the term
$$(\lambda x.y)((\lambda x.xx)(\lambda x.xx))$$
contains two redexes. The inner one whose reduction does not terminate.
If we reduce the redex applying $(\lambda x.y)$ to the rest of the expression, the reduction stops immediately:

$$(\lambda x.y)((\lambda x.xx)(\lambda x.xx)) \to^\beta y$$

A lambda term is said to be in *normal form* if it is irreducible, i.e., it contains no redexes.
Reduction orders are also called *evaluation strategies*. Several of them were investigated. Let us
introduce two of them:

1. *Normal order* reduces the leftmost outermost redex first at each step.
2. *Applicative order* reduces the leftmost innermost redex first at each step.

We say a redex is to the left of another redex if its lambda appears further left. The *leftmost
outermost redex* is the leftmost redex not contained in any other redex. The *leftmost innermost
redex* is the leftmost redex, not containing any other redex. For example, consider the following
lambda term:

$$(\lambda y.y) ((\lambda z.zz) x)\Big((\lambda z.(\lambda a.a)z)(\lambda y.(\lambda z.z)x)\Big)$$
It has five redexes depicted in the following figure by the red color. 

![](../img/redexes.png){ style="width: 70%; margin: auto;" }

The leftmost outermost redex is $(\lambda y.y) ((\lambda z.zz) x)$. On the other hand, the leftmost
innermost redex is $(\lambda z.zz)x$.

Let's see how the above lambda term would be evaluated by the evaluation strategy following the
normal order. The reduced redexes are denoted by the line:
$$
\begin{align*}
&\underline{(\lambda y.y) ((\lambda z.zz) x)}\Big((\lambda z.(\lambda a.a)z)(\lambda y.(\lambda z.z)x)\Big)\\
&\underline{((\lambda z.zz) x)}\Big((\lambda z.(\lambda a.a)z)(\lambda y.(\lambda z.z)x)\Big) \\
&xx\Big(\underline{(\lambda z.(\lambda a.a)z)(\lambda y.(\lambda z.z)x)}\Big) \\
&xx\Big(\underline{(\lambda a.a)(\lambda y.(\lambda z.z)x)}\Big) \\
&xx(\lambda y.\underline{(\lambda z.z)x}) \\
&xx(\lambda y.x)
\end{align*}
$$

Finally, we get to the Church-Rosser theorems. The first theorem states that no matter which
reduction order we choose, we will always get the same normal form provided the reduction process
terminates. The second theorem states that the normal order always terminates, provided a sequence
of $\beta$-reduction steps leads to a normal form.

::: tip Church-Rosser Theorems:
1. Normal forms are unique (independently of evaluation strategy). Consequently, a lambda term
   cannot be reduced to two different normal forms.
2. Normal order always finds a normal form if it exists.
:::

## Programming in lambda calculus

Lambda calculus has no numbers, arithmetic, Booleans, etc. It has only anonymous functions and
function calls. Thus it might look pretty useless. However, lambda calculus is Turing complete. So
it should be no surprise that we can encode all the things like numbers and arithmetic. We will
build a few basic encodings from scratch in the following sections. Besides numbers and arithmetic,
the most interesting thing is how to represent recursive functions in lambda calculus. That is not
straightforward, considering there are only anonymous (nameless) functions.

Although lambda cannot internally introduce names for terms or functions, we can do it in our
exposition to simplify the notation. We will denote specific lambda terms (usually combinators) with
uppercase letters. For instance, the identity function is denoted by $\mathsf{I}$:

$$ \mathsf{I}\equiv \lambda x.x $$

We will also introduce one more convention for writing lambda terms. Each function defined by
abstraction is unary. However, we can have functions of higher arity due to currying. For instance,
a function of two arguments $x,y$ is defined by $\lambda x.(\lambda y.t)$. To simplify this
notation, we often group all the arguments under a single $\lambda$. For example, we replace
$\lambda x.(\lambda y.t)$ with $\lambda xy.t$.

### Booleans

We start with Boolean values and operations. Since Boolean values often appear in conditional
expressions, we model them so that they work directly as the if-then-else expression. The
if-then-else expression consists of a condition (i.e., a term whose value is either to true or
false) and two expressions to be evaluated depending on the result of the condition. Thus we model
Boolean values as binary projection functions:

$$
\begin{align*}
  \mathsf{T} &\equiv \lambda xy.x\\
  \mathsf{F} &\equiv \lambda xy.y
\end{align*}
$$

Boolean values are functions of two arguments returning one of them. More precisely, $\mathsf{T}$ representing "true" returns the first argument, whereas $\mathsf{F}$ representing "false" returns the second. Consequently, if we have a lambda term $ct_1t_2$, where $c$ is a condition term that is evaluated either to $\mathsf{T}$ or $\mathsf{F}$, we get the behavior of the if-then-else expression:

$$
\begin{align*}
        \mathsf{T}ab & \to^\beta a\\
        \mathsf{F}ab & \to^\beta b
\end{align*}
$$

Considering the encoding of Boolean values, it is easy to encode basic Boolean operations conjunction $\wedge$, $\vee$, and negation $\neg$. Recall that the conjunction of two arguments $x,y$ is false if the $x$ is false. If $x$ is true, the result of $x\wedge y$ is just $y$. This can be modeled as follows:

$$\wedge \equiv \lambda xy.xy\mathsf{F}$$

Let us check that the encoding works correctly. For an arbitrary lambda term $t$, we have
$$
\begin{align*}   
    \wedge \mathsf{F}t & \equiv (\lambda xy.xy\mathsf{F})\mathsf{F}t \to^\beta \mathsf{F}t\mathsf{F} \to^\beta \mathsf{F}\\
    \wedge \mathsf{T}t & \equiv (\lambda xy.xy\mathsf{F})\mathsf{T}t \to^\beta \mathsf{T}t\mathsf{F} \to^\beta t
\end{align*}
$$

Similarly, we can define disjunction that is true if the first argument is true and the value of the second argument otherwise.
$$\vee \equiv \lambda xy.x\mathsf{T}y$$
For an arbitrary lambda term $t$, we have
$$
\begin{align*}   
    \vee \mathsf{T}t & \equiv (\lambda xy.x\mathsf{T}y)\mathsf{T}t \to^\beta \mathsf{TT}t \to^\beta \mathsf{T}\\
    \vee \mathsf{F}t & \equiv (\lambda xy.x\mathsf{T}y)\mathsf{F}t \to^\beta \mathsf{FT}t \to^\beta t
\end{align*}
$$
Finally, it is straightforward to encode negation:
$$\neg \equiv \lambda x.x\mathsf{FT}$$
$$
\neg \mathsf{T}\equiv (\lambda x.x\mathsf{FT})\mathsf{T}\to^\beta \mathsf{TFT} \to^\beta \mathsf{F},\quad
\neg \mathsf{F}\equiv (\lambda x.x\mathsf{FT})\mathsf{F}\to^\beta \mathsf{FFT} \to^\beta \mathsf{T}
$$

### Numbers and arithmetic

To encode numbers and arithmetic operations, we use the so-called Church numerals. They encode a natural number $n$ as a binary function applying the first argument $n$-times to the second.

$$
    \begin{align*}
        \mathsf{0} &\equiv \lambda sz.z\\
        \mathsf{1} &\equiv \lambda sz.sz\\
        \mathsf{2} &\equiv \lambda sz.s(sz)\\
        \mathsf{3} &\equiv \lambda sz.s(s(sz))\\
        & \vdots
    \end{align*}
$$

Note that $\mathsf{0}\equiv \mathsf{F}$. It is usual in programming that a single value might have two different meanings depending on its context. For instance, the value $65$ can represent the number $65$ or the uppercase letter $A$.

Using the above encodings for numbers, one can easily define the successor function $n\mapsto n+1$.
$$\mathsf{S} \equiv \lambda \textcolor{red}{n}xy.x(\textcolor{red}{n}xy)$$
The input number $n$ in $(nxy)$ just applies $n$-times $x$ to $y$, i.e., this expression is equivalent to $n$. Next, we add one more application of $x$, i.e., $x(nxy)$. Let us compute the successor of $\mathsf{1}$:

$$
    \begin{align*}
        \mathsf{S1} &\equiv (\lambda nxy.s(nxy))(\lambda sz.sz)\\
        & \to^\beta \lambda xy.x((\lambda sz.sz)xy)\\
        & \to^\beta \lambda xy.x(xy) \equiv \mathsf{2}\\
    \end{align*}
$$

Once we have the successor function $\mathsf{S}$, we can define the addition because we can get the result of $n+m$ by applying $n$-times $\mathsf{S}$ to $m$. Thus we can model addition by the term:

$$\mathsf{A} \equiv \lambda nm.n\mathsf{S}m$$

For example, $2+3$ is computed as follows:
$$
\begin{align*}
    \mathsf{A23} & \to^\beta \mathsf{2S3}
        \equiv (\lambda sz.s(sz))\mathsf{S3} \to^\beta \mathsf{S(S3)} \to^\beta \mathsf{5}
\end{align*}
$$

Multiplication of two numbers $n,m$ can be represented by the term 
$$\mathsf{M}\equiv\lambda nmsz.n(ms)z$$ 
Since $n$ is a numeral, applying it to $(ms)$ and $z$ results in a term applying $n$-times $(ms)$ to $z$, i.e., $(ms)((ms)(\cdots((ms)z)\cdots))$.  Analogously, each application of $(ms)$ results in $m$-many applications of $s$. Altogether, we get $n\cdot m$ many applications of $s$ to $z$. Let us see an example:

$$
\begin{align*}
\mathsf{M23}&\equiv (\lambda nmsz.n(ms)z)\mathsf{23}\to^\beta \lambda sz.\mathsf{2}(\mathsf{3}s)z 
\to^\beta \lambda sz.(\mathsf{3}s)((\mathsf{3}s)z)\\
& \to^\beta \lambda sz.(\mathsf{3}s)(s (s (s z))) \to^\beta \lambda sz.(s (s (s (s (s (s z))))))\equiv\mathsf{6}
\end{align*}
$$

The multiplication term $\mathsf{M}$ can be further simplified to
$$\mathsf{M}'\equiv\lambda nms.n(ms)$$
We can remove the variable $z$ because $\mathsf{M}$ and $\mathsf{M}'$ behave identically. To see that, consider an abstraction $\lambda x.(tx)$ for a term $t$. It defines a function of an argument $x$ applying $t$ to $x$. If we apply it to any expression $e$, we obtain $(\lambda x.(tx))e \to^\beta te$. Thus we can directly replace $\lambda x.(tx)$ just with the term $t$. This simplification is known as *$\eta$-reduction*. Utilizing $\eta$-reduction to 

$$\mathsf{M}\equiv\lambda nmsz.n(ms)z\equiv\lambda nms.(\lambda z.(n(ms))z),$$ 

we end up with $\mathsf{M}'$.

In the following section on recursive functions, we will need the predecessor function on natural
numbers $n\mapsto n-1$ ($0$ is mapped to $0$). Unfortunately, encoding it is more complex than
encoding the successor function $\mathsf{S}$. We first need to introduce an encoding of pairs. Given
two terms $t,s$, we define the pair consisting of $t$ and $s$ as follows:

$$\langle t,s\rangle\equiv \lambda z.zts$$

Thus the pair $\langle t,s\rangle$ is a function of argument $z$ that is applied to the pair's
components. Note that this is exactly how we encoded a 2D point as a function closure in [Lecture
3](lecture03#closures).
```scheme
(define (point x y)
  (lambda (m) (m x y)))
```
Given a pair, we can access its components by applying it to the projection functions $\mathsf{T}$
and $\mathsf{F}$.
$$
\begin{align*}    
  \langle t,s\rangle\mathsf{T} &\equiv (\lambda z.zts)\mathsf{T} \to^\beta \mathsf{T}ts \to^\beta t\\
  \langle t,s\rangle\mathsf{F} &\equiv (\lambda z.zts)\mathsf{F} \to^\beta \mathsf{F}ts \to^\beta s
\end{align*}
$$

To create the predecessor function, we need to find a term such that if we apply it $n$-times to
another term, it gets evaluated to $n-1$. We first define a function mapping a pair $\langle
n,m\rangle$ to $\langle n+1,n\rangle$.

$$\mathsf{\Phi} \equiv \lambda pz.z(\mathsf{S}(p\mathsf{T}))(p\mathsf{T})$$

The function $\mathsf{\Phi}$ takes a pair $p$, extracts its first component $p\mathsf{T}$, computes
its successor $\mathsf{S}(p\mathsf{T})$, and returns a pair consisting of the successor and the
first component. The reason why $\mathsf{\Phi}$ is important is that when we apply it $n$-times to
the pair $\langle\mathsf{0},\mathsf{0}\rangle$, we get the pair $\langle n,n-1\rangle$.
$$
\mathsf{\Phi}\langle\mathsf{0},\mathsf{0}\rangle \to^\beta \langle\mathsf{1},\mathsf{0}\rangle,\quad 
\mathsf{\Phi}\langle\mathsf{1},\mathsf{0}\rangle \to^\beta \langle\mathsf{2},\mathsf{1}\rangle,\quad 
\mathsf{\Phi}\langle\mathsf{2},\mathsf{1}\rangle \to^\beta \langle\mathsf{3},\mathsf{2}\rangle,\quad \ldots
$$

Consequently, we can define the predecessor function as the function applying $n$-times
$\mathsf{\Phi}$ to $\langle\mathsf{0},\mathsf{0}\rangle$ and extracting the second component:

$$\mathsf{P} \equiv \lambda n.n\mathsf{\Phi}\langle\mathsf{0},\mathsf{0}\rangle\mathsf{F}$$

### Zero test

To implement a recursive function, we need a condition when to stop the recursion. In the example we
will discuss in the next section, we test whether a given number is zero. To check whether a given
number is zero, recall that $\mathsf{0}\equiv\mathsf{F}$ is just the projection to the second
argument. Consequently, $\mathsf{0}t\mathsf{T} \to^\beta \mathsf{T}$ for each lambda term $t$. Thus
we need to find a term $t$ such that when applied to $\mathsf{T}$ at least once, it returns
$\mathsf{F}$. Such a term $t$ is the constant function always returning $\mathsf{F}$, i.e., $\lambda
x.\mathsf{F}$. Altogether, we define

$$\mathsf{Z} \equiv \lambda n.n(\lambda x.\mathsf{F})\mathsf{T}$$ 

So we have

$$
\mathsf{Z0} 
    \equiv (\lambda n.n(\lambda x.\mathsf{F})\mathsf{T})\mathsf{0}
    \to^\beta \mathsf{0}(\lambda x.\mathsf{F})\mathsf{T}
    \to^\beta \mathsf{T}
$$
and for $\mathsf{N}>\mathsf{0}$
$$
\mathsf{ZN}
    \equiv (\lambda n.n(\lambda x.\mathsf{F})\mathsf{T})\mathsf{N}
    \to^\beta \mathsf{N}(\lambda x.\mathsf{F})\mathsf{T}
    \to^\beta (\lambda x.\mathsf{F})(\cdots((\lambda x.\mathsf{F})\mathsf{T})\cdots)
    \to^\beta \mathsf{F}
$$

### Recursive functions

Finally, we are getting to the most exciting construction of how to encode recursive functions if we
have only anonymous functions in lambda calculus. Recursive functions can be defined through the
so-called $\mathsf{Y}$-combinator. 

$$\mathsf{Y} \equiv \lambda y.(\lambda x.y(xx))(\lambda x.y(xx))$$

Note that it is an expanded version of the term $(\lambda x.xx)(\lambda x.xx)$ representing the
infinite loop as $\mathsf{Y}$ applies a further argument $y$ to $xx$. Let us see what happens if we
apply $\mathsf{Y}$ to any lambda term $\mathsf{R}$.

$$\mathsf{YR} \to^\beta (\lambda x.\mathsf{R}(xx))(\lambda x.\mathsf{R}(xx)) \equiv \mathsf{R}'$$

We obtained a term denoted $\mathsf{R}'$, which is something like a recursive version of
$\mathsf{R}$. To see that, note that $\mathsf{R}'$ can produce as many applications of $\mathsf{R}$
as we wish:

$$
\begin{align*}
    \mathsf{R}' &\equiv (\lambda x.\mathsf{R}(xx))(\lambda x.\mathsf{R}(xx))\\
    &\to^\beta \mathsf{R}((\lambda x.\mathsf{R}(xx))(\lambda x.\mathsf{R}(xx))) \equiv \mathsf{R}\mathsf{R}'\\
    &\to^\beta \mathsf{R}(\mathsf{R}((\lambda x.\mathsf{R}(xx))(\lambda x.\mathsf{R}(xx)))) \equiv 
    \mathsf{R}(\mathsf{R}\mathsf{R}')\\
    &\to^\beta \ldots
\end{align*}
$$

Whether this process stops or not depends on $\mathsf{R}$. If it contains a leftmost outermost
redex, whose reduction discards $\mathsf{R}'$, the computation following the normal order
terminates. However, the applicative order need not terminate as it can indefinitely reduce
$\mathsf{R}$'.

To see an example, we implement a function computing for a given natural number $n$ the sum
$\sum_{i=0}^n i$. We can define the function recursively using the fact $\sum_{i=0}^n
i=n+\sum_{i=0}^{n-1} i$. In Racket, we would implement such a function as follows:
```scheme
(define (sum-to n)
  (if (= n 0) 
      0
      (+ n (sum-to (- n 1)))))
```

We must apply the $\mathsf{Y}$-combinator in lambda calculus instead. We define a function
corresponding to the body of ``sum-to``, but we replace the recursive call with a call of a function
given as an argument.

$$\mathsf{R} \equiv \lambda rn.\mathsf{Z}n\mathsf{0}(n\mathsf{S}(r(\mathsf{P}n)))$$

The function tests if $n$ is zero by $\mathsf{Z}n$. If it is zero, $\mathsf{Z}n$ evaluates to
$\mathsf{T}$, which consequently returns its first argument, i.e., $\mathsf{0}$. Otherwise,
$\mathsf{Z}n$ evaluates to $\mathsf{F}$, which returns $n\mathsf{S}(r(\mathsf{P}n))$. This
expression is nothing else than $n+r(n-1)$.

Now it remains to turn $\mathsf{R}$ into its recursive version by the $\mathsf{Y}$-combinator. We define

$$\mathsf{R}'\equiv \mathsf{YR}$$

Let us see if it correctly sums up all the natural numbers to $3$. Recall that $\mathsf{R}'\to^\beta
\mathsf{RR}'$.
$$
\begin{align*}
\mathsf{R}'\mathsf{3} & \equiv \mathsf{YR3} \to^\beta \mathsf{R}\mathsf{R}'\mathsf{3} 
    \equiv \mathsf{Z30}(\mathsf{3S}(\mathsf{R}'(\mathsf{P3})))\\
    &\to^\beta \mathsf{F0}(\mathsf{3S}(\mathsf{R}'(\mathsf{P3}))) \to^\beta \mathsf{3S}(\mathsf{R}'(\mathsf{P3}))\\
    &\to^\beta \mathsf{3S}(\mathsf{R}'\mathsf{2}) \to^\beta \mathsf{3S}(\mathsf{RR}'\mathsf{2}) \\
    &\to^\beta \mathsf{3S}(\mathsf{Z20}(\mathsf{2S}(\mathsf{R}'(\mathsf{P2})))) 
    \to^\beta \mathsf{3S}(\mathsf{2S}(\mathsf{R}'\mathsf{1}))\\
    &\to^\beta \mathsf{3S}(\mathsf{2S}(\mathsf{RR}'\mathsf{1})) 
    \to^\beta \mathsf{3S(2S(1S(R'0)))}\\
    &\to^\beta \mathsf{3S(2S(1S(RR'0)))} \equiv \mathsf{3S(2S(1S(Z00(0S(R'(P0))))))} \\
    &\to^\beta \mathsf{3S(2S(1S(T0(0S(R'(P0))))))} \to^\beta \mathsf{3S(2S(1S0)) \to^\beta 6}
\end{align*}  
$$

Note that the expression in the last line contains several redexes. The one hidden in $\mathsf{R}'$
can be reduced forever. On the other hand, if we reduce $\mathsf{T0(0S(R'(P0)))}\to^\beta
\mathsf{0}$, the term $\mathsf{R}'$ disappears. This is why the normal order terminates in this
computation, whereas the applicative one does not.

