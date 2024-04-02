# Lab 7: Lambda calculus


This lab focuses on lambda calculus. First, we focus on the syntax of $\lambda$-expressions. Second,
we focus on its semantics, i.e., the computation specified by $\lambda$-expressions whose one step
is performed by the $\beta$-reduction (and $\alpha$-conversion if necessary). To help you to play
with these notions and concepts, I implemented in Racket an interpreter of $\lambda$-calculus
transforming a given $\lambda$-expression into its normal form (if it exists). It follows the normal
order evaluation strategy. In addition, I implemented a few helpers functions to help you inspect
$\lambda$-expressions. You can download the interpreter:
[`lambda-calculus.rkt`](https://github.com/aicenter/FUP/blob/main/code/lambda-calculus.rkt)

To follow the exercises, it is recommended to have a piece of paper, a pen, DrRacket IDE installed,
and the interpreter. To use the interpreter, download the above-mentioned file and store it in a
directory where you want to create your own code. Then create a new Racket file starting as follows:
```scheme
#lang racket
(require "lambda-calculus.rkt")
```

$\lambda$-expressions are represented in the interpreter as S-expressions. It does not allow to use
of any conventions regarding parenthesis. So you need to place them all explicitly. For instance,
the $\lambda$-expression $\lambda xy.xy(\lambda ab.b)$ has to be represented as follows:
```scheme
'(λ x : (λ y : ((x y) (λ a : (λ b : b)))))
```
So we had to add the outermost parenthesis, expand the shortcuts $\lambda xy.$ to $\lambda
x.(\lambda y.$, and put the parenthesis determining the order of the applications, i.e., as the
application is left-associative, $xy(\lambda ab.b)$ is in fact $((x y) (\lambda ab.b))$.  The symbol
λ can be entered in DrRacket by pressing Ctrl+\. Instead of the dot symbol, the colon symbol is
used. 

The module `lambda-calculus.rkt` provides the following functions:
| Function | Description |
| - | - |
| `(draw-expr expr)` | draws the syntax tree the given $\lambda$-expression `expr`; redexes are colored in red |
| `(substitute expr var val)` | substitutes `val` for the free occurrences of `var` in `expr` |
| `(reduce expr)` | reduces `expr` by a one-step $\beta$-reduction in the normal order |
| `(eval expr [info 'quiet])` | finds the normal form of `expr`; if `info` is set to `'verbose`, displays all the steps; if `info` is `'tree`, then all the steps are drawn as trees |

## Exercise 1
Draw the syntax tree of the $\lambda$-expression $(\lambda x.y(xx))(\lambda y.y(xx))$ and determine which variable occurrences are free and which are bound. 

We will use the helper function `draw-expr`. First, create the correct representation as an S-expression:
```scheme
'((λ x : (y (x x))) (λ y : (y (x x))))
```
Then evaluate the following function call:
```scheme
(draw-expr '((λ x : (y (x x))) (λ y : (y (x x)))))
```
It displays the following tree:

![ex1](/img/lab07-ex1.png)

An occurrence of a variable $v$ is bound if it is in the syntax tree below the node $\lambda v$ and is free otherwise. So, for our expression, the occurrences of $x$ in the left branch are bound, and they are free in the right branch. The occurrence of $y$ in the left branch is free and bound in the right branch.

## Exercise 2
Draw the syntax tree of the $\lambda$-expression $\lambda y.xy(\lambda ab.b)$ and determine which variable occurrences are free and which are bound.

Try first to draw the tree on paper. Then compare your result with the result returned by the function `draw-expr`. The $\lambda$-expression is represented in Racket as follows:
```scheme
'(λ y : ((x y) (λ a : (λ b : b))))
```

## Exercise 3
Find all redexes in $(\lambda x.x y) z ((\lambda u.u) ((\lambda v.v) z))$. Which one is the leftmost outermost redex, and which is the leftmost innermost redex? Reduce the leftmost outermost redex.

::: tip Hint
Try to find the redexes. Then call `(draw-expr expr)` 
for `expr` being the following S-expression:
```scheme
'(((λ x : (x y)) z) ((λ u : u) ((λ v : v) z)))
```
The roots of redexes are colored red. To check that your reduction was correct, call
```scheme
(reduce '(((λ x : (x y)) z) ((λ u : u) ((λ v : v) z))))
```
:::

## Exercise 4
Recall that multiplication of two numbers is computed by $M \equiv \lambda abc.a(bc)$. Find the normal form of $M01$ following the normal order reduction strategy, i.e., compute $0\cdot 1$, which should result in $0$. The numbers $0,1$ are abbreviations for $\lambda$-expressions $\lambda sz.z$ and $\lambda sz.sz$ respectively. 

::: tip Hint
Once you do it on paper, check your result in Racket. You can use Racket definitions and semiquoting to make your $\lambda$-expression more readable.
```scheme
(define zero '(λ s : (λ z : z)))
(define one '(λ s : (λ z : (s z))))
(define M '(λ a : (λ b : (λ c : (a (b c))))))

(eval `((,M ,zero) ,one) 'verbose)  ; displays each reduction step as λ-expression
(eval `((,M ,zero) ,one) 'tree)     ; draws each reduction step as syntax tree
```
:::

## Exercise 5
Recall that a pair $(a,b)$ is represented in $\lambda$-calculus as $(a,b)\equiv \lambda z.zab$. The projections into the first and second components can be obtained by applying $(a,b)$ to Boolean values $T\equiv \lambda ab.a$ and $F\equiv \lambda ab.b$. Thus
$(a,b)T \to^\beta a$ and $(a,b)F \to^\beta b$. We can even define a `cons` function by $CONS \equiv \lambda abz.zab$. In Racket, you can define all these constructions as follows (the final two calls check that it behaves as expected):
```scheme
(define T '(λ x : (λ y : x)))
(define F '(λ x : (λ y : y)))

(define CONS 
  '(λ a : (λ b : (λ z : ((z a) b))))
  )
  
(eval `(((,CONS a) b) ,T))
(eval `(((,CONS a) b) ,F))
```
  
Write a $\lambda$-expression swapping components of a given pair $p$. 

::: tip Hint
The desired $\lambda$-expression should take a pair $p$ and return another pair with swapped components. 
So the expression should start with $\lambda pz.z??$ where the question marks are the components of the returned pair. 
:::

::: details Solution
$\lambda pz.z(pF)(pT)$
:::

Once you have it, define `SWAP` and check that it correctly swaps the components:
```scheme
(eval `(,SWAP ((,CONS a) b))) => '(λ z : ((z b) a))
```

::: details Solution
```scheme
(define SWAP `(λ p : (λ z : ((z (p ,F)) (p ,T)))))
```
:::

## Exercise 6
Since we can create pairs, we can create lists as in Racket. We represent the empty list by the false value $F$. Now we can
create a list `'(a b)` by 
```scheme
(define lst `((,CONS a) ((,CONS b) ,F)))

(eval `(,lst ,T))  => 'a
(eval `((,lst ,F) ,T)) => 'b
```

Write a $\lambda$-expression $NULL?$ testing if a list is empty, i.e., it returns $T$ if it is empty and $F$ otherwise.

::: tip Hint
A list is either a pair or $F$ if it is empty. Let denote it by $p$. 
Recall the definition of the zero test from the lecture
$$Z\equiv \lambda x.xF\neg F,$$
where $\neg\equiv \lambda x.xFT$. 
We need something similar for the list $p$. So our desired $\lambda$-expression should look like $\lambda p.pe_1e_2$ where $e_1,e_2$ have to be filled by suitable $\lambda$-expressions serving as arguments for $p$. If $p$ is empty (i.e., $p\equiv F$), then $p$ is just a projection into the second argument. Thus $e_2$ should be $T$, i.e., we have $\lambda p.pe_1T$. Now, if we substitute for $p$ a pair 
(i.e., $p \equiv \lambda z.zab$), we obtain $(\lambda z.zab)e_1T$. Thus $e_1$ is going to be substituted for $z$, and consequently, it will be applied to $a$ and $b$, i.e., we would end up with $e_1abT$. Since the result in this case should be $F$, we need the result of $e_1ab$ to be $\neg$ because
$\neg F\to^\beta T$.  
:::

::: details Solution
$\lambda p.p(\lambda ab.\neg)T$ 
:::

Check your solution in Racket.

::: details Code
```scheme
(define neg `(λ x : ((x ,F) ,T)))
(define NULL? `(λ p : ((p (λ a : (λ b : ,neg))) ,T)))

(eval `(,NULL? ,F)) ; => T
(eval `(,NULL? ,lst)) ; => F
```
:::

## Exercise 7
Define a $\lambda$-expression computing the length of a list.

::: tip Hint
Follow the approach from the lecture where we defined a function $R\equiv \lambda rn.Zn0(nS(r(Pn)))$ which we turned into a recursive one by $Y$-combinator (i.e., $YR$).
:::

Recall:
$$Y\equiv \lambda y.(\lambda x.y(xx))(\lambda x.y(xx)).$$
You also need the successor function
$$S\equiv \lambda wyx.y(wyx)$$
for adding $1$. The computation of the desired $\lambda$-expression can be expressed in Racket as
follows:
```scheme
(define len 
  (lambda (p) (if (null? p)
                    0
                    (+ (len (cdr p)) 1))))
```

Modify the $\lambda$-expression $R$ by replacing $Z$ with $NULL?$ from the previous exercise. Adding
$1$ can be done by applying the successor function $S$, and the predecessor function $P$ must be
replaced by the expression returning the second component.

::: details Solution
$LEN \equiv \lambda rp.NULL?p0(S(r(pF)))$
:::

Check your solution in Racket:

::: details Code
```scheme
(define S '(λ w : (λ y : (λ x : (y ((w y) x))))))
(define Y '(λ y : ((λ x : (y (x x))) (λ x : (y (x x))))))

(define LEN
  `(λ r :
     (λ lst : (
               ((,NULL? lst) ,zero)
               (,S (r (lst ,F)))))))

(eval `((,Y ,LEN) ,F)) ; => 0
(eval `((,Y ,LEN) ((,CONS a) ,F))) ; => 1 
(eval `((,Y ,LEN) ((,CONS a) ((,CONS b) ,F)))) ; => 2
```
:::
