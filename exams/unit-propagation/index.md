---
title: "Exam Task: Unit Propagation"
subtitle: "From: Exam 4 - 2022"
date: "2024-01-12"
outline: deep
---

# Unit Propagation

The *Davis–Putnam–Logemann–Loveland* (DPLL) algorithm is the core of SAT solvers nowadays. It takes
a propositional formula in CNF (conjunctive normal form) and returns true if, and only if, the
formula is satisfiable. A formula in CNF is usually represented as a set of clauses. A *clause*
is represented as a set of literals. A *literal* is either a propositional variable (e.g. $x$)
or its negation (e.g. $\neg x$). For instance, a formula 
$$
 \varphi=(a\vee b\vee \neg c\vee\neg f)\wedge(b\vee c)\wedge(\neg b\vee e)\wedge \neg b
$$
is represented as 
$$
  \varphi=\{\{a,b,\neg c,\neg f\}, \{b,c\}, \{\neg b, e\}, \{\neg b\}\}.
$$

One of the subroutines of the DPLL algorithm is the unit propagation simplifying the input formula.
A *unit* is a clause containing a single literal, e.g. $\{\neg b\}$.  It is obvious that a
satisficing evalution for a formula in CNF must evaluate all units to true.  This allows simplifying
the input formula. Assume that a set of clauses $\varphi=\{c_1,\ldots,c_n\}$ has a unit, i.e.,
$c_k=\{u\}$ for some $k$ and literal $u$, then $\varphi$ can be simplified by the following rules:

1. if $u\in c_i$, then $c_i$ can be removed from $\varphi$,
2. if $\neg u\in c_i$, then $\neg u$ can be removed from $c_i$.

For example, the formula $\varphi$ in~(\ref{ex1}) has a unit $\{\neg b\}$, so we can simplify to
$\{\{a,\neg c,\neg f\}, \{c\}\}$. Note that by propagating the unit, a new unit was created. Thus we
can continue and propagate the unit $\{c\}$ obtaining $\{\{a,\neg f\}\}$. The resulting set of
clauses has no unit.

Your task is to implement the unit propagation for a given formula $\varphi$ in CNF, i.e., eliminate
all possible unit clauses. See the following pseudocode.
```
while there is a unit clause |{u}| in |φ| do
      |φ| <- unit-propagate(u, |φ|);
```

# Racket

In Scheme, implement a function `(propagate-units cls)` that accepts a list of
clauses and returns a list of clauses after the unit propagation.  A clause is represented as a list
of literals. Positive and negative literal is represented, respectively by the following structures:
```scheme
(struct pos (variable) #:transparent)
(struct neg (variable) #:transparent)
```
As the resulting list of clauses returned by `propagate-units` should represent 
a set, remove all the duplicated clauses from the list.

Your task is to be called \texttt{task3.rkt} and must provide the `propagate-units` and 
both structures `pos` and `neg`.
Hence, the head of your file should start with
```scheme
#lang racket
(provide propagate-units (struct-out pos) (struct-out neg))

(struct pos (variable) #:transparent)
(struct neg (variable) #:transparent)  

; your code here
```

## Hint
To remove an element `v` from a list `lst`, 
you may want to use the function `(remove v lst)`. 
To remove duplicated elements from a list `lst`, 
call the function `(remove-duplicates lst)`.

## Examples
The following shows the behaviour of the `propagate-units` function.

For $\varphi=\{\{\neg x\}\}$ we get 
```scheme
> (propagate-units (list (list (neg "x"))))
'()
```

For $\varphi=\{\{x\}, \{\neg x\},\{y\},\{\neg y\}\}$ we get 
```scheme
> (propagate-units (list (list (pos "x")) (list (neg "x")) (list (pos "y")) (list (neg "y"))))
'(())
```

For $\varphi=\{\{a,b,\neg c,\neg f\}, \{b,c\}, \{\neg b, e\}, \{\neg b\}\}$, we get
```scheme
> (propagate-units (list (list (pos "a") (pos "b") (neg "c") (neg "f")) 
                         (list (pos "b") (pos "c"))
                         (list (neg "b") (pos "e"))
                         (list (neg "b"))))
(list (list (pos "a") (neg "f")))
```
  
# Haskell

In Haskell, implement a function
`propagateUnits :: [Clause] -> [Clause]` that accepts
a list of clauses and returns a list of clauses after the unit propagation.
As the resulting list of clauses returned by `propagateUnits` should represent 
a set, remove all the duplicated clauses from the list.

Literals and clauses are represented as follows:
```haskell
type Variable = String
data Literal = Neg { variable :: Variable }
             | Pos { variable :: Variable } deriving (Eq, Ord)

type Clause = [Literal]
```
and for your convenience, you are provided with the instance of `Show` for literals:
```haskell
instance Show Literal where
  show (Neg x) = "-" ++ x
  show (Pos x) = x
```

Your task is to be called \texttt{Task4.rkt} and must export the `propagateUnits` 
function and the `Literal` data type.
Hence, the head of your file should read

```haskell
  module Task4 ( propagateUnits, Literal (..) ) where
  import Data.List -- for delete, nub functions
```

## Hint

To remove an element from a list, you can use the function 
`delete :: Eq a => a -> [a] -> [a]`. 
To remove duplicated elements from a list, 
call the function `nub :: Eq a => [a] -> [a]`.
Both functions are located in the module `Data.List`

## Examples
The following shows the behaviour of the `propagateUnits` function.

For $\varphi=\{\{\neg x\}\}$ we get 
```haskell
> propagateUnits [[Neg "x"]]
[]
```

For $\varphi=\{\{x\}, \{\neg x\}, \{y\}, \{\neg y\}\}$ we get 
```haskell
> propagateUnits [[Pos "x"], [Neg "x"], [Pos "y"], [Neg "y"]]
[[]]
```

For $\varphi=\{\{a,b,\neg c,\neg f\}, \{b,c\}, \{\neg b, e\}, \{\neg b\}\}$, we get
```haskell
> propagateUnits [ [Pos "a", Pos "b", Neg "c", Neg "f"]
                 , [Pos "b", Pos "c"]
                 , [Neg "b", Pos "e"]
                 , [Neg "b"]]
[[a,-f]]
```

::: details Solution
```haskell
module Task4 ( propagateUnits, Literal (..) ) where
import Data.List

type Variable = String
data Literal = Neg { variable :: Variable }
             | Pos { variable :: Variable } deriving (Eq, Ord)

type Clause = [Literal]

instance Show Literal where
    show (Neg x) = "-" ++ x
    show (Pos x) = x

-- DPLL

negation :: Literal -> Literal
negation (Pos x) = Neg x
negation (Neg x) = Pos x

getUnit :: [Clause] -> Maybe Literal
getUnit cls = case units of
                [] -> Nothing
                (u:us) -> Just u
    where units = concat $ filter ((1==) . length) cls

simplifyClause :: Literal -> Clause -> [Clause]
simplifyClause l c | l `elem` c = []
                   | negation l `elem` c = [delete (negation l) c]
                   | otherwise = [c]

simplify :: [Clause] -> Literal -> [Clause]
simplify cls l = concatMap (simplifyClause l) cls

propagateUnits :: [Clause] -> [Clause]
propagateUnits cls = let mu = getUnit cls
                     in case mu of
                         Nothing -> nub cls
                         Just u -> propagateUnits $ simplify cls u 

dpll :: [Clause] -> Bool
dpll cls = case cls' of
            [] -> True
            []:_ -> False
            (l:_):_ -> dpll (simplify cls' l) || dpll (simplify cls' (negation l))
    where cls' = sort $ propagateUnits cls                  

ex :: [Clause]
ex = [[Neg "p", Neg "q"], [Pos "p", Pos "r"], [Pos "q", Pos "s"], [Pos "s"]]

ex2 = [[Pos "a", Pos "b", Pos "c"],[Pos "b", Neg "c", Neg "f"],[Neg "b", Pos "e"],[Neg "b"]]

ex3 = [[Pos "a", Pos "b"],[Pos "a", Neg "b"],[Neg "a", Pos "c"],[Neg "a", Neg "c"]]
```
:::
