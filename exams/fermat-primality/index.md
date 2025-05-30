---
title: "Exam Task: Fermat Primality Test"
subtitle: "From: Exam 2 - 2021"
date: "2024-01-12"
outline: deep
---

# Fermat Primality Test

In this task, for a given natural number $p$

1. generate a sequence of pseudorandom natural numbers $a$ such that $2\leq a<p-1$,
2. test whether $p$ is prime by checking whether the following equation holds for each generated pseudorandom number $a$

$$
a^{p-1} \equiv 1\ (\texttt{mod }p).
$$

If $(2)$ holds for all numbers $a$, it is highly probable that $p$ is prime.
This probabilistic primality test is known as the *Fermat Primality Test*.  Note, the
*Carmichael numbers*, which are composite yet pass the test for all $a$ relatively prime to the
respective number, are avoided when testing your implementation of this task.

### Pseudo-random Number Generation

To generate pseudorandom numbers in a given interval, use
the *Linear Congruential Generator (LCG)*
$$
  x_{n+1} = (A x_n + C) \ \texttt{mod}\, M,
$$
where $A$, $C$ and $M$ are constants. This equation generates the next pseudorandom number $x_{n+1}$ from the previous $x_n$. The number $x_0$ is the seed.

The number $b$ drawn from $(1)$ can be transformed to the interval $b^\text{lower} \leq b' < b^\text{upper}$ as
$$
  b' = (b \ \texttt{mod}\, (b^\text{upper} - b^\text{lower})) + b^\text{lower}.
$$

## Haskell

Your task is to implement the Fermat Primality Test in Haskell. Your file should be called `Fermat.hs`, and you should export the function `primality :: Int -> Int -> State LCG Bool`. There are two parts to this task: first, implement the LCG, and then the test itself.

### Implementation

The LCG generator is represented as
```haskell
data LCG = LCG  Int Int Int Int deriving Show
```
where the four `Int`s in the `LCG` 4-tuple are $A$, $x_n$, $C$, and $M$ w.r.t. $(2)$.
Implement the function
```haskell
generate_range :: Int -> Int -> State LCG Int
```
which is used to sample random integers $b^\text{lower} \leq b' < b^\text{upper}$ where the lower
and upper bounds are set in the first and second function input, respectively.
The state of the LCG is kept using the `State` monad. So start your code with
```haskell
import Control.Monad.State
```
Note that it is desirable to follow $(2)$ and $(3)$ as closely as
possible, since the tasks are tested with constant seed and thus are considered deterministic.
The function is used as follows.
```haskell
> runState (generate_range 1 100) (LCG 513 1 1 1024)
(20, LCG 513 514 1 1024)
```


Implement the primality test in the function
```haskell
primality :: Int -> Int -> State LCG Bool
```
where the first input is the potential prime and the second is the number of repetitions of the test
(i.e., the number of generated pseudorandom numbers).
The function is used as follows:

```haskell
> evalState (primality 17 1000) (LCG 513 1 1 1024)
True

> evalState (primality 42 1000) (LCG 513 1 1 1024)
False

> evalState (primality 9 0) (LCG 513 1 1 1024)
True
```

### Hint

To prevent overflow of `Int`, compute $a^{p-1}\mod p$ sequentially using
the identity $a^{k+1}\mod p = a\cdot (a^k\mod p)\mod p$, i.e.,
sequentially multiplying by $a$ and applying modulo to each partial result.

::: details Exam Solution
```haskell
module Fermat (LCG (..), primality) where

import Control.Monad
import Control.Monad.State

-- Linear congruential generator
-- A*x + C mod M
data LCG = LCG Int Int Int Int deriving (Show)

lcg (LCG a x c m) =
  let y = (a * x + c) `mod` m
   in (y, LCG a y c m)

generate = state lcg

project low high num = (num `mod` (high - low)) + low

generateRange low high = project low high <$> generate

moduloPower a n m = iterate (\x -> x * a `mod` m) 1 !! n

fermatCheck p r = 1 == moduloPower r (p - 1) p

primality :: Int -> Int -> State LCG Bool
primality x n = do
  rs <- replicateM n (generateRange 1 x)
  pure (all (fermatCheck x) rs)
```
:::
