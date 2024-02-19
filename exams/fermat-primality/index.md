---
title: "Exam Task: Fermat Primality Test"
subtitle: "From: Exam 2 - 2021"
date: "2024-01-12"
author: "Niklas Heim"
author-url: ""
return-url: '../../'
return-text: '← Return home'
---

In this task, for a given natural number $p$

1. generate a sequence of pseudorandom natural numbers $a$ such that $2\leq a<p-1$,
2. test whether $p$ is prime by checking whether the following equation holds for each generated pseudorandom number $a$

\begin{equation}
a^{p-1} \equiv 1\ (\texttt{mod }p).
\end{equation}

If $(1)$ holds for all numbers $a$, it is highly probable that $p$ is prime.
This probabilistic primality test is known as the *Fermat Primality Test*.  Note, the
*Carmichael numbers*, which are composite yet pass the test for all $a$ relatively prime to the
respective number, are avoided when testing your implementation of this task. 

## Pseudo-random Number Generation

To generate pseudorandom numbers in a given interval, use 
the *Linear Congruential Generator (LCG)* 
\begin{equation}
  x_{n+1} = (A x_n + C) \ \texttt{mod }M,
\end{equation}
where $A$, $C$ and $M$ are constants. This equation generates the next pseudorandom number $x_{n+1}$ from the previous $x_n$. The number $x_0$ is the seed.

The number $b$ drawn from $(1)$ can be transformed to the interval $b^\text{lower} \leq b' < b^\text{upper}$ as 
\begin{equation}
  b' = (b \ \texttt{mod } (b^\text{upper} - b^\text{lower})) + b^\text{lower}. 
\end{equation}


Your task is to implement the Fermat Primality Test in Haskell.
There are two parts to this task: first, implement the LCG, and then the test itself.

## Implementation

The LCG generator is represented as
```{.tight-code .haskell}
data LCG = LCG  Int Int Int Int deriving Show
```
where the four `Int`s in the `LCG` 4-tuple are $A$, $x_n$, $C$, and $M$ w.r.t. $(2)$.
Implement the function
```{.tight-code .haskell}
generate_range :: Int -> Int -> State LCG Int
```
which is used to sample random integers $b^\text{lower} \leq b' < b^\text{upper}$ where the lower
and upper bounds are set in the first and second function input, respectively.
The state of the LCG is kept using the `State` monad. So start your code with
```{.tight-code .haskell}
import Control.Monad.State
```
Note that it is desirable to follow $(2)$ and $(3)$ as closely as
possible, since the tasks are tested with constant seed and thus are considered deterministic. 
The function is used as follows.
```{.tight-code .haskell}
> runState (generate_range 1 100) (LCG 513 1 1 1024)  
(20, LCG 513 514 1 1024) 
```


Implement the primality test in the function
```{.tight-code .haskell}
primality :: Int -> Int -> State LCG Bool
```
where the first input is the potential prime and the second is the number of repetitions of the test 
(i.e., the number of generated pseudorandom numbers).
The function is used as follows:

```{.tight-code .haskell}
> evalState (primality 17 1000) (LCG 513 1 1 1024) 
True

> evalState (primality 42 1000) (LCG 513 1 1 1024) 
False

> evalState (primality 9 0) (LCG 513 1 1 1024) 
True
```

## Hint

To prevent overflow of `Int`, compute $a^{p-1}\mod p$ sequentially using 
the identity $a^{k+1}\mod p = a\cdot (a^k\mod p)\mod p$, i.e.,
sequentially multiplying by $a$ and applying modulo to each partial result.

<details class="admonition">
<summary><strong>Solution</strong></summary>
```{.tight-code .haskell}
import Control.Monad.State

---- linear congruential generator
data LCG = LCG  Int Int Int Int deriving Show -- A*x + C mod M

generate :: State LCG Int
generate = do (LCG a x c m) <- get
              let x' = (a * x + c) `mod` m
              put (LCG a x' c m)
              return x'

generate_range :: Int -> Int -> State LCG Int -- a <= x < b
generate_range a b = do x <- generate
                        let x' = (x `mod` (b-a)) + a
                        return x'   

modulo_power :: Int -> Int -> Int -> Int 
modulo_power a n m = iter a n where
    iter b 1 = b
    iter b nn = iter (b*a `mod` m) (nn-1)

fermat_comp :: Int -> Int -> Int
fermat_comp p b = (modulo_power b (p-1) p) 

fermat_check :: Int -> Int -> Int -> State LCG Bool
fermat_check p n 1 = primality p (n-1)  
fermat_check _ _ _ = return False 

primality :: Int -> Int -> State LCG Bool
primality p 0 = return True
primality p n = do b <- generate_range 1 p
                   fermat_check p n (fermat_comp p b)
```
</details>
