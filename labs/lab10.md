# Lab 10: Polymorphic functions

## Exercise 1
Haskell functions can be polymorphic if we use type variables in their definitions. Write a function 
```haskell
permutations :: [a] -> [[a]]
```
taking a list of elements of type `a` and returning a list of all its permutations.

We will use the same approach as in Racket. First, we define a function
```haskell
interleave :: a -> [a] -> [[a]]
```
taking an element `x` of type `a` and a list `ys` of elements of type `a` and returning a list of
lists where `x` is plugged into `ys` by all possible ways. E.g.
```haskell
> interleave 0 [1,2,3]
[[0,1,2,3],[1,0,2,3],[1,2,0,3],[1,2,3,0]]
```

The base case for the `interleave` function is simple as there is only a single way to plug `x` into
the empty list `[]`, namely `[x]`. If the list is of the form `y:ys`, then one way to plug `x` into
it is to prepend `x` (i.e., `x:y:ys`). The remaining possibilities can be computed recursively by
calling `interleave x ys` and prepending `y`.

::: details Solution: `interleave`
```haskell
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : [y:xs | xs <- interleave x ys]
```
:::

Now we can easily define the `permutations` function. The base case for the empty list is trivial.
For a nonempty list `x:xs` we can recursively compute permutations of `xs` and interleave `x` into
all such permutations. Finally, we must concatenate the results into a single list of permutations.
This can be done by the function `concat` implemented in `Prelude` (you also saw how to implement
such a function in the lecture. I called that function `flatten`).

::: details Solution: `permutations`
```haskell
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = concat [interleave x p | p <- permutations xs]
```
:::

## Exercise 2
Use the function `permutations` from the previous exercise to write a function `findHamiltonian`,
which finds all Hamiltonian paths in a given graph. 

We first have to represent graphs in a data structure. To be general, we define a graph data
structure over any data type `a`.  First, we define a type for edges as pairs of values of type `a`.
Second, we define a parametric algebraic data type `Graph a` as a record consisting of a list of
vertices and a list of edges. We also make this type an instance of the type class `Show` by
automatic derivation.
```haskell
type Edge a = (a,a) 
data Graph a = Graph {vertices :: [a], edges :: [Edge a]} deriving Show
```
Now it is possible to define graphs, for instance as follows:
```haskell
gr :: Graph Int
gr = Graph {vertices=[1..6], edges=[(1, 2), (1, 5), (2, 3), (2, 5), (3, 4), (4, 5), (4, 6)]}

> gr
Graph {vertices = [1,2,3,4,5,6], edges = [(1,2),(1,5),(2,3),(2,5),(3,4),(4,5),(4,6)]}
```
Moreover, we have automatically functions `vertices :: Graph a -> [a]`
and `edges :: Graph a -> [Edge a]`:
```haskell
> vertices gr
[1,2,3,4,5,6]

> edges gr
[(1,2),(1,5),(2,3),(2,5),(3,4),(4,5),(4,6)]
```
 
Recall that a Hamiltonian path in a graph is a path going through all the vertices exactly once. To
solve the task, we will use brute force, generating all possible permutations of vertices and
checking whether they form a path. First, we define a helper function `isEdge` taking a pair of
vertices of type `a` and a graph over `a` and returning `True` if those vertices are connected and
`False` otherwise. To test the membership of an element in a list, we can use the function `elem`.
Note the type declaration of `isEdge`.  As the function is polymorphic, we have to assume that `a`
is an instance of the class `Eq` so that we test the membership by the `elem` function.

::: details Solution: `isEdge`
```haskell
isEdge :: Eq a => Edge a -> Graph a -> Bool
isEdge (a,b) g = (a,b) `elem` edgs || (b,a) `elem` edgs where
    edgs = edges g
```
:::

Next, we define a function testing whether a given list of vertices is a path in a given graph. That
can be easily done by list comprehension generating all indexes but the last one. Note the use of
the function `and`. It can be applied to a list of type `[Bool]` performing logical conjunction of
all its members.

::: details Solution: `isPath`
```haskell
isPath :: Eq a => [a] -> Graph a -> Bool
isPath vs g = and [ isEdge pair g | pair <- zip vs (tail vs) ]
```
:::

Finally, we take all permutations of vertices and test which form a path in a given graph.
Collecting all such paths is the list of all Hamiltonian paths.

::: details Solution: `findHamiltonian`
```haskell
findHamiltonian :: Eq a => Graph a -> [[a]]
findHamiltonian g = [p | p <- perms, isPath p g]
    where perms = permutations (vertices g)
```
:::

## Exercise 3
The following exercise focuses on operator overloading. With the boom of neural nets, finding
algorithms computing efficiently and precisely derivatives of functions used to construct network
layers became essential. There are three approaches. The first one is the syntactic derivation
manipulating symbolic expressions. The second one is the approximation via the limit defining the
derivative of a function at a point. Third, computing derivatives via dual numbers. We will discuss
the last approach. 

[Dual numbers](https://en.wikipedia.org/wiki/Dual_number) are numbers of the form $a+b\epsilon$ for
$a,b\in\mathbb{R}$ and $\epsilon$ is something like the complex unit $i$ but instead of $i^2=-1$ we
have $\epsilon^2=0$.[^algebraic] Based on the above definition, we can define over the set of dual
numbers algebraic operations like addition and multiplication:
$$
\begin{align}
  (a+b\epsilon)+(c+d\epsilon) &= (a+c) + (b+d)\epsilon, \\
  (a+b\epsilon)\times(c+d\epsilon)  &=ac+(ad+bc)\epsilon+bd\epsilon^2=ac+(ad+bc)\epsilon.
\end{align}
$$

[^algebraic]: For those who like algebra, it can be constructed by taking the ring of univariate
    polynomials $\mathbb{R}[\epsilon]$ and taking its quotient by the ideal generated by
    $\epsilon^2$, i.e., $\mathbb{R}[\epsilon]/\langle\epsilon^2\rangle$. You can safely ignore the
    last sentence if you do not understand it. As I like algebra, I could not resist the temptation
    to explain it in algebraic language.

Dual numbers can be used to compute the derivative of a function at a point. Consider first a
polynomial $p(x)=b_0+b_1x+b_2x^2$. Let us compute its value at $a+\epsilon$. 

$$
\begin{align}
p(a+\epsilon) &= b_0 + b_1(a+\epsilon) + b_2(a+\epsilon)^2 \\
              &= b_0 + b_1a + b_2a^2 + b_1\epsilon + b_22a\epsilon \\
              &= p(a)+p'(a)\epsilon
\end{align}
$$
So you see that the resulting dual number contains the value of $p(a)$ and also its derivative
$p'(a)$. This can be generalized to any analytic function by taking its Taylor expansion at a point
$a$:

$$
f(x) = \sum_{i=0}^\infty \frac{f^{(n)}(a)}{n!}(x-a)^n
$$
If we evaluate this series at $a+\epsilon$, we get
$$
f(a+\epsilon) = \sum_{i=0}^\infty \frac{f^{(n)}(a)}{n!}(a+\epsilon-a)^n = f(a) + f'(a)\epsilon
$$
as $\epsilon^n=0$ for $n\geq 2$. Thus we again computed the value $f(a)$ and also $f'(a)$ by
evaluating a function $f(x)$ at $a+\epsilon$.

We will represent dual numbers as pairs. Thus we declare a new parametric type over a type `a`. We also automatically derive its instances of
the class `Eq`
and `Ord`. This automatic derivation orders the dual numbers lexicographically; two dual numbers are equal if they have the same components.
```haskell
data DualNum a = DN a a deriving (Eq, Ord)
```
We also define our own instance of `Show`
so that, e.g., `DN 3 10`
is displayed as `3 + 10eps`.
```haskell
instance Show a => Show (DualNum a) where
    show (DN x x') = show x ++ " + " ++ show x' ++ "eps"
```

In order to be able to evaluate a function at a dual number, we define `DualNum a`
as an instance of `Num`
. Then we can compute $f(a)$ and $f'(a)$ for $f$ defined as any composition of functions from the `Num` definition.
```haskell
instance Num a => Num (DualNum a) where
    (DN x x') + (DN y y') = DN (x + y) (x' + y')
    (DN x x') - (DN y y') = DN (x - y) (x' - y')
    (DN x x') * (DN y y') = DN (x * y) (x*y' + y*x')
    fromInteger i = DN (fromInteger i) 0
    abs (DN x x') = DN (abs x) (signum x * x')
    signum (DN x _) = DN (signum x) 0
```
I should likely comment on the above definition a bit. Addition, subtraction, and multiplication are straightforward. The function `fromInteger :: Num a => Integer -> a`
embeds integers into the set of dual numbers. So it maps $a$ to $a+0\epsilon$. The last two definitions are not mathematically correct. We pretend that `signum`
function has the derivative $0$ everywhere, which is not true at $0$. Similarly `abs` has no derivative at $0$.

Dual numbers can be also divided if the first component of the divisor is non-zero as follows: 
$$
\frac{x+x'\epsilon}{y+y'\epsilon}
  = \frac{(x+x'\epsilon)(y-y'\epsilon)}{(y+y'\epsilon)(y-y'\epsilon)}
  = \frac{xy+(x'y-xy')\epsilon}{y^2}
  = \frac{x}{y}+\frac{x'y-xy'}{y^2}\epsilon
$$
So we can make `DualNum a`
an instance of `Fractional`.  
```haskell
instance Fractional a => Fractional (DualNum a) where
    (DN x x') / (DN y y') = DN (x/y) ((x'*y - x*y') / (y*y))
    fromRational r = DN (fromRational r) 0
```

Now we can define a function $f$ and evaluate it at a dual number $a+\epsilon$ to compute $f(a)$ and $f'(a)$. The function has to be polymorphically working for any instance `a`
of the class `Num`
or `Fractional`.
```haskell
f :: Num a => a -> a
f x = x^2 + 1

> f (DN 5 1)
26 + 10eps
```
Indeed, for $f(x)=x^2+1$ we have $f(5)=5^2+1=26$ and $f'(x)=2x$, $f'(5)=10$. Another example
```haskell
g :: Fractional a => a -> a
g x = (x^2 - 2) / (x - 1)

> g (DN 0 1)
2.0 + 2.0eps
```
because $g'(x)=\frac{x^2-2x+2}{(x-1)^2}$, i.e., $g(0)=2$ and $g'(0)=2$.

The next example is more impressive. Consider the function `sqr`
computing the square root of a given number. We define this function by iterative computation via the Raphson-Newton method. The function `iterate f a`
computes an infinite list `[a,f a, f (f a), f (f (f a)),...]`
. So in our case, it calls iteratively `improve` to compute better and better estimates. Once two consecutive elements of this infinite list are close enough, we finish the computation.
```haskell
sqr :: (Fractional a, Ord a) => a -> a
sqr x = convAbs $ iterate improve 1
  where improve r = (r + x/r) / 2
        convAbs (x1:x2:xs) | abs (x1-x2) < 1e-10 = x2
                           | otherwise = convAbs xs
```
I think that it is quite impressive that we can compute a derivative of the function `sqr` at a point even though it is defined by an iterative computation.
```haskell
> sqr (DN 9 1)
3.0 + 0.16666666666666666eps
```
Indeed, for $sqr(x)=\sqrt{x}$ we have $sqr'(x)=\frac{1}{2\sqrt{x}}$ so that $sqr'(9)=\frac{1}{6}=0.1\overline{6}$.

::: details Bonus exercise
For those interested, we can go even further and make `DualNum a`
an instance of `Floating`. If you recall the chain rule, i.e., for a compose function $f(x)=h(g(x))$, we have $f'(x) = h'(g(x))g'(x)$), then it is easy to decode the following definitions.
```haskell
instance (Floating a) => Floating (DualNum a) where
    pi               = DN pi 0
    exp (DN x x')    = DN r (r*x') where r = exp x 
    log (DN x x')    = DN (log x) (x' / x)
    sqrt (DN x x')   = DN r (x' / (2 * r)) where r = sqrt x
    sin (DN x x')    = DN (sin x) (x' * cos x) 
    cos (DN x x')    = DN (cos x) (-x' * sin x) 
    acos (DN x x')   = DN (acos x) (-x' / sqrt(1 - x*x))
    asin (DN x x')   = DN (asin x) ( x' / sqrt(1 - x*x))
    atan (DN x x')   = DN (atan x) ( x' / (1 + x*x))
    sinh x           = (exp x - exp (-x)) / 2
    cosh x           = (exp x + exp (-x)) / 2
    asinh x          = log (x + sqrt (x*x + 1))
    acosh x          = log (x + sqrt (x*x - 1))
    atanh x          = (log (1 + x) - log (1 - x)) / 2
```
Then it is possible to define any polymorphic function over an instance of `Floating` and we can compute its derivative. E.g.
```haskell
f2 :: Floating a => a -> a
f2 x = x^4 + sin (x^2) - exp x * log x + 7

-- the derivative of f2
df2 :: Floating a => a -> a
df2 x = 4*x^3 + 2 * x * cos (x^2) - exp x/x - exp x * log x

-- value of f2 and its derivative at 3
> f2 (DN 3 1)
66.34590079693074 + 73.77182176598502eps

-- value of df2 at 3
> df2 3
73.77182176598502
```
:::

## Task 1
Write a function `merge :: Ord b => (a -> b) -> [a] -> [a] -> [a]`
taking a function `f :: a -> b`
where `b`
is supposed to be an orderable type and two lists of elements of type `a`
. Suppose that these two lists are sorted via `f`
, i.e., for `[a1,a2,a3,...]`
we have `f a1 <= f a2 <= f a3 <= ...`. As a result, it returns a merged sorted list.

Once you have the function `merge`
, implement a function `subseqs :: [a] -> [[a]]` which takes a list and returns all its sublists (i.e., subsequences of its elements) sorted by their length. 

::: tip Hint
The subsequences can be generated recursively because subsequences of `x:xs` are just subsequences
of `xs` together with subsequences of `xs` extended by `x`. To produce the sorted result, use the
`merge` function.
:::

::: details Solution
```haskell
merge :: (a -> Int) -> [a] -> [a] -> [a]
merge _ [] ys = ys
merge _ xs [] = xs
merge f p@(x:xs) q@(y:ys) | f x < f y = x:merge f xs q
                          | otherwise = y:merge f p ys

subseqs :: [a] -> [[a]]
subseqs [] = [[]]
subseqs (x:xs) = merge length (subseqs xs) [x:ys | ys <- subseqs xs]
```
:::
