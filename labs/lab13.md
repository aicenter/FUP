# Lab 13: State Monad

This lab is focused on the state monad `State`. In the lecture, I show you how it is implemented. In
this lab, we are going to use the implementation from the lecture [`State.hs`](/code/State.hs). So
include the following lines in your source file:

```haskell
import Control.Monad
import State
import System.Random
import Data.List
```

The third import is important as we are going to work with pseudorandom numbers. The last import allows us to use some extra functions to manipulate lists.

::: tip Hint
If `import System.Random` doesn't work for you, you need to install the package `random` as follows:
  - either locally into the current directory by `cabal install --lib random --package-env .`
  - or globally by `cabal install --lib random`

If you don't have `cabal` (e.g. computers in labs), put the file [`Random.hs`](/code/Random.hs) into
the directory containing your Lab-13 code and replace `import System.Ramdom` with `import Random`.
:::

The state monad `State s a` is a type constructor taking two parameters `s` and `a` representing
type of states and output, respectively.  You can imagine this type as
```haskell
newtype State s a = State { runState :: s -> (a, s) }
```
The unary type constructor `State s` is an instance of `Monad`.  The values enclosed in this monadic
context are functions taking a state and returning a pair whose first component is an output value
and the second one is a new state. Using the bind operator, we can compose such functions in order
to create more complex stateful computations out of simpler ones.  The function `runState :: State s
a -> s -> (a, s)` is the accessor function extracting the actual function from the value of type
`State s a`.

As `State s` is a monad, we can use all generic functions working with monads, including the
do-notation. Apart from that, the implementation of the state monad comes with several functions
allowing to handle the state.

```haskell
get :: State s s                  -- outputs the state but doesn't change it
put :: s -> State s ()            -- set the state to the given value, outputs empty value
modify :: (s -> s) -> State s ()  -- modifies the state by the given function, outputs empty value

evalState :: State s a -> s -> a  -- computes just the output
evalState p s = fst (runState p s)

execState :: State s a -> s -> s  -- computes just the final state
execState p s = snd (runState p s)
```

States in purely functional languages must be handled via accumulators added into function signatures. Using the state monad allows us to abstract
away those accumulators.

## Exercise 1

Consider the function `reverse` reversing a given list. We can implement it as a tail-recursive function in the same way as in Scheme
using an accumulator.
```haskell
reverseA :: [a] -> [a]
reverseA xs = iter xs [] where
    iter [] acc = acc
    iter (y:ys) acc = iter ys (y:acc)
```

Now we try to implement that via the state monad. The above accumulator is a list. So we will use it as our state. We don't have to output anything
as the resulting reversed list is stored in the accumulator/state. Thus we are interested in the type `State [a] ()` whose values contain functions
of type `[a] -> ((), [a])`. We will implement a function `reverseS :: [a] -> State [a] ()` which takes
a list and returns a stateful computation reversing the given list (i.e., a monadic value enclosing a function of type `[a] -> ((), [a])` reversing the given list).

We will show several variants. The first copies more or less the tail recursive function `reverseA`.
::: details Solution: `reverseS`
```haskell
reverseS :: [a] -> State [a] ()
reverseS [] = return ()                  -- if the list is empty, keep the state untouched and return the empty value
reverseS (x:xs) = do ys <- get           -- if not, extract the state into ys
                     put (x:ys)          -- change the state to x:ys
                     reverseS xs         -- recursive call on the rest xs
```
:::

Now we can execute the returned computation as follows:
```haskell
> runState (reverseS [1,2,3,4]) []
((),[4,3,2,1])

> execState (reverseS [1,2,3,4]) []
[4,3,2,1]
```

The above variant just strips off the first element `x` and modifies the state by the function `(x:)`. Thus we can rewrite it as follows:
::: details Solution: `reverseS`
```haskell
reverseS :: [a] -> State [a] ()
reverseS [] = return ()                  -- if the list is empty, keep the state untouched and return the empty value
reverseS (x:xs) = do modify (x:)         -- if not, update the state
                     reverseS xs         -- recursive call on the rest xs
```
:::

Finally, the above variant is just applying the action `modify (x:)` for every `x` in the list. Thus we can use the monadic function
`mapM_ :: (a -> m b) -> [a] -> m ()` taking a function creating a monadic action from an argument of type `a` and a list of values of type `a`. The resulting action outputs the empty value. Once executed, it executes all the actions
returned by applying the given function to each element in the list.

::: details Solution: `reverseS`
```haskell
reverseS :: [a] -> State [a] ()
reverseS = mapM_ (modify . (:))
```
:::

## Task 1
Suppose you are given a list of elements. Your task is to return a list of all its pairwise different elements together with the number
of their occurrences. E.g. for `"abcaacbb"` it should return `[('a',3),('b',3),('c',2)]` in any order. A typical imperative approach to this problem is to keep a map from elements to their number of occurrences in memory (as a state). This state is updated as we iterate through the list. With the state monad, we can implement this approach in Haskell.

First, we need a data structure representing a map from elements to their numbers of occurrences. We can simply represent it as a list of pairs
`(el, n)` where `el` is an element and `n` is its number of occurrences. We also define a type representing a stateful computation over
the map `Map a Int`.

```haskell
type Map a b = [(a,b)]
type Freq a = State (Map a Int) ()
```

First implement a pure function `update` taking an element `x` and a map `m` and returning an updated map.
```haskell
update :: Eq a => a -> Map a Int -> Map a Int
```
If the element `x` is already in `m` (i.e., there is a pair `(x,n)`), then return the updated map,
which is the same as `m` except the pair `(x,n)` is replaced by `(x,n+1)`. If `x` is not in `m`,
return the map extending `m` by `(x,1)`. To check that `x` is in `m`, use the function
```haskell
lookup :: Eq a => a -> [(a, b)] -> Maybe b
```
that returns `Nothing` if `x` is not in `m` and otherwise `Just n`
where `n` is the number of occurrences of `x`.

::: details Solution: `update` (just copy this if you want to practice state monads only)
```haskell
update :: Eq a => a -> Map a Int -> Map a Int
update x m = case lookup x m of
    Nothing -> (x,1):m
    Just n -> (x,n+1):[p | p <- m, fst p /= x]
```
:::

Once you have that, take the inspiration from Exercise 1 and implement a function `freqS`, taking a list and returning the stateful computation
that computes the map of occurrences once executed. E.g.

```haskell
> execState (freqS "Hello World") []
[('d',1),('l',3),('r',1),('o',2),('W',1),(' ',1),('e',1),('H',1)]
```

::: details Solution: `freqS`

::: code-group
```haskell [mapM_]
freqS :: Eq a => [a] -> State (Map a Int) ()
freqS = mapM_ (modify . update)
```

```haskell [modify]
freqS [] = return ()
freqS (x:xs) = do modify (update x)
                  freqS xs
```

```haskell [get/put]
freqS [] = return ()
freqS (x:xs) = do m <- get
                  let m' = update x m
                  put m'
                  freqS xs
```
:::

## Exercise 2
 Recall that pseudorandom numbers from a given interval $(x,y)$ can be generated by the function
```haskell
randomR :: (RandomGen g, Random a) => (a, a) -> g -> (a, g)
```
located in the module `System.Random`. It takes an interval and a generator and returns a random value of type `a`
in the given interval, together with
a new generator. `Random` is a type class collecting types whose random values can be generated by `randomR`.
A first generator can be created by `mkStdGen :: Int -> StdGen` from a given seed.

If we want to generate a sequence of random numbers, we have to use in each step the new generator obtained from the previous step.
To abstract the generators away, we use the state monad whose states are generators, i.e., `State StdGen a` where `StdGen` is the type
of generators. The type `a` serves as the type of the generated random numbers. To shorten the type annotations, we introduce a new name:
```haskell
type R a = State StdGen a
```

Our task is to implement a function that integrates a function $f\colon\mathbb{R}\to\mathbb{R}$ on the given interval $(a,b)$
by the Monte-Carlo method, i.e., we want to compute approximately $\int_a^b f(x)\mathrm{d}x$.
For simplicity, we assume that $f(x)\geq 0$ for all $x\in (a,b)$. The Monte-Carlo method is a sampling method. If we know an upper bound $u$ for $f$ on
the interval $(a,b)$, we can estimate the area below the graph of $f$ by generating a sequence of random points in the rectangle
$(a,b)\times(0,u)$. Then we count how many points are below $f$. The integral equals approximately $\frac{k}{n}(b-a)u$ where
$k$ is the number of points below the graph of $f$, and $n$ is the number of all generated points (see the picture).
![](/img/montecarlo.png){class="inverting-image" style="width: 100%; margin: auto;" }

### Solution
 We first prepare a stateful computation, generating a sequence of random points in a given rectangle. We define two types:
```haskell
type Range a = (a,a)
type Pair a = (a,a)
```
The first one represents intervals and the second one points. Next, we define a function taking an interval and returning a stateful computation
generating a single random value in the given interval.

::: details Solution: `randR`
```haskell
randR :: Random a => Range a -> R a
randR r = do g <- get                      -- get the current state, i.e. generator
             let (x, g') = randomR r g     -- generate a random number x together with a new generator g'
             put g'                        -- change the state to g'
             return x                      -- output x
```
:::
The above function can be simplified using the constructor `state :: (s -> (a,s)) -> State s a` as was shown in the lecture.
```haskell
randR:: Random a => Range a -> R a
randR r = state (randomR r)
```

Since we need to generate points, we define a function taking two intervals and returning a stateful computation generating a random point in their Cartesian product.
::: details Solution: `randPair`
```haskell
randPair :: Random a => Range a -> Range a -> R (Pair a)
randPair xr yr = do
    x <- randR xr        -- generate x-coordinate
    y <- randR yr        -- generate y-coordinate
    return (x,y)         -- output the generated point
```
:::

Note that we don't have to deal with generators when sequencing `randR xr` with `randR yr`. Now if we want to generate a random point, we can
execute the stateful computation returned by `randPair`. E.g., we create an initial state/generator by `mkStdGen seed` and then use the `evalState` function because we are interested only in the output not the final generator.
```haskell
> evalState (randPair (0,1) (4,5)) (mkStdGen 7)
(0.6533518674031419,4.888537398010264)
```
To simplify the above call, we define a function executing any stateful computation of type `R a`.
```haskell
runRandom :: R a -> Int -> a
runRandom action seed = evalState action $ mkStdGen seed
```
Now we need a sequence of random points. We can define a recursive function doing that as follows:
::: details Solution: `randSeq`
```haskell
randSeq :: Random a => Range a -> Range a -> Int -> R [Pair a]
randSeq _ _ 0 = return []                        -- if the number of points to be generated is 0, returns []
randSeq xr yr n = do p <- randPair xr yr         -- otherwise, generate a point p
                     ps <- randSeq xr yr (n-1)   -- recursively generate the rest of points ps
                     return (p:ps)               -- output p:ps

> runRandom (randSeq (0,1) (4,5) 3) 7
[(0.6533518674031419,4.888537398010264),(0.9946813218691467,4.707024867915484),(0.8495826522836318,4.720133514494717)]
```
:::

The function `randSeq` just sequences the actions `randPair` and collects their results. So we can use `sequence`
allowing to take a list of monadic actions and return the action, which is the sequence of those actions returning the list
of their outputs. To create a list of `randPair` actions, use the function `replicate`.
```haskell
> replicate 5 3
[3,3,3,3,3]

> runRandom (sequence $ replicate 3 (randPair (0,1) (0,1))) 7
[(0.6533518674031419,0.8885373980102645),(0.9946813218691467,0.7070248679154837),(0.8495826522836318,0.7201335144947166)]
```
There is a monadic version of `replicate`. So we can rewrite the last call as follows:
```haskell
> runRandom (replicateM 3 (randPair (0,1) (0,1))) 7
[(0.6533518674031419,0.8885373980102645),(0.9946813218691467,0.7070248679154837),(0.8495826522836318,0.7201335144947166)]
```

Now we are ready to finish the Monte-Carlo integration. It takes as arguments a function $f$, an interval $(a,b)$, an upper bound $u$, and a number of points to be generated.
::: details Solution: `integrate`
```haskell
integrate :: (Double -> Double) -> Range Double -> Double -> Int -> Double
integrate f xr@(a,b) u n = whole * fromIntegral (length below) / (fromIntegral n) where  -- compute the area below f
    below = [(x,y) | (x,y) <- samples, y <= f x]                                         -- get the list of points below f
    whole = (b-a)*u                                                                      -- area of the rectangle
    samples = runRandom (replicateM n (randPair xr (0,u))) 123                           -- generate samples
```
:::

You can test it on functions you know their integrals.
```haskell
> integrate id (0,1) 1 10000    -- f(x)=x on (0,1) should be 0.5
0.499

> integrate (^2) (0,1) 1 10000  -- f(x)=x^2 on (0,1) should be 1/3
0.3383

> integrate sin (0,pi) 1 10000  -- f(x)=sin x on (0,pi) should be 2
2.0065352278478006

> integrate exp (0,1) 3 10000   -- f(x)=e^x on (0,1) should e-1
1.7226
```

## Task 2
 Implement a function generating a random binary tree having $n$ many nodes. To be more specific, consider the following type:
```haskell
data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Eq, Show)
```
The values of `Tree a` are binary trees having a value of type `a` in their nodes together with left and right children.
The `Nil` value indicates that there is no left (resp. right) child. Leaves are of the form `Node x Nil Nil`.

Your task is to implement a function `randTree` taking a number of nodes `n` and natural number `k` and returning a stateful
computation generating a random binary tree having `n` many nodes containing values from $\{0,1,\ldots,k-1\}$.

::: tip Hint
 To generate random integers in a given interval, use the above function `randR`. Generating a random binary tree can be done recursively. In each node, you generate a random integer $m$ from $\{0,\ldots,n-1\}$. This is the number of nodes of the left subtree.
So you recursively generate the left subtree `ltree` with `m` many nodes. Then you recursively generate the right subtree `rtree`
with $n-m-1$ many nodes. Finally, you return `Node x ltree rtree` where `x` is a randomly generated integer from $\{0,1,\ldots,k-1\}$.
The base case for $n=0$ just returns `Nil`, i.e., no subtree.
:::

::: details Solution: `randTree`
```haskell
randTree :: Int -> Int -> R (Tree Int)
randTree 0 _ = return Nil
randTree n k = do m <- randR (0,n-1)
                  ltree <- randTree m k
                  rtree <- randTree (n-m-1) k
                  x <- randR (0,k-1)
                  return $ Node x ltree rtree
```
:::

You can use your function to generate a random binary tree with 10 nodes containing integers from $\{0,1,2\}$ as follows:
```haskell
> runRandom (randTree 10 3) 1
Node 1 (Node 0 (Node 2 Nil Nil) (Node 1 (Node 2 Nil Nil) (Node 1 Nil Nil))) (Node 1 (Node 1 Nil Nil) (Node 2 Nil (Node 2 Nil Nil)))
```

You can also check that the method does not provide a uniform distribution.
```haskell
> trees = runRandom (replicateM 10000 (randTree 3 2)) 1
> execState (freqS trees) []
[387,234,210,448,221,187,207,223,200,214,403,428,218,409,222,379,199,199,235,206,215,209,184,223,206,187,222,416,187,213,190,193,438,231,222,221,205,204,209,196]
```
