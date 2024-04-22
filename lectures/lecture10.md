# Haskell's `IO` & Monads

In this lecture we will accept the fact that in order to do *anything useful* with a programming
language we *need side effects*. A program that does not read input or produce output (IO) is
not very useful, because we would not have a means to access the results it produced.
Unfortunately, producing output is an impure operation.
For example, the `print` function puts something in `stdout`. Calling
`print` again adds a second line to `stdout`, hence, calling the function does not
always produce the same output. We are mutating something outside of our function!

```haskell
𝝺> print "Hello World"   | stdout: Hello World

𝝺> print "Hello World"   | stdout: Hello World
                                   Hello World
```

Believe it or not, this was quite a problem for functional programming languages!  Luckily, the
Haskell community came up with a solution: Haskell will work with the *whole world* as its state!
So, theoretically, `print` becomes a function that accepts a string, the current world, and outputs
a mutated world where the string has appeared on your screen:
```haskell
print :: String -> World -> World
```
This is now a completely pure function!
Very similarly, we could define a pure function that reads an input from the world:
```haskell
readStr :: World -> (String, World)
```
With this approach we could write pure programs that read from an input and write to an output like
this[^tsoding]:
```haskell
helloworld :: World -> World
helloworld w1 = w4 where
  w2         = print "What is your name?" w1
  (name, w3) = readStr w2
  w4         = print ("Hello " ++ name) w3
```

[^tsoding]: Stolen from [here](https://www.youtube.com/watch?v=fCoQb-zqYDI), which is a highly
    recommended video!

Of course it would be awfully annoying to have to pass around the state of the world in every
function that should perform IO. Hence, Haskellers came up with a special type that separates
impure, IO parts of programs from our normal, functional code. This type is called: `IO`.
Essentially, IO is encapsulating the state of the whole world as a function from one `World` state
to another `World` state (including an additional type we can use for, e.g. reading a string from
the world).
```haskell
type IO a = World -> (a, World)
```

In the rest of this lecture we will learn how to abstract away the `World` from our `IO` programs,
such that we can rewrite the `helloworld` function like this:
```haskell
helloworld :: IO ()
helloworld = do
  print "What is your name?"
  name <- readStr
  print ("Hello " ++ name)
```
Where the `World` completely disappeared and we can write code that looks very much like procedural
programming.

::: tip Note
The actual implementation of `IO` of course does not really operate with "the whole world". `IO` is
merely the type that signals: *Mutation happening here! Watch out!* But for the sake of your mental
model its nice to work with a `World` type in this lecture.
:::



## `IO` actions

Haskell's `IO` is a *functor* which satisfies further properties (collected under the name *monad*).
`IO` is a type constructor that produces values of type `IO a`
```haskell
type IO a = World -> (a, World)
```
which are called *actions*. When we run an IO action, it produces a value of type `a`. For example,
the function `readStr` with the definition of `IO` above just becomes:
```haskell
readStr :: IO String
```
so `readStr` can be regarded as an *action* that (once we run it) produces a value of `IO String`
(i.e. a modified world from which we read a `String`).
The `print` function from before has to be slightly modified to work with `IO`. It does not return
anything except a modified world, so we will represent the missing `a` as `()`:
```haskell
print :: String -> World -> ((),World)
```
such that written in terms of `IO` it becomes:
```haskell
print :: String -> IO ()
```
i.e. a function that accepts a string and outputs an action that only contains a mutated world,
nothing else.

With the definitions above we have completely hidden the `World` and we could try to rewrite
`helloworld`:
```haskell
helloworld :: IO ()
helloworld = 
  let ac_name = readStr          -- IO String
  in print ("Hello " ++ ac_name) -- This fails! We cannot ++ with an action!
```
The above code won't compile, because the function `(++) :: String -> String -> String` does not
work for the case we have here: `IO String -> String -> String`. We need a way to manipulate the
values that are hidden inside our `IO` actions. 

Taking a step back, what we really need is a way to sequence the
`readStr :: IO String` action with the `print :: String -> IO ()` action:
```haskell
??? :: IO String -> (String -> IO ()) -> IO ()
```

(Un)fortunately, we ran into the very same problem already last time when trying to sequence
[failing computations](lecture09#safe-computations) and the solution was a function `andThen`.
The more general concept that this function encapsulates is called a *Monad*.

## Monads

In the previous lecture we pulled out the boilerplate that was needed to chain computations into a
function `andThen`
```haskell
andThen :: Maybe a -> (a -> Maybe b) -> Maybe b
```
which accepted a value `Maybe a` from a potentially failing computation and inserted it into a
function `a -> Maybe b`.

The problem of chaining `IO` actions is almost exactly the same! We want to sequence a value coming
from `readStr` which is an `IO String` action and stick it into a `String -> IO ()` function. The
general typeclass that Haskell defines for this is called a `Monad`:
```haskell
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
```
Referring to the type variable `m` as a *computational context* we can unpack the definition above:
* The *bind* operator `(>>=)`, which sequences values and functions with a context `m`.
* `(>>)` does the same as `>>=`, it just neglects the value `a`. In fact it can be implemented in
    terms of `>>=`: `x >> f = x >>= \_ -> f`.
* `return` constructs a computational context. (Note, that this is just a normal function, not a
    statement like in procedural languages. You will see in a few paragraphs why this function is
    called *return*.)

Additionally, we can see that a `Monad` has a type constraint of `Applicative`. We will discuss
`Applicative`s in the next lecture; for now you can just think of this type constraint to be
`Functor` such that, every `Monad` is also a `Functor`.

::: details Monads are functors
Every monad is a functor as we can express `fmap` in terms of `>>=`:
```haskell
fmap :: (Monad m) => (a -> b) -> m a -> m b
fmap f x = x >>= return . f

-- return :: b -> m b
-- return . f :: a -> m b
```
:::


### `IO` Monad

Haskell already implements the monad instance of `IO` (and many other types) for us, so with its
help we can rewrite our `helloworld` function, but first, the two ingredients we need:

1. `>>`: composes two IO actions (the first action is performed only for its side effect), for
   example:

```haskell
main :: IO ()
main = 
  print "hello" >>
  print "world"
```

2. `x >>= f` is the action that performs first `x`, passes its result to `f` which returns a second
   action to be performed:

```haskell
-- print has type: String -> IO ()
main = getLine >>= print
```

The `helloworld` function first asks for a name, then reads from input, and finally prints `"Hello
NAME"`. These steps can be encapsulated in three IO actions, which have to be chained. We can start
with reading from input and printing, because we almost have the function written above. We just
want to do something slightly more complex than `print`:

```haskell
main = getLine >>= \name -> print ("Hello " ++ name)
```
Above, we call `getLine`, which produces an `IO String`. We want to use its value, so we can define
a function that accepts a string `name` and processes it before passing the result to `print`.
As the last step we want to ask `"What is your name?"`. This is also an IO action that as to happen
before we read/print to stdout. We don't care about the output of this action, we just want to
print, so we can use `>>`:
```haskell
main = 
  print "What is your name?" >>
  getLine >>=
  \name -> print ("Hello " ++ name)
```

Sometimes, in order to combine results of previous actions it is useful to just wrap a value in a
monadic context. This is what the function `return` is for:
```haskell
getSquare :: IO Int
getSquare = putStrLn "Enter number:"
            >> getLine
            >>= \line -> let n = read line
                         in return (n*n)
```
Above, we read a line, parse it to an `Int` (via `read`), and then make sure that the thing we
return from our lambda function is actually an `IO` action by using `return`.


## More safe computations

Recalling our two safe functions from the last lecture
```haskell
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just (head xs)

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs
```

We can now define `safeSecond` with the more general `>>=` that works for any monad:
```haskell
safeSecond :: [a] -> Maybe a
safeSecond xs = safeTail xs >>= safeHead
```

::: tip `>>=` implementation for `Maybe`:
```haskell
instance Monad Maybe where
  return  = Just
  Nothing >>= _ = Nothing
  Just x  >>= k = k x
```
:::

Let's assume we want to sum the first two elements of a list. We can do this in a safe way by
sequencing `safeHead`, `safeSecond` and then summing:
```haskell
sumFirstTwo :: Num a => [a] -> Maybe a
sumFirstTwo xs =
  safeHead xs >>=
  \first -> safeSecond xs >>=
  \second -> 
    return (first + second)
```
This kind of nesting of `>>=` and lambda functions can be come very tedious, and confusing. To
simplify things, and make them look very much like procedural programming, we can use `do`-notation.

## `do`-notation
`do`-notation is a syntax block (like e.g. `where` or `let`) that lets you sequence actions more
easily:
- Actions on a separate line get executed
- `value <- x` runs action `x` and binds the result to `v`

With `do` we can rewrite the above function to
```haskell
sumFirstTwo :: Num a => [a] -> Maybe a
sumFirstTwo xs = do
  first <- safeHead xs
  second <- safeSecond xs
  return (first + second)
```


## Other monads

```haskell
instance Monad [] where
  return x = [x]
  xs >>= k = concat (map k xs)
```
