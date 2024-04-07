# Lecture 8 - Haskell Types

## Haskell's type system

- _**Strong**_: Haskell *guarantees* that your program does not have any type-level errors. Strong
    also means that it will not do automatic type coercion (i.e. casting/type conversion).
- _**Static**_: All types are known at compile time, which does not only make your code safer, but
    can also buy you a lot of performance, because no type checks have to be done during runtime.
- _**Inferred**_: Haskell does most of the job of defining types for you by figuring out what your
    types are.

A strong and static type system is great, because it catches (*many*) bugs, because compiler can
*prove* absence of type errors. This requires a little more effort/thinking while writing your code
but you get something in return: higher confidence about correctness of your code and
**you have to write waaay fewer tests**.

::: tip Quote from [*Real World Haskell*](https://book.realworldhaskell.org/read/types-and-functions.html):
> A helpful analogy to understand the value of static typing is to look at it as putting pieces into a jigsaw puzzle. In Haskell, if a piece has the wrong shape, it simply won't fit. In a dynamically typed language, all the pieces are 1x1 squares and always fit, so you have to constantly examine the resulting picture and check (through testing) whether it's correct.
Some examples:
```haskell
𝝺> :type 'a'
'a' :: Char

𝝺> 'a' :: Char
'a'

𝝺> [1,2,3] :: Int

<interactive>:1:0:
    Couldn't match expected type `Int' against inferred type `[a]'
    In the expression: [1, 2, 3] :: Int
    In the definition of `it': it = [1, 2, 3] :: Int
```
:::

## Function types

Functions have types, too! The type signatures that you wrote/saw are created by the type
constructor `->`, which is right associative:
```haskell
mult :: Int -> Int -> Int -> Int
-- means:
mult :: Int -> (Int -> (Int -> Int))
```
This is chosen such that function application associates to the left, which gets us very natural
currying:
```haskell
mult :: Int -> Int -> Int -> Int
mult x y z = x*y*z

mult 5 4 :: Int -> Int
```
All functions in Haskell are automatically curried unless you explicitly define a function
that accepts a tuple:
```haskell
mult :: (a,a,a) -> a
mult (x,y,z) = x*y*z
```
The function above *cannot* be trivially curried.

'Automatically' curried functions can be very handy, for example when using them as higher order
functions:
```haskell
𝝺> map (+1) [1,2,3]
[2,3,4]

𝝺> filter (>0) [-1,0,2,-3,1]
[2,1]
```

## Polymorphism

We have functions like `head` / `length` / etc. that work on *any* list. The `length` function is a good example:
```haskell
length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs
```
You can immediately see that its behaviour does not at all depend on what kind of type `a` the list
contains. The way to obtain the length is always the same.

::: tip Parametric Polymorphism
Functions like `length :: [a] -> Int` are called *parametrically* polymorphic functions (because the
function type signature has a parameter `a`).
:::

There is a second kind of polymorphism, for example we would like the function `==` to work for any
type, but determining equality of two things depends on what we are checking for equality. When
comparing two booleans under the hood we have to do something different than when comparing two
characters, but still we want behaviour like
```haskell
𝝺> 1 == 1
True

𝝺> 'a' == 'b'
False
```

::: tip Ad-hoc Polymorphism
This kind of function overloading that is done with `==` is called *ad-hoc* polymorphism and in
Haskell it is implemented via *typeclasses*.
:::

For example, equality (`Eq`) in Haskell has its own typeclass:
```haskell
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
```
It defines two functions that need to be implemented for a new type to become part of this
typeclass. You can implement the typeclass for a given type by making it an instance of the
typeclass, for example the implementation of `Eq` for booleans could look like this:
```haskell
instance Eq Bool where
    True  == True  = True
    False == False = True
    _     == _     = False
```
There are many other type classes like `Num`, `Fractional` (a number that can be divided), `Ord`,
etc. If you are interested in how they work, try running e.g. `:i Num` in GHCi.

Polymorphic functions can contain **type constraints**, for example
```haskell
(==3) :: (Num a, Eq a) => Int -> Bool
```
is a function that checks if a number is equal to `3`. For this, the input has to be a number itself
and additionally, it has to implement `Eq`. Generally, everything before the `=>` in a type
signature is a type constraint.


## Type aliases

Often it makes sense to define type aliases (i.e. new names) for already existing (collections of)
types. For example, a string is just a list of characters:
```haskell
type String = [Char]
```
Here we have not defined a new type at all, we have just created a new name. This can be useful
for readability of your code, e.g. if you want a position to be a tuple of integers you can do:
```haskell
type Position = (Int, Int)
```

You can also make such aliases parametric with a type variable:
```haskell
type Pair a = (a,a)

mult :: Pair Int -> Int
mult (m,n) = m*n
```

## Algebraic Data Types

If you need completely new types, you can make use of the `data` keyword:
```haskell
data Answer = Yes | No | Unknown
```
Above, `Answer` is called a *type constructor*. It is the name of the collection of values `Yes`,
`No`, and `Unkown`. Similarly `Bool` is the type constructor of `True` and `False`, etc.  The values
like `Yes` are called *data constructors*, because they instantiate a concrete value of your type.
We can define, e.g. a list of answers:
```haskell
answers :: [Answer]
answers = [Yes, No, Unknown]
```
and pattern match on the values to define functions for our new data type:
```haskell
flip :: Answer -> Answer
flip Yes = No
flip No  = Yes
flip Unknown = Unknown
```

::: details What is a type, really?
A type is essentially a *set of values*. For example, a boolean is just
```haskell
data Bool = True | False
```
Analogously, an `Int` is essentially
```haskell
data Int = ... | -1 | 0 | 1 | ...
```
Types assign meaning to otherwise seemingly random bits, so they provide a means of *abstraction*.
Additionally, they prevent us from making mistakes (like adding a number to a string).
:::

The data constructors can have parameters to create values that contain other values. For example,
we can define a `Shape` type, which contains a `Circle` and a `Rect` value. A circle can be
defined by just its radius, but a rectangle has two sides. This can be reflected in the data
constructors:
```haskell
data Shape = Circle Float | Rect Float Float
```
Now we could construct a rectangle by calling `Rect 3 4`.
You can again see the interplay of type and data constructors by defining a function `square`, which
returns a value of type `Shape`. The value itself is a `Rect` with two equal sides.
```haskell
square :: Float -> Shape
square n = Rect n n
```
We can decompose our newly constructed types by pattern matching:
```haskell
area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y
```

Another level of abstraction lets us give parameters to data constructors. One of the most common
Haskell types is `Maybe a` which allows you to define safe operations.
```haskell
data Maybe a = Nothing | Just a
```
This type describes a value of *any type* `a` that either exists, or doesn't. For example, you could
define a safe version of the function `head` which will return `Nothing` for an empty list instead
of failing, and `Just a` for a non-empty list.
```haskell
safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead (x:_) = Just x
```

## Records

Purely positional data declarations can become impractical with a large number of fields. Therefore, fields can be named:
```haskell
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , phone :: String
                     , address :: String }
```

This allows to define records in arbitrary order
```haskell
defaultPerson = Person { lastName="Smith"
                       , firstName="John"
                       , ... }
```
Haskell also automatically defines accessors for each field, e.g.:
```haskell
firstName :: Person -> String
```

## Recursive definitions

Algebraic `data` type definitions (opposed to aliases which are defined with `type`) can be
recursive. For example, we implement our own `List` type
```haskell
data List a = Nil | Cons a (List a) deriving Show
```
which implements a parametric list with elements of type `a`. The values of the list can either be
`Nil` (i.e. the empty list), or they can be a `Cons` of an `a` and again a `List`. Hence, a list of
`[1,2,3]` would be constructed like:
```haskell
Cons 1 (Cons 2 (Cons 3 Nil)) :: Num a => List a
```
## Examples

### Expressions

- something neat: once we have `Show` we can immediately parse expressions from strings (I think?)
