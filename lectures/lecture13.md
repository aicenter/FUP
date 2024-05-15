# Monoids & Foldables

The *fold* operation is one of (if not *the*) most important construction in functional
programming. An example we have seen very often already is using `foldr` to sum a list of numbers:

```haskell
𝝺> sum = foldr (+) 0
𝝺> sum [1,2,3]
6
```

But there are many more things we can do with a fold! Another example is to define `and`:
```haskell
𝝺> and = foldr (&&) True
𝝺> and [True,True,True]
True

𝝺> and [True,False]
False
```
An perhaps a tiny bit more interesting, counting the number of a specific element in a list:
```haskell
𝝺> count e = foldr (\x acc -> if e==x then acc+1 else acc) 0
𝝺> count 2 [1,2,1,2,2,3]
3
```

Importantly, we can implement a number of important functions *in terms of fold*, so theoretically,
we don't need much more than a datastructure being foldable. For example:
```haskell
length = foldr (\_ -> (+1)) 0
map f = foldr ((:) . f) []
```

:::  tip
If we take a look at the type signature of `foldr` we see that it contains a `Foldable` type
constraint:
```haskell
foldr :: Foldable t => (b -> a -> b) -> b -> t a -> b
```
In this lecture we will explore the essence of this `Foldable` typeclass and pick at the different
parts that make a fold. Conceptually, there are two parts to folding:
1. The *aggregation* - represented by the function `b -> a -> b`. We will also call this a *monoid*.
2. The *traversal* - which walks over the foldable datastructure `t a`. This is what the `Foldable`
   typeclass is doing.
:::

## Monoids

What is an algebra: essentially a *domain* and an *operation*, (and some *properties*).
A *monoid* is an algebra with an operation that accepts two thigns from the domain and returns
another thing from the domain with the property of the operation being associative.

- $\mathbb N$, operation: $+::\mathbb N \rightarrow \mathbb N \rightarrow \mathbb N$, associativity ($a+(b+c)=(a+b)+c$)
- $\mathbb R$, operations: $+,-,*,\div$, associativity/commutativity for $+$,$*$
        

*Semigroup*: a set $S$ with an operation `<> :: a -> a -> a` (i.e. binary operation that takes two elements of $S$ and produces another such element)
```haskell
class Semigroup a where
  (<>) :: a -> a -> a -- assumed to be associative

-- list is a semigroup
instance Semigroup [] where
  (<>) = (++)

> [1,2,3] <> [4,5,6]
[1,2,3,4,5,6]
```

*Monoid* is a semigroup with an *identity* element
```haskell
class Semigroup a => Monoid a where
  mempty :: a
  
  mconcat :: [a] -> a
  mconcat = foldr (<>) mempty
  
  mappend :: a -> a -> a
  mappend = (<>)

instance Semigroup [] where
  mempty = []
```

---

We can have multiple monoids for e.g. `Int`s. An example are `+` and `*`
```haskell
newtype Sum a = Sum {getSum :: a}

instance Num a => Semigroup (Sum a) where
  (<>) = (+)
  stimes n (Sum a) = Sum (fromIntegral n * a)

instance Num a => Monoid (Sum a) where
  mempty = Sum 0
```
We can do the same as above for `Product`. *Why?*
- *abstraction*; lets us separate aggregation from traversal
- semigroups give us *associativity* (which we can use, e.g. we can evaluate in any order. *We can distribute large folds over a cluster*!)
---

`Map` is a monoid under `union`:
```haskell
𝝺> Map.fromList [(1,"a")] <> Map.fromList [(1,"b")] <> Map.fromList [(2,"c")]
fromList [(1,"a"),(2,"c")]
```

We could implement another monoid instance for `Map`:
```haskell
newtype MMap k v = MMap (Map.Map k v)

fromList :: Ord k => [(k,v)] -> MMap k v
fromList xs = MMap (Map.fromList xs)

instance (Ord k, Monoid v) => Semigroup (MMap k v) where
  (MMap m1) <> (MMap m2) = MMap (Map.unionWith mappend m1 m2)

𝝺> fromList [(1,"a")] <> fromList [(1,"b")] <> fromList [(2,"c")]
MMap (
 1 : "ab"
 2 : "c"
)

𝝺> fromList [('a', Sum 1)] <> fromList [('a', Sum 2)] <> fromList [('b', Sum 3)]
MMap (
 'a' : Sum {getSum = 3}
 'b' : Sum {getSum = 3}
)
```


## Foldables

![](lecture13/foldlist.png){class="inverting-image"}


With the definition of `Monoid`s, we have the *aggregation* part of the `fold`. Now we can can define the *traversal*-part:

```haskell
class Foldable t where  
  -- not that a->m is unary!
  -- foldMap Sum [1,2]
  foldMap :: Monoid m => (a -> m) -> t a -> m
  -- fold [Sum 1, Sum 2]
  fold :: Monoid m => t m -> m
  -- foldr (+) 0 [1,2]
  foldr :: (a -> b -> b) -> b -> t a -> b
```
- show `:i Foldable`

![](lecture13/fold.png){class="inverting-image"}

```haskell
instance Semigroup Count where
  (Count n1) <> (Count n2) = Count (n1+n2)

instance Monoid Count where
  mempty = Count 0

count :: a -> Count
count _ = Count 1

singleton :: k -> v -> MMap k v
singleton k v = MMap (Map.singleton k v)

𝝺> foldMap (\x -> singleton x (count x)) [1,2,3,3,2,4,5,5,5]
MMap (
 1 : Count 1
 2 : Count 2
 3 : Count 2
 4 : Count 1
 5 : Count 3
)
```

---

```haskell
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

instance Foldable Tree where
    foldMap f (Leaf x) = f x
    foldMap f (Node l r) = foldMap f l <> foldMap f r

tree :: Tree Int
tree = Node (Leaf 7) (Node (Leaf 2) (Leaf 3))

𝝺> foldMap Sum tree
Sum {getSum = 12}
```

---

```haskell
ws = words $ map toLower "Size matters not. Look at me. Judge me by my size, do you? Hmm? Hmm. And well you should not. For my ally is the Force, and a powerful ally it is. Life creates it, makes it grow. Its energy surrounds us and binds us. Luminous beings are we, not this crude matter. You must feel the Force around you; here, between you, me, the tree, the rock, everywhere, yes. Even between the land and the ship."

stats word = (count word, Min $ length word, Max $ length word)

𝝺> stats "size"
(Count 1,Min 4,Max 4)

groupBy :: (Ord k, Monoid m) => (a -> k) -> (a -> m) -> (a -> MMap k m)
groupBy keyf valuef a = singleton (keyf a) (valuef a)

-- foldMap (groupBy head (m3 count (Min . length) (Max . length))) ws
𝝺> foldMap (groupBy head stats) ws
MMap (
 'a' : (Count 10, Min 1, Max  6)
 'b' : (Count  5, Min 2, Max  7)
 'c' : (Count  2, Min 5, Max  7)
 'd' : (Count  1, Min 2, Max  2)
 'e' : (Count  3, Min 4, Max 11)
 'f' : (Count  4, Min 3, Max  6)
 'g' : (Count  1, Min 5, Max  5)
 'h' : (Count  3, Min 4, Max  5)
 'i' : (Count  6, Min 2, Max  3)
 'j' : (Count  1, Min 5, Max  5)
 'l' : (Count  4, Min 4, Max  8)
 'm' : (Count  9, Min 2, Max  7)
 'n' : (Count  3, Min 3, Max  4)
 'p' : (Count  1, Min 8, Max  8)
 'r' : (Count  1, Min 5, Max  5)
 's' : (Count  5, Min 4, Max  9)
 't' : (Count  8, Min 3, Max  5)
 'u' : (Count  2, Min 2, Max  3)
 'w' : (Count  2, Min 3, Max  4)
 'y' : (Count  6, Min 3, Max  4)
)
```

