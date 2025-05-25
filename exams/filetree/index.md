---
outline: deep
title: "Exam Task: Filetree"
subtitle: "In Racket and Haskell"
---

# Filetree

A filetree can be used to efficiently search/replace in large filesystems.
You can think of it as a tree with a variable size of nodes

```
.
├── scripts
│   ├── emptydir
│   ├── ex1
│   │   ├── eval.ss
│   │   └── test.ss
│   └── ex2
│       └── test.ss
├── src
│   ├── complex.hs
│   └── tree.hs
└── tests
    └── test_tree.hs
```


## Racket


In Racket, we define a filetree as nested dictionaries,
where each directory is a key for a filetree.
An empty filetree is just `#hash()` the filetree above would look like
```racket
'#hash(
  ("scripts" . #hash(
    ("emptydir" . #hash())
    ("ex1" . #hash(
      ("eval.ss" . #hash())
      ("test.ss" . #hash())))
    ("ex2" . #hash(
      ("test.ss" . #hash())))))
  ("src" . #hash(
    ("complex.hs" . #hash())
    ("tree.hs" . #hash())))
  ("tests" . #hash(
    ("test_tree.hs" . #hash()))))
```
In this formulation a leaf is represented by a pair of a string and am empty dictionary.  We can
then use filetrees to model a filesystem. The exercise consists in parsing a list of file paths
(directories separated by `/`), input as strings, into a filetree through a function `(parse
paths)`.


### Implementation

You are given a list of files (directories separated by `/`) like this:
```racket
(list "src/tree.hs"
      "src/complex.hs"
      "scripts/ex1/test.ss"
      "scripts/ex1/eval.ss"
      "scripts/emptydir"
      "scripts/ex2/test.ss"
      "tests/test_tree.hs"))
```
Parse the list of files into a tree and check if a new file or directory is
already in the tree with the function
```racket
(exists file tree)
```

Your file should be called `filetree.rkt` and `provide` the `parse` and `exists` functions.

### Hints

For splitting a string into a list of strings you can make use of the function `string-split`.


### Example

```racket
(exists "src/tree.hs" (parse files))
> #t

(exists "src/test.hs" (parse files))
> #f
```

::: details Exam Solution
```racket
#lang racket

(define (insert xs tree)
  (if (empty? xs)
      tree
      (let* ((k (car xs))
             (v (if (dict-has-key? tree k) (dict-ref tree k) #hash()))
             (newv (insert (cdr xs) v)))
        (dict-set tree k newv))))

(define (split file) (string-split file "/"))

(define (parse files)
  (foldl (lambda (file tree) (insert (split file) tree))
         #hash()
         files))

(define (exists file tree)
  (define (helper names tree)
  (cond [(empty? names) #t]
        [(empty? tree) #f]
        [(dict-has-key? tree (car names))
         (helper (cdr names) (dict-ref tree (car names)))]
        [else #f]))
  (helper (split file) tree))


;;;;;;;;;;;;;;;;;;;;;;

(define files
  (list "src/tree.hs"
        "src/complex.hs"
        "scripts/ex1/test.ss"
        "scripts/ex1/eval.ss"
        "scripts/emptydir"
        "scripts/ex2/test.ss"
        "tests/test_tree.hs"))

; (define a (insert '("asdf" "asdf") #hash()))
; (insert '("asdf" "sss") a)

(define tree (parse files))
(exists "src" tree)
(exists "src/complex.hs" tree)
```
:::

## Haskell

For the Haskell implementation you are provided with a module
[`FTree.hs`](/code/FTree.hs) which contains a `FTree` type
including a `Data.Map` from keys to values. We quote the definition here:
```haskell
import Data.Map (Map)
import qualified Data.Map as Map

data FTree a = FNil | FNode (Map a (FTree a)) deriving (Eq)
```
The type `Map k v` is a predefined type which represents a simple associative map.
You can use all functions that are defined on the `Map`. The `FTree` has a
constructor `FNode m` that takes a mapping `m` between strings and filetrees, and a
constructor `FNil`. The mapping `m` contains entries that associate directories and
their subtrees.

### Implementation

In Haskell, you are given a list of files (directories separated by `/`) like
this:

```haskell
files =  ["src/tree.hs"
         ,"src/complex.hs"
         ,"scripts/ex1/test.ss"
         ,"scripts/ex1/eval.ss"
         ,"scripts/emptydir"
         ,"scripts/ex2/test.ss"
         ,"tests/test_tree.hs"]
```

Parse the list of files into an `FTree` and check if a new file or directory is
already in the tree with the function
```
exists :: String -> FTree String -> Bool
```

Your file should be called `Filetree.hs`, contain a module of the same name, and export the `parse` and `exists` functions.

### Example

```haskell
Prelude> exists "src/tree.hs" (parse files)
True

Prelude> exists "src/test.hs" (parse files)
False
```

### Hints

You can use all functions that are available for `Map` (e.g. `empty`, `insert`,
`member`, `lookup`, `findWith`, `findWidthDefault`, ...), you just need to
import it.

For splitting a string into a list of strings you can make use of the functions
```haskell
break :: (a -> Bool) -> [a] -> ([a], [a])
takeWhile :: (a -> Bool) -> [a] -> [a]
dropWhile :: (a -> Bool) -> [a] -> [a]
```

::: details Exam Solution
**Explaination of the solution below**
```haskell
insert :: [String] -> FTree String -> FTree String
insert (d:r) (FNode ls) = FNode (Map.alter (Just . insert r . fromMaybe FNil) d ls)
```
There are two cases to consider here:

- If the key `d` does not exist we want to add `(d, FNil)` to the map.
- If `d` does exist we still might have to update the rest of the file tree, so we want to
    recursively add `(d, insert r (subtree at d))`

Updating a `Maybe` existing value based on a function can be done with `Map.alter`.
Map.alter wants an update function (`Maybe FTree String -> Maybe FTree String`)
This we can achieve via `(fromMaybe FNil)` which constructs a function that returns
`FNil` if it receives a `Nothing` and otherwise the `Just` value it receives
(i.e. the subtree at d).

In the `_exists` function if we find `x` in the current tree level, we want to recurse.
`Map.lookup` returns a `Maybe`, so we can lift `_exists xs` to work on a `Maybe FTree`
```haskell
  _exists (x:xs) (FNode ls) = fromMaybe False (_exists xs <$> (Map.lookup x ls))
```

**Full solution**

```haskell
module Filetree (parse, exists) where
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import FTree


splitOn :: Char -> String -> [String]
splitOn c s = case (break (== c) s) of
                (d, "") -> [d]
                (d, r) -> [d] ++ splitOn c (drop 1 r) where

split :: String -> [String]
split = splitOn '/'

insert :: [String] -> FTree String -> FTree String
insert [] tree = tree
insert (d:r) FNil =       FNode (Map.insert d (insert r FNil) Map.empty)
insert (d:r) (FNode ls) = FNode (Map.alter (Just . insert r . fromMaybe FNil) d ls)

parse :: [String] -> FTree String
parse files = foldl (\tree file -> (insert (splitOn '/' file) tree)) FNil files

exists :: String -> FTree String -> Bool
exists file tree = _exists (split file) tree where
  _exists [] tree = True
  _exists _ FNil = False
  _exists (x:xs) (FNode ls) = fromMaybe False (_exists xs <$> (Map.lookup x ls))


files = ["dir1/tree.hs"
        ,"dir1/complex.hs"
        ,"dir2/ex1/test.ss"
        ,"dir2/ex1/eval.ss"
        ,"dir2/emptydir"
        ,"dir3/ex2/test.ss"
        ,"dir3/test_tree.hs"]

main = do
  let tree = parse files
  putStrLn (showTree tree)

  print $ exists "dir1/tree.hs" tree
  print $ exists "dir1/asdf.hs" tree
  print $ exists "dir2/ex1" tree
  print $ exists "dir2/ex1/eval.ss" tree
  print $ exists "scripts/ex2/eval.ss" tree
```
:::
