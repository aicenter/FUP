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
```scheme
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
```scheme
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
```scheme
(exists file tree)
```


### Hints

For splitting a string into a list of strings you can make use of the function `string-split`.


### Example

```scheme
(exists "src/tree.hs" (parse files))
> #t

(exists "src/test.hs" (parse files))
> #f
```


## Haskell

For the Haskell implementation you are provided with a module
[`FTree.hs`](https://github.com/aicenter/FUP/blob/main/code/FTree.hs) which contains a `FTree` type
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

Your filename has to end in `.hs`.

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
