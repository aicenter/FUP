# Lab 9: Haskell types


## Exercise 1
Define a type representing binary trees storing data in leaves of a general type `a`. Each
non-leaf node has always two children. Make your type an instance of the class `Show` so it can be
displayed in an XML-like format. A leaf node containing a datum `x` 
should be displayed as ''<Leaf
x/>'' and an inner
node `<Node>...children nodes...</Node>`. E.g., the following tree
```
     *
    / \
   *  'd'
  / \
'a'  *
    / \
  'b' 'c'
```
is displayed as
```
<Node>
  <Node>
    <Leaf 'a'/>
    <Node>
      <Leaf 'b'/>
      <Leaf 'c'/>
    </Node>
  </Node>
  <Leaf 'd'/>
</Node>
```

We will declare a recursive parametric data type `Tree a` over a general type `a`. There
will be two data constructors `Leaf` and `Node`.  The leaf contains a datum of type `a`, and the
node has a left and right subtree.
```haskell
data Tree a = Leaf a | Node (Tree a) (Tree a)
```

To make `Tree a` an instance of the `Show` class.  We have to constrain type `a` to be an instance
of `Show`; otherwise, it would not be clear how to display the data stored in the tree. The
definition of the function `show` is then straightforward. 
::: details Solution
```haskell
instance (Show a) => Show (Tree a) where
    show (Leaf x) = "<Leaf " ++ show x ++ "/>"
    show (Node left right) = "<Node>" ++ show left ++ show right ++ "</Node>"
```
:::

Now we can define the tree from the above picture as
```haskell
tree :: Tree Char
tree = Node (Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c'))) (Leaf 'd')

𝝺> tree
<Node><Node><Leaf 'a'/><Node><Leaf 'b'/><Leaf 'c'/></Node></Node><Leaf 'd'/></Node>
```

## Exercise 2
Consider the `Tree a` data type from the previous exercise. Write a function
```haskell
treeDepth :: Tree a -> Int
```
taking a tree and returning its depth.

The tree depth can be computed recursively as $\text{treeDepth}(t) = 1$ if $t$ is a leaf and
$\text{treeDepth}(t)=1+\max(\text{treeDepth}(left),\text{treeDepth}(right))$ if $t$ has two subtrees
$left$ and $right$.  This is a recursive definition that can be directly rewritten into Haskell as
follows:

::: details Solution
```haskell
treeDepth :: Tree a -> Int
treeDepth (Leaf _) = 1
treeDepth (Node left right) = 1 + max (treeDepth left) (treeDepth right)
```
:::

For the tree from the previous exercise, we have
```haskell
𝝺> treeDepth tree
4
```

## Exercise 3
Consider again the `Tree a` data type from Exercise 1. Write a function 
```haskell
labelTree :: Tree a -> Tree (a, Int)
```
labeling the leaves of a tree by consecutive natural numbers in the infix order. So it should replace a leaf datum `x` 
with the pair `(x,n)` for some natural number. So we would like to obtain something like that:
```
     *                  *
    / \                / \
   *  'd'             * ('d',3)
  / \       =>       / \
'a'  *          ('a',0) *
    / \                / \
  'b' 'c'        ('b',1) ('c',2)
```

::: tip
The idea behind this function will be helpful to your homework assignment. So try to understand it well. 
:::

To traverse through the nodes (particularly leaves) can be easily done recursively. The problem is with the counter for labels.
In an imperative programming language, we could introduce a variable `counter` and initialize it by 0. Once we encounter a leaf, we label
it with `counter` and modify
`counter = counter + 1`. Unfortunately, we cannot do that in a purely functional language like Haskell. 

We need an accumulator in the signature of the labeling function holding the counter value. So we could think of a helper function
```haskell
labelHlp :: Tree a -> Int -> Tree (a, Int)
```
taking as input the second argument representing the counter. Unfortunately, this is still not enough because such an accumulator can depend only
on the path leading from the root node into a leaf. However, the accumulator has to depend on the previously labeled leaves. So we must enrich the output of the helper function as well to send the counter value back when returning from a recursive call. So we need
to return not only the labeled tree but also the counter value as follows:
```haskell
labelHlp :: Tree a -> Int -> (Tree (a, Int), Int)
```
When we encounter a leaf node, we label it by the counter accumulator and return a labeled leaf with the increased counter value.
For the non-leaf nodes, we first label the left subtree by the numbers starting with the counter value. This results in a labeled tree and a new
counter value. Second, we label the right subtree with the numbers starting from the new counter value.

::: details Solution
```haskell
labelHlp :: Tree a -> Int -> (Tree (a, Int), Int)
labelHlp (Leaf x) n = (Leaf (x, n), n+1)
labelHlp (Node left right) n = let (left', n') = labelHlp left n
                                   (right', n'') = labelHlp right n'
                                in (Node left' right', n'')
```
:::

Finally, we wrap the helper function in the definition of `labelTree`. This definition just sets the counter value to 0, calls the helper function
and then project into the first component via the function `fst`.
```haskell
labelTree :: Tree a -> Tree (a, Int)
labelTree t = fst (labelHlp t 0)
```

## Task 1:
Define a recursive data type `Polynomial a` representing univariate polynomials with an
indeterminate $x$ whose coefficients are of a general type `a`. The definition will have two data
constructors. First, `Null` 
represents the zero polynomial. Second, `Pol` whose parameters are a
monomial and recursively the rest of the polynomial. Monomials should be represented as pairs of
type `(a, Int)` where the first component is the coefficient and the second is the exponent. E.g.
`(c,e)` represents $cx^e$. You can define a new name for that type as follow:
```haskell
type Monomial a = (a, Int)
```
Make your type an instance of show class. Polynomials should be displayed as follows:
```haskell
𝝺> Null
0

𝝺> Pol (3, 2) Null
3*x^2

𝝺> Pol (-2, 0) Null
(-2)

𝝺> Pol (-1, 0) (Pol (-2, 1) (Pol (1, 3) Null))
(-1) + (-2)*x^1 + 1*x^3
```

::: tip
First you can define a function `format`:
```haskell
format :: (Show a, Ord a, Num a) => Monomial a -> String
```
that returns a string representing a given monomial. Note the constraint on the type `a`. We assume
that coefficients are numeric values that can be added, subtracted, and ordered. The reason for this
is that we need to find out whether the given coefficient is negative, and in that case, we have to
wrap it into parentheses, i.e., we need to compare it with 0. Further, note that a constant monomial
has no `x^0`.  Then define the instance of `Show` for `Polynomial a`.  You need to constrain the
`show` function in the same way as `format`.
:::

<!--
::: details Solution
```haskell
type Monomial a = (a, Int)
data Polynomial a = Null | Pol (Monomial a) (Polynomial a)

format :: (Show a, Num a, Ord a) => Monomial a -> String
format (c, e) | e == 0 = display c
              | otherwise = display c ++ "x*^" ++ show e 
    where display k | k >= 0 = show k
                    | otherwise = "(" ++ show k ++ ")"

instance (Show a, Num a, Ord a) => Show (Polynomial a) where
    show Null = "0"
    show (Pol m Null) = format m
    show (Pol m ms) = format m ++ " + " ++ show ms
```
:::
-->

## Task 2:
Write a function 
```haskell
getDegree :: Polynomial a -> Int 
```
returning the degree of a given polynomial. The zero polynomial has a degree $-1$ by definition. Otherwise, you have to find the highest exponent occurring in the polynomial.

<!--
::: details Solution
```haskell
getDegree :: Polynomial a -> Int
getDegree p = iter p (-1) where
    iter Null n = n
    iter (Pol (_, e) ms) n | e > n = iter ms e
                           | otherwise = iter ms n 
```
:::
-->
