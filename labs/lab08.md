<SolutionHider/>

# Lab 8: Haskell basics

The aim of the lab is to practice function definitions using pattern matching and guarded equations together with the list comprehension.

## Exercise 1
Write a function `separate :: [Int] -> ([Int], [Int])` taking a list and returning a pair of lists. The first
containing elements on indexes 0,2,4,... and the second on the indexes 1,3,5,... E.g.
```haskell
separate [1,2,3,4,5] => ([1,3,5], [2,4])
```

::: tip Hint
Use pattern matching `x:y:xs` and recursion.
:::


::: details Solution
```haskell
separate :: [Int] -> ([Int], [Int])
separate [] = ([], [])
separate [x] = ([x], [])
separate (x:y:xs) = let (evs, ods) = separate xs
                    in (x:evs, y:ods)
```
:::

## Exercise 2
Write a function `numToStr :: Int -> Int -> String` taking as input an integer `n` together with a `radix` denoting the number of symbols used to represent the number `n` (for example 2,10,16 for binary, decimal, hexadecimal representation respectively). This function returns a string containing the representation of `n` in the corresponding numerical system. For the representation, use the standard symbols `0123456789ABCDEF`.

Examples:
```haskell
numToStr 52 10 => "52"
numToStr 5 2 => "101"
numToStr 255 16 => "FF".
```

::: tip Hint
The representation can be obtained by consecutive division of `n` by `radix` and collecting the remainders. The integer division can be computed by the function `div` and the remainder after integer division can be computed by the function `mod`.
:::


::: details Solution
```haskell
numToStr :: Int -> Int -> String
numToStr n radix = if  n < radix then [chars !! n]
                   else (numToStr d radix) ++ [chars !! r]
                 where chars = ['0'..'9'] ++ ['A'..'F']
                       d = n `div` radix
                       r = n `mod` radix
```
:::

## Exercise 3
Write a function `split n xs` that takes a natural number `n` and a list `xs :: [Int]` and splits `xs` into a list of
lists of `n`-many consecutive elements. The last chunk of numbers can be shorter than `n`. E.g.
```haskell
split 3 [1..10] => [[1,2,3],[4,5,6],[7,8,9],[10]]
split 3 [1,2] => [[1,2]]
```
Use the function `split` to implement a function `average_n n xs` taking a list of integers and returning the list of the averages of `n` consecutive elements.
E.g.
```haskell
average_n 3 [-1,0,1,2,3] => [0.0,2.5]
```

::: tip Hint
You can use functions `take n xs` and `drop n xs`. The first one returns the list of the first `n` elements of `xs`. The second returns the remaining list after stripping the first `n` elements off. Further, use function `length xs` returning the length of `xs`.
:::


The function `split` can be written recursively. If the length of `xs` is less than or equal to `n`, then return just `[[xs]]`.
If it is bigger, then take the first `n` elements and cons them to the result of the recursive call of `split` after dropping the first `n` elements.

::: details Solution: `split :: Int -> [Int] -> [[Int]]`
```haskell
split :: Int -> [Int] -> [[Int]]
split n xs | (length xs) <= n = [xs]
           | otherwise = take n xs : (split n (drop n xs))

-- solution using splitAt :: Int -> [a] -> ([a], [a])
split n xs = case splitAt n xs of
               ([], _) -> []
               (a, b)  -> a : split n b
```
:::

The function `average_n` can be easily written via the list comprehension using `split`. The only caveat is the division operation involved in the computation of averages. Even though the inner lists after applying `split` are of the type `[Int]`, their averages are floating numbers. So the type of `average_n` is `Int -> [Int] -> [Float]`. We can compute the sum of an inner list by the function `sum` and its length by `length`, but the type system would complain if we want to divide them. One must convert the integer arguments into floating-point numbers to overcome this problem. This can be done by the function `fromIntegral` converting an integer into any more general numeric type.

::: details Solution: `average_n :: Int -> [Int] -> [Float]`
```haskell
average_n :: Int -> [Int] -> [Float]
average_n n ys =
  [fromIntegral (sum xs) / fromIntegral (length xs) | xs <- xss]
    where xss = split n ys
```
:::

## Task 1
Write a function `copy :: Int -> String -> String` that takes an integer `n` and a string `str` and returns
a string consisting of `n` copies of `str`. E.g.

```haskell
copy 3 "abc" => "abcabcabc"
```

::: details Solution { hideme }
```haskell
copy :: Int -> String -> String
copy n str | n <= 0 = ""
           | otherwise = str ++ copy (n - 1) str

-- tail recursive version
copy2 :: Int -> String -> String
copy2 n str = iter n "" where
    iter k acc | k <= 0 = acc
               | otherwise = iter (k-1) (acc ++ str)
```
:::

## Task 2
The Luhn algorithm is used to check bank card numbers for simple errors such as mistyping a
digit, and proceeds as follows:
  * consider each digit as a separate number;
  * moving left, double every other number from the second last, e.g., 1 7 8 4 => 2 7 16 4;
  * subtract 9 from each number that is now greater than 9;
  * add all the resulting numbers together;
  * the card number is valid if the total is divisible by 10.

Define a function `luhnDouble :: Int -> Int` that doubles a digit and subtracts 9 if the result is
greater than 9. For example:
```haskell
luhnDouble 3 => 6
luhnDouble 7 => 5
```

Using `luhnDouble` and the integer remainder function `mod`, define a function
`luhn :: [Int] -> Bool` that decides if a list of numbers representing a bank card number is valid. For
example:
```haskell
luhn [1,7,8,4] => True
luhn [4,7,8,3] => False
```

::: tip Hint
Since the numbers are processed from right to left, reverse first the list by the function `reverse`. Then apply the function `separate` from Exercise 1 to split the list into the numbers
to be luhnDoubled and the rest.
:::

::: details Solution { hideme }
```haskell
luhnDouble :: Int -> Int
luhnDouble n | n > 4 = 2*n - 9
             | otherwise = 2*n

luhn :: [Int] -> Bool
luhn xs = (sum evs + sum [luhnDouble x | x <- ods]) `mod` 10 == 0
    where rxs = reverse xs
          (evs, ods) = separate rxs
```
:::
