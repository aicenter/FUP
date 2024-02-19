---
title: "Exam Task: Sierpinski Carpet"
subtitle: "From: Exam 3 - 2023"
date: "2024-01-12"
author: "Niklas Heim"
author-url: ""
return-url: '../../'
return-text: '← Return home'
---

 
The *Sierpiński carpet* is a plane fractal first described by Wacław Sierpiński in 1916.
Your task is to generate this fractal in a text format represented as a list of strings. 
Each string represent a single row in the picture. The picture $f(n)$ is defined recursively.
For $n=0$, we define $f(0)="\#"$. For $n>0$, we define $f(n)$ as the picture depicted below.
In other words, $f(n)$ consists of eigth copies of $f(n-1)$ and 
the middle box of the same size as $f(n-1)$ filled with spaces.

<div style="width: 30%; margin: auto;">
![Recursive construction of the Sierpinski carpet.](../../img/sierpinski-carpet-construction.svg)
</div>


The first iterations $f(1)$ and $f(2)$ look as follows:
```
###                #########
# #                # ## ## #
###                #########  
                   ###   ###
                   # #   # #
                   ###   ###  
                   #########
                   # ## ## #
                   #########  
```

The size of the picture grows exponentially with $n$, namely it is $3^n$.

## IO Program

In Haskell, implement a function of type `Int -> [String]` that for a given
natural number $n$ returns the Sierpi\'nski carpet $f(n)$ represented as a list of strings.
Further, create an IO program allowing the user to enter the number $n$. As the size of 
the carpet quickly grows, it also asks the user to enter a window to be displayed.
More precisely, the user enters four numbers `row1`, `row2`, `col1`,
and `col2` separated by spaces. The program then displays the part of the 
carpet $f(n)$ starting on the row `row1` and the column `col1`, and ending on 
the row `row2-1` and the column `col2-1`. The rows and columns are indexed from $0$.
You may assume only valid inputs.

## Examples

```{.tight-code .haskell}
Enter n:
2
Enter row1 row2 col1 col2:
0 4 0 8
########
# ## ## 
########
###   ##
```

```{.tight-code .haskell}
Enter n:
4
Enter row1 row2 col1 col2:
10 11 18 36
# ## ## ## ## ## #
```

_Make sure your program outputs the messages as in the examples_ (i.e. `Enter n:`, etc).  Otherwise
the automatic evaluation will fail.

## Hints

To join the boxes represented as lists of strings in the top figure you may find the function
```{.tight-code .haskell}
zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]  
```
usefule, which, for a ternary function $g$ and three lists $[x_1,x_2,\ldots]$, 
$[y_1,y_2,\ldots]$, $[z_1,z_2,\ldots]$ creates a list of values
$[g(x_1,y_1,z_1),g(x_2,y_2,z_2),\ldots]$. 

To generate lists of a repeated value, you can use the function:
```{.tight-code .haskell}
replicate :: Int -> a -> [a]
```

To truncate a list e.g. from `st` to `end` use the following function:
```{.tight-code .haskell}
cut :: Int -> Int -> [a] -> [a]
cut st end = take (end-st) . drop st  
```

<details class="admonition">
<summary><strong>Solution</strong></summary>
```{.tight-code .haskell}
empty :: Int -> [String]
empty n = replicate n $ replicate n ' '

generateFractal :: Int -> [String]
generateFractal 0 = ["#"]
generateFractal n = upper ++ middle ++ upper
  where fr = generateFractal (n-1)
        size = length fr
        upper = zipWith3 (\x y z -> x ++ y ++ z) fr fr fr
        middle = zipWith3 (\x y z -> x ++ y ++ z) fr (empty size) fr

cut :: Int -> Int -> [a] -> [a]
cut st end = take (end-st) . drop st

main :: IO ()
main = do putStrLn "Enter n:"
          n <- read <$> getLine
          let fr = generateFractal n
          putStrLn "Enter row1 row2 col1 col2:"
          [r1,r2,c1,c2] <- map read . words <$> getLine
          mapM_ (putStrLn . cut c1 c2) $ cut r1 r2 fr
```
</details>
