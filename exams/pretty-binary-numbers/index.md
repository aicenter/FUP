---
title: "Exam Task: Pretty Printing of Binary Numbers"
subtitle: "From: Exam 1 - 2022"
date: "2024-01-12"
outline: deep
---

# Pretty Printing of Binary Numbers

The goal of this assignment is to implement a Haskell program that reads a positive integer from
standard input, converts it into its binary representation, and displays this representation as a
text image. Each digit (i.e., $0$ and $1$) is represented as a $4 \times 4$ grid of characters. More
precisely, the text images of digits $0$ and $1$ are rendered in Haskell like below:

```haskell
    type Img = [String]

    zero :: Img
    zero = [".##.",
            "#..#",
            "#..#",
            ".##."]

    one :: Img
    one =  ["...#",
            "..##",
            "...#",
            "...#"]
```

# Haskell

Implement a function `main :: IO ()` that works as follows:

1. Display a message `Enter integer:`.
2. Let the user enter an integer $n$ (you may assume only valid inputs).
3. Convert $n$ into its binary representation.
4. Display the binary representation using the above text-images. The particular digits must be
   separated by a column consisting of the character `'.'`.

Below you can see an example if you execute the main function and the user enters 
the number 12.
```Haskell
> main
Enter integer:
12
...#....#..##...##.
..##...##.#..#.#..#
...#....#.#..#.#..#
...#....#..##...##.
```

All the displayed lines must end with the new-line character `'\n'` so you can display them e.g. by
`putStrLn`. No trailing whitespaces are allowed at the ends of lines.
