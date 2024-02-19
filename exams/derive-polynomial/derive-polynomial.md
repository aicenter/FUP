---
title: "Exam Task: Derivatives Of Polynomials"
subtitle: "In Racket and Haskell"
date: "2024-01-12"
author: "Niklas Heim"
author-url: ""
return-url: '../../'
return-text: '← Return home'
---

In mathematics, a polynomial is an expression consisting of variables (also called indeterminates) and coefficients, that involves only the operations of addition, subtraction, multiplication, and non-negative integer exponents of variables. An example of a polynomial of a single indeterminate, x, is x2 + 4x , it’s first derivative would be 2x+4.
4.1 Implementation

In Haskell, polynomials can be represented as lists of pairs of the form (coefficient, exponent), e.g. x2 + 4x can be represented as [(4,1), (1,2)]. Implement the function derive_polynomial n polynomial, which takes an integer n and list of pairs polynomial, and computes the n-th derivative of the given polynomial represented as above. The function should return a list of pairs representing the n-th derivative of the given polynomial.
4.2 IO Wrapper
Wrap the derive_polynomial function in an IO main which first asks the user to input the number of nonzero terms of the following univariate polynomial by requesting “Enter number of nonzero terms:”. Afterwards, depending on the number, "Enter term as a pair of the form (coef,exp):” will require the user to sequentially input the terms. Each of these requests expects the user to input a pair of a coefficient and an exponent of the form (coefficient,exponent). Finally it will ask the user to input which derivative of the polynomial to compute by requesting “Enter derivative:”. The main function should then print the corresponding derivative of the polynomial represented by the entered list of pairs.

The output of the IO should print the polynomial in the following format:
The polynomial represented by [(5,1),(9,2),(1,4)] should be printed as “5*x + 9*x^2 + 1*x^4”. Note that terms should be sorted with respect to the exponents (lower exponents come first). For the printing you can assume that all the coefficients in test cases are positive integers. Thus the output of the IO will always has the format of “a*x^n + b*x^k + ...”.
Note the spaces between each of the terms.
In case you implement a type (which is optional) to represent the polynomial internally, make it an instance of Show class by implementing the show function for your polynomial type. 

A couple of notes:
You can expect only valid input (also only valid number of derivatives requested) as well as positive integer coefficients and integer exponents only. 
One option to convert a string str of the form (coef, exp) to a pair of type (Int, Int) is the read function as follows: (read str) :: (Int, Int)
Important note: Your polynomial should be sorted by increasing exponents. So a polynomial read through the IO in the order of [(1,4),(9,2),(5,1)] would be sorted to [(5,1),(9,2),(1,4)]. 

Hint: use the sortBy and compare functions as follows:
sortBy (\(_,e1) (_,e2) -> compare e1 e2) [(1,5), (3,2), (5,1)]
Examples

f (x) = x^4 + 9 x^2 + 5x
f’ (x) = 5 + 18x + 4x^3
The output ordered in increasing order of exponents:

> derive_polynomial 1 [(1,4),(9,2),(5,1)]
[(5,0),(18,1),(4,3)] 
4.2 IO Wrapper

Enter number of non-zero terms:
> 3
Enter term as a pair of the form (coef, exp):
> (1,4)
Enter term as a pair of the form (coef, exp):
> (9,2)
Enter term as a pair of the form (coef, exp):
> (5,1)
Enter derivative:
> 1

5 + 18*x + 4*x^3 
