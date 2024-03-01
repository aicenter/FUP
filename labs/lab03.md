# Lab 3: Higher-order functions

## Exercise 1
Implement a function `(mult-all-pairs lst1 lst2)` taking two lists and returning a list of all
possible binary products between elements from `lst1` and elements from `lst2`. Mathematically,
it could be written by a comprehension term as
```
[ x*y | x in lst1, y in lst2 ]
```
For example:
```scheme
(mult-all-pairs '(1 2 3) '(-2 0)) => (-2 0 -4 0 -6 0)`
```
Once you have it, generalize your
function to `(f-all-pairs f lst1 lst2)` so that the multiplication is replaced by any binary
function `f`:
```scheme
> (f-all-pairs cons '(1 2 3) '(a b))
((1 . a) (1 . b) (2 . a) (2 .  b) (3 . a) (3 . b)) 
```

::: tip Hint
These functions are just applications of two nested `map` functions. For each element `x` in `lst1`,
we multiply by `x` all elements in `lst2`. A function multiplying by `x` can be created from the
multiplication function `*` by partial application of `x`, i.e., we take the curryfied version of
`*` and apply it to `x` yielding `((curry *) x)`. Once we map `((curry *) x)` along `lst2`, the result is a list. So doing it for each `x` in `lst1` results in a
list of lists. Thus we have to flatten the result and append all the lists. This can be done by
`apply`-ing `append`. 
:::

::: details Solution
```scheme
(define (mult-all-pairs lst1 lst2)
  (apply   ; flatten the result
   append
   (map
    (lambda (x) (map ((curry *) x) lst2)) ; multiply all elements of lst2 by x
    lst1)))                               ; do it for each element x in lst1

(define (f-all-pairs f lst1 lst2)
  (apply
   append
   (map
    (lambda (x) (map ((curry f) x) lst2))
    lst1)))
```
:::

## Exercise 2

Suppose we represent univariate polynomials as lists of monomials. Each monomial of
the form $ax^n$ is represented as a list `(a n)` consisting of the coefficient `a` and the exponent
`n`. Thus the polynomial $2-3x+x^2$ is represented by `((2 0) (-3 1) (1 2))`. We
assume that each exponent can occur in the polynomial representation at most once. E.g. `((1 0) (2
0))` is not a valid representation. Devise functions `(add-pol p1 p2)` and `(mult-pol
p1 p2)` taking as arguments two polynomials `p1,p2` and returning their sum and product,
respectively. For example, let `p1` be `((1 0) (1 1))` (i.e., $p_1(x)=1+x$) and
`p2` `((-1 0) (1 1) (3 2))` (i.e., $p_2(x)=-1+x+3x^2$). Then 
```scheme
(add-pol p1 p2) => ((2 1) (3 2))  
(mult-pol p1 p2) => ((-1 0) (4 2) (3 3))
```
Even though it might look tedious, it is not so terrible because we will call higher-order functions to rescue us. Thanks to the folding function `foldl`, we will reduce our problem just to monomials. Let's start by defining simple operations on monomials. To make our code more comprehensible, we define the following functions extracting the coefficient and the exponent from a monomial.
```scheme
(define (get-coef m) (car m)) ; first component
(define (get-exp m) (cadr m)) ; second component
```
Next it is easy to define addition of two monomials of the same exponents, namely

$$ax^n + bx^n = (a+b)x^n.$$

Similarly, multiplication of monomials is defined by

$$(ax^n)(bx^k)=abx^{n+k}.$$

::: details Addition and multiplication on monomials
```scheme
(define (add-mon m1 m2)
  (list (+ (get-coef m1) (get-coef m2)) ; sum coefficients
        (get-exp m1)))                  ; keep exponent

(define (mult-mon m1 m2)
  (list (* (get-coef m1) (get-coef m2)) ; multiply coefficients
        (+ (get-exp m1) (get-exp m2)))) ; sum exponents
```
:::

Now we come to the main trick. Suppose we have two polynomials $p_1(x)=a_0+a_1x$ and
$p_2(x)=b_0+b_1x+b_2x^2$. We can express their sum as
$p_1(x) + p_2(x) = ((p_1(x) + b_0) + b_1x) + b_2x^2$. Thus we need only to add a **polynomial** and
a **monomial** at a single steps. Then the repetitive sum can be done by `foldl` function.
Similarly, for the multiplication, we can first compute products of all monomials by means of the
function `f-all-pairs` from Exercise 1 and then express the results as $p_1(x)p_2(x) = (((a_0b_0 +
a_0b_1x) + a_0b_2x^2) + a_1b_0x) + \cdots$.

Thus we need a function adding a monomial `mon` and a polynomial `pol`. The function has to distinguish two cases: 1) we add a monomial whose exponent does not occur in `pol`, 2) or whose exponent occurs in `pol`. So we first filter monomials in `pol` according to their exponents
to obtain the monomial of the same exponent as `mon` and the remaining monomials. If there is no monomial of the same exponent, we just cons `mon` to the result; otherwise we add monomials of the same exponent and cons it to the result.

::: details Addition of polynomial and monomial
```scheme
(define (add-mon-pol mon pol)
  (define (same-exp? m) (= (get-exp mon) (get-exp m))) ; #t if m has the same exponent as mon
  (define same-mon (filter same-exp? pol))             ; list containing the monomial of the same exponent or empty list
  (define rest (filter (compose not same-exp?) pol))   ; remaining monomials of different exponents
  (if (null? same-mon)                                 
      (cons mon rest)
      (cons (add-mon mon (car same-mon)) rest)))
```
:::

Finally, we can apply the folding function `foldl` to sum all monomials as was shown above. However, there are still two problems we have to deal with. 1) It may happen that the result contains monomials of the form $0x^n$. Such monomials can be clearly filtered out of the result. 2) It is common to sort monomials according to their exponents. Thus we define a function `normalize` solving these two problems.

::: details Final solution
```scheme
(define (normalize p)
  (define (non-zero-coef? m) (not (= 0 (get-coef m)))) 
  (sort
   (filter non-zero-coef? p)
   (lambda (p1 p2) (< (get-exp p1) (get-exp p2)))))
  
(define (add-pol p1 p2)
  (normalize (foldl add-mon-pol p1 p2)))

(define (mult-pol p1 p2)
  (normalize (foldl add-mon-pol '() (f-all-pairs mult-mon p1 p2))))
```
:::

## Task 1
Write a function `linear-combination` taking a list of vectors, a list of coefficients and returning
the corresponding linear combination. The function should be created in the curried form (the list
of vectors being the first argument) because it will be convenient for the next task. For example,
consider a linear combination $2\cdot(1, 2, 3) - 1\cdot(1, 0, 1) + 3\cdot(0, 2, 0) = (1,10,5)$. Then
your implementation should work as follow:
```scheme
((linear-combination '((1 2 3) (1 0 1) (0 2 0))) '(2 -1 3)) => (1 10 5)
```

::: tip Hint
Create first a binary function computing scalar multiplication of a scalar and a vector using `map`.
Then use the fact that `map` can apply the scalar multiplication to two lists simultaneously (in our
case, the list of coefficients and the list of vectors).  This results in a list of vectors
multiplied by respective coefficients. Then it suffices to sum them component by component.
:::

<!--
::: details Solution
```scheme
(define (scalar-mult coef vec)
  (map (curry * coef) vec))

(define (linear-combination vectors)
  (lambda (coefs)
    (apply map + (map scalar-mult coefs vectors))))
```
:::
-->

## Task 2
Use the function from the previous task to define a function `(matrix-mult m1 m2)` computing the
matrix multiplication of `m1` and `m2`. Then apply `foldl` function to define the power of a square
matrix, i.e., a function `(matrix-power k mat)` computing `k`-fold product of `mat`. You can assume
that $k\geq 1$, so there is no need to define the identity matrix. E.g.
```scheme
 (matrix-mult '((1 2 3) 
                (-1 0 2)) 
              '((1 -1) 
                (2 0) 
                (0 3))) => ((5 8) (-1 7)) 
                
 (matrix-power 3 '((2 3) (0 -1))) => ((8 9) (0 -1))
```

::: tip Hint
Use that the matrix multiplication is just a repeated application of the `linear-combination`
function. More precisely, consider $m_1\cdot m_2$. The $i$-th row of the result is just the linear
combination of the rows of $m_2$ with the coefficients taken from the $i$-th row of $m_1$. So it
suffices to apply the `(linear-combination m_2)` to each row of $m_1$. 
:::

To define the matrix power, use the `foldl` function applied to a list composed of the same matrix
`mat`. To create such a list, you can use the function
```scheme
> (make-list n el)
(el el ... el)
```

<!--
::: details Solution
```scheme
(define (matrix-mult m1 m2)
  (map (linear-combination m2) m1))

(define (matrix-power k mat)
  (foldl matrix-mult mat (make-list (- k 1) mat)))
```
:::
-->
