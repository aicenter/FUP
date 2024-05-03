---
outline: deep
---

# Higher-order functions

In the previous lecture, we saw how to iterate through a list by means of recursion. In particular,
we implemented a recursive function filtering a given list according to a Boolean function which was
among the arguments. Thanks to this Boolean function, the filtering function was much more
universal. Functions operating over other functions are essential in functional programming.


::: tip Definition
A function taking other functions as arguments or returning a function or both is called a
*higher-order function*.
:::

Higher-order functions play a crucial role because they provide a higher level of abstraction. One
can unify similar computational patterns into an abstract function parametrized by functions
specifying a concrete function's behavior. An example might be the function
[`filter`](https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._filter%29%29),
whose behavior is modifiable by a Boolean function.
Another example is the function
[`apply`](https://docs.racket-lang.org/reference/procedures.html#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._apply%29%29)
of two arguments. The first is a function, and the second is a list. `apply` unwraps the members of
the list and applies the function to them. For example,
```scheme
> (apply + '(1 2 3 4))
10
> (apply append '((a b c) (d e f) (g h)))
'(a b c d e f g h)
```
We will see more examples in this lecture.

## List processing

I will first discuss fundamental higher-order functions allowing us to iterate through a list and
compute something based on its elements. If we want to transform a list, there are three basic
transformations we may like to apply:

1. Removing some of the list's members based on a condition. For this purpose, there is the function
   [`filter`](https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._filter%29%29)
   we already know.
2. Modifying each member of the list by a function. This can be achieved by the function
   [`map`](https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Fmap..rkt%29._map%29%29).
3. Aggregating the list's members into a single value. The functions
   [`foldl`](https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._foldl%29%29)
   and
   [`foldr`](https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._foldr%29%29)
   can do that.


### `filter`

Here are a few examples of filtering a list:
```scheme
> (filter (lambda (x) (> x 0)) '(0 1 -2 3 -4))
'(1 3)

> (filter char? '(1 #\a 2 #\b 3 #\c))
'(#\a #\b #\c)

> (filter (lambda (x) (eq? (car x) 'a)) '((a b c) (c d) (a d)))
'((a b c) (a d))
```
The first example keeps only positive numbers. The second removes each that is not a character. The
function
[`char?`](https://docs.racket-lang.org/reference/characters.html#%28def._%28%28quote._~23~25kernel%29._char~3f%29%29)
returns `#t` if its argument is a character and `#f` otherwise. The last example keeps only those
sublists starting with the symbol `'a`.

### `map` & `apply`

The function `map` applies a given function to each member of a list and returns the list of their images.
More precisely, for a function $f$ and a list $(a_1,a_2,\ldots,a_n)$, it returns $(f(a_1),f(a_2),\ldots,f(a_n))$.
For example,
```scheme
> (map (lambda (x) (* 2 x)) '(1 2 3))
'(2 4 6)

> (map car '((a b c) (c d) (a d)))
'(a c a)
```

In fact, `map` is even more general as it allows processing several lists of the same length. More precisely, let $f$ be an $m$-ary function. Applying `map` to $f$ and $m$-many lists of length $n$

$$
(a_{11},a_{12},\ldots,a_{1n}),\\
(a_{21},a_{22},\ldots,a_{2n}),\\
\ldots\\
(a_{m1},a_{m2},\ldots,a_{mn}),
$$

results in the following list:
$$
(f(a_{11},a_{21},\ldots,a_{m1}),f(a_{12},a_{22},\ldots,a_{m2}),\ldots,f(a_{1n},a_{2n},\ldots,a_{mn}).
$$

So if we view the input lists as rows of a matrix, `map` applies $f$ column-wise.
Note that all the input lists must be of the same length.

As an example, consider vector addition. If the input lists represent vectors, we can add them as follows:
```scheme
> (map + '(1 2 3) '(3 2 1) '(-4 -4 -4))
'(0 0 0)
```

If the vectors are in a list (i.e., they form a matrix), we must unwrap them using the function `apply`:
```scheme
> (apply map + '((1 2 3)
                 (3 2 1)
                 (-4 -4 -4)))
'(0 0 0)
```

Another nice application of `map` is the function computing the transpose of a matrix. Consider the following example:
```scheme
> (apply map list '((1 2 3)
                    (4 5 6)
                    (7 8 9)))
'((1 4 7)
 (2 5 8)
 (3 6 9))
```
Note that `apply` unwraps the rows from the list. So the above expression is equivalent to this one:
```scheme
(map list '(1 2 3)
          '(4 5 6)
          '(7 8 9))
```
Next, `map` apply the function `list` column-wise. Thus it collects elements from each column and creates a list.
The results are collected in a list. Using quasiquoting, we can write it down as follows:
```scheme
> `(,(list 1 4 7) ,(list 2 5 8) ,(list 3 6 9))
'((1 4 7) (2 5 8) (3 6 9))
```
Note that a special case of the matrix transposition is the useful [zipping](https://en.wikipedia.org/wiki/Zipping_(computer_science)) of two (or more) lists. For two lists of the same length, their zipping is the list consisting of the pairs of respective elements. E.g.,
```scheme
> (map list '(1 2 3) '(a b c))
'((1 a) (2 b) (3 c))
```
One more example of `map` extracts the diagonal of a matrix. It applies the function [`list-ref`](https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._list-ref%29%29) that takes a list and a position and returns the element on that position. To extract the diagonal, we need to get the first element from the first row, the second element from the second row, etc. Note that lists are indexed from zero.
```scheme
> (map list-ref '((1 2 3)
                  (4 5 6)
                  (7 8 9)) (range 0 3))
(1 5 9)
```

### `foldl` & `foldr`

Finally, I will focus on the aggregating higher-order functions `foldl` and `foldr` (known as
*folding functions*). There are two versions of these functions depending on the direction of how
the input list is processed. `foldl` iterates from left to right, and `foldr` from right to left.
They also differ in the space they need to do their computations. `foldl` operates in constant
space, whereas `foldr` needs $O(n)$ where $n$ is the list's length. The explanation is that `foldl`
is tail-recursive, but `foldr` is not.

Both folding functions have three arguments: a binary function, an initial value, and a list. I will
first formulate mathematically what the folding functions do. Let $A,B$ be sets and let $f\colon
A\times B\to B$ be a binary function, $b_0\in B$ an initial value, and
$\bar{a}=(a_1,a_2,\ldots,a_n)$ a list of elements from $A$. The functions `foldl` and `foldr` are
defined as follows:
$$
\mathrm{foldl}(f,b_0,\bar{a})=\begin{cases}
 b_0 & \text{if $\bar{a}$ is empty,}\\
 f(a_n,f(a_{n-1},\ldots,f(a_1,b_0)\ldots)) & \text{otherwise.}
 \end{cases}
$$
$$
\mathrm{foldr}(f,b_0,\bar{a})=\begin{cases}
 b_0 & \text{if $\bar{a}$ is empty,}\\
 f(a_1,f(a_2,\ldots,f(a_n,b_0)\ldots)) & \text{otherwise.}
 \end{cases}
$$
Thus depending on the length of the list, $\mathrm{foldl}$ computes a sequence of elements $b_1,b_2,\ldots,b_n$ such that $b_i=f(a_i,b_{i-1})$ and $b_n$ is the final value returned by $\mathrm{foldl}$. Graphically, we can visualize it as follows:
$$
b_0 \xrightarrow{f(a_1,b_0)} b_1 \xrightarrow{f(a_2,b_1)} b_2 \cdots \xrightarrow{f(a_n,b_{n-1})} b_n
$$
The function $\mathrm{foldr}$ behaves analogously, but the list's elements are processed in the reversed order, starting from $a_n$ and iterating back to $a_0$.
$$
b_0 \xrightarrow{f(a_n,b_0)} b_1 \xrightarrow{f(a_{n-1},b_1)} b_2 \cdots \xrightarrow{f(a_1,b_{n-1})} b_n
$$
We can view $b_i$s as intermediate results, which get updated by the function $f$ based on $a_i$s.

Now, it is time for some concrete examples. We can sum all the elements in a list as follows:
```scheme
> (foldl + 0 '(1 2 3))
> (+ 3 (+ 2 (+ 1 0)))
6

> (foldr + 0 '(1 2 3))
> (+ 1 (+ 2 (+ 3 0)))
6
```
Note the difference between `foldl` and `foldr`.

Following the example above, one can multiply the elements of a list or compute their minimum or maximum. E.g.,
the following expression computes the minimum:
```scheme
> (foldl min +inf.0 '(0 -3 2))
-3.0
```

More exciting examples of folding are situations when the function $f$ operates over different sets $A$ and $B$. Consider a robot that can move up, down, left, or right in a grid. Its position is determined by a vector $(x,y)\in\mathbb{Z}^2$. Any movement changes its position by $\pm 1$ in the respective coordinate. Given an initial position and a sequence of actions, we want to compute the resulting robot's position.

We represent the robot's actions by the symbols `'up`, `'down`, `'left`, and `'right`. First, we need to implement a function `move` that computes a new position from a given action and the current position.
```scheme
(define (move cmd pos)
  (cond
    [(eq? cmd 'up) (map + pos '(0 1))]
    [(eq? cmd 'down) (map + pos '(0 -1))]
    [(eq? cmd 'left) (map + pos '(-1 0))]
    [(eq? cmd 'right) (map + pos '(1 0))]))
```
Note that we apply the vector addition via the function `map`.

Now it is a simple application of folding to compute the resulting position from any initial state for a sequence of actions. For example,
```scheme
> (foldl move '(0 0) '(up right right up left))
'(1 2)
```

## Currying and compositions

There are also higher-order functions that are not related to lists. I will discuss some of them now. At the same time, I will introduce the concept of [currying](https://en.wikipedia.org/wiki/Currying).

Currying is another way to view $n$-ary functions for $n>1$. Consider a binary function

$$f\colon A\times B\to C.$$

This means that $f$ maps a pair $(a,b)\in A\times B$ to an element $f(a,b)\in C$. In other words, if we provide all the inputs, the function $f$ returns a value in $C$. Currying equivalently presents $f$ as a function taking arguments successively one by one.

More precisely, let $B\to C$ denote the set of all functions from $B$ to $C$. The function $f$ can be equivalently viewed as a function $\hat{f}\colon A\to(B\to C)$ mapping elements from $A$ into functions from $B\to C$. Indeed, one can define $\hat{f}(a)=g_a\in B\to C$ where $g_a(b) = f(a,b)$. Consequently, if we apply $\hat{f}$ to an element $a\in A$, we obtain a function $g_a$, and if we apply $g_a$ to $b$, we get the final result. The function $\hat{f}$ is called the *curried* version of $f$.

Let us see a concrete example. Suppose that $f\colon\mathbb{R}\times\mathbb{R}\to\mathbb{R}$ defined by $f(a,b) = \sqrt{a^2 + b^2}$. In other words, $f$ assigns the distance of the point $(a,b)$ from the origin $(0,0)$. Using currying, we can view it as the function $\hat{f}\colon\mathbb{R}\to(\mathbb{R}\to\mathbb{R})$ such that $\hat{f}(a)=g_a\colon\mathbb{R}\to\mathbb{R}$ where $g_a(b)=\sqrt{a^2 + b^2}$. For example, $\hat{f}(2) = g_2$ where $g_2(b) = \sqrt{4 + b^2}$.

The above currying process can be generalized to functions of any arity larger than 2. For example, currying transforms a ternary function $f\colon A\times B\times C\to D$ into a function $$\hat{f}\colon A\to(B\to(C\to D)).$$

We can apply currying to Racket functions as well. A function [`curry`](https://docs.racket-lang.org/reference/procedures.html#%28def._%28%28lib._racket%2Ffunction..rkt%29._curry%29%29) transforms any Racket function into its curried version. For example, if we currify the multiplication and supply only a single argument, we get a function:
```scheme
> ((curry *) 2)
#<procedure:curried:*>
```
If we supply both successively, we get the result:
```scheme
> (((curry *) 2) 3)
6
```
Thus `((curry *) 2)` represents the function "multiply by two". Actually the notation `((curry *) 2)` can be simplified as follows:
```scheme
(curry * 2)
```
Currying helps to simplify our code if we need partially evaluated functions. Suppose, for instance, we need to multiply each list element by two. Compare the following possibilities:
```scheme
> (map (lambda (x) (* 2 x)) '(1 2 3))
'(2 4 6)
> (map (curry * 2) '(1 2 3))
'(2 4 6)
```
Another situation is if we need to implement a curried function. For instance, if we know that we will use that function partially evaluated. We can define it as usual and then apply `curry`. However, that would be too complicated. It is better to define our function directly in the curried form. It is easy to do. We use the lambda abstraction to return a function. For example,
consider the function [`list-ref`](https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._list-ref%29%29) returning the element of a list at a given position. We can create a curried `list-ref` as follows:
```scheme
(define (curried-list-ref lst)
  (lambda (i) (list-ref lst i)))

> ((curried-list-ref '(a b c)) 2)
'c
```
Racket allows simplifying the above construction with the lambda abstraction by adding parentheses:
```scheme
(define ((curried-list-ref lst) i)
  (list-ref lst i))
```

A further higher-order function I would like to mention is [`compose`](https://docs.racket-lang.org/reference/procedures.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._compose%29%29). It allows writing programs in the so-called point-free style. Functions in Racket are usually formulated as function applications to its arguments. For example, suppose we want to implement a function taking a string and removing all characters that are not alphabetical. A standard approach looks as follows:
```scheme
(define (str->alpha str)
  (list->string (filter char-alphabetic? (string->list str))))

> (str->alpha "ab34cd#e")
"abcde"
```
Thus we first convert the string into a list, then apply a filter keeping only alphabetical characters, and finally convert the list back to a string. Note that the name of the string `str` is not important for the definition. It would be sufficient to say that our function `str->alpha` is a composition of three functions without specifying the string name `str`. The function `compose` can do this. It represents the ordinary mathematical function composition operation $\circ$.
```scheme
(define str->alpha
  (compose list->string
           (curry filter char-alphabetic?)
           string->list))
```
The above definition is said to be *point-free* as it does not mention the input data.
Some people claim this approach is less cluttered, especially in languages with an elegant syntax for function composition.
Note that `compose` composes functions from right to left, as is the usual mathematical convention.

### Example - morphic sequences

Now, it is time for a slightly more complex example showing how to apply higher-order functions. For that purpose, I choose morphic sequences and the curves they generate.[^morphic-seq] What is a morphic sequence? It is an infinite sequence of $0$s and $1$s generated by a morphism $\phi\colon\{0,1\}\to\{0,1\}^*$. A morphism $\phi$ assigns to $0$ (resp. $1$) a finite sequence (word) consisting of $0$s and $1$s. For example, the well-known [Thue-Morse sequence](https://en.wikipedia.org/wiki/Thue%E2%80%93Morse_sequence) can be generated by the morphism defined by $\phi(0) = 01$ and $\phi(1) = 10$.

[^morphic-seq]: If you are interested in details on morphic sequences and their curves, see the
    following paper: H. Zantema. Turtle Graphics of Morphic Sequences, *Fractals*, vol. 24, no. 1, 2016. doi:10.1142/S0218348X16500092.

Once we have a morphism $\phi$ and a initial word $w_0\in\{0,1\}^*$, we can generate a sequence of words
$$w_0,w_1=\phi(w_0),w_2=\phi(w_1),w_3=\phi(w_2),\ldots$$
whose limit is the corresponding morphic sequence.

The image $\phi(w_i)$ is computed by replacing each single letter $a$ (i.e., $0$ or $1$ in our case) in $w$ by $\phi(a)$. For instance, the Thue-Morse sequence is generated as follows:

$$
  0, \phi(0)=01, \phi(01)=0110, \phi(0110)=01101001, \ldots
$$

In our example, we will repeat this generating step applying $\phi$ only finitely many times to obtain an initial segment $w_k$ of the morphic sequence.

We can visualize the generated sequence $w_k$ as a curve in the plane. Suppose we have a turtle on the plane at a position heading in a direction. We can turn the turtle or let it make a step ahead while drawing a line. We define a map $\alpha\colon\{0,1\}\to\mathbb{R}$ assigning to our letters $0$ and $1$ some angles $\alpha(0)$ and $\alpha(1)$ in radians. Consequently, we can iterate through the sequence $w_k$, and for each letter $a$, we let the turtle turn by $\alpha(a)$ and make a step ahead.

To see concrete examples of such curves, consider first the morphism $\phi_1$ defined by
$$\phi(0) = 011,\quad \phi(1) = 0.$$
This morphism generates the following sequence if we start with the initial word $0$:
$$w_0=0, w_1=\phi(0)=011, w_2=\phi(011)=01100, w_3=\phi(01100)=01100011011,\ldots$$
To visualize this sequence, we define the turning angles as follows:
$$\alpha_1(0)=\frac{7}{9}\pi,\quad\alpha_1(1)=-\frac{2}{9}\pi$$
The visualization for $w_{15}$ is depicted below:

![](/img/morphic-sequence1.png){ style="width: 60%; margin: auto;" class="inverting-image"}

For the second example, we consider the morphism $\phi_2$ defined by
$$\phi(0) = 00,\quad \phi(1) = 101.$$
This morphism generates the following sequence if we start with the initial word $1$:
$$w_0=1, w_1=\phi(1)=101, w_2=\phi(101)=10100101, \ldots$$
To visualize this sequence, we define the turning angles as follows:
$$\alpha_2(0)=\frac{5}{16}\pi,\quad\alpha_2(1)=-\frac{29}{60}\pi$$
The visualization for $w_{10}$ is depicted below:

![](/img/morphic-sequence2.png){ style="width: 60%; margin: auto;" class="inverting-image"}

Now I will discuss generating these images in Racket using higher-order functions and currying. We
will use the same turtle library as [before](lecture01#drawing-trees) to draw the pictures. Thus our
code starts with the following line:
```scheme
(require graphics/value-turtles)
```

Next, we need to represent the generating morphism $\phi$. Since we will use our code to generate different morphic sequences, it is reasonable to implement a function returning the morphism $\phi$ based on the images of $0$ and $1$. Using currying, we can do it as follows:

```scheme
(define ((phi im0 im1) x)
  (cond
    [(= x 0) im0]
    [(= x 1) im1]))
```
The higher-order function `phi` takes two values, `im0` and `im1`, and returns a function mapping $0$ to `im0` and $1$ to `im1`. Thus to create the morphism $\phi_1$ from the first example, we just call
```scheme
(phi '(0 1 1) '(0))
```
Note that we represent words as lists.

Next, we need to be able to apply $\phi$ to a word, i.e., a list of $0$s and $1$s. As we need to apply $\phi$ to each element in the word, this can be handled by `map`. However, there is a small problem. Consider the following expression:

```scheme
> (map (phi '(0 1 1) '(0)) '(0 1 1))
'((0 1 1) (0) (0))
```

It is not evaluated to `'(0 1 1 0 0)` as we need, but to `'((0 1 1) (0) (0))`. Thus it is necessary to append the inner lists.
```scheme
> (apply append (map (phi '(0 1 1) '(0)) '(0 1 1)))
'(0 1 1 0 0)
```
So we can define a function applying any $\phi$ to any word.
```scheme
(define (apply-morphism morphism w)
  (apply append (map morphism w)))
```
Using `apply-morphism`, we can apply $\phi$ once, but we must repeat it several times. We start with an initial word $w_0$ and $k$-times apply $\phi$ obtaining a word $w_k=\phi(\phi(\ldots\phi(w_0)\ldots))$. This resembles the folding function successively updating an intermediate result by a function. Our function `apply-morphism` takes a morphism $\phi$ and an intermediate state $w_i$ and returns $w_{i+1}$. If we create a list consisting of $k$ many functions $\phi$, we can use `foldl` to compute $w_k$ as follows:
```scheme
> (foldl apply-morphism '(0) (make-list 3 (phi '(0 1 1) '(0))))
'(0 1 1 0 0 0 1 1 0 1 1)
```
The function [`make-list`](https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._make-list%29%29) creates a list of length $k$ with a constant value $v$. E.g.,
```scheme
> (make-list 5 'a)
'(a a a a a)
```
Thus in the above `foldl` code, we created the list of three copies of $\phi_1$ and applied it successively to $w_0=0$.
To make it general, we define a function generating $w_k$ for a given morphism $\phi$, an initial word $w_0$, and the number of iterations $k$.

```scheme
(define (generate-seq morphism w0 k)
  (foldl apply-morphism w0 (make-list k morphism)))
```
<!-- [^composition-alt]:
Alternatively, we could exploit function composition to define `generate-seq`. To compute $w_k$, we need to compose $\phi$ with itself $k$ times and apply the composed function to $w_0$, i.e., $w_k=\phi^{(k)}(w_0)$ where $\phi^{(k)}=\underbrace{\phi\circ\cdots\circ\phi}_{k\text{ times}}$. -->

Now we define the two morphic sequences from the above examples:
```scheme
(define morphic-seq (generate-seq (phi '(0 1 1) '(0)) '(0) 15))
(define morphic-seq2 (generate-seq (phi '(0 0) '(1 0 1)) '(1) 10))
```

Next, we must generate a program for the turtle for a generated word $w_k$. Analogously to the definition of `phi`, we define a curried function `alpha` returning the function $\alpha$ for given angles `ang0` and `ang1` corresponding to $0$ and $1$, respectively.
```scheme
(define ((alpha ang0 ang1) x)
  (cond
    [(= x 0) ang0]
    [(= x 1) ang1]))
```
To create the sequences of turning angles for the turtle defined respectively by $\alpha_1$ and $\alpha_2$ from our morphic sequences, we can simply use `map` as follows:
```scheme
(define angle-seq (map (alpha (/ (* 7 pi) 9) (/ (* -2 pi) 9)) morphic-seq))
(define angle-seq2 (map (alpha (/ (* 5 pi) 16) (/ (* -29 pi) 60)) morphic-seq2))
```

Finally, it remains to create an initial empty picture for the turtle and transform the angle sequences into its commands.
The initial picture is of size 800x800:
```scheme
(define init (turtles 800 800))
```
For a turning angle and an intermediate image, we define a function `turn-draw`, turning the turtle by that angle and moving it one step ahead. The step length is given as a parameter to make `turn-draw` more flexible.
```scheme
(define ((turn-draw length) angle img)
  (draw length (turn/radians angle img)))
```
To draw the curves, it suffices to apply `foldl` to `turn-draw`, the initial picture `img`, and the sequence of angles:
```scheme
(foldl (turn-draw 12) init angle-seq)
(foldl (turn-draw 12) init angle-seq2)
```

Below you can find the complete code:
```scheme
#lang racket
;;; Example - morphic sequences
(require graphics/value-turtles)

(define ((phi im0 im1) x)
  (cond
    [(= x 0) im0]
    [(= x 1) im1]))

(define (apply-morphism morphism w)
  (apply append (map morphism w)))

(define (generate-seq morphism w0 k)
  (foldl apply-morphism w0 (make-list k morphism)))

(define ((alpha ang0 ang1) x)
  (cond
    [(= x 0) ang0]
    [(= x 1) ang1]))

(define morphic-seq (generate-seq (phi '(0 1 1) '(0)) '(0) 15))
(define angle-seq (map (alpha (/ (* 7 pi) 9) (/ (* -2 pi) 9)) morphic-seq))

(define morphic-seq2 (generate-seq (phi '(0 0) '(1 0 1)) '(1) 10))
(define angle-seq2 (map (alpha (/ (* 5 pi) 16) (/ (* -29 pi) 60)) morphic-seq2))

(define init (turtles 800 800))

(define ((turn-draw length) angle img)
  (draw length (turn/radians angle img)))

(foldl (turn-draw 12) init angle-seq)
(foldl (turn-draw 12) init angle-seq2)
```

## Closures

We have seen that higher-order functions can return a value that is a function. Such a function value is created by lambda abstraction. I will discuss a bit more what is this function value. For example, the following curried addition returns a function:

```scheme
(define (make-adder x) (lambda (y) (+ x y)))
```
This lambda expression defines the returned value:
```scheme
(lambda (y) (+ x y))
```
If we inspect the body of the returned function, we can spot the variable `x`, which is not among the arguments. Such a variable is called *free*. The question is how `x` will be evaluated if we call the returned function. Apparently, if we create such a function value, for instance, by calling
```scheme
(make-adder 5)
```
the returned function should interpret `x` as `5`. Similarly, the function returned by
```scheme
(make-adder 7)
```
should interpret `x` as `7`. This means that the above-returned function values are different.
The next question is how these function values are actually represented. A naive approach would be to substitute for `x` particular values and store them as follows:
```scheme
(lambda (y) (+ 5 y))
(lambda (y) (+ 7 y))
```
Nevertheless, it is done differently. The function values are represented as pairs packing the lambda expression together with the values of free variables. Such a pair is called [*function closure*](https://en.wikipedia.org/wiki/Closure_(computer_programming)) because the values of free variables are enclosed within the function value.

More precisely, whenever in a Racket program, an expression like this
```scheme
(lambda (y) (+ x y))
```
gets evaluated (i.e., a new function is defined), a function closure is created. At that moment, the values of the free variables are enclosed in the closure. It is important to remember that the values of the free variables are determined at the moment of a function definition. Such an approach is called *lexical scoping*. Lexical scoping is used by most modern programming languages, particularly Racket. However, there is also another approach called *dynamic scoping*. It determines the values of the free variables when the function's body gets evaluated.

Function closures are sometimes used (not only in Racket) to create a data structure that keeps some values. Suppose we want to build data structures representing points with two coordinates $x$, $y$. It is possible to do it via function closures. We simply enclose $x$ and $y$ inside the closure as follows:
```scheme
(define (point x y)
  (lambda (m) (m x y)))
```
The function `point` works as a constructor for our points. Once we call it with particular coordinates, it defines a function and creates a closure that keeps the function and coordinates. For example, we can make a point $p=(3,10)$ as follows:
```scheme
(define p (point 3 10))
```
Note that the function returned by `point` has a single argument `m`. If we call it, `m` is interpreted as a function applied to the coordinates. Thus we can easily access the coordinates by supplying suitable functions for `m`.

```scheme
(define (get-x p)
  (p (lambda (x y) x)))

(define (get-y p)
  (p (lambda (x y) y)))

> (get-x p)
3
> (get-y p)
10
```
Note that the functions in the definitions of `get-x` and `get-y` are just projections returning the first and the second argument, respectively:
```scheme
(lambda (x y) x))
(lambda (x y) y))
```

## Structures

Racket allows defining record types called [*structures*](https://docs.racket-lang.org/reference/structures.html). They are composed of a number of named fields. If we define such a type, Racket automatically defines a constructor and accessor functions for us. For example, a new structure `person` can be defined as follows:

```scheme
(struct person (first-name surname age))
```
To define an instance of the data type `person`, call the constructor `person`:
```scheme
(define p (person "John"
                  "Down"
                  33))
```
To access the fields, use the accessor functions:
```scheme
> (person-first-name p)
"John"
> (person-surname p)
"Down"
> (person-age p)
33
```
However, if we evaluate the instance `p`, we get no information on the field values:
```scheme
> p
#<person>
```
If you want to inspect the field values, change your definition as follows:
```scheme
(struct person (first-name surname age) #:transparent)
(define p (person "John"
                  "Down"
                  33))

> p
(person "John" "Down" 33)
```
Racket further automatically defines a predicate `person?` testing if a value is of the type `person`.
```scheme
> (person? p)
#t
> (person? 3)
#f
```
