---
outline: deep
title: "Exam Task: Non-deterministic Finite Automata"
subtitle: "From: Exam 2 - 2021"
---


# Non-deterministic Finite Automata

In the seminars, we have encountered *Deterministic Finite Automata* (DFAs).  In this task, we will
work with a generalized version of the DFA that is called *Non-deterministic Finite Automaton
(NFA)*.  NFA is the 5-tuple

* set of states $\mathcal{Q}$,
* a finite set of input symbols $\Sigma$ (called alphabet),
* a set of transitions $\Delta \subseteq \mathcal{Q} \times \Sigma \times \mathcal{Q}$,
* a start state $q_0$,
* a set of final states $\mathcal{F} \subseteq \mathcal{Q}$.

In other words, NFA is just a directed graph whose vertices are states and transitions are edges
labelled by symbols from $\Sigma$, i.e., if $(s,a,t)\in\Delta$ then there is an edge leading
from the state $s$ to the state $t$ labelled by $a\in\Sigma$. We say that NFA accepts a word
$w=a_1a_2\ldots a_n\in\Sigma^*$ (i.e., a finite sequence of symbols form $\Sigma$) if there
exists a path leading from the start state into a final one labelled consecutively by symbols
$a_1,a_2,\ldots,a_n$.

An example of an NFA is depicted in the figure below. This NFA accepts e.g. words $abb$ or
$aba$.  On the other hand, it accept neither $ba$ nor $abab$.

Example of NFA where $\mathcal Q=\{1,2,3,4\}$, $\Sigma=\{a,b\}$, $q_0=1$, $\mathcal F=\{2,3\}$
and $\Delta=\{(1,a,2),(2,b,2),(1,a,3),(3,b,4),(4,a,3),(2,a,4)\}$.

<img src="/img/finite-automata-dfa.svg" style="width: 40%; margin: auto;" class="inverting-image">


## Haskell
Your assignment is to implement a function that generates the language accepted by a given NFA.
However, since such a language is potentially infinite, the problem is simplified to listing all
words of given length that are accepted by the automaton.

For the purpose of this task, the NFA is defined as follows.  First, each possible transition is
represented as the triplet

```haskell
data Transition a b = Tr a b a
```

where the type `a` represents the states in $\mathcal{Q}$ and `b` are the symbols $\Sigma$;
hence, the transition from the first member of the triplet to the third member of the triplet is
possible if and only if the next input symbol is equal to the second member of the triplet.
The NFA itself is defined as
```haskell
data Automaton a b = NFA [Transition a b] a [a]
```
where the first member is the exhaustive list of transitions, the second member is the start state
$q_0$, and the third is the list of final states $\mathcal{F}$. The particular automaton
from the figure is defined as follows:
```haskell
nfa::Automaton Int Char
nfa = NFA [Tr 1 'a' 2,
           Tr 2 'b' 2,
           Tr 1 'a' 3,
           Tr 3 'b' 4,
           Tr 4 'a' 3,
           Tr 2 'a' 4]
           1
           [2,3]
```

### `accepts`-function
The first part of the task is to decide whether a particular word is accepted by a given automaton.
To that end, implement the function
```haskell
accepts :: (Eq a, Eq b) => Automaton a b -> [b] -> Bool
```
which takes and automaton and a list of symbols that represents the word, and returns **True**
iff the word is accepted. Notice, `a` and `b` are instances of `Eq`.  The function is used as
follows.
```haskell
> accepts nfa "aba"
True

> accepts nfa "abab"
False
```


_**Hint:**_ One possibility how to implement the function `accepts` is to maintain a list of
accessible states after consuming an initial segment of the input word.

Consider the NFA from the figure and the word `"aba"`. We start with the list containing the start
state `[1]`. Then we modify this list by the transitions corresponding to the letters as follows:
```haskell
[1] -a-> [2,3] -b-> [2,4] -a-> [3,4]
```
Finally, we check if the resulting list of states
contains a final state. `3` is a final state, the output is `True`.

Another example for `"abab"` is
```haskell
[1] -a-> [2,3] -b-> [2,4] -a-> [3,4] -b-> [4]
```
As `4` is not a final state, the ouput is `False`.

One more example for `"ba"`: 
```haskell
[1] -b-> [] -a-> []
```
So the output is `False`.


### `lwords`-function

Next, given the automaton, its alphabet, and a length, generate the list of all words of the given
length that are accepted by the automaton.  Implement the function
```haskell
lwords :: (Eq a, Eq b) => [b] -> Automaton a b -> Int -> [[b]]
```
where the first input is the alphabet as list of unique symbols, the second is the automaton, and
the third is the word length.  The function returns a list of the accepted words, i.e., a list of
lists of symbols.  In terms of the task evaluation, the ordering of the words is not relevant.  The
function operates as follows.

```haskell
> lwords "ab" nfa 0
[]
> lwords "ab" nfa 1
["a"]
> lwords "ab" nfa 3
["aaa","aba","abb"]
```

_**Hint:**_ first, generate all possible words of the given length.  Then, filter the words using
the function `accepts`.

::: details Solution
```haskell
import Control.Monad.State

data Transition a b = Tr a b a deriving Show
data Automaton a b = NFA [(Transition a b)] a [a] deriving Show

--nfa::Automaton Int Char
--nfa = NFA [(Tr 1 'a' 1), (Tr 1 'b' 1), (Tr 1 'c' 1), (Tr 1 'b' 2), (Tr 1 'c' 2)] 1 [2]


walk :: (Eq a, Eq b) => Automaton a b -> a -> [b] -> Bool
walk aut@(NFA trs start finals) state [] | elem state finals = True
                                         | otherwise = False 
walk aut@(NFA trs start finals) state (c:ws) = or [ (walk aut t ws) | tr@(Tr f c' t) <- trs, c' == c, f == state]

accepts :: (Eq a, Eq b) => Automaton a b -> [b] -> Bool
accepts aut@(NFA trs start finals) word = walk aut start word

combinations :: [a] -> Int -> State [[a]] [[a]]
combinations cs 0 = do ws <- get
                       return ws
combinations cs n = do ws <- get
                       let ws' = [ c:w | c <- cs , w <- ws]
                       put ws'
                       combinations cs (n-1)

lwords :: (Eq a, Eq b) => [b] -> Automaton a b -> Int -> [[b]]
lwords abc aut n = reverse $ [ w |  w <- p, accepts aut w] where
    p = evalState (combinations abc n) [[]]

```
:::

## Racket

Implement the functions `accepts` and `lwords`.  In this task, you may assume that the input word
will always be a string. Thus you can use functions `string->list` and `list->string` for converting
strings to lists of characters and vice versa.
Of course, we need to adapt the automaton representation for Racket.
First, each possible transition is represented as the triplet:
```scheme
(struct transition (from-state symbol to-state))
```
where `from-state` and `to-state` are states in $\mathcal{Q}$ and `symbol` is a symbol in $\Sigma$.
We can construct the NFA itself as
```scheme
(struct automaton (trans init-state final-states))
```
where the first member `trans` is the exhaustive list of transitions, the second member `init-state`
is the start state $q_0$, and the third member `final-state` is the list of final states
$\mathcal{F}$.
The particular automaton from  the figure is defined as follows.
```scheme
(define nfa
  (automaton
   (list (transition 1 #\a 2)
         (transition 2 #\b 2)
         (transition 1 #\a 3)
         (transition 3 #\b 4)
         (transition 4 #\a 3)
         (transition 2 #\a 4))
   1
   (list 2 3)))
```

Implement the function
```scheme
(accepts automaton word)
```
which takes an automaton and a string that represents the word to be parsed by the automaton, and
returns `#t` if the word is accepted, `#f` otherwise.
The function is used as follows:
```scheme
> (accepts nfa "aba")
#t

> (accepts nfa "abab")
#f
```
\noindent

Next, given the automaton, its alphabet, and a length, generate the list of all words of the given
length that are accepted by the automaton. Implement the function
```scheme
(lwords alphabet automaton n)
```
where the first input `alphabet` is the alphabet passed as a string, the second input
`automaton` is the automaton, and the third input `n` is the word length.
The function returns a list of the accepted words, i.e., a list of lists of characters.
In terms of the task evaluation, the ordering of the words is not relevant.
The function operates as follows:
```scheme
> (lwords "ab" nfa 0)
'()

> (lwords "ab" nfa 1)
'("a")

> (lwords "ab" nfa 3)
'("aaa" "aba" "abb")
```

For testing purposes your file should be named `task3.rkt` and start with the following lines:
```scheme
#lang racket

(provide accepts
         lwords)
```

::: details Solution
```scheme
#lang racket

(provide accepts
         lwords)

(struct transition (from-state symbol to-state))
(struct automaton (trans init-state final-states))

(define ((s-next fa a) s)
  (define tr (automaton-trans fa))
  (map transition-to-state
       (filter (lambda (t) (and (equal? s (transition-from-state t))
                                (equal? a (transition-symbol t))))
               tr)))

(define ((next fa) a ss)
  (define tr (automaton-trans fa))
  (apply append (map (s-next fa a) ss)))

(define (accepts fa w)
  (define states (foldl (next fa) (list (automaton-init-state fa)) (string->list w)))
  (if (eq? #f (ormap (lambda (s) (member s (automaton-final-states fa))) states))
      #f
      #t))

(define (words alp n)
  (cond
    ((= n 0) '())
    ((= n 1) (map list alp))
    (else (apply append
                 (map (lambda (w)
                        (map (lambda (a) (cons a w)) alp))
                      (words alp (- n 1)))))))

(define (lwords alp nfa n)
  (filter (lambda (w) (accepts nfa (list->string w)))
          (words (string->list alp) n)))


(define nfa
  (automaton
   (list (transition 1 #\a 2)
         (transition 2 #\b 2)
         (transition 1 #\a 3)
         (transition 3 #\b 4)
         (transition 4 #\a 3)
         (transition 2 #\a 4))
   1
   (list 2 3)))


(accepts nfa "aba")
(lwords "ab" nfa 3)
```
:::
