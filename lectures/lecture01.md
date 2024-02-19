---
outline: deep
---


# Introduction

Functional programming is a declarative style of programming where we structure programs as
compositions of functions. 
There is no exact definition of functional programming. It is usually described by a collection of
concepts and paradigms used in that programming style. To name a few of them, let me mention, for
example, pure functions, immutable data structures, recursion, higher-order functions, pattern
matching, lazy evaluation, etc. We will touch on all the above concepts later on. For now, I will
discuss the fundamental feature of functional programming: building programs by function
composition. This distinguishes functional programming substantially from the usual imperative
programming style.

Recall that imperative programs are essentially sequences of commands/statements executed in the
given order. The role of the commands is to update the state of the computation. The state holds
intermediate results stored in variables. On the other hand, functional programs are written as a
composition of functions, just like formal expressions in mathematics. Consider, for instance, a
program that returns the smallest element for a given list of integers. To construct such a program,
we would start with primitive functions provided by the programming language and create the program
by composing them. The resulting program would be represented by a function transforming any list of
integers into its smallest element. In Haskell, this could be done as follows:

```haskell
getSmallest :: [Int] -> Int
getSmallest xs = head (sort xs)
```

The first line declares the type of the function `getSmallest`, taking a list of integers and
returning an integer. The second line defines `getSmallest` as a composition of functions, `sort`
and `head`. Thus the input list is sorted, and the first element is returned. This definition is
much like in mathematics, where we could define $\mathrm{getSmallest}(xs) =
\mathrm{head}(\mathrm{sort}(xs))$. We can further simplify this definition to $\mathrm{getSmallest}
= \mathrm{head}\circ\mathrm{sort}$ where $\circ$ denotes the [function
composition](https://en.wikipedia.org/wiki/Function_composition). Analogously, we can shorten the
definition in Haskell using the dot operator.

```haskell
getSmallest :: [Int] -> Int
getSmallest = head . sort
```

The above example is simple because we compose just two functions. In reality, we need to compose
many more functions. Moreover, these compositions need not be fixed but can depend on the input. So
let us investigate a more complicated example. Suppose we are given a string, and our task is to
create a histogram of characters occurring in the string. I will compare the imperative approach and
the functional one. An imperative solution in Python could look like this:

```python
def make_histogram(str):
    hist = {}
    for ch in str:
        if ch in hist.keys():
            hist[ch] += 1
        else:
            hist[ch] = 1
    return hist
```

The dictionary `hist` represents the histogram. For example, calling 
```python
make_histogram("hello")
```
returns
```python
{'h': 1, 'e': 1, 'l': 2, 'o': 1}
```
Note how the state is updated. We start with the empty dictionary representing the initial state.
Next, we iterate through the characters in the string and update the dictionary. If the current
character `ch` is in the dictionary, we increase the number of its occurrences by 1. If not, we
insert `ch` into the dictionary with the value 1.

Now we solve the same task using the functional style in Haskell. Before we dive into the actual
Haskell code, we formulate the task mathematically. As our only tool is function composition, we
cannot use a state that we would update. Instead, we will pass the intermediate dictionary to a
function returning its updated version. Let $Char$ and $Dict$ denote the set of characters and
dictionaries, respectively. The input for our task is a string which can be viewed as a list of
characters. The empty dictionary is denoted $\{\}$. We look for a function $\mathrm{mkHist}$ that
transforms a string into the desired histogram. Suppose we have a function $up\colon Dict\times Char
\to Dict$ updating a given dictionary by a given character. When evaluated on the string
$\mathtt{"hello"}$, the function $\mathrm{mkHist}$ should do the following:

$$
\mathrm{mkHist}(\mathtt{"hello"}) = 
\mathrm{up}(\mathrm{up}(\mathrm{up}(\mathrm{up}(\mathrm{up}(\{\}, 
  \mathtt{'h'}), \mathtt{'e'}), \mathtt{'l'}), \mathtt{'l'}), \mathtt{'o'})
$$

Note that we compose the function $\mathrm{up}$ as many times as there are characters in the input
string. However, it is necessary to know the input if we want to write down the composition above.
Thus we need a construction allowing us to iterate through a list and create a sequence of function
compositions, provided we have a function updating the intermediate result. Functional programming
languages offer such a construction. It is a function usually called $\mathrm{foldl}$ or
$\mathrm{foldr}$, depending on which order we want to iterate through the list, either from left to
right or right to left. Using $\mathrm{foldl}$, we could define  $\mathrm{mkHist}$ as follows:

$$\mathrm{mkHist}(str) = \mathrm{foldl}(\mathrm{up}, \{\}, str)$$

The function $\mathrm{foldl}$ takes three arguments. The first is the updating function, the second
is the initial value, and the last is the list to process. Now you can think: *Wait a minute! If we
need a special function for this type of iteration, there might be a lot of functions allowing us to
iterate through a list and do something.* Actually, it is not so bad. There are only a few because
we can parametrize them by functions, so they are pretty general. Moreover, we can define new ones
through recursion if you need something nonstandard. For instance, $\mathrm{foldl}$ can be
recursively defined as follows:

$$
\mathrm{foldl}(f, st, lst) = \begin{cases}
  st & \text{if } lst=[\,]\\
  \mathrm{foldl}(f, f(st, x_1), [x_2,\ldots]) & \text{if } lst=[x_1,x_2,\ldots]
\end{cases}
$$

Note that we compose the function $\mathrm{up}$ as many times as there are characters in the input
string. However, it is necessary to know the input if we want to write down the composition above.
Thus we need a construction allowing us to iterate through a list and create a sequence of function
compositions, provided we have a function updating the intermediate result. Functional programming
languages offer such a construction. It is a function usually called $\mathrm{foldl}$ or
$\mathrm{foldr}$, depending on which order we want to iterate through the list, either from left to
right or right to left. Using $\mathrm{foldl}$, we could define  $\mathrm{mkHist}$ as follows:

$$\mathrm{mkHist}(str) = \mathrm{foldl}(\mathrm{up}, \{\}, str)$$

where $f$ is the function taking an intermediate result $st$ and an element $x_i$ from the list
$lst$ and returning the updated intermediate result. The definition is recursive because the
function $\mathrm{foldl}$ calls itself. If $lst$ is empty, we just return the initial value. If not,
we strip the first element from $lst$, call the function $f(st, x_1)$ creating an updated
intermediated result, and finally, we call $\mathrm{foldl}$ again on the remaining list. If we
expand the definition on a concrete list, say $[1,2,3]$, we get the following:

$$
\begin{align*}
\mathrm{foldl}(f, st, [1,2,3]) & = \mathrm{foldl}(f, f(st, 1), [2,3])\\
                               & = \mathrm{foldl}(f, f(f(st, 1), 2), [3])\\
                               & = \mathrm{foldl}(f, f(f(f(st, 1), 2), 3), [])\\
                               & = f(f(f(st, 1), 2), 3)
\end{align*}
$$


Now the solution in Haskell should be transparent.
```haskell
mkHist :: String -> Map Char Int
mkHist = foldl update empty
    where update m ch = insertWith (+) ch 1 m
```
We use a data structure `Map Char Int` similar to Python's dictionary to store the histogram, where
keys are characters and values are integers. We apply the provided function
[`insertWith`](https://hackage.haskell.org/package/containers-0.6.6/docs/Data-Map-Strict.html#v:insertWith)
to update the intermediate result. It takes four arguments. The first is a binary function updating
existing key-value pairs, the second is the key to be inserted, the third is the updating value, and
the last is the dictionary. For example, if we call 
```{.haskell}
insertWith (+) ch 1 m
```
there are two possible outcomes: either the pair `(ch,1)` is inserted into `m` if `ch` is not
present in `m` or `(ch,v+1)` is inserted into `m` if `(ch,v)` is in `m`.

I hope the above example gave you a flavor of functional programming. Next, I will discuss what
consequences it has using the functional programming style.


## Pure functions

In the example above, we have seen that functional programming structures programs as function
compositions in the mathematical sense. Thus we cannot use a state (i.e., a data structure keeping
intermediate results updated as the computation proceeds); instead, we pass the intermediate results
as arguments to other functions. It is a good idea to distinguish the notion of a function here.
Functions are standard in most programming languages. However, a usual function in an imperative
programming language need not be mathematical. First, it can do other stuff, not only computes its
returning value. Second, its returning value might depend on other data, not only its arguments.
Consider, for instance, the following functions:

```python
counter = 0

def do_other(x):
  global counter
  counter += 1
  return x**2

def depends_on_other(x):
  return counter + x**2
```

The function `do_other` computes $x^2$ but also increments a global variable `counter`. The
returning value of  `depends_on_other` depends on the global variable `counter`. So, if we evaluate
these functions in the following order, we get 

```python
depends_on_other(5) => 25
do_other(3) => 9
depends_on_other(5) => 26
```
Consequently, `depends_on_other` returns different results even if its input is the same.
Mathematical functions cannot do things like this. Therefore people introduced the notion of a pure
function to distinguish mathematical functions in programming terminology.

::: tip Definition
A *pure function* is a function that,  given the same input, always returns the same
output and does no side effects.
:::

Hence a pure function behaves like a mathematical function; its output depends only on its arguments
and nothing more. It cannot depend on a global variable or a random value, read a file on a disk,
connect to a database, or read a pressed key on the keyboard. Moreover, it cannot cause side
effects. What are the side effects? Typical examples of side effects are:

- Mutation of a global variable or any of the input arguments.
- Writing data to a file or database.
- Printing something to the screen. 

As functional programs rely on pure functions, they might seem useless because they cannot do
anything real, like printing to the screen. Fortunately, this is not true, as we will see later on.
Let me first explain why it is a good idea to use pure functions and eliminate state and side
effects.

## Troublesome state

The critical issue caused by the mutable shared state is that it creates implicit connections
between all parts of the program. Changing something in the code might influence seemingly unrelated
pieces of our code because any function, in principle, can access the state and read it or mutate
it. This issue becomes even more severe if we work on a large software project developed by many
programmers. Consider, for example, the following piece of code:

```python:line-numbers
status = 0

def do_something():
  if status == 1:
    raise Exception("Invalid status.")
  else:
    # do some stuff
    print("Do some stuff")

def set_status(data):
  global status
  if valid(data):
    status = 0
  else:
    status = 1
```

We have a global binary variable `status`, which is 0 if the status is OK and 1 if not. There is a
function `do_something` relying on `status` raising an exception if the status is incorrect.
Finally, a function called `set_status` takes data and sets the status based on data validity.
Suppose that the coder responsible for `set_status` decides to enrich the information stored in the
variable `status` by distinguishing various types of invalid data. So he-or-she introduces, say, a
new status value 2. At that moment, the function `do_something` could go wrong because its author
did not expect 2 as a possible value for the status.

Another problem caused by the shared mutable state is nontrivial unit testing. It is easy to test
pure functions without investigating their inner code. Simply choose some testing inputs and check
if the outputs are correct. This approach does not work for impure functions as they depend on
external data or make a side effect. So to test such a function, all the external data must be set
up and the resulting side effects checked. Thus the tester needs to investigate the inner code and
identify them. This task might be challenging if the function calls further nested functions.

Software engineers are aware of the issues caused by the mutable state. Therefore they invented
various approaches how to mitigate them. For instance, the state is privately distributed inside
object instances in object-oriented programming. Only the object's methods can mutate the state, so
the state mutation is protected. Moreover, the methods can check if the updated state values are
consistent. However, it only partially resolves the issues. Functional programming goes even
further. It restricts the mutable state to the bare minimum.

## State separation

Functional programming promotes pure functions as much as possible. Ideally, it is reasonable to
compose our programs using only pure functions because it has the following advantages:

- Unit tests are painless, as explained above.
- Reasoning about programs is more straightforward because there are no hidden links between
  unrelated pieces of code.
- It is safe to reuse any subprogram and call it as many times as you wish because it cannot alter
  any hidden data structure.
- It is less complicated to make your code run concurrently or in parallel. This is because
  functional programs do not force a strict sequential execution in contrast with imperative
  programs. 

On the other hand, it is apparent that side effects are inevitable. We want our programs to be able
to do side effects like writing to a database. Different approaches exist to deal with side effects
in functional programming languages. The general idea is to use pure functions as much as possible
and handle the side effects and stateful computations in a tiny transparent part of your code.

![General structure of functional programs](../img/state_guard.svg){ style="width: 60%; margin: auto;" }

Let me mention a few concrete approaches. Some functional programming languages allow you to have a
mutable state and do side effects. In such cases, it is up to the developer to make the stateful
part of the program sufficiently small and transparent. Among such languages are Lisp-like languages
like Scheme or Racket. 

Another approach Haskell uses is a strict separation of pure code from the stateful one by means of
the type systems. So you are allowed to bring the values computed in the pure part to the stateful
one but not the other way around. Nevertheless, it is still up to the coder to keep the stateful
part as small as possible. We will discuss that approach in more detail later on.

The last approach I want to mention is the approach of the programming language
[Elm](https://elm-lang.org/). Elm is a domain-specific programming language for declaratively
creating web browser-based graphical user interfaces. It is a purely functional language, so all the
functions must be pure. Mutations of the state and side effects are left to the Elm runtime. A
developer just specifies (via pure functions) how to update the state based on the messages
generated by the browser. More precisely, an Elm application consists of three components model,
update, and view. 

1. The **model** specifies a data structure used by the runtime to keep the state of the application
   and a function setting the initial state.
2. The **update** part is a function whose input is the current state and a message generated by the
   browser that returns a new state.
3. The **view** component is responsible for rendering an HTML document for the current state.

The following picture shows the basic architecture. The Elm runtime applies the view component to
render the HTML document. If the browser generates a message, the runtime uses the update function
to mutate the current state.

![Elm architecture](../img/buttons.svg){ style="width: 70%; margin: auto;" }

Below you can find an example of a simple Elm application allowing the user to enter a string and displaying a histogram of digits occurring in the entered string. Note that the state is captured by the type `Model` consisting of the entered string `content` and a dictionary representing the histogram. The function `init` sets up the empty initial state. The function `update` creates a new state based on the message (in this case, a new string entered by the user). Thus `content` is set to `newContent`, and the function `transform` computes a new histogram. Note that `transform` is just a composition of functions (`>>` denotes the function composition): 

1. `toList` breaks the string into a list of characters.
2. `List.filter isDigit` filters only digits.
3. The histogram is computed by `foldl` analogously to the Haskell example above.

Finally, the function `view` renders the application based on the state. It creates a `div` containing an input box and an SVG image. This image consists of bars whose height depicts the number of occurrences and digits in the input string. There is an auxiliary function, `disp`, that iterates through the dictionary and renders either bars or digits depending on the first argument (i.e., either the function `bar` or `chr`).

```elm:line-numbers
module Webapp exposing (main)

import Browser
import Html exposing (Html, Attribute, div, input)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onInput)
import String exposing (toList, fromChar, fromInt)
import Char exposing (isDigit)
import List exposing (indexedMap, foldl)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Dict exposing (Dict, empty)

-- MAIN

main = Browser.sandbox { init = init, update = update, view = view }

-- MODEL

type alias Model =
  { content : String
  , hist : Dict Char Int
  }

init : Model
init =
  { content = "" 
  , hist = empty 
  }

-- UPDATE

type Msg = Change String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
      { model | content = newContent 
      , hist = transform newContent 
      }

transform : String -> Dict Char Int
transform = toList >> List.filter isDigit >> foldl (\k -> Dict.update k add1) empty 

add1 : Maybe Int -> Maybe Int
add1 x = case x of
  Nothing -> Just 1
  Just n -> Just (n+1)
  
-- VIEW

bar : Int -> (Char, Int) -> Svg Msg
bar m (_, n) = rect  
          [ x (fromInt (m*20))
          , y (fromInt (300-n*20))
          , width "20"
          , height (fromInt (n*20))
          , fill "lightgreen"
          , stroke "black"
          , strokeWidth "2"
          ]
          []

chr : Int -> (Char, Int) -> Svg Msg
chr m (c, _) = text_
          [ x (fromInt (m*20 + 10))
          , y "320"
          , fill "black"
          , textAnchor "middle"
          ]
          [ text (fromChar c) ]

disp : (Int -> (Char, Int) -> Svg Msg) -> Model -> List (Svg Msg)
disp f = .hist >> Dict.toList >> indexedMap f 

view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "Enter text", value model.content, onInput Change ] []
    , div [] 
        [ svg
          [ viewBox "0 0 400 400"
          , width "400"
          , height "400"
          ]
          (disp bar model ++ disp chr model)
       ] 
    ]
```

You can test the app by yourself. If you want to play with code, go to this
[site](https://elm-lang.org/examples/hello), erase the code there, and enter the code above.
