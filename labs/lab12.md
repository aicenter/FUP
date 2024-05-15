# Lab 12: Monads in action

This lab will illustrate a complete Haskell program searching for the shortest path in a maze. We will see `Maybe` and `IO` monads in action.
It will be split into two parts. The first part deals with the breadth-first search and the second with parsing the file containing a maze.
Short fragments of code are left for you to fill.

Before you start, make sure that you have the following imports in your source file:
```haskell
import Data.Char
import Control.Applicative
```
We are going to need these libraries for our parser.

## Data structures


As building blocks for a maze, we introduce the following data type:
```haskell
data Block = W | F | S deriving (Eq,Show)
```
The values `W`, `F`, `S` represent respectively a wall, a free space, and a star that we will use to depict solutions.
A data type capturing mazes can be defined as follows:
```haskell
data Maze = M [[Block]]

maze :: Maze   -- a testing maze
maze = M [[W,W,W,W,W],
          [W,F,W,F,W],
          [W,F,W,W,W],
          [W,F,F,F,W],
          [W,W,W,W,W]]
```

To display a maze we make `Maze` into an instance of `Show`.
```haskell
instance Show Maze where
    show (M []) = ""
    show (M (r:rs)) = map dispBlock r ++ "\n" ++ show (M rs)
       where dispBlock W = '#'
             dispBlock F = ' '
             dispBlock S = '*'
```

Finally, we represent a position in a maze by a tuple of integers. A path can be represented as a list of positions and a planning task
is a triple consisting of start and goal positions and a maze.
```haskell
type Pos = (Int, Int)
type Path = [Pos]
type Task = (Pos,Pos,Maze)
```

## Manipulations with maze


We will need to extract a block on a given position and conversely set a block on a given position. To see `Maybe` monad in action,
we implement these functions to be safe. E.g., if we provide a position outside the maze, it will return `Nothing`. We will start by implementing
such safe functions for lists.

Suppose we have an index `n`, an element `x :: a` and a list `xs :: [a]`. We want to implement a function that replaces the element of index
`n` in `xs` by `x` provided that `n` is within the range of indexes of `xs`. If `n` is outside this range, it returns `Nothing`.
```haskell
safePut :: Int -> a -> [a] -> Maybe [a]
```
::: details Solution: `safePut`
```haskell
safePut :: Int -> a -> [a] -> Maybe [a]
safePut n x xs | 0 <= n && n < length xs = Just $ take n xs ++ [x] ++ drop (n+1) xs
               | otherwise = Nothing
```
:::

Similarly, try to implement the function `safeGet` that extract the element of index `n` provided it exists:
```haskell
safeGet :: Int -> [a] -> Maybe a
```
::: details Solution: `safeGet`
```haskell
safeGet :: Int -> [a] -> Maybe a
safeGet n xs | n `elem` [0..length xs-1] = Just $ xs !! n
             | otherwise = Nothing
```
:::

Now we can use the above functions to implement functions extracting and setting a block in a maze.
To extract a block, we first safely extract a row of a maze. If it is successful, we can extract the block from the row.
Using the fact that `Maybe` is a monad, we don't have to test every time if the computation was successful.
::: details Solution: `getBlock`
```haskell
getBlock :: Pos -> Maze -> Maybe Block
getBlock (x,y) (M xss) = do row <- safeGet y xss
                            block <- safeGet x row
                            return block
```
:::
Examples:
```haskell
> getBlock (1,0) maze
Just W

> getBlock (10,10) maze
Nothing
```

Using `safeGet` and `safePut`, try to implement a function that takes
a block `b`, a maze `m`, a position `(x,y)` and returns a new maze created by replacing
the block on `(x,y)` by  `b`.
```haskell
setBlock :: Block -> Pos -> Maze -> Maybe Maze
```

::: details Solution: `setBlock`
```haskell
setBlock :: Block -> Pos -> Maze -> Maybe Maze
setBlock b (x,y) (M xss) = do row <- safeGet y xss
                              row' <- safePut x b row
                              xss' <- safePut y row' xss
                              return (M xss')
```
:::

Example:
```haskell
> setBlock S (1,2) maze
Just #####
# # #
#*###
#   #
#####
```
Finally, if we have a path (i.e., a list of positions), we can set recursively all its positions in a maze. Again using the fact that `Maybe` is a monad.
::: details Solution: `setPath`
```haskell
setPath :: Maze -> Path -> Maybe Maze
setPath m [] = Just m
setPath m (p:ps) = do m' <- setBlock S p m
                      m'' <- setPath m' ps
                      return m''
```
:::

You might note that this is, in fact, a kind of monadic `foldr`. There is, of course, a generic monadic `foldr` called `foldrM`.
If you import `Data.Foldable`, then you can rewrite the above function as follows:
```haskell
setPath = foldrM (setBlock S)
```

As `setPath` returns a value of type `Maybe Maze`, we can extract it from the `Maybe` context by pattern matching.
::: details Solution: `drawSol`
```haskell
drawSol :: Maze -> Path -> Maze
drawSol m ps = case setPath m ps of
                 Nothing -> m
                 Just m' -> m'
```
:::

## Breadth-first search (BFS)

To find a path leading from a start position into the goal position, we need a function taking a position and returning all possible successive positions.
Assume that there are at most eight possible moves. All possibilities are generated by the function `neighbs`. We have to filter only those leading to a free block out of these possibilities. Moreover, it is necessary to check that the input position is permissible as well.

```haskell
neighbs :: Pos -> [Pos]
neighbs (x,y) = [ (x-1,y-1), (x,y-1), (x+1,y-1)
                , (x-1,y),            (x+1,y)
                , (x-1,y+1), (x,y+1), (x+1,y+1) ]

nextPos :: Pos -> Maze -> [Pos]
nextPos p m = case getBlock p m of
                -- if input position is admissible
                -- take all possibilities and filter admissible positions
                Just F -> [ p' | p' <- neighbs p, getBlock p' m == Just F]  
                _ -> []

> nextPos (1,1) maze
[(1,2)]
```

Using `nextPos`, implement the following function taking a path, a maze and returning all its possible extensions. For efficiency reasons
we will represent paths in BFS in the reversed order. Thus extend a given path using the operator `(:)`.
::: details Solution: `extend`
```haskell
extend :: Path -> Maze -> [Path]
extend [] _ = []
extend path@(p:_) m = map (:path) $ nextPos p m

> extend [(1,2),(1,1)] maze
[[(1,1),(1,2),(1,1)],[(1,3),(1,2),(1,1)],[(2,3),(1,2),(1,1)]]
```
:::

Now we can quickly implement BFS. Recall that in BFS, we use a queue storing partial solutions. We will implement this queue naively as a list.
In addition, we have to keep information about already visited positions. We define the function `solve` as just a wrapper for
the `bfs` function implementing BFS. The function `bfs` takes several arguments. The first is a list of already visited positions.
The second is the queue of partial solutions. The third is the goal position and the last one is the maze.
```haskell
solve :: Task -> Maybe Path
solve (p,q,m) = bfs [] [[p]] q m

bfs :: [Pos] -> [Path] -> Pos -> Maze -> Maybe Path
bfs _ [] _ _ = Nothing
-- consider the first path in the queue and its head p
bfs visited (path@(p:_):paths) q m
	-- is path a solution? If yes, return the reversed solution
    | p == q = Just $ reverse path
	-- does path end in an already visited position? If yes, disregard it 
    | p `elem` visited = bfs visited paths q m
	-- add p to visited positions and extend path by all possible positions 
    | otherwise = bfs (p:visited) (paths ++ extend path m) q m

> solve ((1,2),(3,3),maze)
Just [(1,2),(2,3),(3,3)]

> solve ((3,1),(3,3),maze)
Nothing
```

## Type constructor Parser

As a next task, we must create a user interface for the BFS solver. We have to allow the user to specify a maze together with a start and goal
positions. The user provides a string containing all the necessary data via the standard input. It might look as follows:

```
start = (1,1)
goal = (28,4)
#########################################
#                #             #        #
#                #             #        #
###########   #######   ###########     #
#                   #   #         #     #
#          #####################        #
####       #            #      #        #
#       ##########    ################  #
#                                       #
#       #                   #           #
#########################################

```

Our program is supposed to parse this input and display its solution provided it exists:
```
#########################################
#*********       #             #        #
#         *      #             #        #
###########*  #######   ###########     #
#         *         #   #   ****  #     #
#        * #####################*       #
####    *  #            #      # *****  #
#      *##########    ################* #
#       ******************************  #
#       #                   #           #
#########################################
```

We will use the type constructor `Parser` that I explained in the lecture. Below you can find its definition and definitions of all its instances
for `Functor`, `Applicative`, `Monad` and `Alternative`. So you can directly copy them into your source file.

Let me recall the info on `Parser` shortly. A parser over type `a` is a function taking an input string, consuming a part of it, and returning
the parsed value of type `a` and the remaining unused input string. The parsing can fail. That's why it returns a value of type
`Maybe (a, String)`. For instance, if you want to parse an integer and the input string starts with a letter, the parsing fails.

The accessor function `parse` just helps us to remove the data constructor `P`. So if you want to apply a parser `p` to an input `inp`, call
`parse p inp`.

As we want to make `Parser` an instance of `Monad` so that we can sequence parsers, we have to define also instances for super-classes
`Applicative` and `Functor`. Functor instances over data type `a` implements a function `fmap` allowing to lift
a map `f :: a -> b` to a map of type `Parser a -> Parser b`.  The function `fmap` always
keeps the functor structure untouched only changes values of type parameter `a`. So for `Parser a` it just keeps the parsing
function the same expect of modifying the output value `v :: a` by `f v`.

Functors allow lifting unary maps to the functorial context. E.g. we can lift `(+1)` to `Parser Int` but we cannot lift binary `(+)`.
If we lift `(+) :: Int -> Int -> Int` to `Parser Int` by `fmap`, we obtain a function
`Parser Int -> Parser (Int -> Int)`. However, to lift `(+)`, we need type
`Parser Int -> Parser Int -> Parser Int`. Applicative functors implement `<*>` that can transform
`Parser (Int -> Int)` to `Parser Int -> Parser Int`.
The function `pure` just wraps a value into the `Parser` context. It is, in fact, a synonym for the monadic `return`.

The monad instance for `Parser` has to define the bind operator `>>=`. Its implementation first parses a value `v :: a`.
If the parsing fails, then the whole parsing fails. Otherwise, we apply `f v` obtaining the next parser applied to the unused
input `out`.

Finally, we define the instance of `Alternative`. It consists of `empty` and `<|>`. The first is the always failing parser.
The second operator allows trying two parsers for the same input, and the first successful returns its result.

```haskell
newtype Parser a = P { parse :: String -> Maybe (a, String) }

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = P (\inp -> case parse p inp of
                            Nothing -> Nothing
                            Just (v,out) -> Just (f v, out))

instance Applicative Parser where
    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = P (\inp -> case parse pg inp of
                             Nothing -> Nothing
                             Just (g,out) -> parse (fmap g px) out)
    pure v = P (\inp -> Just (v,inp))

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\inp -> case parse p inp of
                           Nothing -> Nothing
                           Just (v,out) -> parse (f v) out)

instance Alternative Parser where
    -- empty :: Parser a
    empty = P (\_ -> Nothing)
    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (\inp -> case parse p inp of
                           Nothing -> parse q inp
                           Just (v,out) -> Just (v,out))
```

## Parsing


Now we are ready to implement the parser for our BFS application. We start with simple parsers, out of which we compose the final one.
The structure of the input `<file>` is specified by the following grammar. The first line contains a definition of the start position
and the second one defines the goal position. The start definition starts with `"start"` followed possibly by spaces,
then `"="`, again possibly followed by spaces, and then a position followed by the new-line character `"\n"`.
The goal definition is analogous. The position is just a tuple of numbers in parentheses separated by a comma and possibly by spaces.
The maze `<map>` consists of rows followed by `"\n"`. Each row is a (possibly empty) sequence of the wall `"#"` and free `" "` blocks.
```haskell
<file>  -> <start> <goal> <map>

<start> -> "start" <sep>* "=" <sep>* <pos> "\n"
<goal>  -> "goal" <sep>* "=" <sep>* <pos> "\n"

<pos>   -> "(" <sep>* <digit>+ <sep>* "," <sep>* <digit>+ <sep>* ")"

<map>   -> <row>*
<row>   -> (<wall> | <sep>)* "\n"

<wall>  -> "#"
<digit> -> 0 | 1 | ... | 9
<sep>   -> " "
```

First, we create a basic parser `item` consuming a single character and failing if there is none.
Based on that we can define a parser `sat` parsing a character satisfying a given predicate. If you need extra exercises, reimplement
the following parsers using the operators `>>=` and `>>`.
```haskell
item :: Parser Char
item = P (\inp -> case inp of
                    "" -> Nothing
                    (x:xs) -> Just (x,xs))

sat :: (Char -> Bool) -> Parser Char
sat pr = do x <- item
            if pr x then return x
            else empty
```

To parse numbers, we need a parser for a single digit. The predicate `isDigit` from `Data.Char` recognizes digits. Further, we need parsers
for a specific character and even a specific string like `"start"`.
```haskell
digit :: Parser Char
digit = sat isDigit

char :: Char -> Parser Char
char c = sat (== c)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)
```
::: details Alternative definition using Applicative
```haskell
string :: String -> Parser String
string [] = return []
string (x:xs) = char x *> string xs *> pure (x:xs)
```
:::

```haskell
> parse digit "34abc"
Just ('3',"4abc")

> parse (string "start") "start = (1,2)"
Just ("start"," = (1,2)")
```
The above function `string` returns a parser whose output value is known in advance (it is `x:xs`). Its only reason is to
check if the parsing does not fail.

As we define `Parser` to be an instance of `Alternative`, we have for free two parser combinators `many` and `some`. Both of them
repeatedly apply a given parser until it fails. The parser `many p` always succeeds even if `p` fails for the first time.
On the other hand, `some p` succeeds only if the first application of `p` succeeds.

```haskell
> parse (many (char 'a')) "aaabc"
Just ("aaa","bc")

> parse (many (char 'a')) "bc"
Just ("","bc")

> parse (some (char 'a')) "aaabc"
Just ("aaa","bc")

> parse (some (char 'a')) "bc"
Nothing
```
Thus `many` can handle (possibly empty) sequences (e.g., an arbitrary series of spaces) and `some` non-empty sequences
(e.g., non-empty sequences of digits representing an integer). To disregard sequences of spaces, we define the following parser
together with the function `token` that transforms any parser to omit spaces at the beginning and the end.

```haskell
space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             x <- p
             space
             return x
```

::: details Alternative definition by Applicative combinators
```haskell
space :: Parser ()
space = do many (sat isSpace) *> pure ()

token :: Parser a -> Parser a
token p = space *> p <* space
```
:::

```haskell
> parse (token (char '=')) " = (1,2)"
Just ('=',"(1,2)")
```

Now we will follow the grammar. We start with a parser for a position.
```haskell
pos :: Parser Pos
pos = do char '('                -- it has to start with '('
         space                   -- possibly followed by spaces
         x <- some digit         -- then parses a nonempty sequence of digits
         token (char ',')        -- then comma possible surrounded by spaces
         y <- some digit         -- then a second non-empty sequence of digits
         space                   -- possibly spaces
         char ')'                -- then the closing ')'
         return (read x, read y) -- the position is returned, sequences of digits are converted by read

> parse pos "(  343, 55 )"
Just ((343,55),"")

> parse pos "(1 2)"
Nothing
```

Using the above parsers, try to define the following function by taking a string and returning the parser of a definition.
```haskell
def :: String -> Parser Pos
```
::: details Solution: `def`
```haskell
def :: String -> Parser Pos
def str = do string str
             token (char '=')
             p <- pos
             char '\n'
             return p
```
:::

Example:
```haskell
> parse (def "start") "start = (3,2)\n"
Just ((3,2),"")
```

Next, we focus on maze parsing. We define simple parsers for blocks. Out of them, we can create a parser for rows. Using the operator
`<|>`, we can define the parser `wall <|> free` which parses either the wall or free block.
```haskell
wall :: Parser Block
wall = do char '#'
          return W

free :: Parser Block
free = do char ' '
          return F

row :: Parser [Block]
row = do bs <- many (wall <|> free)
         char '\n'
         return bs
```

::: details Applicative approach

```haskell
wall :: Parser Block
wall = char '#' *> pure W

row :: Parser [Block]
row = many (wall <|> free) <* char '\n'
```
:::

```hs
> parse row "  ### # \n#      #\n"
Just ([F,F,W,W,W,F,W,F],"#      #\n")
```

A maze is just a (possibly empty) sequence of rows. The input starts with the start and goal definitions, followed by a maze.
```haskell
mapP :: Parser Maze
mapP = do rs <- many row
          return (M rs)

file :: Parser Task
file = do p <- def "start"
          q <- def "goal"
          m <- mapP
          return (p,q,m)
```

::: details Functor and Applicative approach
```haskell
mapP :: Parser Maze
mapP = M <$> many row

file :: Parser Task
file = (,,) <$> def "start"
            <*> def "goal"
            <*> mapP
```
:::

## IO actions


Finally, we put all the pieces together. We start with a function taking a task and returning an IO action that
either displays the found solution or informs that there is no solution. Note that the function `print` is just the composition of `show` followed by `putStrLn`.
```haskell
solveTask :: Task -> IO ()
solveTask t@(p,q,m) = case solve t of
    Nothing -> putStrLn "No solution exists."
    Just ps -> print $ drawSol m ps
```

We need to create a `main` function returning the main IO action to be executed. It reads completely the input by `getContents`.
Then it parses the input. If the parser fails or does not consume the whole input, it prints an error message.
Otherwise, we have a task `t` and `solveTask t` can be executed.

```haskell
main :: IO ()
main = do str <- getContents
          case parse file str of
              Nothing -> putStrLn "Incorrect task!"
              Just (t, "") -> solveTask t
              Just (_, out) -> putStrLn $ "Unused input: " ++ out
```

Now, if we have a text file `maze.txt` with the input, we can run our source code by
```bash
$ runghc lab12.hs < maze.txt
```

Alternatively, we can compile it and then run the compiled executable file.
```bash
$ ghc lab12.hs
$ ./lab12 < maze.txt
```
