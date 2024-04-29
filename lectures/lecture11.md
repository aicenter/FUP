---
outline: deep
---

# Monadic Parsing

In this lecture we will use our newly gained knowledge about functors and monads to build parsers
(this lecture is the foundation for your last [homework](/homework/hw04)).

## Applicative

Before we get into the actual parser implementation we need to introduce one more useful and very
common typeclass: `Applicative`s. Imagine we want to add two `Maybe` values. Naively trying to add
them of course does not work:
```haskell
𝝺> (Just 2) + (Just 3)
<interactive>:1:1: error: [GHC-39999]
    • No instance for ‘Num (Maybe Integer)’ arising ...
```
Using `fmap` will also not get us the desired result:
```haskell
fmap (+) (Just 2) :: Num a => Maybe (a -> a)
```
Because we do not have a way of working with the resulting `Maybe (a -> a)`.
`Applicative` functors allow us to do exactly this!
```haskell
class Functor f => Applicative f where
  (<*>) :: f (a -> b) -> f a -> f b
  pure a -> f a
```
The operator `<*>` accepts a boxed function and applies it to a boxed value, so we can use the
output of our `fmap` expression from before:
```haskell
𝝺> fmap (+) (Just 2) <*> Just 3
Just 5

-- we can write the same with <$>, the fmap operator:
𝝺> (+) <$> (Just 2) <*> Just 3
Just 5
```

::: tip Exercise
Can you see why `<*>` now allows use to lift _**n-ary**_ functions? Hint: currying!
:::

The implementation of `Applicative` for `Maybe` is pretty simple (as usual for `Maybe`).
```haskell
instance Applicative Maybe where
  pure :: a -> Maybe a
  pure = Just

  (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  Nothing <*> _ = Nothing
  _ <*> Nothing = Nothing
  Just f <*> Just a = Just (f a)
```

For lists we have two options to implement the interface. We can either pair up a list of functions
with a list of values, or we can apply each function to each value (cartesian product).
Haskell chooses the latter:
```haskell
instance Applicative [] where
  pure :: a -> [a]
  pure x = [x]

  (<*>) :: [a -> b] -> [a] -> [b]
  fs <*> xs = [f x | f <- fs, x <- xs]
```

such that the `[]` applicative lets us compute e.g. products of lists of functions
```haskell
𝝺> (,) <$> [1,2,3] <*> ['a','b','c']
[ (1,'a'), (1,'b'), (1,'c')
, (2,'a'), (2,'b'), (2,'c')
, (3,'a'), (3,'b'), (3,'c') ]
```

### Example: Validation

Let's say we want to create validated data structures, such as a `Person`:
```haskell
newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person Name Address deriving (Eq, Show)
```
where `Name` and `Address` are type aliases for `String`. 

::: details What is `newtype`?
The `newtype` keyword can be used to define data types with exactly one constructor and exactly one
field, so its similar to a type alias, just that you can define instances for it (and it has some
slightly different lazy/strict evaluation properties than `data`). Small summary is e.g.
[here](https://stackoverflow.com/q/2649305).
:::

For example, the constructor `Name` accepts a `String` and produces a `Name`, but for our
validation we want `Maybe Name`s. This is a familiar case for which we can use `fmap`:
```haskell
validateLength :: Int -> String -> Maybe String
validateLength maxLen s = if (length s) > maxLen
                          then Nothing
                          else Just s

mkName :: String -> Maybe Name
mkName s = Name <$> validateLength 12 s

mkAddress :: String -> Maybe Address
mkAddress a = Address <$> validateLength 25 a
```

Once we try to construct validated `Person`s we have to deal with the problem that `Person` accepts
`Name` and `Address`, but not their boxed maybe values. This is exactly the use case for the
applicative functor:
```haskell
mkPerson :: String -> String -> Maybe Person
mkPerson n a = Person <$> mkName n <*> mkAddress a
```

::: details Monadic `mkPerson`
The same function as above can also be implemented with the monadic instance of `Maybe` and the `do`
notation, but it is a bit longer:
```haskell
mkPerson :: String -> String -> Maybe Person
mkPerson n a = do name <- mkName n
                  addr <- mkAddress a
                  return $ Person name addr
```
:::


## Monadic Parsing


A *parser* is a program that takes an input string and converts it into a data structure containing
all the information that was encoded in the input string. Most commonly, a parser can accept a source
code file and convert it into an AST (abstract syntax tree).

In this lecture we will build a parser that accepts strings of the form
```
(4.0 * (5 + 7 + x))
```
which represent numeric expressions composed of only addition and multiplication.
For simplicity we will assume that all expressions are appropriately parenthesized, so that we do
not have to worry about operator precedence.

In Haskell, we could represent an AST data structure with an `Expr a` defined like:
```haskell
data Expr a = Val a
            | Var String
            | Add [Expr a]
            | Mul [Expr a] deriving Eq
```

The example string above would be converted into the AST:
```haskell
Mul [Val 4, Add [Val 5, Val 7, Var "x"]]
```


The language grammar of the expression we consider is given below.  An expression `<expr>` is
composed of variables `<var>`, values `<val>`, additions `<add>` or multiplications `<mul>` where each
of those four can be surrounded by an arbitrary amount of spaces `<space>*`.  Variables start with a
`<lower>`case letter and then can have an arbitrary number of characters/numbers in their name.

Values can either be either integers or floating point numbers.  Integers `<int>` are defined by an
optional `-` in the front and *at least one* digit (represented by `<digit>+`).  Finally, `<add>` or
`<mul>` are parenthesized expressions with `+`/`*`.
```
<expr> -> <space>* <expr'> <space>*
<expr'> -> <var>
         | <val>
         | <add>
         | <mul>

<var> -> <lower> <alphanum>*
<val> -> <int> "." <digit>+ | <int>
<int> -> "-" <digit>+ | <digit>+ 

<add> -> "(" <expr> ("+" <expr>)+ ")"
<mul> -> "(" <expr> ("*" <expr>)+ ")"    
```


### The `Parser` type

Before we start implementing our parsers we need a good data structure that will represent them.
Essentially, we have three things that our parser must be able to do, which have to be reflected in
the type we define:

1. Parsers accept `String`s and produce trees as output.
2. Parsers can fail.
3. Parsers can return un-parsed strings as partial results.

With can start with 1.) and define a parser as a type that contains a function which accepts a
string and returns `Expr a`.
```haskell
newtype Parser a = P (String -> Expr a)
```

Next, we can include 3.) the un-parsed/rest string:
```haskell
newtype Parser a = P (String -> (Expr a, String))
```

Finally, parsers can fail, so we get:
```haskell
newtype Parser a = P (String -> Maybe (Expr a, String))
```

To conveniently access the function contained in a parser, we give it a name and arrive at the final
type definition:
```haskell
newtype Parser a = P { parse :: String -> Maybe (Expr a, String)}
```

With this datatype, and by leveraging what we know about functors, monads, and applicatives, we will
end up with a very nice way of constructing parsers that looks almost like the grammar we defined in
the beginning:
```haskell
-- <expr'> -> <var> | <val> | <add> | <mul>

expr :: Parser (Expr Float)
expr = token (var <|> val <|> op '+' <|> op '*')
```

But let's start much simpler. Our first parser will just parse a single, arbitrary character.
A `Char` parser always succedes as long as the given string is not empty. We will call this parser
`item`, because it parses an arbitrary item from the input string. To define this parser we use the
data constructor `P`, pass a function accepting a string, and return a `Maybe (Char, String)`:
```haskell
newtype Parser a = P { parse :: String -> Maybe (Expr a, String)}

item :: Parser Char
item = P (\input -> case input of
                      ""     -> Nothing
                      (x:xs) -> Just (x, xs))
```

In order to construct more complicated parsers we just have to manipulate the `parse` function
contained in `P` (to make it do the more complicated things). The function `parse` is hidden inside our new
parser type, so this is where the typeclasses `Functor`, `Applicative`, and `Monad` will come in
handy. Hence, we will make `Parser`s instances of those before starting with implementing the actual
language grammar.

#### `Functor Parser`

Let's say we want to make sure that the first character in a string is a `'c'`. Then we just need to
compose the function inside the `item` parser with `==c`, which we can do my *lifting* `==c` via
`fmap`:
```haskell
instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = P (\input ->
    case parse p input of
      Nothing      -> Nothing
      Just (v,out) -> Just (f v, out))

𝝺> parse (fmap (=='c') item) "cde"
Just (True, "de")
```
In this case `fmap` runs a given parser `p`, and, if it succedes, applies the function `f` to the
resulting value that was parsed.

#### `Applicative Parser`

To lift n-ary functions we already know that we will need `Applicative`.
In our parser context, `<*>` will accept a parser who's parse function returns itself a function,
i.e. an `Expr (a -> b)`:
```haskell
parse :: String -> Maybe (Expr (a -> b), String)
```
If we unpack that function we can use `fmap` to lift it to the second input which is again a parser:
```haskell
instance Applicative Parser where
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P (\inp -> case parse pg inp of
                           Nothing      -> Nothing
                           Just (g,out) -> parse (fmap g px) out)

  pure :: a -> Parser a
  pure v = P (\inp -> Just (v,inp))

𝝺> parse ((/=) <$> item <*> item) "abc"
Just (True, "c")

𝝺> parse ((/=) <$> item <*> item) "aac"
Just (True, "c")
```


#### `Monad Parser`

Most important is probably the monad instance, which will let us sequence multiple small parsers
to combine them to more complex ones. Bind (`>>=`) accepts a parser, and a function that again
returns a parser. We can use this, for example, to make sure that a parsed character is an `a`:
```haskell
instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\inp -> case parse p inp of
                           Nothing      -> Nothing
                           Just (v,out) -> parse (f v) out)

𝝺> parse (item >>= \c -> if c == 'a' then item else return ' ') "abc"
Just ('b', "c")

𝝺> parse (item >>= \c -> if c == 'a' then item else return ' ') "abc"
Just (' ', "bc")
```


#### `Alternative Parser`


Another very convenient type class we will use is `Alternative`. It allows us to try different
parsers and use the result of the parser that succeedes (this is essentially the `|` operator in the
grammar).
```haskell
instance Alternative Parser where
  empty :: Parser a
  empty = P (\_ -> Nothing)

  (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\inp -> case parse p inp of
                         Nothing -> parse q inp
                         Just (v,out) -> Just (v,out))

  -- you get for free: (we will need them in the next section)
  -- many :: Parser a -> Parser [a]
  -- many p = some p <|> pure []

  -- some :: Parser a -> Parser [a]
  -- some p = (:) <$> p <*> many p

𝝺> parse (item <|> return 'x') ""
Just ('x', "")
```


#### Parser building blocks

With the implementations of the necessary typeclasses we can finally start building some basic
parsers. First we can start with a parser that simply checks if a condition is true:
```haskell
sat :: (Char -> Bool) -> Parser Char
sat pr = item >>= \x -> if pr x then return x
                                else empty
```

::: details do-notation
You can also use do-notation to define the `sat` parser:
```haskell
sat :: (Char -> Bool) -> Parser Char
sat pr = do x <- item
            if pr x then return x
                    else empty
```
A good way to figure out how do-notation works for a new type is by assuming that the left of `<-`
will contain whatever the type `a` is that you are boxing, so in our case of `Parser a`, we will
have whatever the parser is parsing in `a`. Which makes sense!;)
:::

Next, we can build two more very simple building block parsers:
```haskell
alphaNum :: Parser Char
alpahNum = sat isAlphaNum

char :: Char -> Parser Char
char c = sat (==c)
```
`alphaNum` makes sure that a character is alpha-numeric, `char c` checks if a character is equal to
`c`.

Note that the resulting characters are still part of the parser output (not `True` like in the
beginning of the lecture):
```haskell
𝝺> parse (char 'a') "agwehj"
Just ('a',"gwehj")
```

Another useful thing to do is parse larger strings instead of single characters. We can do this by
sequencing multiple `char c` parsers.
```haskell
string :: String -> Parser String
string [] = return []
string (x:xs) = char x
                >> string xs
                >> return (x:xs)
```

We can use the `some` and `many` functions that come with the `Alternative` typeclass to
parse zero or more (`many`, corresponds to `*` in the grammar) or one or more (`some`, corresponds
to `+`):
```haskell
𝝺> parse (some (string "abc")) "abcabc_gwehj"
Just (["abc", "abc"], "_gwehj")
```


### Parsing numeric expressions

Finally, we can start implementing our language grammar. This part is what we have worked towards
during the whole lecture today. You will see that all the work in setting things up will pay off:
The parsers we define new look almost like the grammar itself!

For variables we just have to compose a parser making sure we have a lower first letter and a bunch
of alpha-numeric characters:
```haskell
-- <var> -> <lower> <alphanum>*

var :: Parser (Expr a)
var = do x <- sat isLower
         xs <- many alphaNum
         return $ Var (x:xs)
```

To parse values we start with integers, which are either positive or negative `some digit` parsers.
Then we can parse `float`s with the help of `int <|>`. The `float :: Parser Float` also `read`s
the parsed string into an actual `Float`.
```haskell
-- <int> -> "-" <digit>+ | <digit>+ 
-- <val> -> <int> "." <digit>+ | <int>

digit :: Parser Char
digit = sat isDigit

nat :: Parser String
nat = some digit

int :: Parser String
int = do char '-'
         xs <- nat
         return ('-':xs)
      <|> nat


float :: Parser Float
float = do xs <- int
           char '.'
           ys <- nat
           return $ read (xs ++ "." ++ ys)
        <|> read <$> int

val :: Parser (Expr Float)
val = Val <$> float
```

Full expressions can be surrounded by spaces, so we can define a `token` parser that discards an
arbitrary number of spaces:
```haskell
space :: Parser ()
space = many (sat isSpace) *> pure ()

token :: Parser a -> Parser a
token p = do space
             x <- p
             space
             return x

expr :: Parser (Expr Float)
expr = token (var <|> val <|> op '+' <|> op '*')
```
The above `expr` parser is the final implementation of our grammar. The only thing that we have not
defined yet are the `op` parsers:
```haskell
-- <add> -> "(" <expr> ("+" <expr>)+ ")"
-- <mul> -> "(" <expr> ("*" <expr>)+ ")"    

opCons :: Char -> [Expr a] -> Expr a
opCons '+' = Add
opCons '*' = Mul
opCons c = error $ show c ++ " is unknown op"

op :: Char -> Parser (Expr Float)
op c = do char '('
          e  <- expr
          es <- some (char c >> expr)
          char ')'
          return $ opCons c (e:es)

𝝺> parse (op '*') "(2 * 3 * 4) asdfasdf"
Just ((2.0 * 3.0 * 4.0)," asdfasdf")
```

Finally, we initialize the parsing. The only thing we need to do is call our `expr` parser and make
sure that the remaining string is empty:
```haskell
readExpr :: String -> Maybe (Expr Float)
readExpr s = case parse expr s of
  Just (e,"") -> Just e
  Just (e,_) -> Nothing
  _ -> Nothing

𝝺> readExpr "(4 * (5 + 7 + x2) * y2)"
Just (4.0 * (5.0 + 7.0 + x2) * y2)
```
