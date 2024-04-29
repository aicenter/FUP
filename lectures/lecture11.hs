-- Lecture 11
import Control.Applicative
import Data.Char
import Data.List
import Parser

-- Example Maybe as applicative functor

validateLength :: Int -> String -> Maybe String
validateLength maxLen s = if (length s) > maxLen
                          then Nothing
                          else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = Name <$> validateLength 12 s

mkAddress :: String -> Maybe Address
mkAddress a = Address <$> validateLength 25 a

data Person = Person Name Address deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a = Person <$> mkName n <*> mkAddress a
-- It can be done also via monadic instance of Maybe. 
{-
mkPerson n a = do name <- mkName n
                  addr <- mkAddress a
                  return $ Person name addr
-}

-- definability of <*> in terms of >>=
(<<*>>) :: Monad m => m (a -> b) -> m a -> m b
x <<*>> y = x >>= \f -> fmap f y
infixl 4 <<*>>

(<<*) :: Applicative f => f a -> f b -> f a
x <<* y = (\u -> (\_ -> u)) <$> x <*> y 
infixl 4 <<*

(*>>) :: Applicative f => f a -> f b -> f b
x *>> y =  (\_ -> id) <$> x <*> y 
infixl 4 *>>

-- Monadic parsing

data Expr a = Val a
            | Var String
            | Add [Expr a]
            | Mul [Expr a] deriving Eq

instance Show a => Show (Expr a) where
    show (Val c) = show c
    show (Var s) = s
    show (Add es) = "(" ++ intercalate " + " (map show es) ++ ")"
    show (Mul es) = "(" ++ intercalate " * " (map show es) ++ ")"

{-
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
-}

var :: Parser (Expr a)
-- var = fmap Var $ (:) <$> sat isLower <*> many alphaNum
var = do x <- sat isLower
         xs <- many alphaNum
         return $ Var (x:xs)

digit :: Parser Char
digit = sat isDigit

nat :: Parser String
nat = some digit

int :: Parser String
-- int = (:) <$> char '-' <*> nat <|> nat
int = do char '-'
         xs <- nat
         return ('-':xs)
      <|> nat

float :: Parser Float
-- float = fmap read $
--     join <$> int
--          <*> (char '.' *> nat)
--     <|> int
--     where join s1 s2 = s1 ++ "." ++ s2
float = do xs <- int
           char '.'
           ys <- nat
           return $ read (xs ++ "." ++ ys)
        <|> read <$> int

space :: Parser ()
space = many (sat isSpace) *> pure ()

token :: Parser a -> Parser a
-- token p = space *> p <* space
token p = do space
             x <- p
             space
             return x

val :: Parser (Expr Float)
val = Val <$> float

expr :: Parser (Expr Float)
expr = token (var <|> val <|> op '+' <|> op '*')

opCons :: Char -> [Expr a] -> Expr a
opCons '+' = Add
opCons '*' = Mul
opCons c = error $ show c ++ " is unknown op"

op :: Char -> Parser (Expr Float)
-- op c = fmap (opCons c) $
--     (:) <$> (char '(' *> expr)
--         <*> some (char c *> expr)
--         <*  char ')'
op c = do char '('
          e <- expr
          es <- some (char c >> expr)
          char ')'
          return $ opCons c (e:es)

readExpr :: String -> Maybe (Expr Float)
readExpr s = case parse expr s of
    Just (e,"") -> Just e
    Just (e,_) -> Nothing
    _ -> Nothing
