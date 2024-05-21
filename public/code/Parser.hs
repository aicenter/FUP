module Parser where

import Control.Applicative
import Data.Char

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
    -- many p = some p <|> pure []
    -- some p = (:) <$> p <*> many p

-- Parsers
item :: Parser Char
item = P (\inp -> case inp of
                    "" -> Nothing
                    (x:xs) -> Just (x,xs))

sat :: (Char -> Bool) -> Parser Char
sat pr = item >>= \x -> if pr x then return x
                        else empty

alphaNum :: Parser Char
alphaNum = sat isAlphaNum

char :: Char -> Parser Char
char c = sat (== c)

string :: String -> Parser String
string "" = pure ""
string s@(x:xs) = char x *> string xs *> pure s 

sep :: Parser ()
sep = some (sat isSpace) *> pure ()
