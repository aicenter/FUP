module Mazes where

import Control.Applicative
import Data.Foldable
import Data.Char
import Parser

data Block = W | F | S deriving (Eq,Show)
data Maze = M [[Block]]
type Pos = (Int, Int)
type Path = [Pos]
type Task = (Pos,Pos,Maze)

instance Show Maze where
    show (M []) = ""
    show (M (r:rs)) = map dispBlock r ++ "\n" ++ show (M rs)
       where dispBlock W = '#'
             dispBlock F = ' '
             dispBlock S = '*'


safePut :: Int -> a -> [a] -> Maybe [a]
safePut n x xs | 0 <= n && n < length xs = Just $ take n xs ++ [x] ++ drop (n+1) xs
               | otherwise = Nothing

safeGet :: Int -> [a] -> Maybe a
safeGet n xs | n `elem` [0..length xs-1] = Just $ xs !! n
             | otherwise = Nothing

getBlock :: Pos -> Maze -> Maybe Block
getBlock (x,y) (M xss) = do row <- safeGet y xss
                            block <- safeGet x row
                            return block

setBlock :: Block -> Pos -> Maze -> Maybe Maze
setBlock b (x,y) (M xss) = do row <- safeGet y xss
                              row' <- safePut x b row
                              xss' <- safePut y row' xss
                              return (M xss')

setPath :: Maze -> Path -> Maybe Maze
setPath = foldrM (setBlock S)

drawSol :: Maze -> Path -> Maze
drawSol m ps = case setPath m ps of
                 Nothing -> m
                 Just m' -> m'


neighbs :: Pos -> [Pos]
neighbs (x,y) = [(x-1,y), (x+1,y), (x,y-1), (x,y+1),
                 (x-1,y-1), (x-1,y+1), (x+1,y-1), (x+1,y+1)]

nextPos :: Pos -> Maze -> [Pos]
nextPos p m = case getBlock p m of
                -- is the input position admissible?
                -- if yes, take all possibilities and filter admissible positions
                Just F -> [ p' | p' <- neighbs p, getBlock p' m == Just F]
                _ -> []

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
    -- | otherwise = bfs (p:visited) (paths ++ runEval (rpar (extend path m))) q m
    | otherwise = bfs (p:visited) (paths ++ extend path m) q m

extend :: Path -> Maze -> [Path]
extend [] _ = []
extend path@(p:_) m = map (:path) $ nextPos p m





---------------------------------------------------------------------------------------------------
  -- Parse a maze

digit :: Parser Char
digit = sat isDigit

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             x <- p
             space
             return x

pos :: Parser Pos
pos = do char '('                -- it has to start with '('
         space                   -- possibly followed by spaces
         x <- some digit         -- then parses a nonempty sequence of digits
         token (char ',')        -- then comma possible surrounded by spaces
         y <- some digit         -- then a second non-empty sequence of digits
         space                   -- possibly spaces
         char ')'                -- then the closing ')'
         return (read x, read y) -- the position is returned, sequences of digits are converted by read

def :: String -> Parser Pos
def str = do string str
             token (char '=')
             p <- pos
             char '\n'
             return p

wall :: Parser Block
wall = do char '#'
          return W
-- wall = char '#' *> pure W -- Applicative approach

free :: Parser Block
free = do char ' '
          return F

row :: Parser [Block]
row = do bs <- many (wall <|> free)
         char '\n'
         return bs
-- row = many (wall <|> free) <* char '\n' -- Applicative approach

mapP :: Parser Maze
mapP = do rs <- many row
          return (M rs)
-- mapP = M <$> many row -- Functor approach

file :: Parser Task
file = do p <- def "start"
          q <- def "goal"
          m <- mapP
          return (p,q,m)
-- Applicative approach
-- file = (,,) <$> def "start"
--            <*> def "goal"
--            <*> mapP

maze :: Maze   -- a testing maze
maze = M [[W,W,W,W,W],
          [W,F,W,F,W],
          [W,F,W,W,W],
          [W,F,F,F,W],
          [W,W,W,W,W]]


