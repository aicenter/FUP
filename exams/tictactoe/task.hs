-- import Control.Monad
-- import Control.Monad.Operational
-- import Control.Monad.State

import Data.Either
import Data.List

    -- external libraries needed
-- import System.Random

data Symbol = X | O deriving (Eq,Show)
type Square = Either Int Symbol
type Board = [[Square]]
data GameState = Game { board :: Board, activePlayer :: Symbol }


instance Show GameState where
  show (Game board player) = showBoard board


initialGameState :: GameState
initialGameState = Game (map (map Left) [[1,2,3],[4,5,6],[7,8,9]]) X


showSquare (Left x) = " " ++ show x ++ " "
showSquare (Right x) = " " ++ show x ++ " "

showBoard :: Board -> String
showBoard board =
      unlines . surround "+---+---+---+"
    . map (concat . surround "|". map showSquare)
    $ board
    where
    surround x xs = [x] ++ intersperse x xs ++ [x]

printBoard = putStr . showBoard
printGame g = printBoard (board g)

    -- list the possible moves to play
possibleMoves :: Board -> [Int]
possibleMoves board = [k | Left k <- concat board]

      -- play a stone at a square
makeMove :: Int -> GameState -> Maybe GameState
makeMove k (Game board player)
    | not (k `elem` possibleMoves board) = Nothing   -- illegal move
    | otherwise = Just $ Game (map (map replace) board) (switch player)
    where
    replace (Left k') | k' == k = Right player
    replace x                   = x

    switch X = O
    switch O = X
