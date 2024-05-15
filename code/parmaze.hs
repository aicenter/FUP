import System.Environment
import Control.Parallel.Strategies
import Data.Maybe

import Parser
import Mazes

-- fs = ["maze01.txt", "maze02.txt"]
fs = ["maze-40x40-01.txt"
     ,"maze-40x40-02.txt"
     ,"maze-40x40-03.txt"
     ,"maze-40x40-04.txt"
     ,"maze-40x40-04.txt"
     ,"maze-40x40-04.txt"
     ,"maze-40x40-04.txt"
     ,"maze-40x40-04.txt"
     ,"maze-40x40-04.txt"
     ,"maze-40x40-05.txt"]


myParMap :: (a -> b) -> [a] -> Eval [b]
myParMap f [] = return []
myParMap f (a:as) = do
  b  <- rpar  (f a)
  bs <- myParMap f as
  return (b:bs)

-- myParMap :: (a -> b) -> [a] -> [b]
-- myParMap f [] = []
-- myParMap f (a:as) = runEval $ do
--   b  <- rpar (f a)
--   return (b : myParMap f as)


main :: IO ()
main = do
  fs <- getArgs
  ss <- mapM readFile fs
  let ms = catMaybes (map (parse file) ss)
      xs = runEval $ myParMap (solve . fst) ms -- `using` parList rseq
  print $ length $ filter isJust xs


-- ghc -threaded -rtsopts --make parmap.hs
-- ./parmap +RTS -N2 -s - RTS arg arg arg
