import Control.Monad (replicateM)
import Data.Char (intToDigit)

slice :: Int -> Int -> [a] -> [a]
slice f t xs = drop f $ take t xs

neighbours :: Int -> Int -> [String] -> String
neighbours x y board = slice (y-1) (y+2) board >>= slice (x-1) (x+2)


sweep :: [String] -> [String]
sweep board = mapi2d pretty board where

  -- fill coordinates with numbers/*/.
  mapi2d f = zipWith (\y-> zipWith (\x e-> f e x y) [0..]) [0..]
  pretty e x y = finalizer (mines x y) e
  finalizer _ '*' = '*'
  finalizer 0 _ = '.'
  finalizer n _ = intToDigit n

  -- compute number of mines at coords
  mines x y = count (neighbours x y board)
  count xs = length $ filter (=='*') xs

readInput :: IO [String]
readInput = do
  count <- readLn
  lines <- replicateM (count) getLine
  return lines

test_board = ["..."
             ,".**"
             ,"..."]

main = do
  lines <- readInput
  putStrLn "\nSweep Result:"
  let sw = sweep lines
  mapM_ putStrLn sw
