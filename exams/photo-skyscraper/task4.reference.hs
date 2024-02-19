module Task4 (bestView) where

import Data.List

roofs xss = sum $ inner <$> xss 
  where
    inner xs = length (group $ scanl1 max xs)

morph 'N' = transpose
morph 'S' = fmap reverse . transpose
morph 'E' = fmap reverse
morph _ = id

bestView :: [[Int]] -> (Char, Int)
bestView city = 
  let dirs = "NSEW"
      views = roofs . (`morph` city) <$> dirs
      opts = zip dirs views
  in last $ sortOn snd opts
