import Data.List
import Control.Parallel (par, pseq)
import Data.Monoid
import Control.Parallel.Strategies

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
    let (ys, zs) = splitAt n xs
    in  ys : chunks n zs

integral' :: (Ord a, Floating a, Enum a) => (a -> a) -> a -> a -> a -> a
integral' f step start end = foldl' (+) 0 quads
 where
  quad (a,b) = (b-a) * f ((a+b)/2)
  -- quads = map quad (zip (init steps) (tail steps)) -- this is where we parallelize with `using`
  quads = map quad (zip (init steps) (tail steps)) `using` parListChunk 10000 rseq
  steps = [start, start+step .. end]

f x = sin x * cos x / (sinh x * cosh x)

integralchunk :: (Ord a, Floating a, Enum a) => (a -> a) -> a -> a -> a -> Int -> a
integralchunk f step start end chunksize = sum cs
 where
  cs = [sum xs | xs <- chunks chunksize quads] `using` parList rseq
  quad (a,b) = (b-a) * f ((a+b)/2)
  quads = map quad (zip (init steps) (tail steps))
  steps = [start, start+step .. end]

integralr :: (Ord a, Floating a, Enum a) => (a -> a) -> a -> a -> a -> a
integralr f step start end = foldr (+) 0 quads
 where
  quad (a,b) = (b-a) * f ((a+b)/2)
  quads = map quad (zip (init steps) (tail steps))
  steps = [start, start+step .. end]

main = do
  let s = 0.0000001
  print $ integralchunk f s 0 (4*pi) 100
  --print $ inte f s 0 (4*pi)
