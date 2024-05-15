import Data.List
import Data.Monoid
import Control.Parallel (par, pseq)
import Control.Parallel.Strategies
import Control.Exception
import Data.Time.Clock
import Text.Printf
import System.Environment

-- <<main
main = do
  [n] <- getArgs
  let s = 0.000001
      r = case (read n) of
            1 -> integral f s 0 (4*pi)
            2 -> integralchunk f s 0 (4*pi) 1000
  t0 <- getCurrentTime
  print r
  printTimeSince t0

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
    let (ys, zs) = splitAt n xs
    in  ys : chunks n zs

f x = sin x * cos x / (sinh x * cosh x)

integralchunk :: (Ord a, Floating a, Enum a) => (a -> a) -> a -> a -> a -> Int -> a
integralchunk f step start end chunksize = sum cs
 where
  cs = [sum xs | xs <- chunks chunksize quads] `using` parList rseq
  quad (a,b) = (b-a) * f ((a+b)/2)
  quads = map quad (zip (init steps) (tail steps))
  steps = [start, start+step .. end]

integral :: (Ord a, Floating a, Enum a) => (a -> a) -> a -> a -> a -> a
integral f step start end = foldr (+) 0 quads
 where
  quad (a,b) = (b-a) * f ((a+b)/2)
  quads = map quad (zip (init steps) (tail steps))
  steps = [start, start+step .. end]


printTimeSince t0 = do
  t1 <- getCurrentTime
  printf "time: %.4fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)
