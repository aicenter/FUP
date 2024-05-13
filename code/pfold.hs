import Data.List
import Control.Parallel (par, pseq)
import Data.Monoid
import Control.Parallel.Strategies

{-
import Control.Parallel
import Data.List

pfold :: (Num a, Enum a) => (a -> a -> a) -> [a] -> a
pfold _ [x] = x
pfold mappend xs  = (ys `par` zs) `pseq` (ys `mappend` zs) where
  len = length xs
  (ys', zs') = splitAt (len `div` 2) xs
  ys = pfold mappend ys'
  zs = pfold mappend zs'

main = print $ pfold (+) [ foldl' (*) 1 [1..x] | x <- [1..5000] ]
  -- need a more complicated computation than (+) of numbers
  -- so we produce a list of products of many numbers
-}

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
    let (ys, zs) = splitAt n xs
    in  ys : chunks n zs


data Integrate a = I {func :: (a -> a), val :: a} | V {val :: a}

instance Fractional a => Semigroup (Integrate a) where
  (V x) <> (I f y) = V ((y-x) * f ((x+y)/2))
  (V x) <> (V y) = V (x+y)
  _ <> _ = error "asdf"

instance Fractional a => Monoid (Integrate a) where
  mempty = V 0

parallelFold chunkSize xs = foldl' (par. pseq. mappend) mempty ys where
  ys = (map (foldl' mappend mempty) (chunks chunkSize xs))

integral :: (Ord a, Floating a, Enum a) => (a -> a) -> a -> a -> a -> a
integral f step start end = foldl' (+) 0 quads
 where
  quad (a,b) = (b-a) * f ((a+b)/2)
  -- quads = map quad (zip (init steps) (tail steps)) -- this is where we parallelize with `using`
  quads = map quad (zip (init steps) (tail steps)) `using` parListChunk 10000 rseq
  steps = [start, start+step .. end]

f x = sin x * cos x / (sinh x * cosh x)

inte :: (Ord a, Floating a, Enum a) => (a -> a) -> a -> a -> a -> a
inte f step start end = foldr (+) 0 quads
 where
  quad (a,b) = (b-a) * f ((a+b)/2)
  quads = map quad (zip (init steps) (tail steps))
  steps = [start, start+step .. end]

--main = do
--  let s = 0.000001
--  print $ inte f s 0 (4*pi)

main = do
  let s = 0.000001
  print $ val (parallelFold 10000 ((map (I f) [s,2*s..4*pi]) :: [Integrate Float]))
