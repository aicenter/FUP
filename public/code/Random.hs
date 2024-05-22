{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Random (
    StdGen,
    mkStdGen,
    Random (..)
) where

newtype StdGen = Gen { gen :: Integer } deriving Show

mkStdGen :: Int -> StdGen
mkStdGen = Gen . fromIntegral 

class Random a where
    randomR :: (a,a) -> StdGen -> (a, StdGen)

-- randR :: Integral a => (a,a) -> StdGen -> (a, StdGen)
randR :: (Integer,Integer) -> StdGen -> (Integer, StdGen)
randR (l,u) g@Gen{ gen } | l > u = randR (u,l) g
                         | u == l = (l, g)
                         | otherwise = (n, Gen{ gen=gen' })
    where gen' = (6364136223846793005 * gen + 1442695040888963407) `mod` (2^64-1)
          n = l + (fromIntegral (gen' `div` 2^16) `mod` (u-l+1))

instance Random Int where
    randomR (l,u) g = (fromIntegral n, g')
        where (n, g') = randR (fromIntegral l, fromIntegral u) g

instance Random Integer where
    randomR = randR

instance Random Double where
    randomR (l,u) g | u < l = randomR (u,l) g 
                    | u == l = (l, g)
                    | otherwise = (r, g')
        where mb = 2^32 :: Integer
              (p, g') = randR (0, mb) g :: (Integer, StdGen)
              r = l + (u-l)*(fromIntegral p / fromIntegral mb)