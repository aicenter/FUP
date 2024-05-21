import Data.Monoid
import Data.Foldable ( foldl', foldl )
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Char ( toLower )
import Text.Read ( readMaybe )
import Data.Maybe ( fromMaybe, isJust )

m2 :: (a -> m1) -> (a -> m2) -> (a -> (m1,m2))
m2 f g = \x -> (f x, g x)

m3 :: (a -> m1) -> (a -> m2) -> (a -> m3) -> (a -> (m1,m2,m3))
m3 f g h = \x -> (f x, g x, h x)

data Min a = Min a | MinEmpty deriving (Show)

data Max a = Max a | MaxEmpty deriving (Show)

newtype Count = Count Int deriving (Show)

instance (Ord a) => Semigroup (Min a) where
  MinEmpty <> m = m
  m <> MinEmpty = m
  (Min a) <> (Min b) = Min (min a b)

instance (Ord a) => Monoid (Min a) where
  mempty = MinEmpty

instance (Ord a) => Semigroup (Max a) where
  MaxEmpty <> m = m
  m <> MaxEmpty = m
  (Max a) <> (Max b) = Max (max a b)

instance (Ord a) => Monoid (Max a) where
  mempty = MaxEmpty

instance Semigroup Count where
  (Count n1) <> (Count n2) = Count (n1+n2)

instance Monoid Count where
  mempty = Count 0

count :: a -> Count
count _ = Count 1

-- Set is a monoid under union
--Set.fromList [1..5] <> Set.fromList [3..10]

-- Map is a monoid under union as well
--Map.fromList [(1,"a")] <> Map.fromList [(1,"b")]

-- Another Monoid instance for Map
newtype MMap k v = MMap (Map.Map k v)

fromList :: Ord k => [(k,v)] -> MMap k v
fromList xs = MMap (Map.fromList xs)

singleton :: k -> v -> MMap k v
singleton k v = MMap (Map.singleton k v)

showMap :: (Show k, Show v) => Map.Map k v -> String
showMap m = "Map (\n" ++ ls ++ ")" where
  ls = concat $ map line (Map.toList m)
  line (k,v) =  " " ++ show k ++ " : " ++ show v ++"\n"

instance (Show k, Show v) => Show (MMap k v) where
  show (MMap m) = "M" ++ showMap m

instance (Ord k, Monoid v) => Semigroup (MMap k v) where
  (MMap m1) <> (MMap m2) = MMap (Map.unionWith mappend m1 m2)

instance (Ord k, Monoid v) => Monoid (MMap k v) where
  mempty = MMap Map.empty


--MMap (Map.fromList [(1,"a")]) <> MMap (Map.fromList [(1,"b")])

-- Foldable
myFoldMap :: Monoid m => (a -> m) -> [a] -> m
myFoldMap f = mconcat . map f

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

instance Foldable Tree where
    foldMap f (Leaf x) = f x
    foldMap f (Node l r) = foldMap f l <> foldMap f r

tree :: Tree Int
tree = Node (Leaf 7) (Node (Leaf 2) (Leaf 3))

-- Folding over Set
--foldMap Sum $ Set.fromList [1..10]

-- Folding with the Set monoid
--foldMap Set.singleton [1,2,4,2,1,5]

-- Folding over Map
-- foldMap id $ Map.fromList [(1,"a"),(2,"b")]

-- Folding with Map monoids
--foldMap (\x -> Map.singleton x (x^2)) [1..10]
--foldMap (\x -> MMap (Map.singleton x (count x))) [1,2,3,3,2,4,5,5,5]

-- Mean of a list
mean :: Fractional a => [a] -> a
mean xs = s / fromIntegral l where
    (Sum s,Count l) = foldMap (m2 Sum count) xs

-- filtering in folding
mfilter :: Monoid m => (a -> Bool) -> (a -> m) -> (a -> m)
mfilter pred f x = if pred x then f x else mempty

--foldMap (m2 Sum (mfilter (<=20) Sum)) [5,10,20,45.4,35,1,3.4]
--foldMap (m2 (mfilter (>0) Sum) (mfilter (<0) Sum)) [3,-2,5,-8]

-- GroupBy operation on Map 
groupBy :: (Ord k, Monoid m) => (a -> k) -> (a -> m) -> (a -> MMap k m)
groupBy keyf valuef a = singleton (keyf a) (valuef a)


ws = words $ map toLower "Size matters not. Look at me. Judge me by my size, do you? Hmm? Hmm. And well you should not. For my ally is the Force, and a powerful ally it is. Life creates it, makes it grow. Its energy surrounds us and binds us. Luminous beings are we, not this crude matter. You must feel the Force around you; here, between you, me, the tree, the rock, everywhere, yes. Even between the land and the ship."

stats word = (count word, Min $ length word, Max $ length word)
-- foldMap (groupBy head (m3 count (Min . length) (Max . length))) ws
-- 𝝺> foldMap (groupBy head stats) ws

-- Example CSV
type Record a b = [(a,b)]

splitOn :: Char -> String -> [String]
splitOn delimiter = foldr f [""]
            where f c l@(x:xs) | c == delimiter = "":l
                               | otherwise = (c:x):xs

parseRecord :: String -> Record String Float
parseRecord str = [ (k,fromMaybe 0 v) | (k,v) <- pairs, isJust v ]
  where keys = ["hw1", "time1", "hw2", "time2", "hw3", "time3", "hw4", "time4"]
        vals = map readMaybe $ tail (splitOn ',' str)
        pairs = zip keys vals

parseCSV :: [String] -> Record String Float
parseCSV lines = mconcat $ map parseRecord lines

-- getStats :: Record String Float -> Record String (Float, Float)
getStats rec = avgs
  where (MMap stats) = foldMap (groupBy fst (m3 count (Sum . snd) (Max . snd))) rec
        avgs = fmap (\(Count n, Sum s, Max m) -> (s/fromIntegral n, m)) stats

-- Lazy vs strict fold
lazyFold :: Integer
lazyFold = foldl (+) 0 [1..10^7]

strictFold :: Integer
strictFold = foldl' (+) 0 [1..10^7]

main :: IO ()
main = do lns <- lines <$> readFile "FUP-hw.csv"
          let rec = parseCSV lns
          print $ foldMap (groupBy fst (m3 count (Sum . snd) (Max . snd))) rec
          putStrLn $ showMap $ getStats rec
