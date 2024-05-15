-- Lecture 12 - State monad

-- if the following import doesn't work for you, execute the following command in the directory where this file located
-- cabal install --lib --package-env . random
-- It installs locally the package random containing the module System.Random
-- To install it globally execute:
-- cabal install --lib random

import System.Random
import Control.Monad
import State

-- Tree labelling (implicit state)
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

labelHlp :: Tree a -> Int -> (Tree (a, Int), Int)
labelHlp (Leaf x) n = (Leaf (x, n), n+1)
labelHlp (Node left right) n = let (left', n') = labelHlp left n
                                   (right', n'') = labelHlp right n'
                                in (Node left' right', n'')

labelTree' :: Tree a -> Tree (a, Int)
labelTree' t = fst (labelHlp t 0)

-- Tree labelling via monad

fresh :: State Int Int
fresh = state (\n -> (n, n+1))

label :: Tree a -> State Int (Tree (a, Int))
label (Leaf x) = do i <- fresh
                    return $ Leaf (x, i)
label (Node l r) = do l' <- label l
                      r' <- label r
                      return $ Node l' r'

labelTree :: Tree a -> Tree (a, Int)
labelTree t = evalState (label t) 0

tree :: Tree Char
tree = Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c'))

--labeledTrees = runState (mapM label (replicate 5 tree)) 0 
labeledTrees :: [Tree (Char, Int)]
labeledTrees = evalState (replicateM 5 (label tree)) 0

-- Making tree labeling more general
mapTreeM :: Monad m => (a -> m b) -> Tree a -> m (Tree b)
mapTreeM f (Leaf x) = Leaf <$> f x 
mapTreeM f (Node l r) = Node <$> mapTreeM f l <*> mapTreeM f r
-- mapTreeM f (Node l r) = do l' <- mapTreeM f l
--                            r' <- mapTreeM f r
--                            return $ Node l' r'

labelTree2 :: Tree a -> Tree (a,Int)
labelTree2 t = evalState (mapTreeM step t) 0
    where step :: a -> State Int (a, Int)
          step x = do i <- fresh
                      return (x, i) 

data Nums = Zero | One | Two | Three | Four deriving (Show, Enum)

safeFromEnum :: Int -> Maybe Nums
safeFromEnum n | n <= 4 = Just $ toEnum n
               | otherwise = Nothing

tree2 :: Tree Int
tree2 = Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4))

-- mapTreeM safeFromEnum tree2

-- Random numbers in Haskell

-- a pseudorandom generator
rand100 :: Int -> (Int, Int)
rand100 seed = (n, newseed)
    where
        newseed = (1664525 * seed + 1013904223) `mod` (2^32)
        n = (newseed `mod` 100)

randInt :: Int -> StdGen -> (Int, StdGen)
randInt m = randomR (0,m)

rand3Int :: Int -> StdGen -> ([Int], StdGen)
rand3Int m g0 = ([n1,n2,n3],g3)
    where
        (n1,g1) = randomR (0,m) g0
        (n2,g2) = randomR (0,m) g1
        (n3,g3) = randomR (0,m) g2

type R a = State StdGen a

runRandom :: R a -> Int -> a
runRandom m seed = evalState m (mkStdGen seed)

randIntS :: Int -> R Int
randIntS m = state $ randomR (0,m)

rand3IntS :: Int -> R [Int]
rand3IntS n = do n1 <- randIntS n
                 n2 <- randIntS n
                 n3 <- randIntS n
                 return [n1,n2,n3]
--rand3IntS n = replicateM 3 (randIntS n)

manyRandIntS :: Int -> R [Int]
manyRandIntS n = mapM randIntS $ repeat n

--take 10 $ runRandom (manyRandIntS 100) 1

main :: IO ()
main = do seed <- randomIO :: IO Int
          putStrLn "How many random numbers do you want?"
          n <- read <$> getLine :: IO Int
          putStrLn "Upper bound?"
          ub <- read <$> getLine :: IO Int
          let rs = take n $ runRandom (manyRandIntS ub) seed
          print rs
