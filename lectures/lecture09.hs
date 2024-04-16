-- Lecture 10
{-# LANGUAGE RecordWildCards #-}

data Vector a = Vec { x::a, y::a, z::a } deriving Show

isZero :: (Eq a, Num a) => Vector a -> Bool
isZero Vec{x=0,y=0,z=0} = True
isZero _ = False

last :: Vector a -> a
last Vec{z=w} = w

norm :: Floating a => Vector a -> a
norm Vec{..} = sqrt (x^2 + y^2 + z^2)

-- Read instance with type annotations
n = read "3" :: Int
d = read "3" :: Double
ns = read "[1,2,3]" :: [Int]
p = read "(1,\"abc\")" :: (Int, String)

add :: Int -> String -> Int
add x s = x + read s

data Test a = One | Two a deriving (Show, Read)

-- Example case expression
describeList :: [a] -> String
describeList xs = "The list is "
                  ++ case xs of
                      [] -> "empty."
                      [_] -> "a singleton list."
                      _ -> "a longer list."

describeList2 :: [a] -> String
describeList2 xs = "The list is " ++ what xs
    where what [] = "empty."
          what [_] = "a singleton list."
          what _ = "a longer list."

-- Type constructor Tree and its Functor instance
data Tree a = Tree a [Tree a] deriving Show

tree :: Tree Int
tree = Tree 1 [Tree 2 [Tree 3 []], Tree 4 []]

instance Functor Tree where
    fmap f (Tree x []) = Tree (f x) []
    fmap f (Tree x ts) = Tree (f x)
                              (map (fmap f) ts)

addOne :: (Functor f, Num a) => f a -> f a
addOne = fmap (+1)

-- Maybe as Functor
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just (head xs)

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

add1ToHead :: [Int] -> Int
add1ToHead = (+1) . head

safeAdd1ToHead :: [Int] -> Maybe Int
safeAdd1ToHead = fmap (+1) . safeHead

-- Maybe - composing failing computation
second :: [a] -> a
second = head . tail

andThen :: Maybe a -> (a -> Maybe b) -> Maybe b
andThen Nothing _ = Nothing
andThen (Just x) f = f x

safeSecond :: [a] -> Maybe a
safeSecond xs = safeTail xs `andThen` safeHead

safeFourth :: [a] -> Maybe a
safeFourth xs =
  safeTail xs `andThen`
  safeTail `andThen`
  safeTail `andThen`
  safeHead

andThenE :: Either e a -> (a -> Either e b) -> Either e b
andThenE (Left err) _ = Left err
andThenE (Right x) f = f x

safeHeadE :: [a] -> Either String a
safeHeadE [] = Left "No head exists."
safeHeadE (x:_) = Right x

safeTailE :: [a] -> Either String [a]
safeTailE [] = Left "No tail exists."
safeTailE (_:xs) = Right xs

safeSecondE :: [a] -> Either String a
safeSecondE xs = safeTailE xs `andThenE` safeHeadE

table :: [(String,String)]
table = [("first","John"),("second","Smith"),("address","Prague")]
table' :: [(String,String)]
table' = [("second","Smith"),("address","Prague")]

liftBinary :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
liftBinary f x y = case (x,y) of
    (Just u, Just v) -> Just $ f u v
    _ -> Nothing

wholeName :: [(String,String)] -> Maybe String
wholeName tb = op f s
    where f = lookup "first" tb
          s = lookup "second" tb
          join s1 s2 = s1 ++ " " ++ s2
          op = liftBinary join

wholeName' :: [(String,String)] -> Maybe String
wholeName' tb = lookup "first" tb `andThen`
          \f -> lookup "second" tb `andThen`
          \s -> Just $ join f s
    where join s1 s2 = s1 ++ " " ++ s2

