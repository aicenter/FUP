-- Lecture 11 - IO and monads
import System.IO
import Data.List

-- IO monad
getSquare :: IO Int
getSquare = putStrLn "Enter number:"
            >> getLine
            >>= \line -> let n = read line
                         in return (n*n)
-- getSquare = putStrLn "Enter number:"
--            >> fmap ( (^2) . read ) getLine

-- Maybe monad
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just (head xs)

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

safeSecond :: [a] -> Maybe a
safeSecond xs = safeTail xs >>= safeHead

sumFirstTwo :: Num a => [a] -> Maybe a
sumFirstTwo xs = safeHead xs
                 >>= \first -> safeSecond xs
                 >>= \second ->
                     return (first + second)

-- IO Separation 

-- From Maybe we can get the value 
getMaybe :: Maybe Int -> Int
getMaybe (Just x) = x
getMaybe _ = 0

-- For Maybe we can use data constructors to lift a function
addOneM :: Maybe Int -> Maybe Int
addOneM (Just x) = Just (x+1)
addOneM Nothing = Nothing

-- For IO there is no data constructor
succIO :: IO Char -> IO Char
succIO = fmap succ

-- Do notation
getSquare2 :: IO Int
getSquare2 = do putStrLn "Enter number:"
                line <- getLine
                let n = read line
                return (n*n)

-- List monad
subsets' :: [a] -> [[a]]
subsets' [] = [[]]
subsets' (l:ls) = do
    rest <- subsets' ls
    [rest, l:rest]

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (l:ls) = subsets ls
                 >>= \rest -> [rest, l:rest]

g xs ys = do x <- xs
             y <- ys
             [x,y]

-- Monadic sequencing 
seqM :: Monad m => [m a] -> m [a]
seqM [] = return []
seqM (m1:ms) = m1
               >>= \x -> seqM ms
               >>= \xs -> return (x:xs)

ioActions :: [IO ()]
ioActions = [print "Hello!",
             putStrLn "just kidding",
             getChar >>= putChar]

-- Monadic map
safeDiv :: (Eq a, Fractional a) => a -> a -> Maybe a
safeDiv x y = case y of
                0 -> Nothing
                _ -> Just (x/y)

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' _ [] = return []
mapM' f (x:xs) = do y <- f x
                    rest <- mapM' f xs
                    return (y:rest)
                --  >>= \y -> mapM' f xs
                --  >>= \rest -> return (y:rest)

mapM'_ :: Monad m => (a -> m b) -> [a] -> m ()
mapM'_ _ [] = return ()
mapM'_ f (x:xs) = f x
                  >> mapM'_ f xs
                  >> return ()

-- Complete program
newtype Pol a = Pol [a]

toPol :: String -> Pol Float
toPol = Pol . map read . words

readPol :: Int -> IO (Pol Float)
readPol n = putStrLn ("Enter " ++ show n ++ ". polynomial:")
            >> toPol <$> getLine

instance (Eq a, Num a, Show a) => Show (Pol a) where
    show (Pol []) = "0"
    show (Pol xs) = intercalate " + " [ disp n y | (n, y) <- zip [0..] xs, y /= 0 ]
                  where disp 0 y = show y
                        disp n y = show y ++ "*x^" ++ show n

addPol :: Num a => Pol a -> Pol a -> Pol a
addPol p@(Pol xs) q@(Pol ys) | length xs < length ys = addPol q p
                             | otherwise = Pol $ zipWith (+) xs ys ++ drop (length ys) xs

sumPol :: Num a => [Pol a] -> Pol a
sumPol = foldl addPol (Pol [])

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

main :: IO ()
main = do putStrLn "How many polynomials?"
          n <- read <$> getLine
          ps <- mapM readPol [1..n]
          putStr "Result: "
          print $ sumPol ps
          putStrLn "Continue? [y/n]"
          c <- getCh
          if c == 'y' then main
          else return ()