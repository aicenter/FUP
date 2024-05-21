import Data.Map (Map, empty, insertWith)
import Data.Char (isDigit)
import Data.List (sort)

makeHistogram :: String -> Map Char Int
makeHistogram = foldl update empty
    where update m ch = insertWith (+) ch 1 m

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ st [] = st
myFoldl f st (c:cs) = myFoldl f (f st c) cs

getSmallest :: [Int] -> Int
getSmallest = head . sort