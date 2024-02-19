module Task4 ( encode ) where
import Data.Char
import Data.List

normalize :: String -> String
normalize s = [toLower c | c <- s, isAlpha c]

getNumCols :: Int -> Int
getNumCols n = ceiling $ sqrt $ fromIntegral n

makeRows :: String -> Int -> [String]
makeRows s n | s == "" = []
             | len < n = [s ++ replicate (n-len) ' ']
             | otherwise = take n s:makeRows (drop n s) n
        where len = length s

encode :: String -> String
encode s = unwords rows'
        where ns = normalize s
              n = getNumCols $ length ns
              rows = makeRows ns n
              rows' = transpose rows