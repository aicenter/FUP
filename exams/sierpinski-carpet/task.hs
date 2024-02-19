empty :: Int -> [String]
empty n = replicate n $ replicate n ' '

-- join :: [[String]] -> [String]
-- join frs = [ concatMap (!! i) frs | i <- [0..length (head frs) - 1] ]

generateFractal :: Int -> [String]
generateFractal 0 = ["#"]
generateFractal n = upper ++ middle ++ upper
  where fr = generateFractal (n-1)
        size = length fr
        upper = zipWith3 (\x y z -> x ++ y ++ z) fr fr fr --join $ replicate 3 fr
        middle = zipWith3 (\x y z -> x ++ y ++ z) fr (empty size) fr --join [fr, empty size, fr]

cut :: Int -> Int -> [a] -> [a]
cut st end = take (end-st) . drop st

main :: IO ()
main = do putStrLn "Enter n:"
          n <- read <$> getLine
          let fr = generateFractal n
          putStrLn "Enter row1 row2 col1 col2:"
          [r1,r2,c1,c2] <- map read . words <$> getLine
          mapM_ (putStrLn . cut c1 c2) $ cut r1 r2 fr

