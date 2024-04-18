type Img = [String]

empty :: Img
empty = replicate 4 ""

zero :: Img
zero = [".##.",
        "#..#",
        "#..#",
        ".##."]

one :: Img
one =  ["...#",
        "..##",
        "...#",
        "...#"]

concat2Img :: Img -> Img -> Img
concat2Img = zipWith (\s t -> s ++ "." ++ t)

concatImgs :: [Img] -> Img
concatImgs (im:ims) = foldl concat2Img im ims
concatImgs _ = empty

numToStr :: Int -> Int -> String
numToStr n radix = if  n < radix then [chars !! n]
                   else numToStr d radix ++ [chars !! r]
                 where chars = ['0'..'9'] ++ ['A'..'F']
                       d = n `div` radix
                       r = n `mod` radix

binToImgs :: String -> [Img]
binToImgs = map (\c -> case c of
                         '0' -> zero
                         '1' -> one)

genSol :: Int -> String
genSol n = unlines strs
        where strs = concatImgs $ binToImgs $ numToStr n 2
        
main :: IO ()
main = do putStrLn "Enter integer:"
          n <- read <$> getLine
          let bin = numToStr n 2
          let ims = binToImgs bin
        --   putStrLn bin
          mapM_ putStrLn $ concatImgs ims

