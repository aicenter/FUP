-- average matrix w h = 

transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

mat :: [[Int]]
mat = [[1..4]
      ,[1..4]]

avg n = map mean . (chunks n)

mean xs = sum xs / fromIntegral (length xs)

averageBlocks matrix w h = cols (rows matrix) where
  rows = map (avg w)
  cols = transpose . map (avg h) . transpose

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
    let (ys, zs) = splitAt n xs
    in  ys : chunks n zs


f x | x==1      = "a"
    | otherwise = x
