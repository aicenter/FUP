data Piece = Nil | Knight deriving Show

enumerate :: [a] -> [(Int, a)]
enumerate xs = zip [0..] xs


isknight :: Piece -> Bool
isknight Nil = False
isknight Knight = True


knight_coords :: [[Piece]] -> [(Int, Int)]
knight_coords board = map (\(i,j,_) -> (i,j)) ij_ks where 

  ij_ks = filter (\(_,_,k) -> isknight k) (concat ij_xs)
  ij_xs = map (\(i, jxs) -> _insert i jxs) (enumerate (map enumerate board))

  _insert :: a -> [(b,c)] -> [(a,b,c)]
  _insert x = map (\(y,z) -> (x,y,z))



is_valid_pair :: (Int,Int) -> (Int,Int) -> Bool
is_valid_pair (x,y) (u,v) | ( (abs (x-u)) == 2 && (abs (y-v)) == 1 ) = False
                          | ( (abs (x-u)) == 1 && (abs (y-v)) == 2 ) = False
                          | otherwise = True


is_valid :: [[Piece]] -> Bool
is_valid board = all (uncurry is_valid_pair) [(x,y) | x <- cs, y <- cs] where
  cs = knight_coords board


board =  [[Knight, Nil, Nil ,Knight],
          [Nil, Nil, Nil, Knight],
          [Knight, Nil, Nil, Nil],
          [Nil, Knight, Nil, Nil]]

board2 = [[Nil, Knight, Nil, Nil],
          [Nil, Nil, Nil, Knight],
          [Knight, Nil, Nil, Nil],
          [Nil, Nil, Knight, Nil]]

