module Task4 (grid) where
import Data.List
import Data.Char (toLower)

sortDists :: [(a, Int)] -> [(a, Int)]
sortDists = sortBy (\(_,x) (_,y) -> compare x y)

manhattan (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

dists points (x,y) = map (\(c,a,b) -> (c, manhattan (x,y) (a,b))) points

character ((cx,0):_) = cx
character ((cx,_):[]) = toLower cx
character ((cx,dx):(cy,dy):_) = if dx==dy then '.' else toLower cx

grid points = [[closest (i,j) | i <- [0..w]] | j<-[0..h]] where
  closest = character . sortDists . dists points
  w = maximum (map second points)
  h = maximum (map third points)

first (x,_,_) = x
second (_,x,_) = x
third (_,_,x) = x

points :: [(Char, Int, Int)]
points = [
       ('A', 1, 1),
       ('B', 1, 6),
       ('C', 8, 3),
       ('D', 3, 4),
       ('E', 5, 5),
       ('F', 8, 9)]

-- grid points
-- > ["aaaaa.ccc"
--   ,"aAaaa.ccc"
--   ,"aaaddeccc"
--   ,"aadddeccC"
--   ,"..dDdeecc"
--   ,"bb.deEeec"
--   ,"bBb.eeee."
--   ,"bbb.eeeff"
--   ,"bbb.eefff"
--   ,"bbb.ffffF"]

