import Data.List

import Data.List

type Node = Int
type Cost = Float
type Edge = (Node,Node,Cost)
type Graph = ([Node],[Edge])
type Path = [Node]

nextPos :: Node -> Graph -> [(Node,Cost)]
nextPos n g = [(y,z) | (x,y,z) <- snd g, x == n] ++ [(x,z) | (x,y,z) <- snd g, y == n]

extend :: (Path,Cost) -> Graph -> [(Path,Cost)]
extend ([],_) _ = []
extend (path@(p:_),c) m = map (\(n,c') -> (n:path,c+c')) $ nextPos p m

lowcost :: Ord b => (a,b) -> (a,b) -> Ordering
lowcost (_,x) (_,y) | x < y = LT
                    | otherwise = GT
                    
bfs :: [Node] -> [(Path,Cost)] -> Node -> Graph -> Maybe (Path, Cost)
bfs _ [] _ _ = Nothing
bfs visited upaths q m
    -- is path a solution? If yes, return the reversed solution
    | p == q = Just (reverse path, c)
    -- does path end in an already visited position? If yes, disregard it 
    | p `elem` visited = bfs visited paths q m
    | otherwise = bfs (p:visited) (paths ++ extend (path,c) m) q m 
    -- consider the first path in the queue and its head p
    where ((path@(p:_),c):paths) = sortBy lowcost upaths
          
cheapflight :: Node -> Node -> Graph -> Maybe (Path,Cost)
cheapflight s t g = bfs [] [([s],0)] t g


nodes :: [Node]
nodes = [1..6]

edges :: [Edge]
edges = [(1,2,0.5), (1,3,1.0), (2,3,2.0), (2,5,1.0), (3,4,4.0), (4,5,1.0)]

graph :: Graph
graph = (nodes,edges)

-- cheapflight 2 5 graph
-- > Just ([2,1,3], 1.5)
