import Data.List

type Vertex = Char
type Edge = (Vertex,Vertex)
type Weight = Int 
type Graph = [(Edge,Weight)]

graph :: Graph
graph = [(('A','B'), 7)
        ,(('A','D'), 5)
        ,(('D','B'), 9)
        ,(('C','B'), 8)
        ,(('E','B'), 7)
        ,(('C','E'), 5)
        ,(('D','E'), 15)
        ,(('D','F'), 6)
        ,(('F','E'), 8)
        ,(('F','G'), 11)
        ,(('E','G'), 9)
        ]

deduplicate :: [Vertex] -> [Vertex]
deduplicate [] = []
deduplicate (v:vs) = v:(deduplicate filtered)
    where filtered = [ u | u <- vs, u /= v ]

vertices :: Graph -> [Vertex]
vertices g = deduplicate $ concat [ [a,b] | ((a,b), _) <- g ]

sortGraph :: Graph -> Graph 
sortGraph g = sortOn snd g 

checkEdge :: Edge -> [Vertex] -> Bool
checkEdge (a,b) visit = p /= q
    where p = a `elem` visit
          q = b `elem` visit

findEdge :: [Vertex] -> Graph -> (Edge, Weight)
findEdge visit graph = head [ (e,w) | (e,w) <- graph, checkEdge e visit ]

jarnik :: Graph -> Graph
jarnik graph = iter vs [] where
  graph' = sortGraph graph 
  (_:vs) = vertices graph

  iter :: [Vertex] -> Graph -> Graph
  iter [] tree = tree
  iter visit tree = iter visit' tree' where
    edge@((a,b),_) = findEdge visit graph'
    visit'    = [ v | v <- visit, v /= a && v /= b ]
    tree'       = edge:tree
