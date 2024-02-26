import Data.List

data Tree a = Leaf { val :: a }
            | Node { val :: a,
                     kids :: [Tree a] } deriving Show
type Edge a = (a,a)

tr = Node {val = 1, kids = [Leaf {val = 2},Leaf {val = 3}]}

addEdge (Leaf x) (s,t) | x == s = Node x [Leaf t]
                       | otherwise = Leaf x
addEdge (Node x kids) (s,t)
  | x == s    = Node x (sortOn val ((Leaf t):kids))
  | otherwise = Node x [addEdge n (s,t) | n <- kids]


buildTree tree edges = foldl addEdge tree edges
