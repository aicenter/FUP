module Task4 (findPath, commonAncestor, Tree(..)) where

data Tree a = Leaf a
            | Node a (Tree a) (Tree a) deriving (Eq,Show)

tree = Node 1 (Node 2 (Leaf 5) (Leaf 6)) (Node 3 (Leaf 4) (Leaf 7))
tree2 = Node 1 (Node 2 (Leaf 3)
                       (Node 4 (Leaf 5)
                               (Leaf 6)))
               (Node 7 (Leaf 8)
                       (Leaf 9))

findPath :: Eq a => a -> Tree a -> [a]
findPath x (Leaf y) | x == y = [x]
                    | otherwise = []
findPath x (Node y l r) | x == y = [x]
                        | otherwise = let pl = findPath x l
                                          pr = findPath x r
                                      in if null (pl ++ pr) then []
                                         else y:(pl++pr)

commonAncestor :: Eq a => a -> a -> Tree a -> Maybe a
commonAncestor x y t | null common = Nothing 
                     | otherwise = Just $ last common 
    where px = findPath x t
          py = findPath y t
          common = [x | (x,y) <- zip px py, x==y ]
          