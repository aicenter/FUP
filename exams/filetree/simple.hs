import Data.Map (Map)
import qualified Data.Map as Map
import FTree


split :: String -> Char -> (String,String)
split s c = _split "" s c where
  _split d [] c = (d,"")
  _split d (x:xs) c | x==c    = (d,xs)
                    | otherwise = _split (d++[x]) xs c

find :: Ord k => k -> Map k (FTree a) -> FTree a
find d m = Map.findWithDefault FNil d m

branch :: String -> FTree String
branch file = _branch file where
  _branch "" = FNil
  _branch file = FNode (Map.insert d (branch r) Map.empty) where
    (d,r) = split file '/'

insert :: FTree String -> String -> FTree String
insert FNil file = branch file
insert (FNode ls) file
  | subtree == FNil = FNode (Map.insert d (branch r) ls)
  | otherwise       = FNode (Map.insert d (insert subtree r) ls)
  where subtree = Map.findWithDefault FNil d ls
        (d,r) = split file '/'

parse :: [String] -> FTree String
parse files = _parse files FNil where
  _parse [] t = t
  _parse (f:fs) t = _parse fs (insert t f)


exists :: String -> FTree String -> Bool
exists file tree = _exists (split file '/') tree where
  _exists (d,"") (FNode m) = Map.member d m
  _exists (d,r) FNil = False
  _exists (d,r) (FNode ls) = _exists (split r '/') (find d ls)

-- main = do 
--   let files = ["dir1/tree.hs"
--               ,"dir1/complex.hs"
--               ,"dir2/ex1/test.ss"
--               ,"dir2/ex1/eval.ss"
--               ,"dir2/emptydir"
--               ,"dir3/ex2/test.ss"
--               ,"dir3/test_tree.hs"]
--   let tree = parse files
--   putStrLn (showTree tree)
-- 
--   print $ exists "dir1/tree.hs" tree
--   print $ exists "dir1/asdf.hs" tree
--   print $ exists "dir2/ex1" tree
--   print $ exists "dir2/ex1/eval.ss" tree
--   print $ exists "scripts/ex2/eval.ss" tree

