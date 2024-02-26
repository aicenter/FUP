import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import FTree


splitOn :: Char -> String -> [String]
splitOn c s = case (break (== c) s) of
                (d, "") -> [d]
                (d, r) -> [d] ++ splitOn c (drop 1 r) where

split :: String -> [String]
split = splitOn '/'

insert :: [String] -> FTree String -> FTree String
insert [] tree = tree
-- recursively insert into a new, empty Map
insert (d:r) FNil =       FNode (Map.insert d (insert r FNil) Map.empty)
-- update existing tree.
-- we are not sure yet if `d` exists in the the tree
-- Two cases:
--  - d does not exist -> add (d, FNil) to tree
--  - d does exist     -> add (d, insert r (subtree at d))
-- Updating an existing value based on a function can be done with `Map.alter`
-- Map.alter wants an update function (Maybe FTree String -> Maybe FTree String)
-- This we can achieve via (fromMaybe FNil) which constructs a function that returns
-- FNil if it receives a Nothing and otherwise the Just value it receives (i.e. the subtree at d)
insert (d:r) (FNode ls) = FNode (Map.alter (Just . insert r . fromMaybe FNil) d ls)

parse :: [String] -> FTree String
parse files = foldl (\tree file -> (insert (splitOn '/' file) tree)) FNil files

exists :: String -> FTree String -> Bool
exists file tree = _exists (split file) tree where
  _exists [] tree = True
  _exists _ FNil = False
  -- if we find x in the current tree level, we want to recurse
  -- Map.lookup returns a Maybe, so we can lift `_exists xs` to work on a Maybe FTree
  _exists (x:xs) (FNode ls) = fromMaybe False (_exists xs <$> (Map.lookup x ls))


files = ["dir1/tree.hs"
        ,"dir1/complex.hs"
        ,"dir2/ex1/test.ss"
        ,"dir2/ex1/eval.ss"
        ,"dir2/emptydir"
        ,"dir3/ex2/test.ss"
        ,"dir3/test_tree.hs"]

main = do 
  let tree = parse files
  putStrLn (showTree tree)

  print $ exists "dir1/tree.hs" tree
  print $ exists "dir1/asdf.hs" tree
  print $ exists "dir2/ex1" tree
  print $ exists "dir2/ex1/eval.ss" tree
  print $ exists "scripts/ex2/eval.ss" tree

