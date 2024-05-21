module FTree (
  FTree(FNil,FNode),
  showTree,
  printTree,
  depth,
  leaves
) where

import Data.Map (Map)
import qualified Data.Map as Map

data FTree a = FNil 
             | FNode {leaves :: Map a (FTree a)} deriving (Eq,Show)

showTree FNil = ""
showTree (FNode m) = disp 3 (Map.toList m) where
    indent n s = take (n-3) [' ', ' '..] ++ "*" ++ take 3 ['-', '-'..] ++ s ++ "\n"
    disp n [] = ""
    disp n ((k,FNode vs):rls) = indent n k 
                              ++ disp (n+4) (Map.toList vs)
                              ++ disp n rls
    disp n ((k,FNil):rls) = indent n k ++ disp n rls

printTree = putStrLn . showTree

depth tree = _depth 0 tree where
  _depth d FNil = d
  _depth d (FNode m) = maximum (map (\t -> _depth (d+1) t) (Map.elems m))
