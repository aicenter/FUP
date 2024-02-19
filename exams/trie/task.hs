import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

-- import Data.Map

data Trie a = Trie { terminal :: Bool
                   , trie :: Map a (Trie a)
                   } deriving (Eq, Show)

empty :: Trie a
empty = Trie True Map.empty

-- insert :: [a] -> Trie a -> Trie a
insert [] t = t
insert (x:xs) (Trie True _) = Trie False (Map.singleton x (insert xs empty))

-- insert (x:xs) (Trie False m) = case Map.lookup x m of
--                                  (Just t) -> Trie False (Map.update (\t -> Just (insert xs t)) x m)
--                                  Nothing -> Trie False (Map.singleton x (insert xs empty))


insert (x:xs) (Trie False m) = Trie False (Map.update (\t -> Just (insert xs t)) x m)


insertRec :: Ord a => [a] -> Trie a -> Trie a
insertRec [] (Trie _ m)     = Trie True m
insertRec (x:xs) (Trie e m) =
  Trie e (Map.alter (Just . insertRec xs . fromMaybe empty) x m)


tt = insert "asdf" empty

-- insert (x:xs) (Trie False m) = Trie False (hlp (Map.lookup x m)) where
--   hlp (Just t) = insert xs t
--   hlp Nothing = Trie False (Map.singleton x (insert xs empty))

-- -- insert (x:xs) (Trie e m) = 
-- 
-- -- find subtrie
-- v = Map.lookup x m
-- -- insert remainder
-- (Just t) = insert xs t
-- -- singleton map
-- Nothing = Trie False (Map.singleton x (insert xs empty))


m = Map.fromList [(1, 'a'), (2, 'b')]
