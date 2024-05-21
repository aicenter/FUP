{-# LANGUAGE TupleSections #-}

-- Module illustrating a combination of State and IO monads together manually without the monad transformer StateT
-- As an illustration, it is used in the tree labelling problem where we label a tree by two labels. The first is
-- a unique integer and the second a string entered by the user.

module IOState where

-- My state
newtype IOState s a = IOS { runState:: s -> IO (a, s) }

instance Functor (IOState s) where
-- fmap :: (a -> b) -> IOState s a -> IOState s b
    fmap f st = IOS (\s -> do (x,s') <- runState st s
                              pure (f x,s'))

instance Applicative (IOState s) where
-- pure :: a -> IOState s a
    pure x = IOS (\s -> pure (x,s))
-- (<*>) :: IOState s (a -> b) -> IOState s a -> IOState s b
    stf <*> stx = IOS (\s -> do (f,s') <- runState stf s
                                (x,s'') <- runState stx s'
                                pure (f x, s''))

instance Monad (IOState s) where
-- (>>=) :: IOState s a -> (a -> IOState s b) -> IOState s b
    stx >>= f = IOS (\s -> do (x,s') <- runState stx s
                              runState (f x) s')

state :: (s -> IO (a,s)) -> IOState s a
state = IOS

evalState :: IOState s a -> s -> IO a
evalState st x = fst <$> runState st x

execState :: IOState s a -> s -> IO s
execState st x = snd <$> runState st x

get :: IOState s s
get = state (\x -> pure (x,x))

put :: s -> IOState s ()
put x = state (\_ -> pure ((),x))

modify :: (s -> s) -> IOState s ()
modify f = do x <- get
              put (f x)
              return ()

liftIO :: IO a -> IOState s a
liftIO action = IOS (\s -> fmap (,s) action)

-- Tree labelling
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

fresh :: IOState Int Int
fresh = state (\n -> pure (n, n+1))

label :: Show a => Tree a -> IOState Int (Tree (a, Int, String))
label (Leaf x) = do i <- fresh
                    liftIO $ putStrLn $ "How do you want to label " ++ show x ++ "?"
                    str <- liftIO getLine
                    return $ Leaf (x, i, str)
label (Node l r) = do l' <- label l
                      r' <- label r
                      return $ Node l' r'

labelTree :: Show a => Tree a -> IO (Tree (a, Int, String))
labelTree t = evalState (label t) 0

tree :: Tree Char
tree = Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c'))

