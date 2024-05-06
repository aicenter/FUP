module State where

-- My state
newtype State s a = S { runState:: s -> (a, s) }

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f st = S (\s -> let (x,s') = runState st s
                       in (f x,s'))

instance Applicative (State s) where
  pure :: a -> State s a
  pure x = S (\s -> (x,s))
  (<*>) :: State s (a -> b) -> State s a -> State s b
  stf <*> stx = S (\s -> let (f,s') = runState stf s
                             (x,s'') = runState stx s'
                         in (f x, s''))

instance Monad (State s) where
  (>>=) :: State s a -> (a -> State s b) -> State s b
  stx >>= f = S (\s -> let (x,s') = runState stx s
                         in runState (f x) s')

state :: (s -> (a,s)) -> State s a
state = S

evalState :: State s a -> s -> a
evalState st x = fst $ runState st x

execState :: State s a -> s -> s
execState st x = snd $ runState st x

get :: State s s
get = state (\x -> (x,x))

put :: s -> State s ()
put x = state (\_ -> ((),x))

modify :: (s -> s) -> State s ()
modify f = do x <- get
              put (f x)
              return ()
