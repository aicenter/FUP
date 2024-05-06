-- Brainf*ck
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Applicative
import Parser
import System.Environment
import Data.Char
import System.IO

-- Brainfuck syntax: parser
data Cmd = Comma | Dot | MLeft | MRight | Plus | Minus | Cycle [Cmd] deriving Show
type Prg = [Cmd]

charToCmd :: Char -> Cmd
charToCmd ',' = Comma
charToCmd '.' = Dot
charToCmd '<' = MLeft
charToCmd '>' = MRight
charToCmd '+' = Plus
charToCmd '-' = Minus

program :: Parser Prg
program = many (cmd <|> loop)

cmd :: Parser Cmd
cmd = charToCmd <$> (char ',' <|> char '.' <|> char '<' <|> char '>' <|> char '+' <|> char '-')

loop :: Parser Cmd
loop = Cycle <$> (char '[' *> program <* char ']')

readPrg :: String -> Prg
readPrg s = case parse program s of
    Just (p, "") -> p
    Just (p, _) -> error "Unused input"
    Nothing -> error "Incorrect program"

-- Example program
addPrg :: Prg
-- ",>,[-<+>]<."
addPrg = [Comma, MRight, Comma, Cycle [Minus, MLeft, Plus, MRight], MLeft, Dot]

-- Brainfuck semantics: Tape and command
data Dir = L | R
data Tape a = T { left :: [a], val :: a, right :: [a] }

freshTape :: Int -> Tape Int
freshTape len = T { left = [],
                    val = 0,
                    right = replicate len 0 }

instance Show a => Show (Tape a) where
    show T {..} = unwords (map show [reverse left, [val], right])

change :: (a -> a) -> Tape a -> Tape a
change f s@T{ val } = s{ val = f val }

move :: Dir -> Tape a -> Tape a
move L s@T{..} = s{ left = tail left,
                    val = head left,
                    right = val:right }
move R s@T{..} = s{ left = val:left,
                    val = head right,
                    right = tail right }

-- Interpreter
evalCmd :: Cmd -> StateT (Tape Int) IO ()
evalCmd MLeft = modify $ move L
evalCmd MRight = modify $ move R
evalCmd Plus = modify $ change (+1)
evalCmd Minus = modify $ change (subtract 1)
evalCmd Comma = do s <- get
                   n <- liftIO $ ord <$> getChar
                   put s{ val=n }
evalCmd Dot =   do T { val } <- get
                   liftIO $ putChar (chr val)
evalCmd (Cycle prg) = evalCycle prg

evalCycle :: Prg -> StateT (Tape Int) IO ()
evalCycle prg = do T { val } <- get
                   if val == 0 then return ()
                   else do o1 <- eval prg
                           o2 <- evalCycle prg
                           return ()

-- helper function displaying debug info
evalCmdPrint :: Cmd -> StateT (Tape Int) IO ()
evalCmdPrint cmd = do evalCmd cmd
                      s <- get
                      liftIO $ print s

eval :: Prg -> StateT (Tape Int) IO ()
eval = mapM_ evalCmd
-- eval = mapM_ evalCmdPrint -- replace the above line with this one if you want to display tape after each step

runPrg :: Prg -> IO ()
runPrg prg = evalStateT (eval prg) $ freshTape 3000

withoutEcho :: Bool -> IO () -> IO ()
withoutEcho True action = do hSetEcho stdin False
                             action
                             hSetEcho stdin True
withoutEcho _ action = action 

main :: IO ()
main = do hSetBuffering stdin NoBuffering
          hSetBuffering stdout NoBuffering
          args <- getArgs                 -- get command line arguments
          str <- readFile $ head args     
          let p = readPrg str
          let echo = length args > 1 && (args !! 1) == "-noecho" -- is there a second argument "-noecho"?
          withoutEcho echo $ runPrg p
          putStrLn "Bye!"
