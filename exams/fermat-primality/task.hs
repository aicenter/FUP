--module Fermat (LCG(LCG), generate_range, modulo_power, primality) where

import Control.Monad.State

---- notes: avoid Carmichael numbers in testing, since it's hard to guess whether the generator will encounter a co-prime
---- notes: describe the generator to be fully deterministic, including generate_range

---- linear congruential generator
data LCG = LCG  Int Int Int Int deriving Show -- A*x + C mod M

generate :: State LCG Int
generate = do (LCG a x c m) <- get
              let x' = (a * x + c) `mod` m
              put (LCG a x' c m)
              return x'

generate_range :: Int -> Int -> State LCG Int -- a <= x < b
generate_range a b = do x <- generate
                        let x' = (x `mod` (b-a)) + a
                        return x'   

modulo_power :: Int -> Int -> Int -> Int 
modulo_power a n m = iter a n where
    iter b 1 = b
    iter b nn = iter (b*a `mod` m) (nn-1)

fermat_comp :: Int -> Int -> Int
fermat_comp p b = (modulo_power b (p-1) p) 

fermat_check :: Int -> Int -> Int -> State LCG Bool
fermat_check p n 1 = primality p (n-1)  
fermat_check _ _ _ = return False 

primality :: Int -> Int -> State LCG Bool
primality p 0 = return True
primality p n = do b <- generate_range 1 p
                   fermat_check p n (fermat_comp p b)

