factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

factorial2 :: Integer -> Integer
factorial2 n = iter n 1 where
    iter 0 acc = acc
    iter k acc = iter (k-1) (k*acc)

power :: Integer -> Integer -> Integer
power _ 0 = 1
power n k = n * power n (k-1)

(+/+) :: Integer -> Integer -> Integer
x +/+ y = 2*x + y
infixr 8 +/+

discr :: Float -> Float -> Float -> Float
discr a b c = x - y 
    where x = b*b
          y = 4*a*c

discr2 :: Float -> (Float -> (Float -> Float))
discr2 a b c = 
    let x = b*b
        y = 4*a*c
    in x - y

g :: Int -> Int
g x = y+z where
            y = 1+2  -- layout rule: mult-line definition of y
                +x
            z = 10   -- layout rule: block continuation 

myAbs :: Int -> Int
myAbs x | x >= 0 = x

solver :: Float -> Float -> Float -> String
solver a b c | d >= 0 = let f1 x y z = ((-y) + sqrt z) / 2*x -- let can be inside guarded equations
                            f2 x y z = ((-y) - sqrt z) / 2*x -- let can contain also function definitions
                        in show (f1 a b d, f2 a b d)
             | d < 0 = "Complex roots"
    where d = discr a b c -- where applies to all guarded equations

second :: (Int, Int, Int) -> Int 
second (_,x,_) = x

f :: (Int, [Char], (Int, Char)) -> [Char]
f (1, x:xs, (2,y)) = x:y:xs    
f (_, _, _) = ""

flatten :: [[Int]] -> [Int]
flatten xss = [x | xs <- xss, x <- xs]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], mod n x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]

primes :: [Int]
primes = [x | x <- [2..], prime x]

qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort bigger 
   where 
    smaller = [y | y <- xs, y <= x]
    bigger = [y | y <- xs, y > x]
